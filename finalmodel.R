#Required Packagaes - Dplyr, GGPlot2 and GGally
library(GGally)
library(usmap)
library(ggmap)
library(tidyverse)
library(ggcorrplot)
library(FactoMineR)
library(factoextra)
library(gridExtra)
library(caret)
library(nnet)
library(randomForest)
library(forcats)
library(DALEX)



#loading the dataset
wildfires <- read.csv("../FW_Veg_Rem_Combined.csv")
nrow(wildfires)

#dropping the first two columns since they're not really relevant
wildfires <- wildfires[, -c(1, 2)]
str(wildfires)

#getting rid of missing data
wildfires <- wildfires %>% filter(weather_file != "File Not Found")
nrow(wildfires)

#turning time columns into POSIXct for better visualization
wildfires <- wildfires %>%
    mutate(discovery_date =
        as.POSIXct(disc_clean_date, format = "%m/%d/%Y"))
wildfires$discovery_date
wildfires <- wildfires %>%
    mutate(discovery_time =
        as.POSIXct(disc_date_final, format = "%m/%d/%Y %H:%M"))
wildfires$discovery_time
wildfires <- wildfires %>%
    mutate(contained_time =
        as.POSIXct(cont_date_final, format = "%m/%d/%Y %H:%M"))
wildfires$contained_time

wildfires$putout_time <-
    difftime(wildfires$contained_time, wildfires$discovery_time, units = "mins")
wildfires$putout_time

#we've now cleaned most of the data.
#Don"t need disc_clean_date, disc_date_final, cont_date_final,
#cont_clean_date, disc_date_pre, disc_pre_year, disc_pre_month
#we also don"t need wstation_usaf, wstation_byear,
#wstation_eyear and wstation_wban as these are identifiers

wildfires_clean <- wildfires %>%
  select(-c("disc_clean_date", "disc_date_final", "cont_date_final",
  "cont_clean_date", "disc_date_pre", "disc_pre_year", "disc_pre_month",
  "weather_file", "fire_mag", "wstation_usaf", "wstation_byear",
  "wstation_eyear", "wstation_wban"))

str(wildfires_clean)

#need to clean up data. fire_size_class,
#stat_cause_descr, state and vegetation and month are factors:
factors <- c("fire_size_class", "stat_cause_descr",
              "state", "Vegetation", "discovery_month")
wildfires_clean[factors] <- lapply(wildfires_clean[factors], factor)
str(wildfires_clean)


#barchart of fire sizes, heavily imbalanced, no As
wildfires_clean %>%
  group_by(fire_size_class) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = reorder(fire_size_class, (count)), y = count)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Number of Fires in Each Class",
    x = "Fire Size Class",
    y = "Number of Fires"
  ) +
  geom_label(aes(label = count)) +
  theme(axis.text.x = element_blank())

#barchart of causes
wildfires_clean %>%
  group_by(stat_cause_descr) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = reorder(stat_cause_descr, (count)), y = count)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Causes of Fire", y = "Number of Fires", x = "Cause") +
  geom_label(aes(label = count)) +
  theme(axis.text.x = element_blank())

#we can see that that there"s a lot of misisng/undefined.
#We will add them to "Miscellaneous"
sum(wildfires_clean$stat_cause_descr == "Missing/Undefined")
wildfires_clean <- wildfires_clean %>%
  filter(stat_cause_descr != "Missing/Undefined")

#there's also only 58 "Structure" and 175 "Fireworks".
#Maybe we should consider adding these to "Misc"
#but let's see if they are overrepresented in one single class type first

#which cause is predominant in each class
ggplot(wildfires_clean, aes(x = fire_size_class, fill = stat_cause_descr)) +
  geom_bar(position = "fill", stat = "count") +
  geom_label(aes(label = ..count..), stat = "count", position = "fill") +
  labs(
    x = "Fire Class",
    y = "Percentage of Causes",
    title = "Which Causes Cause Which Classes of Fires",
       fill = "Causes"
       )

wildfires_clean %>%
  count(fire_size_class, stat_cause_descr)

#fireworks and structure clearly aren't overrepresented anywhere.
#This means they can be added to misc category
wildfires_clean["stat_cause_descr"][wildfires_clean["stat_cause_descr"] == "Fireworks" | wildfires_clean["stat_cause_descr"] == "Structure"] <- "Miscellaneous"
wildfires_clean <- droplevels(wildfires_clean)
str(wildfires_clean)


#plotting count of wildfires over months
wildfires_clean %>%
  mutate(Month = factor(discovery_month, levels = month.abb)) %>%
  ggplot() + geom_bar(aes(Month, ..count..), stat = "count") +
  labs(title = "Number of fires by month", x = "Month", y = "Number of Fires")

wildfires_clean %>%
  mutate(Month = factor(discovery_month, levels = month.abb)) %>%
  group_by(Month) %>%
  summarize(Average = mean(fire_size)) %>%
  ggplot() + geom_col(aes(Month, Average)) +
  labs(title = "Size of fires by month", x = "Month", y = "Average Fire Size")


#now let's look at wildfire size
wildfires_clean %>%
  ggplot() + geom_histogram(aes(log(fire_size))) +
  labs(
    title = "Histogram of Fire Sizes",
    x = "Log(Fire Size)",
    y = "Number of Fires"
    )

##now let's look at duration (or putout time)
wildfires_clean %>%
  ggplot() +
  geom_histogram(aes(putout_time), na.rm = TRUE) +
  labs(title = "How Long it Takes to Put out a Fire") +
  xlim(0, 2400)

#boxplots of fires per month
wildfires_clean %>%
  mutate(Month = factor(discovery_month, levels = month.abb)) %>%
  group_by(Month, format(discovery_date, format = "%Y")) %>%
  summarize(fires = n()) %>%
  ggplot() + geom_boxplot(aes(x = Month, y = fires)) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Number of Fires by Month",
    y = "Number of Fires"
  )

#let's see if cause has any correlation with area burnt
wildfires_clean %>%
  ggplot() + geom_boxplot(aes(stat_cause_descr, log(fire_size))) +
  coord_flip() +
  labs(
    title = "Comparison of Fire Sizes by Cause",
    y = "Log of Fire Size",
    x = "Causes"
    )

#lightning clearly burns more on average

#checking if any noteworthy insight in vegetation and fire-size
wildfires_clean %>%
  ggplot(aes(Vegetation, log(fire_size))) +
  geom_boxplot() +
  labs(
    title = "Comparison of Fire Size by Vegetation Type",
    y = "Log of Fire Size"
    )

#vegetation 4 and 14 seem to have more chances of burning
#but there isn't a clear correaltion

#fire size by state
firedata <- wildfires_clean %>%
              group_by(state) %>%
              summarize(average_fire_size = mean(fire_size)) %>%
              arrange(desc(average_fire_size))

#fire size by state visualized
plot_usmap(data = firedata, values = "average_fire_size", color = "red") +
  scale_fill_continuous(low = "white", high = "red") +
  labs(title = "Average Fire Size by State", fill = "Average Fire Size")

#correlation, Caution this takes long to run
wildfires_clean %>%
  mutate(FireSize = log(fire_size)) %>%
  select(Temp_cont, Wind_cont, Hum_cont, Prec_cont, FireSize) %>%
  ggpairs()


colnames(wildfires_classification)
wildfires_scaled <- scale(wildfires_classification[,-c(1:4, 6)])
ggcorrplot(cor(wildfires_scaled[,-c(1:4, 6)]))

wildfires_clean %>%
  ggplot() +
    geom_density(aes(x = Temp_pre_30), color = "red") +
    geom_density(aes(x = Temp_pre_15), col = "blue") +
    geom_density(aes(x = Temp_pre_7), col = "green") +
    geom_density(aes(x = Temp_cont), col = "orange") +
    labs(title = "Comparison of Temperature Values", x = "Temp")


wildfires_clean %>%
  ggplot() +
    geom_density(aes(x = Wind_pre_30), color = "red") +
    geom_density(aes(x = Wind_pre_15), col = "blue") +
    geom_density(aes(x = Wind_pre_7), col = "green") +
    geom_density(aes(x = Wind_cont), col = "orange") +
    labs(title = "Comparison of Wind Values", x = "Wind")

wildfires_clean %>%
  ggplot()
    geom_point(aes(x = remoteness, y = fire_size))


# full lat/long map of all fires
sbbox <- make_bbox(wildfires_clean$longitude, wildfires_clean$latitude, f = .1)
ausbg <- get_map(location = sbbox, zoom = 4,
                source = "osm",
                color = "color",
                maptype = "terrain")

ausbg <- ggmap(ausbg)

ausbg +
  stat_density2d(data = wildfires_clean, aes(x = longitude, y = latitude,
                     fill = ..level..,  alpha = I(.2)),
                 size = 1, bins = 5, geom = "polygon") +
  geom_point(data = wildfires_clean,
            mapping = aes(x = longitude, y = latitude),
            color = "red", alpha = .2,
            size = wildfires_clean$fire_size / 1e20) +
  scale_fill_gradient(low = "grey50", high = "grey20") +
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = "aliceblue",
                                colour = "aliceblue"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

    
#we can see that there is a clear mismatch in classes.
#There are two options, subsampling and resampling.
#Since there's a lot of data available, we will subsample to go down to 467
#we don't need a lot of the columns in wildfires_clean when doing classification
#so we will remove them, and mutate some columns as well

wildfires_classification <- wildfires_clean %>%
  select(!c(fire_name, fire_size, latitude, longitude, putout_time,
            discovery_time, discovery_date, contained_time))


wildfires_classification %>% colnames()
wildfires_final <- wildfires_classification[,-c(8:10,12:14,16:18,20:22)]

wildfires_final <- wildfires_final %>% mutate_if(is.numeric, scale)

#dividing into training and test sets


set.seed(20220531)
index.tr <- createDataPartition(
                y = wildfires_final$fire_size_class,
                p = 0.8,
                list = FALSE
                )
                
data.tr <- wildfires_final[index.tr,]
data.te <- wildfires_final[-index.tr,]

#now, we will subsample

classBs <- data.tr %>% filter(fire_size_class == "B")
classCs <- data.tr %>% filter(fire_size_class == "C")
classDs <- data.tr %>% filter(fire_size_class == "D")
classEs <- data.tr %>% filter(fire_size_class == "E")
classFs <- data.tr %>% filter(fire_size_class == "F")
classGs <- data.tr %>% filter(fire_size_class == "G")

rows <- c(
  nrow(classBs),
  nrow(classCs),
  nrow(classDs),
  nrow(classEs),
  nrow(classFs),
  nrow(classGs)
  )

minrows <- min(rows)

indexB <- sample(nrow(classBs), size = minrows, replace = FALSE)
indexC <- sample(nrow(classCs), size = minrows, replace = FALSE)
indexD <- sample(nrow(classDs), size = minrows, replace = FALSE)
indexF <- sample(nrow(classEs), size = minrows, replace = FALSE)
indexG <- sample(nrow(classFs), size = minrows, replace = FALSE)

subsampled <- data.frame(
  rbind(classEs,
  classBs[indexB, ],
  classCs[indexC, ],
  classDs[indexD, ],
  classFs[indexF, ],
  classGs[indexG, ])
  )

subsampled %>% count(fire_size_class)

data.te$fire_size_class

#now we've subsampled the data, it's time to choose our models and train the dataset.
#from the outset, we can remove Naive Bayes from our models because we know the variables are not independent (especially the weather ones)
#logistic regression also can only be used only with two classes, for multi-class, it requires a bit too much work
#we can try four models: KNN, CART (takes too long so we'll use random forest), Neural Net, and SVM. Let's start with KNN

#KNN

trctrl <- trainControl(method = "cv", number = 10)
trgrid <- expand.grid(k = seq(from = 1, to = 15, by = 1))

mod.knn.cv <- train(
  fire_size_class ~ .,
  data = subsampled,
  method = 'knn',
  metric = 'Accuracy',
  tuneGrid = trgrid,
  trControl = trctrl
  )

final.knn <- knn3(
  data = subsampled,
  fire_size_class ~ .,
  k = mod.knn.cv$bestTune)

final.predict.knn <- predict(final.knn, newdata = data.te, type = "class")

confusionMatrix(
  as.factor(final.predict.knn),
  as.factor(data.te$fire_size_class)
  )


#Pretty low accuracy. Unimpressive. Let's try other models

trctrl <- trainControl(method = "cv", number = 10)
trgrid <- expand.grid(size = seq(from = 1, to = 10, by = 1),
                      decay = seq(from = 0.1, to = 0.5, by = 0.1))

mod.nnet <- train(
  fire_size_class ~ ., data = subsampled,
  method = "nnet",
  metric = "Accuracy",
  tuneGrid = trgrid,
  trControl = trctrl
  )

final.nnet <- nnet(
  fire_size_class ~ .,
  data = subsampled,
  size = mod.nnet$bestTune[1,1],
  decay = mod.nnet$bestTune[2])

predict.nnet <- predict(final.nnet, newdata = data.te, type = "class")

confusionMatrix(as.factor(predict.nnet), data.te$fire_size_class)

#we can now try either random forests.
#Since traditional methods aren't working,
#perhaps accuracy can be improved by random forests

trGrid <- expand.grid(mtry = seq(from = 1, to = 10, by = 1))
mod.rf <- train(
  fire_size_class ~ .,
  data = subsampled,
  metric = "Accuracy",
  method = "rf",
  trControl = trctrl,
  tuneGrid = trGrid)

final.rf <- randomForest(fire_size_class ~ .,
  data = subsampled,
  mtry = mod.rf$bestTune[1, 1],
  importance = TRUE
  )

pred.rf <- predict(final.rf, newdata = data.te)

confusionMatrix(as.factor(pred.rf), as.factor(data.te$fire_size_class))

varImpPlot(final.rf)
x_train <- select(subsampled, -fire_size_class)
y_train <- pull(subsampled, fire_size_class)

explainer_rf <- DALEX::explain(model = final.rf,
                                data = x_train,
                                y = y_train,
                                label = "Random Forest")


calculate_importance <- function(your_model_explainer, n_permutations = 10) {
  imp <- model_parts(explainer = your_model_explainer,
                     B = n_permutations,
                     type = "ratio",
                     N = NULL)
  return(imp)
}

importance_rf <- calculate_importance(explainer_rf)

library(ggplot2)
plot(importance_rf) +
  ggtitle("Mean variable-importance ratio over 10 permutations", "")

#none of the models perform that well. However, we removed plenty of
#important measures. Perhaps we should try to keep them and perform a dimension
#reduction. Using the best model from before.

wildfires.pca <- PCA(wildfires_scaled[, -c(1:4, 6)], ncp = 18, graph = TRUE)
fviz_contrib(wildfires.pca, choice = "var", axes = 1:2)
summary(wildfires.pca)
fviz_pca_biplot(wildfires.pca,
                col.ind = factor(wildfires_classification$fire_size_class))
pca <- prcomp(wildfires_scaled[, -c(1:4, 6)])

#the first four have 76% of variance. Let's use them
dimension_reduced <- cbind(
  fire_size_class = as.factor(wildfires_classification[, "fire_size_class"]),
  pca$x[, 1:4]
  ) %>%
  as.data.frame()

dimension_reduced$fire_size_class <- as.factor(
  dimension_reduced$fire_size_class
  )

set.seed(20220531)
index.tr <- createDataPartition(
                y = dimension_reduced$fire_size_class,
                p = 0.8,
                list = FALSE
                )
                
dim.tr <- dimension_reduced[index.tr,]
dim.te <- dimension_reduced[-index.tr,]

#now, we will subsample

dimclassBs <- dim.tr %>% filter(fire_size_class == "1")
dimclassCs <- dim.tr %>% filter(fire_size_class == "2")
dimclassDs <- dim.tr %>% filter(fire_size_class == "3")
dimclassEs <- dim.tr %>% filter(fire_size_class == "4")
dimclassFs <- dim.tr %>% filter(fire_size_class == "5")
dimclassGs <- dim.tr %>% filter(fire_size_class == "6")

rows <- c(
  nrow(dimclassBs),
  nrow(dimclassCs),
  nrow(dimclassDs),
  nrow(dimclassEs),
  nrow(dimclassFs),
  nrow(dimclassGs)
  )

minrows <- min(rows)

dimindexB <- sample(nrow(dimclassBs), size = minrows, replace = FALSE)
dimindexC <- sample(nrow(dimclassCs), size = minrows, replace = FALSE)
dimindexD <- sample(nrow(dimclassDs), size = minrows, replace = FALSE)
dimindexF <- sample(nrow(dimclassFs), size = minrows, replace = FALSE)
dimindexG <- sample(nrow(dimclassGs), size = minrows, replace = FALSE)

dimred.tr <- data.frame(
  rbind(dimclassEs,
  dimclassBs[dimindexB, ],
  dimclassCs[dimindexC, ],
  dimclassDs[dimindexD, ],
  dimclassFs[dimindexF, ],
  dimclassGs[dimindexG, ])
  )

dimred.te %>% count(fire_size_class)

trctrl <- trainControl(method = 'cv', number = 10)

trgrid <- expand.grid(size = seq(from = 1, to = 10, by = 1),
                      decay = seq(from = 0.1, to = 0.5, by = 0.1))

dim.nn <- train(
  fire_size_class ~ .,
  data = dimred.tr,
  method = "nnet",
  tuneGrid = trgrid,
  trControl = trctrl,
  metric = "Accuracy"
  )

reduced.mod <- nnet(
  fire_size_class ~ .,
  data = dimred.tr,
  size = dim.nn$bestTune[1,1],
  decay = dim.nn$bestTune[2]
  )

reduced.pred <- predict(reduced.mod, newdata = dim.te, type = "class")

confusionMatrix(as.factor(reduced.pred), as.factor(dim.te$fire_size_class))


#the best model is NN but none of them perform well.
#So now, we'll try and predict just two classes but use only NN

#merging classes
wildfires_classification$fire_size_class <-
  fct_collapse(
    wildfires_classification$fire_size_class,
    BC = c("B","C"),
    DE = c("D","E"),
    FG = c("F", "G")
    )

wildfires_classification <- droplevels(wildfires_classification)

wildfires_reduced <- wildfires_classification[,-c(8:10,12:14,16:18,20:22)]
wildfires_reduced <- wildfires_classification %>% mutate_if(is.numeric, scale)
str(wildfires_reduced)

data.tr.re <- wildfires_reduced[index.tr, ]
data.te.re <- wildfires_reduced[-index.tr, ]

#now, we will subsample

classBCs <- data.tr.re %>% filter(fire_size_class == "BC")
classDEs <- data.tr.re %>% filter(fire_size_class == "DE")
classFGs <- data.tr.re %>% filter(fire_size_class == "FG")

reducedrows <- c(nrow(classBCs), nrow(classDEs), nrow(classFGs))
reducedminrows <- min(reducedrows)

indexBC <- sample(nrow(classBCs), size = reducedminrows, replace = FALSE)
indexFG <- sample(nrow(classFGs), size = reducedminrows, replace = FALSE)

reduced <- data.frame(rbind(classDEs, classBCs[indexBC, ], classFGs[indexFG, ]))

trctrl <- trainControl(method = 'cv', number = 10)
trgrid <- expand.grid(size = seq(from = 1, to = 10, by = 1),
                      decay = seq(from = 0.1, to = 0.5, by = 0.1))

reduced.nnet <- train(
  fire_size_class ~ ., data = reduced,
  method = 'nnet',
  metric = "Accuracy",
  tuneGrid = trgrid,
  trControl = trctrl
  )

final.reduced.nnet <- nnet(
  fire_size_class ~ ., data = reduced,
  size = reduced.nnet$bestTune[1,1],
  decay = reduced.nnet$bestTune[2])

finalpredict.nnet <- predict(
  final.reduced.nnet,
  newdata = data.te.re,
  type = "class"
  )

confusionMatrix(as.factor(finalpredict.nnet), data.te.re$fire_size_class)

#in short, we can conclude that alone, temperature, vegetation, remoteness and state are not good enough for a triage.
#We need more data to fit the model better. Some surprising learnings : vegetation does not seem to matter for fire_size_class prediction
#temperature in the past 30 days and precipitation also don't have an impact.
#research indicates that droughts are the biggest predictors so instead of precipitation in the last 30, maybe we need to go further backwards





