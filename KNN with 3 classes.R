library(dplyr)
library(ggplot2)
library(GGally)
library(usmap)
library(ggmap)
library(ggcorrplot)
#loading the dataset
wildfires <- read.csv("E:/trim6/Machine Learning in Business Analytics/Project/Datasets/wildfire/wildfire.csv")
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
#barchart of fire sizes, heavily imbalanced, no As
ggplot(wildfires_clean) +
  geom_bar(aes(fire_size_class), stat = "count") +
  coord_flip() +
  labs(title = "Histogram of Fire Sizes",
       x = "Fire Size Class",
       y = "Number of Fires"
  )
#barchart of causes
wildfires_clean %>%
  group_by(stat_cause_descr) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = reorder(stat_cause_descr, (count)), y = count)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Causes of Fire", y = "Number of Fires", x = "Cause") +
  geom_label(aes(label = count))
#we can see that that there"s a lot of misisng/undefined.
#Should count/remove them
sum(wildfires_clean$stat_cause_descr == "Missing/Undefined")
wildfires_clean <- wildfires_clean %>%
  filter(stat_cause_descr != "Missing/Undefined")
#there's also only 58 "Structure" and 175 "Fireworks".
#Maybe we should consider adding these to "Misc"
#but let's see if they are overrepresented in one single class type first
#which cause is predominant in each class
ggplot(wildfires_clean, aes(x = fire_size_class, fill = stat_cause_descr)) +
  geom_bar(position = "fill", stat = "count") +
  geom_label(aes(label = ..count..), stat = "count", position = "fill")
wildfires_clean %>%
  count(fire_size_class, stat_cause_descr)
#fireworks and structure clearly aren't overrepresented anywhere.
#This means they can be added to misc category
wildfires_clean["stat_cause_descr"][wildfires_clean["stat_cause_descr"] == "Fireworks" | wildfires_clean["stat_cause_descr"] == "Structure"] <- "Miscellaneous"
#plotting count of wildfires over months
wildfires_clean %>%
  mutate(Month = factor(discovery_month, levels = month.abb)) %>%
  ggplot() + geom_bar(aes(Month, ..count..), stat = 'count')
#now let's look at wildfire size
wildfires_clean %>%
  ggplot() + geom_histogram(aes(log(fire_size)))
##now let's look at duration (or putout time)
wildfires_clean %>%
  ggplot() + geom_histogram(aes(log(as.numeric(putout_time))), na.rm = TRUE)
#boxplots of fires per month
wildfires_clean %>%
  mutate(Month = factor(discovery_month, levels = month.abb)) %>%
  group_by(Month, format(discovery_date, format = "%Y")) %>%
  summarize(fires = n()) %>%
  ggplot() + geom_boxplot(aes(x = Month, y = fires)) +
  scale_y_continuous(labels = scales::comma)
#let's see if cause has any correlation with area burnt
wildfires_clean %>%
  ggplot() + geom_boxplot(aes(stat_cause_descr, log(fire_size))) +
  coord_flip()
#lightning clearly burns more on average
#checking if any noteworthy insight in vegetation and fire-size
wildfires_clean %>%
  ggplot(aes(Vegetation, log(fire_size))) + geom_boxplot()
#fire size by state
firedata <- wildfires_clean %>%
  group_by(state) %>%
  summarize(average_fire_size = mean(fire_size)) %>%
  arrange(desc(average_fire_size))
#fire size by state visualized
plot_usmap(data = firedata, values = "average_fire_size", color = "red") +
  scale_fill_continuous(low = "white", high = "red")
#correlation, Caution this takes long to run
wildfires_clean %>%
  select(Temp_cont, Wind_cont, Hum_cont, Prec_cont, fire_size) %>%
  ggpairs()
# full lat/long map of all fires
sbbox <- make_bbox(wildfires_clean$longitude, wildfires_clean$latitude, f = .1)
ausbg = get_map(location = sbbox, zoom = 4,
                source = "osm",
                color = "color",
                maptype = "terrain")
ausbg = ggmap(ausbg)
ausbg +
  stat_density2d(data = wildfires_clean, aes(x = longitude, y=latitude, 
                                             fill = ..level..,  alpha = I(.2)),
                 size = 1, bins = 5, geom = "polygon") +
  geom_point(data = wildfires_clean, mapping = aes(x = longitude, y = latitude),
             color = "red", alpha = .2, size = wildfires_clean$fire_size / 1e20) +
  scale_fill_gradient(low = "grey50", high = "grey20") +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background = element_rect(fill = "aliceblue",
                                        colour = "aliceblue"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none")


colnames(wildfires_clean)

wildfires_clean %>% ggplot(aes(fire_size_class)) + geom_bar() + geom_text(stat = 'count', 
                                                                          aes(label = ..count..), hjust = -0.5) + coord_flip()

wildfires_clean$fire_size_class <- as.factor(wildfires_clean$fire_size_class)
levels(wildfires_clean$fire_size_class)[c(5,4,3)] <-"small"
levels(wildfires_clean$fire_size_class)[c(2,4)] <-"medium"
wildfires_classification <- wildfires_clean %>% select(!c(fire_name, fire_size, latitude, longitude, 
                                                          putout_time, discovery_time, discovery_date, contained_time))

library(ggcorrplot)
library(FactoMineR)
library(factoextra)
library(gridExtra)

colnames(wildfires_classification)
wildfires_classification$Vegetation <- as.factor(wildfires_classification$Vegetation)
write.table(wildfires_classification,file="E:/trim6/Machine Learning in Business Analytics/Project/Datasets/wildfire/demo.csv",
            sep=",", append=FALSE)
wildfires_scaled <- scale(wildfires_classification[,-c(1,2,3,4,6)])
ggcorrplot(cor(wildfires_scaled[,-c(1:4)]))

wildfires.pca <- PCA(wildfires_scaled[,-c(1:4)], ncp = 18, graph = TRUE)
fviz_contrib(wildfires.pca, choice = "var", axes = 1)

fviz_pca_biplot(wildfires.pca,
                col.ind = factor(wildfires_classification$fire_size_class))

#from the plot, we can see wind_pre_15, hum_pre_7 and temp_cont are better predictors. We can remove the other temp variables for ease of statistical techniques

wildfires_final <- wildfires_classification[,-c(7,8,9,11,13,14,15,16,18,19,20,21)]
colnames(wildfires_final)


library(caret)
library(rpart)

index.tr <- createDataPartition(y = wildfires_final$fire_size_class, p = 0.8, list = FALSE)
data.tr <- wildfires_final[index.tr,]
data.te <- wildfires_final[-index.tr,]

# I didn't balance the data since we are going with 3 classes






#KNN

#to use knn, we need to create dummy variables for state, vegetation and month



y <- wildfires_final$fire_size_class
wildfires_final.comp <- wildfires_final %>% select(!fire_size_class)
wildfires_final.num <- wildfires_final.comp %>% select(where(is.numeric)) %>% scale() %>% as.data.frame()

library(fastDummies)

wildfires_final.dumm <- wildfires_final.comp %>% select(!where(is.numeric)) %>% 
  dummy_cols(remove_first_dummy = FALSE, remove_selected_columns = TRUE)
wildfires_final.dat <- data.frame(wildfires_final.num, wildfires_final.dumm)
wildfires_final.dat$fire_size_class <- y



mod.knn <- knn3(data = data.tr.knn, fire_size_class ~ ., k = 3)
predict.knn <- predict(mod.knn, newdata = data.te.knn, type = "prob")
fire_size_pred.knn <- colnames(predict.knn)[max.col(predict.knn,ties.method="random")]

confusionMatrix(as.factor(fire_size_pred.knn), as.factor(data.te.knn$fire_size_class))

#66%






