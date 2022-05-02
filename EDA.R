#Required Packagaes - Dplyr, GGPlot2 and GGally
library(dplyr)
library(ggplot2)
library(GGally)

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
#Don't need disc_clean_date, disc_date_final, cont_date_final, 
#cont_clean_date, disc_date_pre, disc_pre_year, disc_pre_month
#we also don't need wstation_usaf, wstation_byear, 
#wstation_eyear and wstation_wban as these are identifiers

wildfires_clean <- wildfires %>% 
  select(-c('disc_clean_date', 'disc_date_final', 'cont_date_final', 
  'cont_clean_date', 'disc_date_pre','disc_pre_year', 'disc_pre_month', 
  'weather_file', 'fire_mag', 'wstation_usaf', 'wstation_byear', 
  'wstation_eyear', 'wstation_wban'))

str(wildfires_clean)

#barchart of fire sizes, heavily imbalanced, no As
ggplot(wildfires_clean) + geom_bar(aes(fire_size_class), stat = "count") +
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
  labs(title = "Causes of Fire", y = "Number of Fires", x = "Cause")

#which cause is predominant in each class
ggplot(wildfires_clean, aes(x = fire_size_class, fill = stat_cause_descr)) +
  geom_bar(position = "fill", stat = "count")

#we can see that that there's a lot of misisng/undefined. 
#Should count/remove them
sum(wildfires_clean$stat_cause_descr == "Missing/Undefined")

wildfires_clean <- wildfires_clean %>% filter(stat_cause_descr != "Missing/Undefined")

#replotting
#which cause is predominant in each class
ggplot(wildfires_clean, aes(x = fire_size_class, fill = stat_cause_descr)) +
  geom_bar(position = "fill", stat = "count")


#correlation, Caution this takes long to run
wildfires_clean %>% select(Temp_cont, Wind_cont, Hum_cont,Prec_cont, fire_size) %>% 
  ggpairs()
#Compose
wildfires_clean$Vegetation<-as.character(wildfires_clean$Vegetation)
wildfires_clean %>% select(Vegetation, fire_size_class) %>% 
  ggpairs()
wildfires_clean %>% select(stat_cause_descr, fire_size_class) %>% 
  ggpairs()
