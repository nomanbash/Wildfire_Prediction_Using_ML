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

#lastly, let's see if a spatial representation yields some insights



#correlation, Caution this takes long to run
wildfires_clean %>%
  select(Temp_cont, Wind_cont, Hum_cont, Prec_cont, fire_size) %>%
  ggpairs()
