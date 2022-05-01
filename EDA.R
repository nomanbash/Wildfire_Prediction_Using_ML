#Required Packagaes - Dplyr and GGPlot2
library(dplyr)
library(ggplot2)

#loading the dataset
wildfires <- read.csv("FW_Veg_Rem_Combined.csv")
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
#we also don't need wstation_usaf, wstation_byear, wstation_eyear and wstation_wban as these are identifiers

wildfires_clean <- wildfires %>% select(-c('disc_clean_date', 'disc_date_final', 'cont_date_final', 'cont_clean_date', 'disc_date_pre','disc_pre_year', 'disc_pre_month', 'weather_file', 'fire_mag', 'wstation_usaf', 'wstation_byear', 'wstation_eyear', 'wstation_wban'))

str(wildfires_clean)

#histogram of fire sizes, heavily imbalanced, no As
ggplot(wildfires_clean) + geom_histogram(aes(fire_size_class), stat = "count") +
labs(title = "Histogram of Fire Sizes",
    x = "Fire Size Class",
    y = "Number of Fires"
)
ggplot(wildfires_clean) + geom_histogram(aes(stat_cause_descr), stat = "count")
