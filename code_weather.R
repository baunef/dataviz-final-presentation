setwd("C:/Users/Lukas Gräb/Desktop/Studium/Master/Telling Stories and Data Visualization in R")

install.packages("tidyverse")

library(tidyverse)
library(lubridate)
library(padr)
library(labeling)
library(ggpubr)
library(cowplot)
library(gridExtra)

#### LOAD AND CLEAN DATA; CREATE SEPARATE DATASETS FOR EACH PROFILE ####

# LOAD DATA
netflixdata            <- data.frame(read.csv("ViewingActivity.csv"))
weatherdata_munich     <- data.frame(read.csv("weatherdata_munich.csv"))
weatherdata_gießen     <- data.frame(read.csv("weatherdata_gießen.csv"))
weatherdata_dillenburg <- data.frame(read.csv("weatherdata_dillenburg.csv"))

# INSERT ONE ROW PER PROFILE FOR 01.01.2020 AND 31.01.2023 WITH Start.Time = 2020-01-01 00:00:00 AND Duration = 00:00:00 TO MAKE SURE THE FULL RANGE OF INTEREST IS COVERED
netflixdata <- netflixdata %>% add_row(Profile.Name = "Lukas & Larissa", Start.Time = "2020-01-01 00:00:00", Duration = "00:00:00")
netflixdata <- netflixdata %>% add_row(Profile.Name = "Lukas & Larissa", Start.Time = "2023-01-31 00:00:00", Duration = "00:00:00")
netflixdata <- netflixdata %>% add_row(Profile.Name = "Lukas & Nina",    Start.Time = "2020-01-01 00:00:00", Duration = "00:00:00")
netflixdata <- netflixdata %>% add_row(Profile.Name = "Lukas & Nina",    Start.Time = "2023-01-31 00:00:00", Duration = "00:00:00")
netflixdata <- netflixdata %>% add_row(Profile.Name = "Tommy & Lea",     Start.Time = "2020-01-01 00:00:00", Duration = "00:00:00")
netflixdata <- netflixdata %>% add_row(Profile.Name = "Tommy & Lea",     Start.Time = "2023-01-31 00:00:00", Duration = "00:00:00")

# SORT ROWS BY DATE
netflixdata <- netflixdata[order(netflixdata$Start.Time),]

# REMOVE TIME IN NETFLIXDATA, SO ONLY THE DAY REMAINS
netflixdata$Start.Time <- gsub("[0-9][0-9]:[0-9][0-9]:[0-9][0-9]", "", netflixdata$Start.Time)

# REMOVE ENTRIES IN NETFLIXDATA FROM BEFORE 2020 AND AFTER JANUARY 2023 USING FILTER-FUNCTION FROM DPLYR PACKAGE
netflixdata <- netflixdata %>% filter(netflixdata$Start.Time > "2020-01-01" & netflixdata$Start.Time < "2023-02-01")

# CONVERT "Duration" IN NETFLIXDATA FROM HH:MM:SS TO SECONDS USING PACKAGE LUBRIDATE, THEN TO HOURS BY DIVIDING BY 60 AND 60
netflixdata$Duration <- lubridate::hms(netflixdata$Duration)
netflixdata$Duration <- period_to_seconds(netflixdata$Duration)
netflixdata$Duration <- (netflixdata$Duration/60)
netflixdata$Duration <- (netflixdata$Duration/60)

# CONVERT "tsun" IN WEATHERDATA FROM MINUTES TO HOURS BY DIVIDING ALL ENTRIES BY 60
weatherdata_munich$tsun     <- (weatherdata_munich$tsun/60)
weatherdata_gießen$tsun     <- (weatherdata_gießen$tsun/60)
weatherdata_dillenburg$tsun <- (weatherdata_dillenburg$tsun/60)

# CREATE SEPARATE DATASETS FOR EACH ACCOUNT (PROFILE "Gast 2" HAS NO ENTRIES - HAS NEVER BEEN USED)
netflixdata_LukasLarissa <- filter(netflixdata, Profile.Name == "Lukas & Larissa")
netflixdata_LukasNina    <- filter(netflixdata, Profile.Name == "Lukas & Nina")
netflixdata_TommyLea     <- filter(netflixdata, Profile.Name == "Tommy & Lea")

# ADD UP ALL DURATIONS THAT HAPPENED ON THE SAME DAYS (https://stackoverflow.com/questions/69588883/how-to-sum-values-in-one-column-based-on-values-in-other-columns-r)
netflixdata_full         <- netflixdata              %>% group_by(Start.Time) %>% summarize(summed_duration = sum(Duration))
netflixdata_LukasLarissa <- netflixdata_LukasLarissa %>% group_by(Start.Time) %>% summarize(summed_duration = sum(Duration))
netflixdata_LukasNina    <- netflixdata_LukasNina    %>% group_by(Start.Time) %>% summarize(summed_duration = sum(Duration))
netflixdata_TommyLea     <- netflixdata_TommyLea     %>% group_by(Start.Time) %>% summarize(summed_duration = sum(Duration))

# CONVERT TIBBLES TO DATAFRAME
netflixdata_full         <- as.data.frame(netflixdata_full)
netflixdata_LukasLarissa <- as.data.frame(netflixdata_LukasLarissa)
netflixdata_LukasNina    <- as.data.frame(netflixdata_LukasNina)
netflixdata_TommyLea     <- as.data.frame(netflixdata_TommyLea)

# FORMAT COLUMN Start.Time AS DATES
netflixdata_full         <- mutate(netflixdata_full,         Start.Time = as.Date(Start.Time, format = "%Y-%m-%d"))
netflixdata_LukasLarissa <- mutate(netflixdata_LukasLarissa, Start.Time = as.Date(Start.Time, format = "%Y-%m-%d"))
netflixdata_LukasNina    <- mutate(netflixdata_LukasNina,    Start.Time = as.Date(Start.Time, format = "%Y-%m-%d"))
netflixdata_TommyLea     <- mutate(netflixdata_TommyLea,     Start.Time = as.Date(Start.Time, format = "%Y-%m-%d"))

# ADD MISSING DATES IN CASE THAT ON SOME DAYS NO ONE WATCHED ANYTHING ON NETFLIX (https://stackoverflow.com/questions/16742725/adding-missing-rows)
netflixdata_full         <- netflixdata_full         %>% pad %>% fill_by_value(value)
netflixdata_LukasLarissa <- netflixdata_LukasLarissa %>% pad %>% fill_by_value(value)
netflixdata_LukasNina    <- netflixdata_LukasNina    %>% pad %>% fill_by_value(value)
netflixdata_TommyLea     <- netflixdata_TommyLea     %>% pad %>% fill_by_value(value)

# COMBINE NETFLIXDATA AND WEATHERDATA (RAINFALL & SUNTIME) IN NEW DATAFRAMES
# FULL- AND GAST1 DATASETS USE WEATHERDATA FROM MUNICH, THE OTHER PROFILES USE WEATHERDATA SPECIFIC TO THEIR LOCATION OF RESIDENCE
combined_data_full         <- data.frame(netflixdata_full$Start.Time,         netflixdata_full$summed_duration,         weatherdata_munich$tmax,     weatherdata_munich$prcp,     weatherdata_munich$tsun)
combined_data_LukasLarissa <- data.frame(netflixdata_LukasLarissa$Start.Time, netflixdata_LukasLarissa$summed_duration, weatherdata_munich$tmax,     weatherdata_munich$prcp,     weatherdata_munich$tsun)
combined_data_LukasNina    <- data.frame(netflixdata_LukasNina$Start.Time,    netflixdata_LukasNina$summed_duration,    weatherdata_gießen$tmax,     weatherdata_gießen$prcp,     weatherdata_gießen$tsun)
combined_data_TommyLea     <- data.frame(netflixdata_TommyLea$Start.Time,     netflixdata_TommyLea$summed_duration,     weatherdata_dillenburg$tmax, weatherdata_dillenburg$prcp, weatherdata_dillenburg$tsun)

# CHANGE COLUMN NAMES
colnames(combined_data_full)         <- c("date", "watchtime", "max_temperature", "rainfall", "suntime")
colnames(combined_data_LukasLarissa) <- c("date", "watchtime", "max_temperature", "rainfall", "suntime")
colnames(combined_data_LukasNina)    <- c("date", "watchtime", "max_temperature", "rainfall", "suntime")
colnames(combined_data_TommyLea)     <- c("date", "watchtime", "max_temperature", "rainfall", "suntime")

# CHANGE MISSING DATE VALUES FROM NA TO 0
combined_data_full         ["watchtime"][is.na(combined_data_full["watchtime"])]         <- 0
combined_data_LukasLarissa ["watchtime"][is.na(combined_data_LukasLarissa["watchtime"])] <- 0
combined_data_LukasNina    ["watchtime"][is.na(combined_data_LukasNina["watchtime"])]    <- 0
combined_data_TommyLea     ["watchtime"][is.na(combined_data_TommyLea["watchtime"])]     <- 0

#### END ####


#### LINEAR REGRESSION & PLOTS ####

# LINEAR REGRESSION WATCHTIME/SUNTIME FOR PROFILE LUKAS & LARISSA
lm(formula = watchtime ~ suntime, data = combined_data_LukasLarissa) # RESULT: NO STATISTICALLY SIGNIFICANT CORRELATION

p01 <- ggplot(combined_data_LukasLarissa, aes(x = suntime, y = watchtime))          +
  theme_minimal()                                                           +
  geom_point(size = 0.75)                                                   +
  ggtitle(label = "Lukas & Larissa (Munich)")                               +
  theme(plot.title = element_text(hjust = 0.5, size = 11))                  +
  scale_x_continuous(name = "", limits = c(0, 16))                          +
  scale_y_continuous(name = "", limits = c(0, 15))                          +
  geom_smooth(method = "lm", se = FALSE, col = "orange", fullrange = TRUE)


# LINEAR REGRESSION WATCHTIME/SUNTIME FOR PROFILE LUKAS & NINA
lm(formula = watchtime ~ suntime, data = combined_data_LukasNina) # RESULT: NO STATISTICALLY SIGNIFICANT CORRELATION

p02 <- ggplot(combined_data_LukasNina, aes(x = suntime, y = watchtime))             +
  theme_minimal()                                                           +
  theme(axis.text.y = element_blank())                                      +
  geom_point(size = 0.75)                                                   +
  ggtitle(label = "Lukas & Nina (Gießen)")                                  +
  theme(plot.title = element_text(hjust = 0.5, size = 11))                  +
  scale_x_continuous(name = "Suntime in Hours", limits = c(0, 16))          +
  scale_y_continuous(name = "", limits = c(0, 15))                          +
  geom_smooth(method = "lm", se = FALSE, col = "orange", fullrange = TRUE)


# LINEAR REGRESSION WATCHTIME/SUNTIME FOR PROFILE TOMMY & LEA
lm(formula = watchtime ~ suntime, data = combined_data_TommyLea) # RESULT: NO STATISTICALLY SIGNIFICANT CORRELATION

p03 <- ggplot(combined_data_TommyLea, aes(x = suntime, y = watchtime))              +
  theme_minimal()                                                           +
  theme(axis.text.y = element_blank())                                      +
  geom_point(size = 0.75)                                                   +
  ggtitle(label = "Tommy & Lea (Dillenburg)")                               +
  theme(plot.title = element_text(hjust = 0.5, size = 11))                  +
  scale_x_continuous(name = "", limits = c(0, 16))                          +
  scale_y_continuous(name = "", limits = c(0, 15))                          +
  geom_smooth(method = "lm", se = FALSE, col = "orange", fullrange = TRUE)


# LINEAR REGRESSION WATCHTIME/MAX_TEMPERATURE FOR PROFILE LUKAS & LARISSA
lm(formula = watchtime ~ max_temperature, data = combined_data_LukasLarissa) # RESULT: NO STATISTICALLY SIGNIFICANT CORRELATION

p04 <- ggplot(combined_data_LukasLarissa, aes(x = max_temperature, y = watchtime))  +
  theme_minimal()                                                           +
  geom_point(size = 0.75)                                                   +
  ggtitle(label = "")                                                       +
  scale_x_continuous(name = "", limits = c(0, 35))                          +
  scale_y_continuous(name = "Watchtime in Hours", limits = c(0, 15))        +
  geom_smooth(method = "lm", se = FALSE, col = "red", fullrange = TRUE)


# LINEAR REGRESSION WATCHTIME/MAX_TEMPERATURE FOR PROFILE LUKAS & NINA
lm(formula = watchtime ~ max_temperature, data = combined_data_LukasNina) # RESULT: NO STATISTICALLY SIGNIFICANT CORRELATION

p05 <- ggplot(combined_data_LukasNina, aes(x = max_temperature, y = watchtime))     +
  theme_minimal()                                                           +
  theme(axis.text.y = element_blank())                                      +
  geom_point(size = 0.75)                                                   +
  ggtitle(label = "")                                                       +
  scale_x_continuous(name = "Maximum Temperature in °C", limits = c(0, 35)) +
  scale_y_continuous(name = "", limits = c(0, 15))                          +
  geom_smooth(method = "lm", se = FALSE, col = "red", fullrange = TRUE)


# LINEAR REGRESSION WATCHTIME/MAX_TEMPERATURE FOR PROFILE TOMMY & LEA
lm(formula = watchtime ~ max_temperature, data = combined_data_TommyLea) # RESULT: NO STATISTICALLY SIGNIFICANT CORRELATION

p06 <- ggplot(combined_data_TommyLea, aes(x = max_temperature, y = watchtime))      +
  theme_minimal()                                                           +
  theme(axis.text.y = element_blank())                                      +
  geom_point(size = 0.75)                                                   +
  ggtitle(label = "")                                                       +
  scale_x_continuous(name = "", limits = c(0, 35))                          +
  scale_y_continuous(name = "", limits = c(0, 15))                          +
  geom_smooth(method = "lm", se = FALSE, col = "red", fullrange = TRUE)


# LINEAR REGRESSION WATCHTIME/RAINFALL FOR PROFILE LUKAS & LARISSA
lm(formula = watchtime ~ rainfall, data = combined_data_LukasLarissa) # RESULT: NO STATISTICALLY SIGNIFICANT CORRELATION

p07 <- ggplot(combined_data_LukasLarissa, aes(x = rainfall, y = watchtime))         +
  theme_minimal()                                                           +
  geom_point(size = 0.75)                                                   +
  ggtitle(label = "")                                                       +
  scale_x_continuous(name = "", limits = c(0, 45))                          +
  scale_y_continuous(name = "", limits = c(0, 15))                          +
  geom_smooth(method = "lm", se = FALSE, col = "blue", fullrange = TRUE)


# LINEAR REGRESSION WATCHTIME/RAINFALL FOR PROFILE LUKAS & NINA
lm(formula = watchtime ~ rainfall, data = combined_data_LukasNina) # RESULT: NO STATISTICALLY SIGNIFICANT CORRELATION

p08 <- ggplot(combined_data_LukasNina, aes(x = rainfall, y = watchtime))            +
  theme_minimal()                                                           +
  theme(axis.text.y = element_blank())                                      +
  geom_point(size = 0.75)                                                   +
  ggtitle(label = "")                                                       +
  scale_x_continuous(name = "Rainfall in mm", limits = c(0, 45))            +
  scale_y_continuous(name = "", limits = c(0, 15))                          +
  geom_smooth(method = "lm", se = FALSE, col = "blue", fullrange = TRUE)


# LINEAR REGRESSION WATCHTIME/RAINFALL FOR PROFILE TOMMY & LEA
lm(formula = watchtime ~ rainfall, data = combined_data_TommyLea) # RESULT: NO STATISTICALLY SIGNIFICANT CORRELATION

p09 <- ggplot(combined_data_TommyLea, aes(x = rainfall, y = watchtime))             +
  theme_minimal()                                                           +
  theme(axis.text.y = element_blank())                                      +
  geom_point(size = 0.75)                                                   +
  ggtitle(label = "")                                                       +
  scale_x_continuous(name = "", limits = c(0, 45))                          +
  scale_y_continuous(name = "", limits = c(0, 15))                          +
  geom_smooth(method = "lm", se = FALSE, col = "blue", fullrange = TRUE)

#### END ####


#### CREATE GROUPED PLOT ####

final_plot <- ggarrange(p01, p02, p03, p04, p05, p06, p07, p08, p09, 
                        ncol = 3, nrow = 3)

annotate_figure(final_plot, top = text_grob("The Influence of Weather on Time spent on Netflix", face = "bold", size = 15))

#### END ####