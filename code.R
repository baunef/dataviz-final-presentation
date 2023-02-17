# R
# Sonja, Lukas, Ferdinand
# 20230216 
# DataViz

rm(list = ls())

# Packages ----------------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(lubridate)
library(dplyr)
library(stringr)
library(readr)
library(extrafont)
library(tufte)
library(ggthemes)
library(ggdark)
library(jcolors)
library(padr)
library(labeling)
library(ggpubr)
library(cowplot)
library(gridExtra)

# Functions ---------------------------------------------------------------

# Convert episode number from sesason-based to total
episode_number <- function(s, e, show) {
  sum(show[1:s,2]) + e
} 

# Extract the full episode number for entire show data frame
long_extract <- function(my_show) {
  max_ep <- NULL
  for (s in sort(unique(my_show$Season))) {
    max_ep <- c(max_ep, max(my_show$Episode[which(my_show$Season == s)]) )
  }
  ep_stat <- data.frame(season = 0:max(my_show$Season), max_ep = c(0, max_ep))
  
  long_episodes <- NULL
  for (i in 1:nrow(my_show)) {
    long_episodes <- c(long_episodes, episode_number(my_show$Season[i], my_show$Episode[i], ep_stat))
  }
  long_episodes
}

# Create data frame for a specific show 
binge_me <- function(act_shows, show_name) {
  my_show <- act_shows %>% 
    filter(Show==show_name) %>% filter(Duration > 120) %>% 
    select(-c(Show, Country, Episode.Title)) %>% 
    type_convert()
  
  my_show$End.Time <- my_show$Start.Time + my_show$Duration
  
  my_show$long_episode <- long_extract(my_show)
  
  my_show
}



# Graphic Parameters ------------------------------------------------------

mygraphics <- theme_minimal() + theme(legend.position="top", 
                                      legend.title = element_blank(), 
                                      plot.title = element_text(size=20, hjust = 0.5),
                                      legend.text = element_text(size=14),
                                      axis.title = element_text(size = 14), 
                                      axis.text = element_text(size = 14), 
                                      plot.caption = element_text(size=14)
                                      )

# Data Import -------------------------------------------------------------

ferdi <- data.frame(read.csv("./data/ferdi/ViewingActivity.csv")) %>% 
  select(-c(Attributes, Device.Type, Supplemental.Video.Type)) %>% 
  type_convert()
sonja <- data.frame(read.csv("./data/sonja/ViewingActivity.csv")) %>% 
  select(-c(Attributes, Device.Type, Supplemental.Video.Type)) %>% 
  type_convert()
lukas <- data.frame(read.csv("./data/lukas/ViewingActivity.csv")) %>% 
  select(-c(Attributes, Device.Type, Supplemental.Video.Type)) %>% 
  type_convert()

activity <- rbind(ferdi, sonja, lukas)


# Favorite Shows ----------------------------------------------------------


## Preprocessing ----------------------------------------------------------

act_shows <- rbind(ferdi, sonja, lukas) %>%
  filter(str_detect(Title, "hook_|Trailer|Teaser|Clip | Clip:|shortpreview_|Coming Soon: |Hook [:digit:]|(Clip)|Recap:|(Rückblick)|DIRECT_LIFT|Inciting Incident:|CLM 4| Thematic|Bumper", TRUE)) %>%
  filter(str_detect(Title, "Season [:digit:]|Staffel [:digit:]|Episode [:digit:]|Folge [:digit:]")) %>%
  tidyr::separate(Title, c("Show", "Season"), sep=": Season |: Staffel |: Stranger Things |: Limit|: Tiger King |: Series |: Part |: Minis|: Volume |: Ausgabe |: Teil |: Collection|: Buch ", extra="merge") %>%
  tidyr::separate(Season, c("Season", "Episode.Title"), sep=": ", extra="merge") %>%
  tidyr::separate(Episode.Title, c("Episode.Title", "Episode"), sep=" \\(Episode |Episode |\\(Folge ", extra="merge") %>%
  tidyr::separate(Episode, "Episode", sep="\\)", extra="drop")

act_shows$Season[act_shows$Season=="ed Series"] <- 0
act_shows$Season[act_shows$Season=="erie"] <- 0
act_shows$Season[act_shows$Season=="4 Remix"] <- 4

act_shows$Show[act_shows$Show=="Das Büro"] <- "The Office (U.S.)"

act_shows$Season = replace_na(as.integer(act_shows$Season), 0)


## All Profiles -----------------------------------------------------------

mystart <- as_datetime("2019-11-01")
myend   <- as_datetime("2023-01-01")

fav_shows_all <- act_shows %>% 
  filter(Start.Time > mystart) %>% filter(Start.Time < myend) %>%
  aggregate(Duration ~ Show, sum) %>% arrange(desc(Duration))

fav_shows_ind <- act_shows %>% 
  filter(Start.Time > mystart) %>% filter(Start.Time < myend) %>%
  aggregate(Duration ~ Profile.Name + Show, sum) %>% arrange(desc(Duration))

fav_shows_ind <- left_join(fav_shows_ind, fav_shows_all, by="Show") %>% rename(Duration = Duration.x, Duration_ALL = Duration.y)

plot_favs <- fav_shows_ind %>% arrange(desc(Duration_ALL)) %>% filter(Show %in% fav_shows_all[1:20,]$Show) %>%
  ggplot(aes(x = fct_reorder(Show, Duration_ALL), y = as.numeric(Duration)/3600/24)) + 
  geom_bar(stat='identity', aes(fill=Profile.Name), position="stack") + 
  coord_flip(ylim=c(0,60)) + 
  labs(x="", y="Duration / days",
       title=paste("Most watched shows by profile between", format(mystart, "%d %b '%y"), " and ", format(myend, "%d %b '%y")),
       caption="Source: Netflix") + 
  mygraphics + 
  scale_fill_manual(values = colorRampPalette(brewer.pal(11, "Set3"))(22)) + guides(fill = guide_legend(nrow = 1))


## w/o Tommy & Lea --------------------------------------------------------

fav_shows_no_tl <- act_shows %>% filter(Profile.Name != "TL") %>% 
  filter(Start.Time > mystart) %>% filter(Start.Time < myend) %>%
  aggregate(Duration ~ Show, sum) %>% arrange(desc(Duration))

fav_shows_ind_no_tl <- act_shows %>% filter(Profile.Name != "TL") %>% 
  filter(Start.Time > mystart) %>% filter(Start.Time < myend) %>%
  aggregate(Duration ~ Profile.Name + Show, sum) %>% arrange(desc(Duration))

fav_shows_ind_no_tl <- left_join(fav_shows_ind_no_tl, fav_shows_no_tl, by="Show") %>% rename(Duration = Duration.x, Duration_ALL = Duration.y)

plot_favs_no_tl <- fav_shows_ind_no_tl %>% arrange(desc(Duration_ALL)) %>% filter(Show %in% fav_shows_no_tl[1:20,]$Show) %>%
  ggplot(aes(x = fct_reorder(Show, Duration_ALL), y = as.numeric(Duration)/3600/24)) + 
  geom_bar(stat='identity', aes(fill=Profile.Name), position="stack") + 
  coord_flip(ylim=c(0,60)) +
  labs(x="", y="Duration / days",
       title=paste("Most watched shows by profile between", format(mystart, "%d %b '%y"), " and ", format(myend, "%d %b '%y")),
       caption="Source: Netflix") + 
  mygraphics + 
  scale_fill_manual(values = colorRampPalette(brewer.pal(11, "Set3"))(22)) + guides(fill = guide_legend(nrow = 1))


# Favorite Seasons --------------------------------------------------------

mystart <- as_datetime("2019-11-01")
myend   <- as_datetime("2023-01-01")

fav_shows_season <- act_shows %>% 
  filter(Start.Time > mystart) %>% filter(Start.Time < myend) %>%
  aggregate(Duration ~ Season + Show, sum) 

fav_shows_max <- act_shows %>% 
  filter(Start.Time > mystart) %>% filter(Start.Time < myend) %>%
  aggregate(as.integer(Season) ~ Show, n_distinct) %>% setNames(c("Show", "Season_MAX")) %>% arrange(desc(Season_MAX))

fav_shows_season <- left_join(fav_shows_season, fav_shows_max, by="Show")

fav_shows_all <- act_shows %>% 
  filter(Start.Time > mystart) %>% filter(Start.Time < myend) %>%
  aggregate(Duration ~ Show, sum) %>% arrange(desc(Duration))

fav_shows_season <- left_join(fav_shows_season, fav_shows_all, by="Show") %>% rename(Duration = Duration.x, Duration_ALL = Duration.y)

fav_shows_season$Season <- as.integer(replace_na(fav_shows_season$Season, 0))


## Graph ------------------------------------------------------------------

plot_seasons <- fav_shows_season %>% arrange(desc(Season_MAX), Season) %>% filter(Season_MAX == 6) %>% filter(as.numeric(Duration_ALL)/3600/24 > 1) %>%
  ggplot(aes(x = Season, y = as.numeric(Duration)/as.numeric(Duration_ALL)/Season_MAX)) + 
  geom_bar(stat='identity', position="dodge", aes(fill=Show), show.legend = FALSE) + 
  facet_wrap(vars(Show), scales = "free_x") + 
  labs(x="Season", y="Normalized* Duration",
       title=paste("Most watched seasons of highly requested shows between", format(mystart, "%d %b '%y"), " and ", format(myend, "%d %b '%y")),
       caption="* Duration normalized by dividing each Show by its total watchtime and number of seasons;\nSource Netflix") + 
  mygraphics +
  scale_x_continuous("Season", breaks = 1:23) +
  scale_fill_manual(values = colorRampPalette(brewer.pal(11, "Set3"))(22))

# Progress ----------------------------------------------------------------

show_me <- "Sex Education"

dates <- c(as_datetime("2019-01-11"), as_datetime("2020-01-17"), as_datetime("2021-09-17"))
labels <- c("Release of Season 1", "Release of Season 2", "Release of Season 3")
releases <- data.frame(dates, labels)

binge_sex_ed <- binge_me(act_shows = act_shows, show_name = show_me) %>%
  ggplot(aes(x=End.Time, y=long_episode)) + 
  geom_label(data=releases, mapping=aes(x=dates, y=0, label=labels), color="gray35", nudge_x = 24*36000, hjust = 0) +
  annotate(geom="point", x=as_datetime("2019-01-11"), y=0, color="gray35", size=3) +
  annotate(geom="point", x=as_datetime("2020-01-17"), y=0, color="gray35", size=3) +
  annotate(geom="point", x=as_datetime("2021-09-17"), y=0, color="gray35", size=3) +
  geom_vline(aes(xintercept=as_datetime("2019-01-11")), color="gray35") +
  geom_vline(aes(xintercept=as_datetime("2020-01-17")), color="gray35") +
  geom_vline(aes(xintercept=as_datetime("2021-09-17")), color="gray35") +
  geom_point(aes(color=Profile.Name), size=3, alpha=.25) +
  scale_fill_manual(values = colorRampPalette(brewer.pal(11, "Set3"))(22)) + 
  labs(x="Time", y="Episodes", title = paste("Watch Pattern of ", show_me), caption = "Source: Netflix") + 
  guides(colour = guide_legend(nrow = 1)) + 
  mygraphics

show_me <- "Rick and Morty"
binge_rnm <- binge_me(act_shows = act_shows, show_name = show_me) %>%
  ggplot(aes(x=End.Time, y=long_episode)) + 
  geom_point(aes(color=Profile.Name), size=3, alpha=.25) +
  scale_fill_manual(values = colorRampPalette(brewer.pal(11, "Set3"))(22)) + 
  labs(x="Time", y="Episodes", title = paste("Watch Pattern of ", show_me), caption = "Source: Netflix") + 
  guides(colour = guide_legend(nrow = 1)) + 
  mygraphics

show_me <-  "Better Call Saul"
binge_bcs <- binge_me(act_shows = act_shows, show_name = show_me) %>%
  ggplot(aes(x=End.Time, y=long_episode)) + 
  geom_point(aes(color=Profile.Name), size=3, alpha=.25) +
  geom_step(aes(color=Profile.Name)) + 
  scale_fill_manual(values = colorRampPalette(brewer.pal(11, "Set3"))(22)) + 
  labs(x="Time", y="Episodes", title = paste("Watch Pattern of ", show_me), caption = "Source: Netflix") + 
  guides(colour = guide_legend(nrow = 1)) + 
  mygraphics

# Peak Hours --------------------------------------------------------------

activity$date <- as.Date(activity$Start.Time,format="%u")
activity$wd=wday(activity$date)

activity$tnew=format(as.POSIXct(activity$Start.Time), format = "%H") 
activity$tnew=as.numeric(activity$tnew)

peak_plot <- activity %>% group_by(wd,tnew) %>% tally() %>%
  ggplot(aes(x = as_factor(wd),y = tnew, fill = n)) + 
  geom_raster(alpha=0.87)+
  scale_y_reverse(breaks = seq(0, 24, by = 5),
                  labels=c("12 pm","5 am","10 am","3 pm","8 pm"))+
  scale_fill_jcolors_contin(palette = "pal4") +
  labs(y="",x="",fill="Viewing Activity",title="Viewing Activity by Weekday and Time", 
       caption = "Source: Netflix")+
  scale_x_discrete(labels=c("1" = "Monday",
                            "2" = "Tuesday",
                            "3" = "Wednesday",
                            "4" = "Thursday",
                            "5" = "Friday",
                            "6" = "Saturday",
                            "7" = "Sunday"))+
  mygraphics + 
  theme(axis.text.x = element_text(vjust=4),
        axis.text.y = element_text(hjust=1.5),
        plot.margin=unit(c(0.5,0.5,0.5,0), 'cm'), 
        legend.position = "right")


# Weather -----------------------------------------------------------------

ferdi <- data.frame(read.csv("./data/ferdi/ViewingActivity.csv")) %>% 
  select(-c(Attributes, Device.Type, Supplemental.Video.Type))

sonja <- data.frame(read.csv("./data/sonja/ViewingActivity.csv")) %>% 
  select(-c(Attributes, Device.Type, Supplemental.Video.Type))

lukas <- data.frame(read.csv("./data/lukas/ViewingActivity.csv")) %>% 
  select(-c(Attributes, Device.Type, Supplemental.Video.Type))

netflixdata <- rbind(ferdi, sonja, lukas)

weatherdata_munich     <- data.frame(read.csv("./data/weather/weatherdata_munich.csv"))
weatherdata_giessen     <- data.frame(read.csv("./data/weather/weatherdata_giessen.csv"))
weatherdata_dillenburg <- data.frame(read.csv("./data/weather/weatherdata_dillenburg.csv"))

## INSERT ONE ROW PER PROFILE FOR 01.01.2020 AND 31.01.2023 WITH Start.Time = 2020-01-01 00:00:00 AND Duration = 00:00:00 TO MAKE SURE THE FULL RANGE OF INTEREST IS COVERED
netflixdata <- netflixdata %>% add_row(Profile.Name = "LL", Start.Time = "2020-01-01 00:00:00", Duration = "00:00:00")
netflixdata <- netflixdata %>% add_row(Profile.Name = "LL", Start.Time = "2023-01-31 00:00:00", Duration = "00:00:00")
netflixdata <- netflixdata %>% add_row(Profile.Name = "LN",    Start.Time = "2020-01-01 00:00:00", Duration = "00:00:00")
netflixdata <- netflixdata %>% add_row(Profile.Name = "LN",    Start.Time = "2023-01-31 00:00:00", Duration = "00:00:00")
netflixdata <- netflixdata %>% add_row(Profile.Name = "TL",     Start.Time = "2020-01-01 00:00:00", Duration = "00:00:00")
netflixdata <- netflixdata %>% add_row(Profile.Name = "TL",     Start.Time = "2023-01-31 00:00:00", Duration = "00:00:00")

## SORT ROWS BY DATE
netflixdata <- netflixdata[order(netflixdata$Start.Time),]

## REMOVE TIME IN NETFLIXDATA, SO ONLY THE DAY REMAINS
netflixdata$Start.Time <- gsub("[0-9][0-9]:[0-9][0-9]:[0-9][0-9]", "", netflixdata$Start.Time)

## REMOVE ENTRIES IN NETFLIXDATA FROM BEFORE 2020 AND AFTER JANUARY 2023 USING FILTER-FUNCTION FROM DPLYR PACKAGE
netflixdata <- netflixdata %>% filter(netflixdata$Start.Time > "2020-01-01" & netflixdata$Start.Time < "2023-02-01")

## CONVERT "Duration" IN NETFLIXDATA FROM HH:MM:SS TO SECONDS USING PACKAGE LUBRIDATE, THEN TO HOURS BY DIVIDING BY 60 AND 60
netflixdata$Duration <- lubridate::hms(netflixdata$Duration)
netflixdata$Duration <- period_to_seconds(netflixdata$Duration)
netflixdata$Duration <- (netflixdata$Duration/60)
netflixdata$Duration <- (netflixdata$Duration/60)

## CONVERT "tsun" IN WEATHERDATA FROM MINUTES TO HOURS BY DIVIDING ALL ENTRIES BY 60
weatherdata_munich$tsun     <- (weatherdata_munich$tsun/60)
weatherdata_giessen$tsun     <- (weatherdata_giessen$tsun/60)
weatherdata_dillenburg$tsun <- (weatherdata_dillenburg$tsun/60)

## CREATE SEPARATE DATASETS FOR EACH ACCOUNT (PROFILE "Gast 2" HAS NO ENTRIES - HAS NEVER BEEN USED)
netflixdata_LukasLarissa <- filter(netflixdata, Profile.Name == "LL")
netflixdata_LukasNina    <- filter(netflixdata, Profile.Name == "LN")
netflixdata_TommyLea     <- filter(netflixdata, Profile.Name == "TL")

## ADD UP ALL DURATIONS THAT HAPPENED ON THE SAME DAYS (https://stackoverflow.com/questions/69588883/how-to-sum-values-in-one-column-based-on-values-in-other-columns-r)
netflixdata_full         <- netflixdata              %>% group_by(Start.Time) %>% summarize(summed_duration = sum(Duration))
netflixdata_LukasLarissa <- netflixdata_LukasLarissa %>% group_by(Start.Time) %>% summarize(summed_duration = sum(Duration))
netflixdata_LukasNina    <- netflixdata_LukasNina    %>% group_by(Start.Time) %>% summarize(summed_duration = sum(Duration))
netflixdata_TommyLea     <- netflixdata_TommyLea     %>% group_by(Start.Time) %>% summarize(summed_duration = sum(Duration))

## CONVERT TIBBLES TO DATAFRAME
netflixdata_full         <- as.data.frame(netflixdata_full)
netflixdata_LukasLarissa <- as.data.frame(netflixdata_LukasLarissa)
netflixdata_LukasNina    <- as.data.frame(netflixdata_LukasNina)
netflixdata_TommyLea     <- as.data.frame(netflixdata_TommyLea)

## FORMAT COLUMN Start.Time AS DATES
netflixdata_full         <- mutate(netflixdata_full,         Start.Time = as.Date(Start.Time, format = "%Y-%m-%d"))
netflixdata_LukasLarissa <- mutate(netflixdata_LukasLarissa, Start.Time = as.Date(Start.Time, format = "%Y-%m-%d"))
netflixdata_LukasNina    <- mutate(netflixdata_LukasNina,    Start.Time = as.Date(Start.Time, format = "%Y-%m-%d"))
netflixdata_TommyLea     <- mutate(netflixdata_TommyLea,     Start.Time = as.Date(Start.Time, format = "%Y-%m-%d"))

## ADD MISSING DATES IN CASE THAT ON SOME DAYS NO ONE WATCHED ANYTHING ON NETFLIX (https://stackoverflow.com/questions/16742725/adding-missing-rows)
netflixdata_full         <- netflixdata_full         %>% pad %>% fill_by_value(value)
netflixdata_LukasLarissa <- netflixdata_LukasLarissa %>% pad %>% fill_by_value(value)
netflixdata_LukasNina    <- netflixdata_LukasNina    %>% pad %>% fill_by_value(value)
netflixdata_TommyLea     <- netflixdata_TommyLea     %>% pad %>% fill_by_value(value)

## COMBINE NETFLIXDATA AND WEATHERDATA (RAINFALL & SUNTIME) IN NEW DATAFRAMES
## FULL- AND GAST1 DATASETS USE WEATHERDATA FROM MUNICH, THE OTHER PROFILES USE WEATHERDATA SPECIFIC TO THEIR LOCATION OF RESIDENCE
combined_data_full         <- data.frame(netflixdata_full$Start.Time,         netflixdata_full$summed_duration,         weatherdata_munich$tmax,     weatherdata_munich$prcp,     weatherdata_munich$tsun)
combined_data_LukasLarissa <- data.frame(netflixdata_LukasLarissa$Start.Time, netflixdata_LukasLarissa$summed_duration, weatherdata_munich$tmax,     weatherdata_munich$prcp,     weatherdata_munich$tsun)
combined_data_LukasNina    <- data.frame(netflixdata_LukasNina$Start.Time,    netflixdata_LukasNina$summed_duration,    weatherdata_giessen$tmax,     weatherdata_giessen$prcp,     weatherdata_giessen$tsun)
combined_data_TommyLea     <- data.frame(netflixdata_TommyLea$Start.Time,     netflixdata_TommyLea$summed_duration,     weatherdata_dillenburg$tmax, weatherdata_dillenburg$prcp, weatherdata_dillenburg$tsun)

## CHANGE COLUMN NAMES
colnames(combined_data_full)         <- c("date", "watchtime", "max_temperature", "rainfall", "suntime")
colnames(combined_data_LukasLarissa) <- c("date", "watchtime", "max_temperature", "rainfall", "suntime")
colnames(combined_data_LukasNina)    <- c("date", "watchtime", "max_temperature", "rainfall", "suntime")
colnames(combined_data_TommyLea)     <- c("date", "watchtime", "max_temperature", "rainfall", "suntime")

## CHANGE MISSING DATE VALUES FROM NA TO 0
combined_data_full         ["watchtime"][is.na(combined_data_full["watchtime"])]         <- 0
combined_data_LukasLarissa ["watchtime"][is.na(combined_data_LukasLarissa["watchtime"])] <- 0
combined_data_LukasNina    ["watchtime"][is.na(combined_data_LukasNina["watchtime"])]    <- 0
combined_data_TommyLea     ["watchtime"][is.na(combined_data_TommyLea["watchtime"])]     <- 0

#### END ####


#### LINEAR REGRESSION & PLOTS ####

## LINEAR REGRESSION WATCHTIME/SUNTIME FOR PROFILE LUKAS & LARISSA
lm(formula = watchtime ~ suntime, data = combined_data_LukasLarissa) # RESULT: NO STATISTICALLY SIGNIFICANT CORRELATION

p01 <- ggplot(combined_data_LukasLarissa, aes(x = suntime, y = watchtime))          +
  mygraphics                                                                +
  geom_point(size = 0.75)                                                   +
  ggtitle(label = "LL (Munich)")                               +
  theme(plot.title = element_text(hjust = 0.5, size = 11))                  +
  scale_x_continuous(name = "", limits = c(0, 16))                          +
  scale_y_continuous(name = "", limits = c(0, 15))                          +
  geom_smooth(method = "lm", se = FALSE, col = "orange", fullrange = TRUE)


## LINEAR REGRESSION WATCHTIME/SUNTIME FOR PROFILE LUKAS & NINA
lm(formula = watchtime ~ suntime, data = combined_data_LukasNina) # RESULT: NO STATISTICALLY SIGNIFICANT CORRELATION

p02 <- ggplot(combined_data_LukasNina, aes(x = suntime, y = watchtime))             +
  mygraphics                                                                +
  theme(axis.text.y = element_blank())                                      +
  geom_point(size = 0.75)                                                   +
  ggtitle(label = "LN (Gießen)")                                  +
  theme(plot.title = element_text(hjust = 0.5, size = 11))                  +
  scale_x_continuous(name = "Suntime in Hours", limits = c(0, 16))          +
  scale_y_continuous(name = "", limits = c(0, 15))                          +
  geom_smooth(method = "lm", se = FALSE, col = "orange", fullrange = TRUE)


## LINEAR REGRESSION WATCHTIME/SUNTIME FOR PROFILE TOMMY & LEA
lm(formula = watchtime ~ suntime, data = combined_data_TommyLea) # RESULT: NO STATISTICALLY SIGNIFICANT CORRELATION

p03 <- ggplot(combined_data_TommyLea, aes(x = suntime, y = watchtime))              +
  mygraphics                                                                +
  theme(axis.text.y = element_blank())                                      +
  geom_point(size = 0.75)                                                   +
  ggtitle(label = "TL (Dillenburg)")                               +
  theme(plot.title = element_text(hjust = 0.5, size = 11))                  +
  scale_x_continuous(name = "", limits = c(0, 16))                          +
  scale_y_continuous(name = "", limits = c(0, 15))                          +
  geom_smooth(method = "lm", se = FALSE, col = "orange", fullrange = TRUE)


## LINEAR REGRESSION WATCHTIME/MAX_TEMPERATURE FOR PROFILE LUKAS & LARISSA
lm(formula = watchtime ~ max_temperature, data = combined_data_LukasLarissa) # RESULT: NO STATISTICALLY SIGNIFICANT CORRELATION

p04 <- ggplot(combined_data_LukasLarissa, aes(x = max_temperature, y = watchtime))  +
  mygraphics                                                                +
  geom_point(size = 0.75)                                                   +
  ggtitle(label = "")                                                       +
  scale_x_continuous(name = "", limits = c(0, 35))                          +
  scale_y_continuous(name = "Watchtime in Hours", limits = c(0, 15))        +
  geom_smooth(method = "lm", se = FALSE, col = "red", fullrange = TRUE)


## LINEAR REGRESSION WATCHTIME/MAX_TEMPERATURE FOR PROFILE LUKAS & NINA
lm(formula = watchtime ~ max_temperature, data = combined_data_LukasNina) # RESULT: NO STATISTICALLY SIGNIFICANT CORRELATION

p05 <- ggplot(combined_data_LukasNina, aes(x = max_temperature, y = watchtime))     +
  mygraphics                                                                +
  theme(axis.text.y = element_blank())                                      +
  geom_point(size = 0.75)                                                   +
  ggtitle(label = "")                                                       +
  scale_x_continuous(name = "Maximum Temperature in ?C", limits = c(0, 35)) +
  scale_y_continuous(name = "", limits = c(0, 15))                          +
  geom_smooth(method = "lm", se = FALSE, col = "red", fullrange = TRUE)


## LINEAR REGRESSION WATCHTIME/MAX_TEMPERATURE FOR PROFILE TOMMY & LEA
lm(formula = watchtime ~ max_temperature, data = combined_data_TommyLea) # RESULT: NO STATISTICALLY SIGNIFICANT CORRELATION

p06 <- ggplot(combined_data_TommyLea, aes(x = max_temperature, y = watchtime))      +
  mygraphics                                                                +
  theme(axis.text.y = element_blank())                                      +
  geom_point(size = 0.75)                                                   +
  ggtitle(label = "")                                                       +
  scale_x_continuous(name = "", limits = c(0, 35))                          +
  scale_y_continuous(name = "", limits = c(0, 15))                          +
  geom_smooth(method = "lm", se = FALSE, col = "red", fullrange = TRUE)


## LINEAR REGRESSION WATCHTIME/RAINFALL FOR PROFILE LUKAS & LARISSA
lm(formula = watchtime ~ rainfall, data = combined_data_LukasLarissa) # RESULT: NO STATISTICALLY SIGNIFICANT CORRELATION

p07 <- ggplot(combined_data_LukasLarissa, aes(x = rainfall, y = watchtime))         +
  mygraphics                                                                +
  geom_point(size = 0.75)                                                   +
  ggtitle(label = "")                                                       +
  scale_x_continuous(name = "", limits = c(0, 45))                          +
  scale_y_continuous(name = "", limits = c(0, 15))                          +
  geom_smooth(method = "lm", se = FALSE, col = "blue", fullrange = TRUE)


## LINEAR REGRESSION WATCHTIME/RAINFALL FOR PROFILE LUKAS & NINA
lm(formula = watchtime ~ rainfall, data = combined_data_LukasNina) # RESULT: NO STATISTICALLY SIGNIFICANT CORRELATION

p08 <- ggplot(combined_data_LukasNina, aes(x = rainfall, y = watchtime))            +
  mygraphics                                                                +
  theme(axis.text.y = element_blank())                                      +
  geom_point(size = 0.75)                                                   +
  ggtitle(label = "")                                                       +
  scale_x_continuous(name = "Rainfall in mm", limits = c(0, 45))            +
  scale_y_continuous(name = "", limits = c(0, 15))                          +
  geom_smooth(method = "lm", se = FALSE, col = "blue", fullrange = TRUE)


## LINEAR REGRESSION WATCHTIME/RAINFALL FOR PROFILE TOMMY & LEA
lm(formula = watchtime ~ rainfall, data = combined_data_TommyLea) # RESULT: NO STATISTICALLY SIGNIFICANT CORRELATION

p09 <- ggplot(combined_data_TommyLea, aes(x = rainfall, y = watchtime))             +
  mygraphics                                                                +
  theme(axis.text.y = element_blank())                                      +
  geom_point(size = 0.75)                                                   +
  ggtitle(label = "")                                                       +
  scale_x_continuous(name = "", limits = c(0, 45))                          +
  scale_y_continuous(name = "", limits = c(0, 15))                          +
  geom_smooth(method = "lm", se = FALSE, col = "blue", fullrange = TRUE)



#### CREATE GROUPED PLOT ####

final_plot <- ggarrange(p01, p02, p03, p04, p05, p06, p07, p08, p09, 
                        ncol = 3, nrow = 3) + 
  mygraphics +
  labs(y="",x="",fill="Viewing Activity",title="The Influence of Weather on Time spent on Netflix", 
       caption = "Source: Netflix, Meteostat")


# END END END END END END END END END END END END END END END END END -----










