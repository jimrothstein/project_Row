# 002_study_2017-2018_season

rm(list=ls())
library(tidyverse)
library(lubridate)


#### 001_just TO SEE the data: ####
# read the data, AS character, just to see
data0 <- read_csv(file = "./data/concept2-season-2018.csv",
         col_types = cols(.default = col_character()), # everything char
         col_names = TRUE)   # use 1st line
data0$`Work Time (Seconds)`<- as.integer(data0$`Work Time (Seconds)`)
data0$`Work Distance`<- as.integer(data0$`Work Distance`)

# 1st convert, including hms;   then convert to date only
data0$Date<- ymd(ymd_hms(data0$Date))
data0$`Avg Watts`<- as.integer(data0$`Avg Watts`)

#### 003-skip col_only etc ####                                         
# glimpse tells us (1) colnames (2) which are useful
c_names <- colnames(data0)
print(c_names)

#### 002_ ####
# a start?
data1 <- read_csv(file = "./data/concept2-season-2018.csv",
                  col_names = TRUE,
                  col_types = cols_only(
                          Date =  col_datetime(),
                          Description = col_character(),
                          "Work Time (Seconds)" = col_integer(),
                          "Work Distance"= col_integer(),
                          "Pace" = col_character(),
                          "Avg Watts" = col_integer()
                          
                  )
)
                  #### 004_use dplyr ######
                  
data1 <- select(data0,Date=Date,
        Description,
        Time = "Work Time (Seconds)",
        Distance = "Work Distance",
        Pace = "Pace",
        Watts = "Avg Watts")

# KWatt-hours
data2 <- mutate(data1, energy = Watts*Time/3600)





         
