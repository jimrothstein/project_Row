# 003_read_all_row_records.R

rm(list=ls())
library(tidyverse)
library(lubridate)
library(readr)
library(purrr)

#### functions ####

# read as character
readLog <- function(t) {
        read_csv(file = t,
                          col_types = cols(.default = col_character()), # everything char
                          col_names = TRUE)   # use 1st line
}


#### 001_just TO SEE the data: ####

# data13 <- readLog("./data/concept2-season-2013.csv")
# data14 <- readLog("./data/concept2-season-2014.csv")
# data15 <- readLog("./data/concept2-season-2015.csv")
# data16 <- readLog("./data/concept2-season-2016.csv")
# data17 <- readLog("./data/concept2-season-2017.csv")
# data18 <- readLog("./data/concept2-season-2018.csv")

#### 001a_redo_using purrr ####
yr  <- list("2013","2014","2015","2016","2017","2018")
		
# append year
name <- paste0("./data/concept2-season-",yr)
# apend .csv
season  <- paste0(name,".csv")

# apply f readLog to each element of list season
data  <- map(season, readLog)
# data is new list, 1 tibble for each year, give each element a label
names(data) <- yr

# manually verify, using str, list of tibbles
data[["2014"]]
# if object is tibble
str(data[["2013"]], max.level=1)       # 
str(data[["2013"]], max.level=1, list.len=5)  
str(data[["2013"]], max.level=1, list.len=5, vec.len=2)  # works!

# but if object is list of tibbles
str(data, max.level=1, list.len=5, vec.len=2)  
str(data, max.level=2, list.len=1, vec.len=2) # best, for now 




#### 002_combine all years ####

# data <-NULL
# data <- union_all(data13,data14)
# data <- union_all(data,data15)
# data <- union_all(data,data16)
# data <- union_all(data,data17)
# data <- union_all(data,data18)
# this rk is junk
#### 002a_redo, combine  using dplyr::bind_rows ####

data_single_tibble  <- bind_rows(data) # data 
# verify
str(data_single_tibble, max.level=2, list.len=1, vec.len=2)
str(data_single_tibble, max.level=1, list.len=6, vec.len=2)# shows list of 6

#### 003_convert to integer from character ####


data_single_tibble$`Work Time (Seconds)`<- as.integer(data_single_tibble$`Work Time (Seconds)`)
data_single_tibble$`Work Distance`<- as.integer(data_single_tibble$`Work Distance`)
data_single_tibble$`Avg Watts`<- as.integer(data_single_tibble$`Avg Watts`)

#### 004_convert Date to right format ####
data_single_tibble$Date<- ymd(ymd_hms(data_single_tibble$Date))

#### 005-select colunms we need ####                                    
# glimpse tells us (1) colnames (2) which are useful
c_names <- colnames(data_single_tibble)
print(c_names)

data1 <- select(data_single_tibble,Date=Date,
                Description,
                Time = "Work Time (Seconds)",
                Distance = "Work Distance",
                Pace = "Pace",
                Watts = "Avg Watts")

#### 006_ calc KWatt-hours #### ####
data2<- mutate(data1, energy = Watts*Time/3600)   # Watt-hours
glimpse(data2)



#### 007_plots ####

# basic
g <- ggplot(data2, aes(Date,energy)) +
        geom_point()
g

# fancier
# use geom_col for height= value of y
g <- ggplot(data2, aes(Date, energy)) +
        geom_col(fill = "darkorange") +
        geom_hline(yintercept = c(40,60), 
                   color = c('blue','red'), linetype=2) +
        labs (title = "Rowing -- Daily Watt-hour",  
              subtitle = "60 = maximum, 40 = typical\n60 = 1 lightbulb for 1 hour",
              caption = "source: my records",
              y = "Watt-hours") 
# annotate ("text", label="LAS",x=2012.833,y=3) 


g

# time
# plot TIME (min) of each day
g <- ggplot(data2, aes(Date, Time/60)) +
        geom_col(fill = "green") +
        #geom_hline(yintercept = c(40,60), 
        #           color = c('blue','red'), linetype=2) +
        labs (title = "Rowing -- Daily TIME (min)",  
              subtitle = "",
              caption = "source: my records",
              y = "Time (min) ") 
g
