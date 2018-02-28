# 003b_read_all_row_records.R
# using purrr, map


rm(list=ls())
library(tidyverse)
library(lubridate)
# ---- Need these 2 ?			----
library(readr)
library(purrr)

#### functions ####

f  <- function (df) {
	df$Date  <- df$Date1 }
g  <- function (df) {
	mutate(df, Date=Date1)
}

t1  <- tibble(Date1=c("23FEB2018", "2/23/2018"),
	     var1 = c(1,2))

t2  <- tibble(Date1=c("23FEB2018", "2/23/2018"),
	     var1 = c(8,9))

l  <- list(ONE=t1,
	     TWO=t2)
str(t1)
str(l)

z  <- map(.x=l, f)
w  <- map(.x=l, g)
str(z)
w
z[[1]]
is_tibble(z[[1]])
#### 003_convert to integer, date from character ####

f1  <- function (df) {
	df$`Work Time`  <- as.integer(df$`Work Time (Seconds)`)
	df$`Work Distance`<- as.integer(df$`Work Distance`)
	df$`Avg Watts`<- as.integer(df$`Avg Watts`)
}
z1 <- map(.x = data_list_tibbles, ~( .x$`Work Time`  <- 
    as.integer(.x$`Work Time (Seconds)`) 
)
) 
z1
data_list_tibbles
z  <- map(.x = data_list_tibbles, .f = f1)
z
z[[1]]
str(z[[1]], max.level=2, list.len=5, vec.len=2)
map(.x=data_list_tibbles, ~ymd(ymd_hms(.x$Daxte)))


#### 005-select colunms we need ####                                    
c_names <- colnames(data_list_tibbles[["2013"]])
print(c_names)

data1  <- map(.x=data_list_tibbles, ~select(.x,Date=Date,
                Description,
                Time = "Work Time (Seconds)",
                Distance = "Work Distance",
                Pace = "Pace",
                Watts = "Avg Watts")
)
str(data1, max.level=2, list.len=4, vec.len=2)	# 4 used TWICE, #tibbles & #fields!

#### 006_ calc KWatt-hours #### ####

data2  <- map(.x=data1, ~mutate(.x, energy =Watts*Time/3600)) 	#Watt-hours



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
#

x1  <-  mtcars %>% 
  split(.$cyl) %>%
  map(~lm(mpg~wt), data=.)

