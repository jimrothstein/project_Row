# 003a_read_all_row_records.R
# using purrr, map


rm(list=ls())
library(tidyverse)
library(lubridate)
# ---- Need these 2 ?			----
library(readr)
library(purrr)

#### functions ####

# read as character
readLog <- function(t) {
    read_csv(file = t,
	col_types = cols(.default = col_character()), # everything char
	col_names = TRUE)   # use 1st line
}

#### 001a_Read from file, using purrr ####
# Assemble file names
yr  <- list("2013","2014","2015","2016","2017","2018")
		
# append year
name <- paste0("./data/concept2-season-",yr)

# append .csv
season  <- paste0(name,".csv")
# apply f readLog to each element of list season
data_list_tibbles <- map(season, readLog)



# data_list_tibbles is new list, 1 tibble for each year, give each element a label
names(data_list_tibbles) <- yr

# manually verify, using str, list of tibbles
str(data_list_tibbles[["2013"]], max.level=2, list.len=5, vec.len=2)  # works!

# but if object is list of tibbles
str(data_list_tibbles, max.level=2, list.len=5, vec.len=2) # best, for now 

#### 003_convert to integer, date from character ####

f1  <- function (df) {
	mutate(df, 
	    
	`Work Time`  = as.integer(`Work Time (Seconds)`),
	`Work Distance`= as.integer(`Work Distance`),
	`Avg Watts`= as.integer(`Avg Watts`))
}


data0  <- map(.x = data_list_tibbles, .f = f1)
str(data0[[1]])
#
data00  <- map(.x=data0, 
	       
	       ~mutate(.x, Date =ymd(ymd_hms(Date)))
	       )

data00[[1]]
#### 005-select colunms we need ####                                    
c_names <- colnames(data_list_tibbles[["2013"]])
print(c_names)

data1  <- map(.x=data00, ~select(.x,Date=Date,
                Description,
                Time = "Work Time",
                Distance = "Work Distance",
                Pace = "Pace",
                Watts = "Avg Watts")
)

data1[[1]]
str(data1, max.level=2, list.len=4, vec.len=2)	# 4 used TWICE, #tibbles & #fields!

#### 006_ calc KWatt-hours #### ####

data2  <- map(.x=data1, ~mutate(.x, energy =Watts*Time/3600)) 	#Watt-hours

data2[[1]]

#### 007_plots ####

# basic
g <- ggplot(data2, aes(Date,energy)) +
        geom_point()
g

plots  <- map (.x=data2, ~ggplot(.x,aes(Date,energy)) + geom_point())
str(plots[[1]]
    )
# f
plots
#jkancier
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
  map(~lm(mpg~wt, data=.))	# data=. inside lm()


