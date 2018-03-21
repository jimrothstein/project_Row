#003_read_all_row_records.R
# using purrr, map

#### 000_Variables ####
        #  yr                           char list of years ("seasons")
        #  seasons                      char list of file names


#### 001 setup	####
rm(list=ls())
library(tidyverse)
library(lubridate)
# ---- Need these 2 ?			----
library(readr)
library(purrr)


#### 002 functions	####
# read all fields as character
readLog <- function(t) {
    read_csv(file = t,
	col_types = cols(.default = col_character(),
	                 `Avg Watts` = col_integer()),
	col_names = TRUE)   # use 1st line
}

## convert tibble fields to integer, date from character ####
f_mutate <- function (df) {
	df  %>% 
	mutate(    
	`Work Time`  = as.integer(`Work Time (Seconds)`),
	`Work Distance`= as.integer(`Work Distance`),
	`Avg Watts`= as.integer(`Avg Watts`))
}

#### 003_Construct File Names & Path for Data Read using purrr ####
#
# Begin, years (actually seasons, later) 
yr  <- list("2013","2014","2015","2016","2017","2018")
		
# append year and .csv
seasons <- paste0("./data/concept2-season-",yr, ".csv")

#### 004_ Import the data ####
# apply f readLog to each element of list season
data_list_tibbles <- map(seasons, readLog)

# name each tibble in list with year
names(data_list_tibbles) <- yr

#### 005 verify: list of tibbles ##### ---

# manually verify, using str, list of tibbles
str(data_list_tibbles[["2013"]], max.level=2, list.len=5, vec.len=2)  # works!

# but if object is list of tibbles
str(data_list_tibbles, max.level=2, list.len=5, vec.len=2) # best, for now 

#### 006_Cleanup ####
#       Convert to integer, date format
data00  <- data_list_tibbles %>% 
    map(.f = f_mutate) %>%

    map( ~mutate(., Date =ymd(ymd_hms(Date)))
	       )

#### 005-select colunms we need ####                                    
# build empty tibble with correct fields
c_names <- colnames(data_list_tibbles[["2013"]])
#print(c_names)

# select columns
data1  <- map(.x=data00, ~select(.x,Date=Date,
                Description,
                Time = "Work Time",
                Distance = "Work Distance",
                Pace = "Pace",
                Watts = "Avg Watts")
)
#### 006_ calc KWatt-hours #### ####

# note mutuate needs placeholder "."
data2 <- data1 %>% map(
        ~mutate(., 
                Watts = ifelse(is.na(Watts), 0,Watts),
                Energy = Watts*Time/3600)
          )
#### 007_plots ####

# basic
plots  <- map (.x=data2, ~ggplot(.x,aes(Date,Energy)) + geom_point())

# use geom_col for height= value of y
#fancier
plots1 <- map2 (.x=data2,.y=yr, ~ggplot(.x, aes(Date, Energy)) +
        geom_col(fill = "darkorange") +
        geom_hline(yintercept = c(40,60), 
                   color = c('blue','red'), linetype=2) +
        labs (title = paste0("Rowing -- Daily Watt-hour",.y), 
              subtitle = "60 = maximum, 40 = typical\n60 = 1 lightbulb for 1 hour",
              caption = "source: my records",
              y = "Watt-hours")  +
        scale_x_date(date_labels = "%b %Y",
          date_breaks = "3 months",
          date_minor_breaks = "1 month",
          limits = as.Date(c(paste0(as.integer(.y)-1,"-05-01"), 
                             paste0(.y,"-04-30"))
                           )   
          )
)
map2( .x = yr,
	.y = plots1,
	.f = ~ggsave(filename=paste0("./plots/energy-",.x,".png")
	)
 )
walk(plots1, print)
# time
# plot TIME (min) of each day
plots2  <- map2(.x=data2,.y=yr, ~ggplot(.x, aes(Date, Time/60)) +
        geom_col(fill = "green") +
        #geom_hline(yintercept = c(40,60), 
        #           color = c('blue','red'), linetype=2) +
        labs (title = "Rowing -- Daily TIME (min)",  
              subtitle = .y, #"",
              caption = "source: my records",
              y = "Time (min) ") +
        scale_x_date(date_labels = "%b %Y") 
)
walk(plots2,print)
#
# ---- easier to split 1 tibble into many?----

x1  <-  mtcars %>% 
  split(.$cyl) %>%
  map(~lm(mpg~wt, data=.))	# data=. inside lm()


