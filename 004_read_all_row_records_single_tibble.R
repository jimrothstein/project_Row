#004_read_all_row_records_single_tibble.R
# using purrr , but 1 big tibble

#### 001 setup	####
rm(list=ls())
library(tidyverse)
library(lubridate)
library(jimPackage)
# ---- Need these 2 ?			----
library(readr)
library(purrr)  # yes, need
#### 002 functions	####

# import all fields as character
readLog <- function(t) {
    read_csv(file = t,
	col_types = cols(.default = col_character(),
	                 `Avg Watts` = col_integer()),
	col_names = TRUE)   # use 1st line
}

### convert tibble fields to 
## integer, date from character ####i
## select desired fields
## calc watt-hours
f_mutate <- function (df) {
 df  %>% 
                
        mutate(                    # convert columns
	`Work Time`  = as.integer(`Work Time (Seconds)`),
	`Work Distance`= as.integer(`Work Distance`),
	`Avg Watts`= as.integer(`Avg Watts`),
	Date = ymd(ymd_hms(Date))
	) %>% 
                
        select(Date=Date,
                Description,
                Time = "Work Time",
                Distance = "Work Distance",
                Pace = "Pace",
                Watts = "Avg Watts") %>%
                
        mutate( Watts = ifelse(is.na(Watts), 0,Watts),
                Energy = Watts*Time/3600)
}
###### end function ####

## 003_gather all *.csv files, list of tibbles
files <- dir (path="./data/",
              pattern="*.csv")

## import each .csv file, return 1 tibble with all the data
data <- paste0("./data/",files) %>% 
        map_dfr(.f=readLog) %>%
        f_mutate()


#### 007_plots ####
# basic
data_labels <- tibble(x=as.Date(c("2012-4-21","2012-9-21")),
                      y=c(80,80),
                      label=c("Shenzhen - no data","Las Vegas"))
# use geom_col for height= value of y
#fancier
p <-data %>% ggplot(aes(Date, Energy)) +
        geom_col(fill = "darkorange") +
        geom_hline(yintercept = c(40,60), 
                   color = c('blue','red'), linetype=2) +
        labs (title = paste0("Rowing -- Daily Watt-hour"), 
              subtitle = "60 = maximum, 40 = typical\n60 = 1 lightbulb for 1 hour",
              caption = "source: my records",
              y = "Watt-hours")  +
        scale_x_date(date_labels = "%b %Y",
          date_breaks = "1 year "
)
p + geom_text(data=data_labels,
              aes(x,y,label = label), color="black", size=2)

#########################
##  FIX ########
map2( .x = yr,
	.y = plots1,
	.f = ~ggsave(filename=paste0("./plots/energy-",.x,".png")
	)
 )
walk(plots1, print)
# plot TIME (min) of each day
plots2  <- map2(.x=data2,.y=yr, ~ggplot(.x, aes(Date, Time/60)) +
        geom_col(fill = "green") +
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
