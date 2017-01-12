## ROW, version 3.0



## tags:  tibble, ggplot

rm(list=ls())

library(tidyverse)
library(lubridate)



f <- "~/Downloads/R_projects/project_Row/data/2016_11Nov_2016-2017_season.csv"
df <- read.csv(file=f,
                header = TRUE,
                sep=",",
                na.strings=c("NA","N/A"),
                stringsAsFactors = FALSE)
df$Date<-as.Date(df$Date)  # remove time
# fix Date
df$Date<-ymd(df$Date)

d<-as_tibble(df)
d
                # # number fields by adding prefix
                # # paste is concatination 
                # names(d) <- paste( as.character(c(1:18)),
                #                     names(d))


# keep only a few relevant columns (unless concept2 changes)
d1<-d %>%
        select(c(1,2,4,7,10,11))

d1

## Change Colnames, do with mutate?
x<-c("Date","Description","Time(sec)","Dist(meters)","Pace","Power(watts)")
colnames(d1)<-x
d1

## another way?
z <- d1[,c(3,4,6)]
names(z)<- paste(as.character(c(1:3)),
                 c("Time(sec)", "Dist(meters)","Power(watts)")
)

## Skip Zoo or TS --------


## Skip Clumsy (pre-tidydata) ways of massaging data, choosing & changing field


d1
p<- ggplot(d1, aes(x=Date, y=Meters)) +
        geom_point()
p
p <- ggplot(d1, aes(x=Meters)) +
        geom_histogram()
p
