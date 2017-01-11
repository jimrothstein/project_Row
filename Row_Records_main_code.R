#############   S T O P ##################

# USE ROW_Version1.R   #

#############   STOP   ##################

# Importing .csv DATA fron CONCEPT2 website ####
#


# clean out environment
rm(list=ls())

done <- TRUE

if (!done){
    install.packages(c("dplyr","tidyr","magritter","readr",
                   "ggplot2","scales","stringr","zoo"), 
                 destdir = "~/Downloads/R_projects/R_packages")
}

# load packages
library(dplyr)
library(ggplot2)

# using these?
library(tidyr)
library(zoo)
library(lubridate)

# include fucntcions saved separately
source("~/Downloads/R_projects/project_Row/function_row.R", echo=T)

####### Seasons & Data ####
# April 1, 2015 -- March 31, 2016 define 2016 Season or YEAR
#
# 12/2011 -- start rowing (Santa Cruz, 2012 season)
#
# And very first log entry:  7/24/2012 (Berkeley, i.e. partial 2013 season)
#
# So have data for Seasons: 2013 (i.e. partial), 2014, 2015, 2016, 2017 (in progress)







###### 2016 Season (April 1, 2015 - March 31, 2016) ####

# read it, use full path to avoid ambiguity
r.2016 <- read.csv("~/Downloads/R_projects/project_Row/data/15Nov2015_2016_season.csv", as.is=TRUE)

#
check.data(x=r.2016)

# ignore low days
criteria <- "r.2016$Meters > 1000"
mean(r.2016$Meters[r.2016$Meters > 1000])
median(r.2016$Meters[r.2016$Meters> 1000])

#################


######## 2015 season ####
# import (season ends in new year), as.is=TRUE, avoids 'factor'
r.2015 <- read.csv("~/Downloads/R_projects/project_Row/data/11Nov2015_2015_season.csv", as.is=TRUE)

######## 2014 ####
r.2014 <- read.csv("~/Downloads/R_projects/project_Row/data/11Nov2015_2014_season.csv", as.is=TRUE)

######## 2013  ####  
#(first 'season' with records, begins 7/24/2012)
r.2013 <- read.csv("~/Downloads/R_projects/project_Row/data/11Nov2015_2013_season.csv", as.is=TRUE)

##### v, list to hold each year
v<- c(r.2013,r.2014, r.2015,r.2016)
#####

##### boxplot, removing days less than 1000 meters
boxplot(r.2015$Meters[r.2015$Meters>1000]) # ignore less than 1000 meters
boxplot(r.2016$Meters[r.2016$Meters>1000])
boxplot(subset(r.2015, Meters>1000)$Meters)  # shorter
boxplot(with(subset(r.2015,Meters>1000))) # even shorter

#### ggplot

df <- subset(r.2016,Meters>1000)

g <- ggplot(df, aes(x = Meters))
g +  geom_histogram(binwidth=200)

g_histogram(df)

######

# 4 boxplots across
par(mfrow = c(1, 4), mar = c(5, 4, 2, 1))
with(subset(r.2013,Meters>1000), boxplot(Meters,  main = "2013"))
with(subset(r.2014,Meters>1000), boxplot(Meters,  main = "2014"))
with(subset(r.2015,Meters>1000), boxplot(Meters,  main = "2015"))
with(subset(r.2016,Meters>1000), boxplot(Meters,  main = "2016"))
# with(subset(pollution, region == "east"), plot(latitude, pm25, main = "East"))


#####  Histograms ####
#hist, 1 for each year
par(mfrow = c(1, 4), mar = c(5, 4, 2, 1))

with(subset(r.2016,Meters>1000), 
     hist(Meters,  main = "Distribution of Daily Meters, 2016", xlab="Meters",
     col='lightblue', xlim=c(0,12000) ))
with(subset(r.2015,Meters>1000), 
     hist(Meters,  main = "Distribution of Daily Meters, 2015", xlab="Meters",
          col='lightgreen', xlim=c(0,12000) ))
with(subset(r.2014,Meters>1000), 
     hist(Meters,  main = "Distribution of Daily Meters, 2014", xlab="Meters",
          col='lightblue', xlim=c(0,12000) ))
with(subset(r.2013,Meters>1000), 
     hist(Meters,  main = "Distribution of Daily Meters, 2013", xlab="Meters",
          col='lightyellow', xlim=c(0,12000) ))


#### 2015 details & linear model ####
# clean df
season.2015<-clean(r.2015)

# show fields
ls(season.2015)

# show all rows, Meters only
season.2015$Meters

summary(season.2015$Meters)
# built-in summary


# --- histogram ####
hist(season.2015$Meters)

# --- histogram end ---

# --- scaterplot ####

# first, group by month
# using %>% notation from xxxxxx package
season.2015 %>% group_by(YearMonth) %>% summarise(count = n())

plot(season.2015$Meters, season.2015$Time , type="n", xlim = c(0,11000))  # basic plot, no points?

# now points, each of 3 species gets default color
points(season.2015$Meters, season.2015$Time, pch=19) # color points! col=season.2015$YearMonth)

# linear model
model <- lm(Time ~ Meters, season.2015)
abline(model, lwd = 2)
# ---- scatter plot end ---

# --- Plot Time Series ####

# ---  TS - simple as possible ####

# --- MESS ####

# boxplot
# not sure what this does (convert YearMonth to factor ?)
x<- season.2015
x <- transform(x, YearMonth=factor(YearMonth))

#
boxplot(Meters ~ YearMonth,data = x,xlab="YearMonth",ylab="Meters")

hist(x$Meters)

###################
# now, no need to specify the db

Total.Time
# display each Total.Time, not gss$Total.Time

# =================

new1 <- new %>% group_by(new$YearMonth)

#Summarise daily data 
# dataSum <- gss %>% group_by(Date) %>% summarise(Count=n())
gss$YearMonth <- as.yearmon(gss$Date)   # format ="%m/%d/%Y")

#### calc energy #####
# W x Time = Energy (Watt-min)

# Total.time stored as char, convert to seconds, min or hours (best?)
e.2015 <- r.2015$Total.Time * 2.80/(r.2015$Pace * 60/500)**3


#try  - convert Total.Time (char) to number
#options(digits.secs=3)
#as.POSIXlt(<char>, format="%H:%M%S")$hour min sec [no 3.2 sec]


# --- MESS ---