# ROW_VERSION 1

# STOP  - go to version 2

# tags:  xts, row, concept2

# Imports .csv DATA fron CONCEPT2 website ####
#


# clean out environment
rm(list=ls())


# load packages
library(xts)

####### Seasons & Data ####
# 
# season n, ends March 31, year n   BUT begins on April 1, year n-1
#
# 12/2011 -- start rowing (Santa Cruz, 2012 season)
# And very first log entry:  7/24/2012 (Berkeley, i.e. partial 2013 season)
#
# April 1, 2015 -- March 31, 2016 define 2016 Season or YEAR
# So have data for Seasons: 2013 (i.e. partial), 2014, 2015, 2016, 2017 (in progress)
#
#######

###### 2017 Season (April 1, 2016 - March 31, 2017) ####
#
#
# read .csv data
# clumsy
x <- as.xts(read.zoo("~/Downloads/R_projects/project_Row/data/2016_11Nov_2016-2017_season.csv", 
                     stringsAsFactors=FALSE, 
                     header=TRUE, 
                     sep=","))


### Replace above
df <- read.csv("~/Downloads/R_projects/project_Row/data/2016_10Sep_2016-2017_season.csv", 
               stringsAsFactors=FALSE, 
               header=TRUE, 
               sep=",")

# 18 fields, only a few useful
fields<-c(1,4,7,10,11)

u.df<-subset(df,select=fields)
u.df$Date<- as.POSIXct(u.df$Date)   # char --> POSIXct

# NEXT:   convert using str to get rid of time
z<- zoo(u.df,order.by = u.df$Date)


# NeXT:   piece apart charcter of PACE!!



###################################################
###################################################
### STOP


## OLD

#f <- "~/Downloads/R_projects/project_Row/data/2016_10Sep_2016-2017_season.csv"
#y<- read.table(file=f, header=TRUE,sep=",", colClasses=c("Date","character","character", "integer",NA, NA,"integer",NA,NA, "character","integer","integer"))
# keep columns that make sense
x<- subset(x, select=c(1,2,3,6,9,10,11))
head(x)

# change names:
colnames(x) <- c("Description","Minutes","Seconds","Meters","Pace","SPM","Watts")

head(x)

# why this does not work??
x[,4] <- sapply(x[,4],as.integer)
x[,4] <- as.numeric(x[,4])
str(x)
plot(x[,1], major.ticks='months', minor.ticks=FALSE,main=NULL, col=3,ylim=c(0:100))

###############################  zoo ##################3






##################################                      
# check and print simple things (make no changes)
check.data(x=r.2017)

# simple boxplot
df <- subset(r.2017,Work.Distance>1000)   #2016 use 'Distance'  <2016 use 'Meters'
g <- ggplot(df, aes(x = 1, y=Work.Distance))  # no grouping on x, use x=1 ??
g +  geom_boxplot()


######## ggplot histogram ####
g <- ggplot(df, aes(x = Work.Distance))
g +  geom_histogram(binwidth=200)
# repeat?
# g_histogram(df)

g <- ggplot(df, aes(x= Pace))
g + geom_histogram(binwidth = 200)

###################################### End 2017 ################################


####### 2016 Season (April 1, 2015 - March 31, 2016) ####
# read csv, full path to avoid ambiguity
r.2016 <- read.csv("~/Downloads/R_projects/project_Row/data/2016_4May_concept2-season-2016_COMPLETE.csv", as.is=TRUE)

#
check.data(x=r.2016)  # number of rows, dim;  no changes to x
#
# ignore low days, mean & median
criteria <- "r.2016$Meters > 1000"
mean(r.2016$Meters[r.2016$Meters > 1000])
median(r.2016$Meters[r.2016$Meters> 1000])
#######


######## import 2015 season ####
# import (season ends in new year), as.is=TRUE, avoids 'factor'
r.2015 <- read.csv("~/Downloads/R_projects/project_Row/data/11Nov2015_2015_season.csv", as.is=TRUE)

######## 2014 ####
r.2014 <- read.csv("~/Downloads/R_projects/project_Row/data/11Nov2015_2014_season.csv", as.is=TRUE)

######## 2013  ####  
#(first 'season' with records, begins 7/24/2012)
r.2013 <- read.csv("~/Downloads/R_projects/project_Row/data/11Nov2015_2013_season.csv", as.is=TRUE)

##### v, list to hold all years
v<- c(r.2013,r.2014, r.2015,r.2016)
#####

# ##### boxplot, removing days less than 1000 meters
# boxplot(r.2015$Meters[r.2015$Meters>1000]) # ignore less than 1000 meters
# boxplot(r.2016$Meters[r.2016$Meters>1000])
# boxplot(subset(r.2015, Meters>1000)$Meters)  # shorter
# boxplot(with(subset(r.2015,Meters>1000))) # even shorter

####### ggplot boxplot 2016 season, only ####
df <- subset(r.2016,Distance>1000)   #2016 use 'Distance'  <2016 use 'Meters'
g <- ggplot(df, aes(x = 1, y=Distance))  # no grouping on x, use x=1 ??
g +  geom_boxplot()

######## ggplot boxplot ####
group <- c(2013,2014,2015,2016 ) #???? 
g<- ggplot(df,aes(x= v, y=Meters))
g <- g + geom_boxplot()
g
######## ggplot histogram ####
df <- subset(r.2016,Distance>1000)   #2016 use 'Distance'  <2016 use 'Meters'
g <- ggplot(df, aes(x = Distance))
g +  geom_histogram(binwidth=200)
# repeat?
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