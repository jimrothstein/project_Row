# Row Timeline with ZOO

# study:
# https://cran.r-project.org/web/packages/zoo/vignettes/zoo-read.pdf


# clean out environment
rm(list=ls())


# load packages
library("zoo")
library("lubridate")  #simplify time/data
###### 2017 Season (April 1, 2016 - March 31, 2017) ####
#
#


# read .csv data, setdirectly into zoo, column 1 is time
# a ZOO object is not class ts; not class df; but IS class matrix
z <- read.zoo("~/Downloads/R_projects/project_Row/data/2016_10Sep_2016-2017_season.csv",
              sep=",", 
              header=TRUE, 
              FUN=as.POSIXct)

head(z[,c(1,2,3,4,5)])

# reduce number of columns (where 1 = Description,  time field is included for free)
x<-subset(z,select=c(1,2,3,6,9,10,11))
head(x[,1:7])

# change column names (again, time is ignored, for free) .... 
colnames(x) <- c("Description","Minutes","in Sec","Meters","Pace","Watts","Cal/hr")
head(x[,1:7])

# CLEAN NA
# (1) don't want to DROP a date with no rowing, but be careful in calc
# (2) if any ZOO entry is a character, then ALL the values become characters

# LEAVE as NA

w <- x


# add a new column, with pace but in seconds/500 meters
# ifelse (test, yes, no)
w$sec_500m <- ifelse(as.numeric(w$Meters)==0,NA,as.numeric(w$"in Sec")*500/(as.numeric(w$Meters)) )
w
w$Meters <- ifelse(as.numeric(w$Meters) == 0, NA,as.numeric(w$Meters) )
w$"in Sec" <- ifelse(as.numeric(w$Meters) == 0, NA,as.numeric(w$"in Sec") )




# PROBLEM
#
#
#  zoo var contains character column, not numeric   - CONVERT?
# skip zero?  SET 0 to NA


# stats
# avg meters (BUT ignore days with 0)
u1 <- mean(as.numeric(w$Meters), na.rm=TRUE)  # X
u2 <- mean(as.numeric(w$sec_500m), na.rm=TRUE) # X
u3 <- mean(as.numeric(w$Watts), na.rm = TRUE) # OK?
#

v <- !is.nan(x$sec_500m)






# convert seconds to minutes

# break pace into seconds
x$min <- x$Pace
mu=mean(x$Meters,na.rm=TRUE)

plot(z[,2],plot.type='single', col="blue",lty=1,lwd=2,ylim=c(0,1000))




##### change column names #######
# r.2017 <- read.csv("~/Downloads/R_projects/project_Row/data/2016_10Sep_2016-2017_season.csv", as.is=TRUE)
# 
# # keep columns 1:11
# df <-subset(r.2017,select=c(1:11))
# head(df)
# 
# # change names:
# colnames(df) <- c("Date","Description","Minutes","Seconds","Rest: Minutes","Rest:Seconds","Meters","Rest:Meters","Pace","SPM","Watts")
# head(df)
# 
# # create a time index, use zoo:as.Date()  ...... NOT WORKING
# dt <- as.Date(df[,1], format="%Y/%m/%d")
# head(dt)




###############################  zoo ##################3



