## ROW, version 2.0


# import row data in R, as df
## tags:  xts

##   xts advantages, vs zoo? 
##   1.   formal time index?  (Date, POSIXct, yearmon, yearqtr etc)
##   2.   xts has more internal properties:    .rownames?   .class?
##   3.   xts has user-added attributes

#########################################
##  quanmod package?  (NOT used, just documenting)

## useful for financial
## periodicity(xts);  nice ways to aggregage or pickout friday or last day of month
##      endpoints(xts, on='months') or 'weeks'
##      to.period(xts,'months')
##      to.monthly
########################################


rm(list=ls())
getwd()
library(xts)


f <- "~/Downloads/R_projects/project_Row/data/2016_11Nov_2016-2017_season.csv"
df <- read.csv(file=f,
                header = TRUE,
                sep=",",
                na.strings=c("NA","N/A"),
                stringsAsFactors = FALSE)

typeof(df)      # data.frame
class(df)       # data.frame
str(df)         # see some problems:  fix Date and remove unnecassary fields

# fix Date
df$Date<-as.POSIXct(df$Date)

# number fields, with prefix
# paste is concatination 
names(df) <- paste( as.character(c(1:18)),
                    names(df))



# keep only a few relevant columns (unless concept2 changes)
df <- subset(df, select=c(1,2,4,7,10,11))
head(df)

# all remain character
str(df)




# Need?
#df2$Date <- as.POSIXct(strptime(df2$Date,"%Y-%m-%d %H:%M:%S"))   # not POSIXct?  as.Date seems to work fine

# create t index
t <- df[,1]

# data (careful, no chr allowed) - no description, no Pace
d <- df[,c(3,4,6)]
names(d)<- paste(as.character(c(1:3)),
                 c("Time(sec)", "Dist(meters)","Power(watts)")
                 )
# xts constructor
x <- xts(x = d, order.by =t)


str(x)
indexClass(x) # POSIXct, good
coredata(x)

# August 1, 2016 ; subset
x['2016-08-01']

# all the data from first 'week', 'month', '3days'
first(x,'month')

# vector for each monnth
axTicksByTime(x,ticks.on='months')

# plot seconds (col 1) in green
plot.xts(x=x[,1],major.ticks='months', auto.grid=TRUE, 
     minor.ticks=FALSE, main=NULL, 
     col=3, 
     ylab="seconds", xlab="2016-17 season" )

abline(h=c(1800,3600))
# NEXT   meters
plot.xts(x=x[,2],major.ticks='months', auto.grid=TRUE, 
         minor.ticks=FALSE, main=NULL, 
         col=3, 
         ylab="meters", xlab="2016-17 season" )
abline(h=6000)

# power
plot.xts(x=x[,3], type='p',
         major.ticks='months', auto.grid=TRUE, 
         minor.ticks=FALSE, main=NULL, 
         col=3, 
         ylab="Power (watts)", xlab="2016-17 season" )
abline(h=50)
visits<-c(as.POSIXct("2016-08-15"), as.POSIXct(("2016-09-19")))
abline(v=visits)
