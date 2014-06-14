#file with raw R-code for peer assessment
#see PA1_template.Rmd for the actual submission file

rm(list=ls(all=TRUE)) 
gc()

library(ggplot2)

wdir<-'C:/Users/Michael/Documents/GitHub/RepData_PeerAssessment1'
setwd(wdir)

#uncomment this if you have not yet unzipped the data
#unzip('activity.zip')

df <- read.csv('activity.csv')

#group by date and aggregate the sum of steps
dailysteps <- aggregate(df$steps,list(Day=df$date),FUN=sum,na.rm=TRUE)

#histogram of total number of steps taken each day
ggplot(dailysteps, aes(x=x)) + geom_histogram(fill="#FF9999", colour="black") +
  ylab('count')+xlab('total number of steps in a day')

#display mean and median number of daily steps
mean(dailysteps$x)
median(dailysteps$x)

#group by interval and aggregate the mean of steps
intervalsteps <- aggregate(df$steps,list(Interval=df$interval),FUN=mean,na.rm=TRUE)

#plot steps versus interval
ggplot(intervalsteps, aes(Interval)) +
  geom_line(aes(y=x),color='springgreen3') +
  ylab('Mean number of steps in interval')

#the interval with the maximum average steps, 104th interval
#between 835 and 840 minutes
intervalsteps[which(intervalsteps$x==max(intervalsteps$x)),'Interval']

#imputation

#number of incomplete rows, i.e. ones with missing data
sum(!complete.cases(df))

#http://stackoverflow.com/questions/9322773/how-to-replace-na-with-mean-by-subset-in-r-impute-with-plyr
#impute interval group-wise means
library(plyr)
impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
df_filled <- ddply(df, ~interval, transform, steps = impute.mean(steps))
#ddply messes up the ordering, reorder first by date then by interval
df_filled <- df_filled[order(df_filled$date,df_filled$interval),]

#group by date and aggregate the sum of steps, filled data
dailysteps_filled <- aggregate(df_filled$steps,
                               list(Day=df_filled$date),FUN=sum,na.rm=TRUE)

#histogram of total number of steps taken each day, filled data
ggplot(dailysteps_filled, aes(x=x)) + geom_histogram(fill="#FF9999", colour="black") +
  ylab('count')+xlab('total number of steps in a day')

#display mean and median number of daily steps, filled data
mean(dailysteps_filled$x)
median(dailysteps_filled$x)

#weekday plots
dates <- as.Date(df_filled$date,format='%Y-%m-%d')
wkdays <- weekdays(dates)
wkdays[wkdays %in% c('Saturday','Sunday')]<-'weekend'
wkdays[!wkdays %in% c('weekend')]<-'weekday'
df_filled$weekdays <- wkdays

#aggregate over both the weekday and the interval
intervalsteps2 <- aggregate(df_filled$steps,
                  list(Interval=df_filled$interval,Weekday=df_filled$weekdays),
                  FUN=mean,na.rm=TRUE)

#plot steps versus interval with a weekday facet grid
ggplot(intervalsteps2, aes(Interval)) +
  geom_line(aes(y=x,color=Weekday)) +
  facet_wrap(~ Weekday,nrow=2)+
  ylab('Number of steps')+
  theme(legend.position='none') #remove legend