#annova homework

library(DBI)
library(RPostgreSQL)
library(dplyr)
library(ggplot2)
library(plotrix)
library(Hmisc)
library(car)
library(moments)
library(reshape2)

#pulled csv from folder not db. it was not specified in class touse data from db or directory
## must set wd for this to work

setwd("Class 3302/Data/")

flights=read.csv("On_Time_On_Time_Performance_2013_2.csv")
head(flights)

# Cleaning & melting data 
## had assitance with metling 
flights.delayed$ID = paste(row.names(flights.delayed), flights.delayed$Year, flights.delayed$Month, sep="")
View(flights.delayed)
flights.delayed = flights.delayed[, 3:9]

flights.delayed.m = melt (flights.delayed, variable.name = "TypeOfDelay", value.name = "ArrDelay", id.vars = "ID")
View(flights.delayed.m)
head(flights.delayed.m)
tail(flights.delayed.m)
summary(flights.delayed.m)


#getting rid of the non values and applying it to the the data frame 
##helped by collin and matt for following steps 

na.zero <- function (x) {
  x[is.na(x)] <- 0
  return(x)
}

flights.delayed.m = na.zero(flights.delayed.m)

summary(flights.delayed.m)

#Subsetting the table to remove duplicates found when melting,
##source: Help from Patrick Magnusson
flights.delayed.m = subset (flights.delayed.m, flights.delayed.m$ArrDelay!=0)
summary(flights.delayed.m)

#as in class, create the lienar model for testing 
flights.delayed.l = lm(data=flights.delayed.m, ArrDelay ~ TypeOfDelay)
flights.delayed.l

flights.delayed.a = aov(flights.delayed.l)
flights.delayed.a

flights.delayed.l.as = summary(flights.delayed.l)
flights.delayed.l.as


#Musing ggplot to plot results
flights.delayed.ggp = ggplot(flights.delayed.m, aes(x=TypeOfDelay, y=ArrDelay)) +
  stat_boxplot(geom="errorbar") +
  geom_boxplot() +
  labs(y="TimeOfDelay", x="TypeOfDelay")

flights.delayed.ggp


#analyze different potentially different outcomes
#source: matt
delayed.l.t = TukeyHSD(flights.delayed.a)
delayed.l.t

leveneTest(flights.delayed.l)

degrees = anova(flights.delayed.l)[, "Df"]
names(degrees) = c("between", "within")
f.crit = qf(.05, degrees["between"], degrees["within"], lower.tail=FALSE)
f.value = flights.delayed.l.as[[1]]$`F value`[1]
f.crit
f.value

#annnova plot and graph. this was from class

ncp = 0
frameEmpty = "black"
areaEmpty = "white"
frameH0 = "green4"
areaH0 = "green3"
frameH1 = "red4"
areaH1 = "red2"

#creating the distribution settings

df1 = degrees[1] 
df2 = degrees[2] 
length = 500 

x = seq(from = 0, to = f.value +2, length = length) 
dd = data.frame (x =seq (from = 0, to = f.value+2, length = length),
                 y = df (x = x, df1 = df1, df2 = df2, ncp = ncp))

#f Plot
pf = ggplot(data = dd) 
pf = pf + labs(y = "Relative frequency", x = "F-values") 
pf = pf + geom_area (aes(x = x, y = y),
                     color = frameH0, fill = areaH0) 
pf = pf + geom_area(data = subset(dd, x > f.crit),
                    aes(x = x, y = y),
                    fill = areaH1, color = frameH1) 
pf = pf + geom_vline (xintercept = f.crit, colour = frameH1, linetype = "longdash")
pf = pf + geom_vline (xintercept = f.value, colour = "black", linetype = "dotted")
pf = pf + scale_x_continuous(breaks = sort(round(c(seq(from = min(dd$x),
                                                       to = round (max (dd$x), 0),
                                                       by = 2), f.crit, f.value, 2))))
pf = pf + annotate ("text", y = .6, x = f.value + 1,
                    label = paste ("Pr(>F) = ", round(flights.delayed.l.as[[1]]$Pr[1],3)))
pf

# collin and matt assited throughout the annova analysis
##the graph is taken from your code 
