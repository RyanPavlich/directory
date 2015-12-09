library(dplyr)
library(moments)
library(DBI)
library(RPostgreSQL)
library(plotrix)
library(randomForest)
library(ggplot2)
library(knitr)

drv <- dbDriver("PostgreSQL")

# #                host = 'localhost',
#               port = '5432',
#              user = 'postgres',
#              password = '588658'
#)

flights.db = src_postgres(dbname = "flights", host = "localhost", port = 5432, user = "postgres", password = 588658) 
flights <- tbl(flights.db, "connections")

flights.desc <- flights %>%
  select(Year, Month, ArrDelay) %>%
  filter(Year == 2010) %>%
  collect() %>%
  group_by(Month) %>%
  summarise(
    count = n(),
    sum = sum(ArrDelay, na.rm = TRUE),
    min = min(ArrDelay, na.rm = TRUE),
    max = max(ArrDelay, na.rm = TRUE),
    mean = mean(ArrDelay, na.rm = TRUE),
    median = median(ArrDelay, na.rm = TRUE),
    range = max - min,
    q1 = as.numeric(quantile(ArrDelay, na.rm = TRUE)[2]),
    q3 = as.numeric(quantile(ArrDelay, na.rm = TRUE)[4]),
    wmin = if ((q1 - 1.5*iqr) < min){min} else {q1-1.5*iqr},
    wmax = if ((q3 - 1.5*iqr) > max){max} else {q1+1.5*iqr},
    iqr = q3 - q1,
    sd = sd(ArrDelay, na.rm = TRUE),
    var = var(ArrDelay, na.rm = TRUE),
    se = std.error(ArrDelay, na.rm = TRUE),
    kurt = kurtosis(ArrDelay, na.rm = TRUE),
    skew = skewness(ArrDelay, na.rm = TRUE)
  )
ggplot(flights.desc, aes(x = as.factor(Month))) +
  geom_errorbar(aes(ymin = wmin,
                    ymax = wmax)) +
  geom_boxplot(aes(lower = q1,
                   upper = q3,
                   middle = median,
                   ymin = wmin,
                   ymax = wmax), stat = "identity")

