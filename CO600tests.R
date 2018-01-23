library(lubridate)
library(dplyr)
library(timeDate)


#able to filter using datetime values between startdate and enddate
df1 <- activeenergyburned %>% 
    filter(startdate > as_datetime("2018-01-09 16:23:51") & enddate
           < as_datetime("2018-01-09 16:29:29"))
View(df1)


#able to filter using between method, however we can only use either startdate or enddate
df2 <- activeenergyburned %>%
  filter(between(enddate, as_datetime("2018-01-09 16:18:33"), as_datetime("2018-01-09 16:29:29"))) %>%
  summarise(total = sum(total))
View(df2)

#able to filter the data of the last 7 days and retrieve the total energyburned
df3 <- activeenergyburned %>%
  filter(between(enddate, now() - days(3), now())) %>%
  summarise(total = sum(total))
View(df3)

#test
n <- 7
res <- rep(NA, n)
for(i in 1:n){
  df4 <- activeenergyburned %>%
    filter(between(enddate, now() - days(i), now() - days(i - 1))) %>%
    summarise(total = sum(total))
  #View(df4)
  res[i] <- df4
}

print(res)

View(activeenergyburned)


