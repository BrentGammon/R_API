# plumber.R
library("ggpubr",warn.conflicts = FALSE)
library("dplyr",warn.conflicts = FALSE)
library("lubridate",warn.conflicts = FALSE)
library("gridExtra",warn.conflicts = FALSE)
library('ggplot2',warn.conflicts = FALSE)
library("jsonlite")
#install.packages("tidyr")
library("tidyr",warn.conflicts = FALSE)


#' @filter cors
cors <- function(res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  plumber::forward()
}


#' dashboardcharts endpoint
#' @param dataset1 The first dataset
#' @param dataset2 The second dataset
#' @param dataset3 The third dataset
#' @param dataset4 The fourth dataset
#' @param dataset5 The fifth dataset
#' @param dataset6 The sixth dataset
#' @param dataset7 The seventh dataset
#' @param dataset8 The eigth dataset
#' @param dataset9 The ninth dataset
#' @post /dashboardcharts
#' @png (width = 1000, height = 1000)
function(dataset1,
         dataset2,
         dataset3,
         dataset4,
         dataset5,
         dataset6,
         dataset7,
         dataset8,
         dataset9) {
  options(scipen=999)
  conv <- as.data.frame(dataset1) #activeenergyburned
  conv2 <- as.data.frame(dataset2) #deepsleep
  conv3 <- as.data.frame(dataset3) #flightsclimbed
  conv4 <- as.data.frame(dataset4) #heartrate
  conv5 <- as.data.frame(dataset5) #sleep
  conv6 <- as.data.frame(dataset6) #sleepheartrate
  conv7 <- as.data.frame(dataset7) #stepcounter
  conv8 <- as.data.frame(dataset8) #walkingrunningdistance
  conv9 <- as.data.frame(dataset9) #userinput

  View(conv9)

  
  df1 <- sumtotal(conv, '1 day') #activeenergyburned
  df2 <- meantotal(conv2, '1 day') #deepsleep
  df3 <- sumtotal(conv3, '1 day') #flightsclimbed
  df4 <- meantotal(conv4, '1 day') #heartrate
  df5 <- meantotal(conv5, '1 day') #sleep
  df6 <- meantotal(conv6, '1 day') #sleepheartrate
  df7 <- sumtotal(conv7, '1 day') #stepcounter
  df8 <- sumtotal(conv8, '1 day') #walkingrunningdistance
  
  View(df1)
  View(df2)
  View(df3)
  View(df4)
  View(df5)
  View(df6)
  View(df7)
  View(df8)

  df10 <- df1 %>%
    select(total, hour) %>%
    ggplot(aes(x=hour, y=total))+
    geom_bar(stat="identity", fill = "#FF6666", alpha=0.7) +
    xlab("Date") +
    ylab("Active Energy Burned")
  
  df20 <- df2 %>%
    select(total, hour) %>%
    ggplot(aes(x=hour, y=total))+
    geom_bar(stat="identity", fill = "#FF6666", alpha=0.7) +
    xlab("Date") +
    ylab("Deep Sleep")
  
  df30 <- df3 %>%
    select(total, hour) %>%
    ggplot(aes(x=hour, y=total))+
    geom_bar(stat="identity", fill = "#FF6666", alpha=0.7) +
    xlab("Date") +
    ylab("Flights Climbed")
  
  df40 <- df4 %>%
    select(total, hour) %>%
    ggplot(aes(x=hour, y=total))+
    geom_bar(stat="identity", fill = "#FF6666", alpha=0.7) +
    xlab("Date") +
    ylab("HeartRate")
  
  df50 <- df5 %>%
    select(total, hour) %>%
    ggplot(aes(x=hour, y=total))+
    geom_bar(stat="identity", fill = "#FF6666", alpha=0.7) +
    xlab("Date") +
    ylab("Sleep")
  
  df60 <- df6 %>%
    select(total, hour) %>%
    ggplot(aes(x=hour, y=total))+
    geom_bar(stat="identity", fill = "#FF6666", alpha=0.7) +
    xlab("Date") +
    ylab("Sleep Heartrate")
  
  df70 <- df7 %>%
    select(total, hour) %>%
    ggplot(aes(x=hour, y=total))+
    geom_bar(stat="identity", fill = "#FF6666", alpha=0.7) +
    xlab("Date") +
    ylab("Step Counter")
  
  df80 <- df8 %>%
    select(total, hour) %>%
    ggplot(aes(x=hour, y=total))+
    geom_bar(stat="identity", fill = "#FF6666", alpha=0.7) +
    xlab("Date") +
    ylab("Walking Running Distance")
  
  dfstress <- conv9 %>%
    mutate(startdate = ymd_hms(startdate)) %>%
    select(stresslevel, startdate) %>%
    ggplot(aes(x=startdate, y=stresslevel))+
    geom_bar(stat="identity", fill = "#FF6666", alpha=0.7) +
    xlab("Date") +
    ylab("Stress Level")
  
  dftired <- conv9 %>%
    mutate(startdate = ymd_hms(startdate)) %>%
    select(tirednesslevel, startdate) %>%
    ggplot(aes(x=startdate, y=tirednesslevel))+
    geom_bar(stat="identity", fill = "#FF6666", alpha=0.7) +
    xlab("Date") +
    ylab("Tiredness Level")
  
  dfhealth <- conv9 %>%
    mutate(startdate = ymd_hms(startdate)) %>%
    select(healthinesslevel, startdate) %>%
    ggplot(aes(x=startdate, y=healthinesslevel))+
    geom_bar(stat="identity", fill = "#FF6666", alpha=0.7) +
    xlab("Date") +
    ylab("Healthiness Level")
  
  dfactive <- conv9 %>%
    mutate(startdate = ymd_hms(startdate)) %>%
    select(activitylevel, startdate) %>%
    ggplot(aes(x=startdate, y=activitylevel))+
    geom_bar(stat="identity", fill = "#FF6666", alpha=0.7) +
    xlab("Date") +
    ylab("Activity Level")
  
  dfscatstress <- conv9 %>%
    mutate(startdate = ymd_hms(startdate)) %>%
    select(stresslevel, startdate) %>%
    ggplot(aes(x=startdate, y=stresslevel))+
    geom_point(aes(y = stresslevel) ,colour = "#FFA500", alpha=0.7) +
    geom_line(alpha=0.5) +
    xlab("Date") +
    ylab("Stress Level")
  
  dfscattired <- conv9 %>%
    mutate(startdate = ymd_hms(startdate)) %>%
    select(tirednesslevel, startdate) %>%
    ggplot(aes(x=startdate, y=tirednesslevel))+
    geom_point(aes(y = tirednesslevel) ,colour = "#FFA500", alpha=0.7) +
    geom_line(alpha=0.5) +
    xlab("Date") +
    ylab("Tiredness Level")
  
  dfscathealth <- conv9 %>%
    mutate(startdate = ymd_hms(startdate)) %>%
    select(healthinesslevel, startdate) %>%
    ggplot(aes(x=startdate, y=healthinesslevel))+
    geom_point(aes(y = healthinesslevel) ,colour = "#FFA500", alpha=0.7) +
    geom_line(alpha=0.5) +
    xlab("Date") +
    ylab("Healthiness Level")
  
  dfscatactive <- conv9 %>%
    mutate(startdate = ymd_hms(startdate)) %>%
    select(activitylevel, startdate) %>%
    ggplot(aes(x=startdate, y=activitylevel))+
    geom_point(aes(y = activitylevel) ,colour = "#FFA500", alpha=0.7) +
    geom_line(alpha=0.5) +
    xlab("Date") +
    ylab("Activity Level")
  
  
  dfA <- ggarrange(df10, df20, df30, df40, df50, df60, df70, df80, dfstress, dftired, dfhealth, dfactive, dfscatstress, dfscattired, dfscathealth, dfscatactive
                   + rremove("x.text"),
            ncol = 4, nrow = 4)
  
  plot(dfA)
    
}






#' Correlation endpoint
#' @param dataset1 The first dataset
#' @param dataset2 The second dataset
#' @param parameter1 1st parameter
#' @param parameter2 2nd parameter
#' @param duration Month, Week, Day, Hours
#' @post /correlation
#' @png (width = 1000, height = 1000)
function(dataset1,
         dataset2,
         parameter1,
         parameter2,
         duration) {
  options(scipen=999)
  print('hello world')
  conv <- as.data.frame(dataset1)
  conv2 <- as.data.frame(dataset2)

  
  userinputvalues <- c('stresslevel','tirednesslevel','activitylevel','healthinesslevel')
  watchinputvalues <- c('activeenergyburned','deepsleep','flightsclimbed','heartrate','sleep','sleepheartrate','stepcounter','walkingrunningdistance')
  heartratevalues <- c('heartrate','sleepheartrate')
  
  if(parameter1 %in% watchinputvalues && parameter2 %in% watchinputvalues){
    #watchwatch
    
    if(parameter1 %in% heartratevalues){
      df1 <- meantotal(conv, 'hour')
      df2 <- meantotal(conv, '1 day')
      df3 <- meantotal(conv, '7 days')
      df4 <- meantotal(conv, '30 days')
    }else{
      df1 <- sumtotal(conv, 'hour')
      df2 <- sumtotal(conv, '1 day')
      df3 <- sumtotal(conv, '7 days')
      df4 <- sumtotal(conv, '30 days')
    }
    
    if(parameter2 %in% heartratevalues){
      df10 <- meantotal(conv2,'hour')
      df20 <- meantotal(conv2, '1 day')
      df30 <- meantotal(conv2, '7 days')
      df40 <- meantotal(conv2, '30 days')
    }else{
      df10 <- sumtotal(conv2, 'hour')
      df20 <- sumtotal(conv2, '1 day')
      df30 <- sumtotal(conv2, '7 days')
      df40 <- sumtotal(conv2, '30 days')
    }
  }else if(parameter1 %in% userinputvalues && parameter2 %in% userinputvalues){
    #useruser
    
    df1 <- meantotal(conv, 'hour')
    df2 <- meantotal(conv, '1 day')
    df3 <- meantotal(conv, '7 days')
    df4 <- meantotal(conv, '30 days')
    
    df10 <- meantotal(conv2,'hour')
    df20 <- meantotal(conv2, '1 day')
    df30 <- meantotal(conv2, '7 days')
    df40 <- meantotal(conv2, '30 days')
  }
  
  df100 <- df10[(df10$hour %in% df1$hour), ]
  df200 <- df1[(df1$hour %in% df10$hour), ]

  df300 <- df20[(df20$hour %in% df2$hour), ]
  df400 <- df2[(df2$hour %in% df20$hour), ]

  df500 <- df30[(df30$hour %in% df3$hour), ]
  df600 <- df3[(df3$hour %in% df30$hour), ]

  df700 <- df40[(df40$hour %in% df4$hour), ]
  df800 <- df4[(df4$hour %in% df40$hour), ]

  
  
  title <-
    paste("Correlation between", parameter1, "and", parameter2, "over ...", sep = " ")
  
  attach(mtcars)
  par(mfrow=c(2,2), mar=c(5,4,6,2), cex=1.5)
  
  plot(df100$total, df200$total, type = "p", ann = FALSE)
  title("1 Hour", xlab = parameter1, ylab = parameter2)

  plot(df300$total, df400$total, type = "p", ann = FALSE)
  title("1 Day", xlab = parameter1, ylab = parameter2)

  plot(df500$total, df600$total, type = "p", ann = FALSE)
  title("1 Week", xlab = parameter1, ylab = parameter2)

  plot(df700$total, df800$total, type = "p", ann = FALSE)
  title("1 Month", xlab = parameter1, ylab = parameter2)

  mtext(title, side = 3, line = -2, outer=TRUE, cex = 2.5)
  
  
  # if(parameter1 == 'heartrate' || parameter1 == 'sleepheartrate'){
  #   df1 <- meantotal(conv, 'hour')
  #   df2 <- meantotal(conv, '1 day')
  #   df3 <- meantotal(conv, '7 days')
  #   df4 <- meantotal(conv, '30 days')
  # }else{
  #   df1 <- sumtotal(conv, 'hour')
  #   df2 <- sumtotal(conv, '1 day')
  #   df3 <- sumtotal(conv, '7 days')
  #   df4 <- sumtotal(conv, '30 days')
  # }
  # 
  # if(parameter2 == 'heartrate' || parameter2 == 'sleepheartrate'){
  #   df10 <- meantotal(conv2,'hour')
  #   df20 <- meantotal(conv2, '1 day')
  #   df30 <- meantotal(conv2, '7 days')
  #   df40 <- meantotal(conv2, '30 days')
  #   
  # }else{
  #   df10 <- sumtotal(conv2, 'hour')
  #   df20 <- sumtotal(conv2, '1 day')
  #   df30 <- sumtotal(conv2, '7 days')
  #   df40 <- sumtotal(conv2, '30 days')
  # }
  
}




#' Infomation about dataset 1 and 2 
#' @param dataset1 The first dataset  
#' @param dataset2 The second dataset
#' @param parameter1 The name of datase1
#' @param parameter2 The name of dataset2
#' @post /datasetInformation
function(dataset1,dataset2,parameter1,parameter2){
  conv <- as.data.frame(dataset1)
  conv2 <- as.data.frame(dataset2)
  View(conv)
  View(conv2)
  
  if(parameter1 == 'heartrate' || parameter1 == 'sleepheartrate'){
    df1 <- meantotal(conv, 'hour')
    df2 <- meantotal(conv, '1 day')
    df3 <- meantotal(conv, '7 days')
    df4 <- meantotal(conv, '30 days')
  }else{
    df1 <- sumtotal(conv, 'hour')
    df2 <- sumtotal(conv, '1 day')
    df3 <- sumtotal(conv, '7 days')
    df4 <- sumtotal(conv, '30 days')
  }
  
  if(parameter2 == 'heartrate' || parameter2 == 'sleepheartrate'){
    df10 <- meantotal(conv2,'hour')
    df20 <- meantotal(conv2, '1 day')
    df30 <- meantotal(conv2, '7 days')
    df40 <- meantotal(conv2, '30 days')
    
  }else{
    df10 <- sumtotal(conv2, 'hour')
    df20 <- sumtotal(conv2, '1 day')
    df30 <- sumtotal(conv2, '7 days')
    df40 <- sumtotal(conv2, '30 days')
  }
  
  
  df100 <- df10[(df10$hour %in% df1$hour), ]
  df200 <- df1[(df1$hour %in% df10$hour), ]
  
  
  df300 <- df20[(df20$hour %in% df2$hour), ]
  df400 <- df2[(df2$hour %in% df20$hour), ]
  
  
  df500 <- df30[(df30$hour %in% df3$hour), ]
  df600 <- df3[(df3$hour %in% df30$hour), ]
  
  
  df700 <- df40[(df40$hour %in% df4$hour), ]
  df800 <- df4[(df4$hour %in% df40$hour), ]
  

 
 


  
  json <- list(datasetSummary(dataset1),datasetSummary(dataset2),correlation(df100,df200),correlation(df300,df400),correlation(df500,df600),correlation(df700,df800),
               tTestJSON(df100$total,df200$total),tTestJSON(df300$total,df400$total),tTestJSON(df500$total,df600$total),tTestJSON(df700$total,df800$total))
  names(json) <- c("dataset1", "dataset2","hourCor","1dayCor","7daysCor","30daysCor","hourTest","1dayTest","7daysTest","30daysTest")
  json <- toJSON(json)
  
}

#' MoodWatchCorrelation endpoint
#' @param dataset1 The first dataset
#' @param dataset2 The second dataset
#' @param parameter1 1st parameter
#' @param parameter2 2nd parameter
#' @post /moodwatchcorrelation
#' @png (width = 1000, height = 1000)
function(dataset1,
         dataset2,
         parameter1,
         parameter2) {
  options(scipen=999)
  
     conv <- as.data.frame(dataset1)
     conv2 <- as.data.frame(dataset2)
     View(conv)
     View(conv2)
  
  userinputvalues <- c('stresslevel','tirednesslevel','activitylevel','healthinesslevel')
  watchinputvalues <- c('activeenergyburned','deepsleep','flightsclimbed','heartrate','sleep','sleepheartrate','stepcounter','walkingrunningdistance')
  heartratevalues <- c('heartrate','sleepheartrate')
  
  if(parameter1 %in% userinputvalues || parameter1 %in% heartratevalues){
    df1 <- meantotal(conv, '3 hours')
    df2 <- meantotal(conv, '6 hours')
    df3 <- meantotal(conv, '1 day')
    df4 <- meantotal(conv, '10 days')
  }else{
    df1 <- sumtotal(conv, '3 hours')
    df2 <- sumtotal(conv, '6 hours')
    df3 <- sumtotal(conv, '1 day')
    df4 <- sumtotal(conv, '10 days')
  }
  
  if(parameter2 %in% userinputvalues || parameter2 %in% heartratevalues){
    df10 <- meantotal(conv, '3 hours')
    df20 <- meantotal(conv, '6 hours')
    df30 <- meantotal(conv, '1 day')
    df40 <- meantotal(conv, '10 days')
  }else{
    df10 <- sumtotal(conv, '3 hours')
    df20 <- sumtotal(conv, '6 hours')
    df30 <- sumtotal(conv, '1 day')
    df40 <- sumtotal(conv, '10 days')
  }
  
  df100 <- df10[(df10$hour %in% df1$hour), ]
  df200 <- df1[(df1$hour %in% df10$hour), ]
  
  df300 <- df20[(df20$hour %in% df2$hour), ]
  df400 <- df2[(df2$hour %in% df20$hour), ]
  
  df500 <- df30[(df30$hour %in% df3$hour), ]
  df600 <- df3[(df3$hour %in% df30$hour), ]
  
  df700 <- df40[(df40$hour %in% df4$hour), ]
  df800 <- df4[(df4$hour %in% df40$hour), ]
  
  
  
  title <-
    paste("Correlation between", parameter1, "and", parameter2, "over ...", sep = " ")
  
  attach(mtcars)
  par(mfrow=c(2,2), mar=c(5,4,6,2), cex=1.5)
  
  plot(df100$total, df200$total, type = "p", ann = FALSE)
  title("1 Hour", xlab = parameter1, ylab = parameter2)
  
  plot(df300$total, df400$total, type = "p", ann = FALSE)
  title("1 Day", xlab = parameter1, ylab = parameter2)
  
  plot(df500$total, df600$total, type = "p", ann = FALSE)
  title("1 Week", xlab = parameter1, ylab = parameter2)
  
  plot(df700$total, df800$total, type = "p", ann = FALSE)
  title("1 Month", xlab = parameter1, ylab = parameter2)
  
  mtext(title, side = 3, line = -2, outer=TRUE, cex = 2.5)
  
  
  # if(parameter1 %in% moodvalues){
  #   #dataset1 represents mood data from express
  #   #dataset2 represents watch data from express
  #   
  #   conv <- as.data.frame(dataset1)
  #   conv2 <- as.data.frame(dataset2)
  #   View(conv)
  #   View(conv2)
  #   
  #   #dfMoodData and dfWatchData are created from the formatted datasets coming from express
  #   
  #   dfMoodData <- conv %>%
  #     mutate(startdate = ymd_hms(startdate)) %>%
  #     select(startdate, id, level)
  #   
  #   dfWatchData <- conv2 %>%
  #     mutate(startdate = ymd_hms(startdate)) %>%
  #     mutate(total = as.numeric(total)) %>%
  #     select(startdate, total)  
  #   
  #   #moodwatchmean and moodwatchsum aggregate the watch data using the mood submission timestamps
  #   #heartrate values are aggregated by mean
  #   #dfAggrMoodWatch is a single dataframe that consists of the full join of mood data and aggregated watch data
  #   #by mood submission timestamps
  #   
  #   meanparameters <- c('heartrate','sleepheartrate')
  #   
  #     if(parameter2 %in% meanparameters || parameter2 %in% meanparameters){
  #       dfAggrMoodWatch <- moodwatchmean(dfMoodData, dfWatchData)  
  #     }else{
  #       dfAggrMoodWatch <- moodwatchsum(dfMoodData, dfWatchData)
  #     }
  #   
  #   #dfAggrMoodWatch is used to create dfAggrMood and dfAggrWatch. They consist of the useful columns
  #   #if further aggregation is required
  #   
  #   dfAggrMood <- dfAggrMoodWatch %>%
  #     select(startdate, level, total)
  #   
  #   dfAggrWatch <- dfAggrMoodWatch %>%
  #     select(startdate, total)
  #   
  #   plot(dfAggrMood$level, dfAggrWatch$total, type = "p", ann = FALSE)
  #   
  # }else{
  #   #if parameter 1 was not included in the moodvalues list, parameter 2 is a mood values,
  #   #therefore dataset1 (which is mood data) is assigned to conv2
  #   #dataset2 (watch data)  is assigned to conv
  #   
  #   conv <- as.data.frame(dataset2) 
  #   conv2 <- as.data.frame(dataset1)
  #   
  #   #dfMoodData and dfWatchData are created from the formatted datasets coming from express
  #   
  #   dfMoodData <- conv2 %>%
  #     mutate(startdate = ymd_hms(startdate)) %>%
  #     select(startdate, id, level)
  #   
  #   dfWatchData <- conv %>%
  #     mutate(startdate = ymd_hms(startdate)) %>%
  #     mutate(total = as.numeric(total)) %>%
  #     select(startdate, total)  
  #   
  #   meanparameters <- c('heartrate','sleepheartrate')
  #   
  #     if(parameter1 %in% meanparameters || parameter1 %in% meanparameters){
  #       dfAggrMoodWatch <- moodwatchmean(dfMoodData, dfWatchData)
  #     }else{
  #       dfAggrMoodWatch <- moodwatchsum(dfMoodData, dfWatchData)
  #     }
  #   
  #   dfAggrMood <- dfAggrMoodWatch %>%
  #     select(startdate, level)
  #   
  #   dfAggrWatch <- dfAggrMoodWatch %>%
  #     select(startdate, total)
  #   
  #   plot(dfAggrMood$level, dfAggrWatch$total, type = "p", ann = FALSE)
  #   
  # }
  
  
}


tTestJSON <- function(data1,data2){
  item <- t.test(data1,data2,paired=TRUE)
  
  json <- list(item[["statistic"]][["t"]], item[["p.value"]], item[["estimate"]][["mean of the differences"]],item[["conf.int"]])
  names(json) <- c("tValue", "pValue", "MD", "confInt")
  json
  
}

correlation <- function(dataset1, dataset2) {
  d1 <- as.data.frame(as.numeric(dataset1$total))
  d2 <- as.data.frame(as.numeric(dataset2$total))
  cor(d1, d2)
}

datasetSummary <- function(dataset) {
  convJSON <- as.data.frame(as.numeric(dataset$total))
  datasetStats <- as.data.frame(summary(convJSON))
  View(datasetStats)
  datasetStatsObject<-datasetStats %>%
    select(Freq)
  datasetStatsObject 
}

sumtotal <- function(conv, duration1){
  conv %>%
    mutate(startdate = ymd_hms(startdate)) %>%
    mutate(total = as.numeric(total)) %>%
    mutate(month_name = month(startdate)) %>%
    group_by(hour = floor_date(startdate, duration1)) %>%
    summarize(total = sum(total))
}

meantotal <- function(conv, duration1){
  print(duration1)
  conv %>%
    mutate(startdate = ymd_hms(startdate)) %>%
    mutate(total = as.numeric(total)) %>%
    mutate(month_name = month(startdate)) %>%
    group_by(hour = floor_date(startdate, duration1)) %>%
    summarize(total = mean(total))
}

moodwatchsum <- function(dfMood, dfWatch){
  
    full_join(dfMood, dfWatch, by = "startdate") %>% 
    arrange(startdate) %>% 
    fill(id, level, .direction = "up") %>% 
    group_by(id, level) %>% 
    summarize(total = sum(total, na.rm = TRUE)) %>%
    na.omit() %>%
    ungroup() %>% 
    select(total) %>% 
    bind_cols(dfMood, .)
  
}

moodwatchmean <- function(dfMood, dfWatch){
  
  full_join(dfMood, dfWatch, by = "startdate") %>% 
    arrange(startdate) %>% 
    fill(id, level, .direction = "up") %>% 
    group_by(id, level) %>% 
    summarize(total = sum(total, na.rm = TRUE)) %>% #should be mean instead of sum here
    na.omit() %>%
    ungroup() %>% 
    select(total) %>% 
    bind_cols(dfMood, .)
  
}
