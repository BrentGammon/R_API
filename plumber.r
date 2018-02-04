# plumber.R
library("ggpubr")
library("dplyr")
library("lubridate")
library("gridExtra")

#' @filter cors
cors <- function(res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  plumber::forward()
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
  
  conv <- as.data.frame(dataset1)
  conv2 <- as.data.frame(dataset2)

  
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
  View(df100)
  View(df200)


  df300 <- df20[(df20$hour %in% df2$hour), ]
  df400 <- df2[(df2$hour %in% df20$hour), ]


  df500 <- df30[(df30$hour %in% df3$hour), ]
  df600 <- df3[(df3$hour %in% df30$hour), ]


  df700 <- df40[(df40$hour %in% df4$hour), ]
  df800 <- df4[(df4$hour %in% df40$hour), ]

  
  
  title <-
    paste("Correlation between", parameter1, "and", parameter2, "over ...", sep = " ")
  
  attach(mtcars)
  par(mfrow=c(2,2), mai=c(0.8,1,2,1), cex=1.5)
  plot(df100$total, df200$total, type = "p", ann = FALSE)

  title("1 Hour")
  plot(df300$total, df400$total, type = "p", ann = FALSE)

  title("1 Day")
  plot(df500$total, df600$total, type = "p", ann = FALSE)

  title("1 Week")
  plot(df700$total, df800$total, type = "p", ann = FALSE)

  title("1 Month")
  mtext(title, side = 3, line = -2, outer=TRUE, cex = 2.5)

  
}

sumtotal <- function(conv, duration1){
  conv %>%
    mutate(startdate = ymd_hms(startdate)) %>%
    mutate(total = as.numeric(total)) %>%
    mutate(month_name = month(startdate, label = TRUE)) %>%
    group_by(hour = floor_date(startdate, duration1)) %>%
    summarize(total = sum(total))
}

meantotal <- function(conv, duration1){
  conv %>%
    mutate(startdate = ymd_hms(startdate)) %>%
    mutate(total = as.numeric(total)) %>%
    mutate(month_name = month(startdate, label = TRUE)) %>%
    group_by(hour = floor_date(startdate, duration1)) %>%
    summarize(total = mean(total))
}
