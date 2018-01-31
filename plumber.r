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
#' @png
function(dataset1,
         dataset2,
         parameter1,
         parameter2,
         duration) {
  
  options(scipen=999)
  
  conv <- as.data.frame(dataset1)
  conv3 <- as.data.frame(dataset1)
  conv5 <- as.data.frame(dataset1)
  conv7 <- as.data.frame(dataset1)
  
  #1 Hour
  df1 <- conv %>%
    mutate(startdate = ymd_hms(startdate),
           enddate = ymd_hms(enddate)) %>%
    mutate(total = as.numeric(total)) %>%
    mutate(month_name = month(startdate, label = TRUE)) %>%
    group_by(hour = floor_date(startdate, "hour")) %>%
    summarize(total = sum(total))

  conv2 <- as.data.frame(dataset2)

  df2 <- conv2 %>%
    mutate(startdate = ymd_hms(startdate),
           enddate = ymd_hms(enddate)) %>%
    mutate(total = as.numeric(total)) %>%
    mutate(month_name = month(startdate, label = TRUE)) %>%
    group_by(hour = floor_date(startdate, "hour")) %>%
    summarize(total = sum(total))

  df3 <- df2[(df2$hour %in% df1$hour), ]
  df4 <- df1[(df1$hour %in% df2$hour), ]
  
  #1 Day
  df5 <- conv3 %>%
    mutate(startdate = ymd_hms(startdate),
           enddate = ymd_hms(enddate)) %>%
    mutate(total = as.numeric(total)) %>%
    mutate(month_name = month(startdate, label = TRUE)) %>%
    group_by(hour = floor_date(startdate, "1 day")) %>%
    summarize(total = sum(total))


  conv4 <- as.data.frame(dataset2)

  df6 <- conv4 %>%
    mutate(startdate = ymd_hms(startdate),
           enddate = ymd_hms(enddate)) %>%
    mutate(total = as.numeric(total)) %>%
    mutate(month_name = month(startdate, label = TRUE)) %>%
    group_by(hour = floor_date(startdate, "1 day")) %>%
    summarize(total = sum(total))

  df7 <- df6[(df6$hour %in% df5$hour), ]
  df8 <- df5[(df5$hour %in% df6$hour), ]

  # #7 Days
  df9 <- conv5 %>%
    mutate(startdate = ymd_hms(startdate),
           enddate = ymd_hms(enddate)) %>%
    mutate(total = as.numeric(total)) %>%
    mutate(month_name = month(startdate, label = TRUE)) %>%
    group_by(hour = floor_date(startdate, "7 days")) %>%
    summarize(total = sum(total))

  conv6 <- as.data.frame(dataset2)

  df10 <- conv6 %>%
    mutate(startdate = ymd_hms(startdate),
           enddate = ymd_hms(enddate)) %>%
    mutate(total = as.numeric(total)) %>%
    mutate(month_name = month(startdate, label = TRUE)) %>%
    group_by(hour = floor_date(startdate, "7 days")) %>%
    summarize(total = sum(total))

  df11 <- df10[(df10$hour %in% df9$hour), ]
  df12 <- df9[(df9$hour %in% df10$hour), ]


  # #30 Days
  df13 <- conv7 %>%
    mutate(startdate = ymd_hms(startdate),
           enddate = ymd_hms(enddate)) %>%
    mutate(total = as.numeric(total)) %>%
    mutate(month_name = month(startdate, label = TRUE)) %>%
    group_by(hour = floor_date(startdate, "30 days")) %>%
    summarize(total = sum(total))

  conv8 <- as.data.frame(dataset2)

  df14 <- conv8 %>%
    mutate(startdate = ymd_hms(startdate),
           enddate = ymd_hms(enddate)) %>%
    mutate(total = as.numeric(total)) %>%
    mutate(month_name = month(startdate, label = TRUE)) %>%
    group_by(hour = floor_date(startdate, "30 days")) %>%
    summarize(total = sum(total))

  df15 <- df14[(df14$hour %in% df13$hour), ]
  df16 <- df13[(df13$hour %in% df14$hour), ]
  
  # title <-
  #   paste("Correlation between", parameter1, "and", parameter2, "over 7 days", sep = " ")
  # plot1 <- plot(df1$total, df3$total, type = "p", ann = FALSE)
  # title("Correlation between", parameter1, "and", parameter2, "over x days", sep = " ", xlab = parameter1, ylab = parameter2)
  
  # 4 figures arranged in 4 rows and 1 columns
  attach(mtcars)
  par(mfrow=c(4,1))
  plot(df3$total, df4$total, type = "p", ann = FALSE)
  plot(df7$total, df8$total, type = "p", ann = FALSE)
  plot(df11$total, df12$total, type = "p", ann = FALSE)
  plot(df10$total, df12$total, type = "p", ann = FALSE)
}

