# plumber.R
library("ggpubr")
library("dplyr")
library("lubridate")

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
  
  conv <- as.data.frame(dataset1)
  
  # for (times in t("hour", "3 hours", "6 hours", "12 hours", "day")) {
  # }
  
  df1 <- conv %>%
    mutate(startdate = ymd_hms(startdate),
           enddate = ymd_hms(enddate)) %>%
    mutate(total = as.numeric(total)) %>%
    mutate(month_name = month(startdate, label = TRUE)) %>%
    group_by(hour = floor_date(startdate, "7 days")) %>%
    summarize(total = sum(total))
  
  conv2 <- as.data.frame(dataset2)
  
  df2 <- conv2 %>%
    mutate(startdate = ymd_hms(startdate),
           enddate = ymd_hms(enddate)) %>%
    mutate(total = as.numeric(total)) %>%
    mutate(month_name = month(startdate, label = TRUE)) %>%
    group_by(hour = floor_date(startdate, "7 days")) %>%
    summarize(total = sum(total))
  
  df3 <- df2[(df2$hour %in% df1$hour), ]
  View(df1)
  View(df2)
  View(df3)
  
  title <-
    paste("Correlation between", parameter1, "and", parameter2, "over 7 days", sep = " ")
  plot(df1$total, df3$total, type = "p", ann = FALSE)
  title(title, xlab = parameter1, ylab = parameter2)
}

