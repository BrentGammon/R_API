# plumber.R
library("ggpubr")
library("dplyr")
library("lubridate")

#' @filter cors
cors <- function(res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  plumber::forward()
}


#' Echo the parameter that was sent in
#' @param msg The message to echo back.
#' @get /echo
function(msg=""){
  list(msg = paste0("The message is: '", msg, "'"))
}

#' Plot out data from the iris dataset
#' @param spec If provided, filter the data to only this species (e.g. 'setosa')
#' @get /plot
#' @png
function(spec){
  myData <- iris
  title <- "All Species"

  # Filter if the species was specified
  if (!missing(spec)){
    title <- paste0("Only the '", spec, "' Species")
    myData <- subset(iris, Species == spec)
  }

  plot(myData$Sepal.Length, myData$Petal.Length,
       main=title, xlab="Sepal Length", ylab="Petal Length")
}

#' Correlation endpoint
#' @param dataset1 The first dataset
#' @param dataset2 The second dataset
#' @post /correlation
# @application/json
#' @png
function(dataset1, dataset2) {
  print(dataset1)
  conv <- as.data.frame(dataset1)
  #conv$startdate = as.datetime(conv$startdate)
  #conv$enddate = as_datetime(conv$enddate)
  
  df1 <- conv %>%
    mutate(startdate = ymd_hms(startdate), enddate = ymd_hms(enddate)) %>%
    mutate(total = as.numeric(total)) %>%
    mutate(month_name = month(startdate, label = TRUE)) %>% 
    group_by(hour=floor_date(startdate, "hour")) %>%
    summarize(total=sum(total))
    #group_by(hour=hour(startdate)) %>%
    #summarize(total=sum(total))
  View(df1)
  
  
  
  conv2 <- as.data.frame(dataset2)
  #conv2$startdate = as_datetime(conv2$startdate)
  #conv2$enddate = as_datetime(conv2$enddate)
  
  df2 <- conv2 %>%
    mutate(startdate = ymd_hms(startdate), enddate = ymd_hms(enddate)) %>%
    mutate(total = as.numeric(total)) %>%
    mutate(month_name = month(startdate, label = TRUE)) %>% 
    group_by(hour=floor_date(startdate, "hour")) %>%
    summarize(total=sum(total))
  View(df2)
  
  plot(df1, df2)
  
#View(conv2)
#View(conv)
  #View(conv2)
  #dataset1
}



# #' Plot out correctlation from dataset passed in 
# #' @param req The dataset objects
# #' @post /demo
# function(req) {
#     list(req = paste0(req))
# }

# #' @param req
# #' @post /x
# function(req){
#   list(d1 = req$data1, d2=req$data2))
# }


#' @param a The message to echo back.
#' @param b The message to echo back.
#' @get /corrPlot
#' @png
corrPlot <- function(a, b){
    #as.numeric(strsplit("1,2,3,4,5,6,7,8", ",")[[1]])
     x <-as.numeric(strsplit(a, ",")[[1]])
     y <-as.numeric(strsplit(b, ",")[[1]])
    plot(y,x)
}

