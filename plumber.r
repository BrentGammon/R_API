#install.packages("plumber")
#plumber.R

#' Echo the parameter that was sent in
#' @param msg The message to echo back.
#' @get /echo

function(msg ='Hello World'){
  list(msg = paste0("The message is: '", msg, "'"))
}

#' Plot out data from the iris dataset
#' @param spec If provided, filter the data to only this species
#' @get /plot
#' @png


function(spec = 'setosa'){
  #list(spec = paste0("The message is: '", spec, "'"))
  myData <- iris
  title <- "All Species"

# Filter if the missing species was specified
if(spec != ''){
  title <- paste0("Only the '", spec, "' Species")
  myData <- subset(iris, Species == spec)
}

plot(myData$Sepal.Length, myData$Petal.Length,
     main=title, xlab = "Sepal Length", ylab = "Petal Length")
}

