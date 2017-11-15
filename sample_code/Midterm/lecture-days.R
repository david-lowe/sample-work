
lecture.days <- function(start.date,end.date,...){
  if(missing(start.date) | missing(end.date)) stop("Please provide a start and end date for the semester in the format YYYY-MM-DD.")
  if(is.null(c(...))==F){
    off.days <- c(...)
  }
  date.range <- data.frame(date=seq(as.Date(start.date), as.Date(end.date), by="days"))
  date.range$day <- weekdays(date.range[,1])
  date.range$class.day <- grepl("Tuesday",date.range[,2]) | grepl("Thursday",date.range[,2])
  date.range$class.day[which(date.range$date %in% off.days)] <- FALSE
  cat(paste("\t",date.range$date[which(date.range$class.day == TRUE)],"\n"))
}

lecture.days("2016-09-29","2017-04-30", seq(as.Date("2016-12-08"), as.Date("2017-01-09"), by="days"))


attach(CLEO)
