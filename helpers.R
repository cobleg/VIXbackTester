


## helper code

# download data and prepare data as a zoo object
getData <- function(ticker) {
  dat1 <-
    read.csv(
      file = paste(
        'https://www.alphavantage.co/query?function=TIME_SERIES_DAILY_ADJUSTED&symbol=',
        ticker,
        '&outputsize=full&apikey=GKYDTK1KW2X56JNE&datatype=csv',
        sep = ''
      )
    )
  
  
  dat1$timestamp <- as.Date(dat1$timestamp, format = '%Y-%m-%d')
  dat2 <- zoo(dat1$adjusted_close, dat1$timestamp)

  
  names(dat2) <- ticker
  
  return(dat2)
}

## aggregate data to weekly, monthly, quarterly, yearly
# weekly
weekly <- function(dat) {
  library(xts)
  library(zoo)
  
  dat1 <- as.xts(dat)
  
  fridays = as.POSIXlt(time(dat1))$wday == 5
  indx <- c(0, which(fridays))
  dat2 <- period.apply(dat1, INDEX = indx, FUN = last)
  dat2 <- aggregate(dat2, identity, mean)
  return(dat2)
}
