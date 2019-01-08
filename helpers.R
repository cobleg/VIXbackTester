

## helper code

# download data and prepare data as a zoo object
getData <- function(ticker, ...){
  getData <- function(s) read.csv(file=paste('https://www.alphavantage.co/query?function=TIME_SERIES_DAILY_ADJUSTED&symbol=', ticker, '&outputsize=full&apikey=GKYDTK1KW2X56JNE&datatype=csv',sep=''));
  
  dat1 = getData(ticker);
  dat1$timestamp <- as.Date(dat1$timestamp, format='%Y-%m-%d')
  dat2 <- zoo(dat1$adjusted_close, dat1$timestamp)
  names(dat2) <- ticker
  return(dat2)
}



