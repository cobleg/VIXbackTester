

##### Create backtesting analytics


# calculate log returns of trades
getReturns <- function(signalArray, priceArray, brokerFee, tradeType){
  df <- merge(signals = signalArray, prices = priceArray)
  df <- df[!(df$signals==0),]  # remove rows where there is no signal
  
  ## remove duplicated signals
  runLengths <- rle(as.vector(df$signals))$lengths # record number of duplicated signals
  
  # construct a boolean vector to determine which duplicate rows to delete
  vec <- vector()
  for(i in 1: length(runLengths)){
    if(runLengths[i] > 1){
      vec <- c(vec, TRUE, rep(FALSE,runLengths[i]-1))
    } else {
      vec <- c(vec, TRUE)
    }
  }
  
  df <- df[vec,] # subset the vector to create a vector of alternating signals
  
  ##
  
  # check that the first signal is a trade entry, not an exit
  # if the first signal is an exit signal, delete the row.
  while(df[1,1] == -1)
  {
    df <- df[-1,]
  }
  
  # create dataframe containing buy and sell prices
  entryPrice <- df[df$signals == 1,"prices"]
  
  exitPrice <- df[df$signals == -1,"prices"]
  
  entryPrice <- zoo(entryPrice, index(exitPrice)) # align buy dates with sell dates
  df1 <- merge(entryPrice, exitPrice)
  
  # add brokerage charges to buy and deduct from sell prices
  
  df1$entryPrice <- df1$entryPrice + brokerFee 
  df1$exitPrice <- df1$exitPrice - brokerFee
  
  # calculate return on each trade
  if(tradeType == "long"){
    returns <- log(df1$exitPrice/df1$entryPrice)
  } else {
    returns <- log(df1$entryPrice/df1$exitPrice) # for short trades
  }
  
  return(returns)
}



