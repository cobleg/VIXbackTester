# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(kableExtra)
library(PerformanceAnalytics)
library(TTR)
library(zoo)

source("helpers.R")
source("helpers2.R")

shinyServer(function(input, output) {
  
  lookBackWindow <- reactive({ input$lookBackWindow })
  
  monitoringVariable <- reactive({
    dat <- getData(ticker = input$monitoringVariableName)
    if(input$frequency == "Weekly"){
      dat <- weekly(dat)
    }
    return(dat)
  })
  
  tradingAsset <- reactive({
    dat <- getData(input$tradingAssetName)
    
    if(input$frequency == "Weekly"){
      dat <- weekly(dat)
    }
    return(dat)
  })
  
  
  
  dat1 <- reactive({
    # define parameters
    
    entryThresholdLongN <- input$percentileEntryLong
    exitThresholdLongN <- input$percentileExitLong
    entryThresholdShortN <- input$percentileEntryShort
    exitThresholdShortN <- input$percentileExitShort
    
    exitThresholdLongNL <- exitThresholdLongN - 5
    exitThresholdLongNU <- exitThresholdLongN + 5
    
    exitThresholdShortNL <- exitThresholdShortN - 5
    exitThresholdShortNU <- exitThresholdShortN + 5
    
    # calculate rolling percentiles
    entryThresholdLong <- rollapply(monitoringVariable(), width=lookBackWindow(), align="right", quantile, probs=entryThresholdLongN/100)
    exitThresholdLongL <- rollapply(monitoringVariable(), width=lookBackWindow(), align="right", quantile, probs=exitThresholdLongNL/100)
    exitThresholdLongU <- rollapply(monitoringVariable(), width=lookBackWindow(), align="right", quantile, probs=exitThresholdLongNU/100)
    
    entryThresholdShort <- rollapply(monitoringVariable(), width=lookBackWindow(), align="right", quantile, probs=entryThresholdShortN/100)
    exitThresholdShortL <- rollapply(monitoringVariable(), width=lookBackWindow(), align="right", quantile, probs=exitThresholdShortNL/100)
    exitThresholdShortU <- rollapply(monitoringVariable(), width=lookBackWindow(), align="right", quantile, probs=exitThresholdShortNU/100)
    
    merge.zoo(monitoringVariable = monitoringVariable(), tradingAsset = tradingAsset(), entryThresholdLong, exitThresholdLongL, exitThresholdLongU, entryThresholdShort, exitThresholdShortL, exitThresholdShortU)
  })
  
  dat2 <- reactive({
    
    percentrankMV <- runPercentRank(monitoringVariable(), n = lookBackWindow())
    merge.zoo(dat1(), percentrankMV)
  })
  
  dat3 <- reactive({
    percentrankTA <- runPercentRank(dat2()$tradingAsset, n = lookBackWindow())
    merge.zoo(dat2(), percentrankTA)
  })
  
  dat4 <- reactive({   # flag the vix values that are (less than or equal to the 10th percentile) and (greater than or equal to the 90th percentile)
    signals <- zoo(matrix(nrow=length(dat3()[,"monitoringVariable"]), ncol=2, NA), order.by = index(dat3()))
    signals[,1][dat3()[, "monitoringVariable"] <= dat3()[,"entryThresholdLong"]] <- 1
    signals[,2][dat3()[,"monitoringVariable"] > dat3()[,"entryThresholdShort"]] <- 1
    signals[,1][(dat3()[,"monitoringVariable"] >= dat3()[,"exitThresholdLongL"]) & (dat3()[,"monitoringVariable"] <= dat3()[,"exitThresholdLongU"])] <- -1
    signals[,2][(dat3()[,"monitoringVariable"] >= dat3()[,"exitThresholdShortL"]) & (dat3()[,"monitoringVariable"] <= dat3()[,"exitThresholdShortU"])] <- -1
    
    signals <- na.fill(signals, 0)
    names(signals) <- c('Long', 'Short')
    
    signalsLong <- signals[cumsum(rle(as.vector(signals$Long))$lengths),"Long"] # remove duplicated signals
    signalsShort <- signals[cumsum(rle(as.vector(signals$Short))$lengths),"Short"] # remove duplicated signals
    signals <- merge(signalsLong, signalsShort)
    if(nrow(signals) != 0){
      merge.zoo(dat3(), signals)} else {
        dat3()
      }
    
  })
  
  dat5 <- reactive({
    merge.zoo( dat4(), tradingAsset = tradingAsset() )
    
  })
  
  dat6 <- reactive({
    
    # calculate returns for the trading asset
    monitoringVariableReturns <- diff( log( monitoringVariable() ) )
    tradingAssetReturns <- diff( log( tradingAsset() ) )
    
    merge(dat4(), tradingAssetReturns)
    
    
  })
  
  dat7 <- reactive({
    dat6()[ complete.cases( dat6() ) ]
  })
  
  output$Test1 <- renderText({
    paste("You chose ", input$tradingAssetName)
  })
  
  output$Test2 <- renderText({ colnames( dat5() ) })
  
  # get the trade threshold percentiles 
  tradeThresholds <- reactive({
    as.data.frame(quantile(tail(dat2()$monitoringVariable, input$lookBackWindow), probs = c(input$percentileEntryLong/100, 
                                                                                            input$percentileExitLong/100, 
                                                                                            input$percentileEntryShort/100,
                                                                                            input$percentileExitShort/100)) )
  })
  
  # create a reactive table
  output$table <- reactive({
    extremes <- tradeThresholds()
    names(extremes) <- paste0("Percentile values (", input$monitoringVariableName, ")")
    row.names(extremes) <- c("Entry (Long)", "Exit (Long)", "Entry (Short)", "Exit (Short)")
    extremes  %>%
      knitr::kable("html")  %>%
      kable_styling(bootstrap_options = "striped",
                    full_width = F)
    # add_header_above(c(" ", "VIX"))
  })
  
  ## Calculate the trade entry and exit values for the trading asset (e.g. VXX)
  
  tradeEntryValues <- reactive({ quantile(tail(dat7()$monitoringVariable, input$lookBackWindow), probs = c(input$percentileEntryLong/100, 
                                                                                                           input$percentileExitLong/100, 
                                                                                                           input$percentileEntryShort/100,
                                                                                                           input$percentileExitShort/100)) })
  
  linearMod <- reactive({ 
    lm(tail(tradingAsset, input$lookBackWindow)~tail(monitoringVariable, input$lookBackWindow), data = dat7() )
  })
  
  # create a reactive table
  output$table3 <- reactive({
    tradeValues <- vector()
    for(i in 1:length(tradeEntryValues()  )){
      tradeValues[i] <- tradeEntryValues()[i] * linearMod()$coefficients[2] + linearMod()$coefficients[1]
    }
    tradeValues <- as.data.frame(tradeEntryValues())
    names(tradeValues) <- paste0( "Percentile values (",input$tradingAssetName, ")" )
    row.names(tradeValues) <- c("Entry (Long)", "Exit (Long)", "Entry (Short)", "Exit (Short)")
    tradeValues  %>%
      knitr::kable("html")  %>%
      kable_styling(bootstrap_options = "striped",
                    full_width = F)
    
  })
  
  
  ## get the most recent data between the last observation (t) back to t-n where n is the Lookback Window parameter
  
  percentileCalcInput <- reactive({
    df <- data.frame(tail(dat5()$monitoringVariable, input$lookBackWindow)
    )
    
  })
  
  # show recent data in table format
  output$table2 <- reactive({
    withProgress(message = 'Fetching data, please wait', value = 1, {
      myTable <-  percentileCalcInput()
      #  myTable <- dat6() # test code
      names(myTable) <- c(input$monitoringVariableName) 
      myTable %>%
        knitr::kable("html") %>%
        kable_styling(bootstrap_options = "striped",
                      full_width = F)
    })
  })
  
  ## generate performance analysis
  tradeSize <- reactive({ input$contractSize * input$contractNumber })
  brokerFee <- reactive({ input$brokerFee/tradeSize() })
  returnsLong <- reactive({ getReturns(signalArray = dat7()$signalsLong, priceArray = dat7()$tradingAsset, brokerFee = brokerFee(), tradeType = "long") })
  returnsShort <- reactive({ getReturns(signalArray = dat7()$signalsShort, priceArray = dat7()$tradingAsset, brokerFee = brokerFee(), tradeType = "short") })
  
  
  output$performancePlotLong <- renderPlot({
    # chart equity curve, daily performance, and drawdowns
    par(cex.main = 2)
    charts.PerformanceSummary(returnsLong(), main="Performance (Long trades)")
    
  })
  
  output$performancePlotShort <- renderPlot({
    
    # chart equity curve, daily performance, and drawdowns
    par(cex.main = 2)
    charts.PerformanceSummary(returnsShort(), main="Performance (Short trades)") 
    
    
  })
  
  
})