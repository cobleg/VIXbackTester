

# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(
  # Application title
  titlePanel("Volatility Trading Calculator"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "frequency",
        "Select time series frequency:",
        c("Daily" = "Daily",
          "Weekly" = "Weekly"),
        selected = "Weekly"
      ),
      selectInput(
        "monitoringVariableName",
        "Ticker (monitoring variable):",
        c("VIX" = "vix", "ANZ" = "ANZ.AX")
      ),
      selectInput(
        "tradingAssetName",
        "Ticker (tradable asset):",
        c(
          "VXX" = "vxx",
          "VXXB" = "vxxb",
          "ANZ" = "ANZ.AX"
        )
      ),
      sliderInput(
        "percentileEntryLong",
        "Percentile entry threshold (long):",
        min = 1,
        max = 50,
        value = 10
      ),
      
      
      sliderInput(
        "percentileEntryShort",
        "Percentile entry threshold (short):",
        min = 50,
        max = 100,
        value = 90
      ),
      
      
      sliderInput(
        "percentileExitLong",
        "Percentile exit threshold (long):",
        min = 30,
        max = 100,
        value = 45
      ),
      
      
      sliderInput(
        "percentileExitShort",
        "Percentile exit threshold (short):",
        min = 30,
        max = 100,
        value = 45
      ),
      
      sliderInput(
        "lookBackWindow",
        "Lookback window",
        min = 5,
        max = 200,
        value = 10
      ),
      
      
      numericInput(
        "contractNumber",
        "Number of contracts",
        min = 1,
        max = 10,
        value = 5
      ),
      
      numericInput(
        "contractSize",
        "Size of contracts",
        min = 10,
        max = 1000,
        value = 100,
        step = 10
      ),
      
      numericInput(
        "brokerFee",
        "Broker fee per trade",
        min = 10,
        max = 1000,
        value = 20
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(tabsetPanel(
      # tabPanel("Testing", textOutput("Test1"), textOutput("Test2")),
      tabPanel(
        "Read me",
        helpText(
          br(),
          "This app generates trading signals targeting volatility. Volatility is measured by a volatility index (e.g. the VIX).
          Volatility at an extreme deviation represents a short-term trading opportunity. The challenge is to identify moments of extreme
          deviation. Such extreme moments typically do not persist, offering a profitable opportunity for traders to bet that volatility is likely to return to a less extreme point closer to the mean.",
          br(),
          br(),
          "The deviation is the difference between a specific observation and the sample mean. The sample mean is calculated from the most recent data using consecutive observations
          between the last observation and the previous n observations. The parameter 'n' is defined by the Lookback window parameter and is, by default, set equal to 10 days.",
          br(),
          br(),
          "An extreme deviation exceeds a specific threshold percentile. For example, if volatility falls below the 10th percentile, then
          it is highly likely that volatility will increase."
        ),
        br(),
        "The success of this strategy depends on how fast the mean of volatility changes as this will affect the accurate measurement of extreme deviations. Given the risk,
        this app provides backtesting to provide guidance on the likely success of this strategy."
        ),
      tabPanel(
        "Recent history",
        column(
          width = 4,
          helpText(
            br(),
            br(),
            br(),
            "This table shows the most recent data. The number of rows of data shown is controlled by the Lookback window parameter.",
            br(),
            br(),
            "Note that this is the data used to calculate the mean volatility and the extreme deviation measurements shown on the next tab: Percentile thresholds."
          )
        ),
        column(width = 4, tableOutput("table2"))
        
      ),
      tabPanel(
        "Percentile thresholds",
        column(
          width = 4,
          helpText(
            br(),
            br(),
            "The table shown here provide information about the trade entry and exit thresholds for both long and short trades.",
            br(),
            br(),
            "The first two rows indicate the entry price level for entering and exiting a long trade, respectively. Note that this
            assumes that volatility is likely to increase over the next few trading days. Enter a long trade if the volatility index (e.g. the VIX) is less than the entry level indicated.",
            br(),
            br(),
            "The third and fourth rows indicate the entry price level for entering and exiting a short trade. Note that this assumes
            that volatility is likely to decrease over the next few trading days.
            Enter a short trade if the volatility index is greater than the value shown in the third row of these tables."
          )
          ),
        column(width = 4, tableOutput("table")),
        column(width = 4, tableOutput("table3"))
          ),
      tabPanel(
        "Strategy backtesting results",
        fluidRow(plotOutput("performancePlotLong")),
        fluidRow(plotOutput("performancePlotShort"))
      )
      ))
    )
))
