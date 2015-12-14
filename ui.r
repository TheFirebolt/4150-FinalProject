library(shiny) #load the shiny package
install.packages('devtools')
devtools::install_github('rstudio/shinyapps')

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  # automatic update
  # Application title
  titlePanel(title = h2( "IEOR 4150 Class Project : Analysis of stock returns")),
                        
  titlePanel(title = h4 ("Team : The Firebolt, Fall 2015")),
             
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      #h4("stock name"),
   
      selectInput("stock", "Please select a stock to display Histogram", choices = c("FB" =1 , "TWTR"=2  ,"GOOG" =3,"GRPN"=4  ,"INTC" =5,"JD" =6 , "MSFT"=7  ,"AAPL" =8,"BIDU"=9  ,"ORCL" =10 , "SP500" = 11 ), 
                    selectize = TRUE, multiple = FALSE),
      br(),
      #multiple = "True" for comparison
      sliderInput("bins", "Select number of bins for Histogram:", min = 10, max = 100, value = 30),
      br(),
      radioButtons("color", "Select color for the Stock", choices = c("Red", "Blue", "Green"), selected = "Blue"),
      br(),
      selectInput("confLevel", "Please select a Confidence Level", choices = c("0.99" = 0.995 , "0.95" = 0.975,"0.90" =0.95 ), 
                   selectize = TRUE, multiple = FALSE),
      br(),
      selectInput("SigLevel", "Please select a Significant Level for Hypothesis Testing", choices = c("0.99" = 0.01 , "0.95" = 0.05,"0.90" =0.10 ), 
                   selectize = TRUE, multiple = FALSE),
      br(),
      selectInput("Stock2", "Please select a 2nd Stock for Regressin Analysis", choices = c("FB" =1 , "TWTR"=2  ,"GOOG" =3,"GRPN"=4  ,"INTC" =5,"JD" =6 , "MSFT"=7  ,"AAPL" =8,"BIDU"=9  ,"ORCL" =10 , "SP500" = 11), 
                  selected = 2, selectize = TRUE, multiple = FALSE)
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type ="tab",
                  tabPanel("Summary",tableOutput("Summary")),
                  tabPanel("Histogram",plotOutput("histPlot"), align= "center"),
                  tabPanel("Statistics", " -- Sample Mean  --",textOutput("samplemean"), br()," -- Sample Standard Deviation  --",
                           textOutput("samplesd"), br(), " -- Confidence Interval of Sample Mean --", textOutput("interval"), br(),
                           "-- Confidence Interval of Sample Variance --", textOutput("sdinterval"), br(),
                           " -- Correlation  --",textOutput("cor"),br(),
                           " -- Hypothesis Testing of two stock mean  --",textOutput("hp"),br()),
                  tabPanel("Regression Analysis", plotOutput("Reg1"), textOutput("Reg1Sum"), plotOutput("Reg2"), textOutput("Reg2Sum"),br(),br(),br(),align = "center"),
                  tabPanel("Time Series", plotOutput("lineSeries"), align = "center"),
                  tabPanel("Data", tableOutput("Data"))
                  
                  )
    )
)

))
