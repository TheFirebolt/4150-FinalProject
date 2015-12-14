
library(shiny)
 
GRPN <- read.csv("~/GRPN.csv")
INTC <- read.csv("~/INTC.csv")
ORCL <- read.csv("~/ORCL.csv")
FB <- read.csv("~/FB.csv")
GOOG <- read.csv("~/GOOG.csv")
TWTR <- read.csv("~/TWTR.csv")
JD <- read.csv("~/JD.csv")
MSFT <- read.csv("~/MSFT.csv")
AAPL <- read.csv("~/AAPL.csv")
BIDU <- read.csv("~/BIDU.csv")
SP500 <- read.csv("~/SP500.csv")

#Calculate Log returns
FB.daily <- - diff(log(FB$Adj.Close))  
TWTR.daily<- - diff(log(TWTR$Adj.Close)) 
GOOG.daily <- - diff(log(GOOG$Adj.Close)) 
ORCL.daily<- - diff(log(ORCL$Adj.Close)) 
JD.daily<- - diff(log(JD$Adj.Close)) 
MSFT.daily<- - diff(log(MSFT$Adj.Close)) 
AAPL.daily<- - diff(log(AAPL$Adj.Close)) 
BIDU.daily<- - diff(log(BIDU$Adj.Close)) 
GRPN.daily <- - diff(log(GRPN$Adj.Close)) 
INTC.daily<- - diff(log(INTC$Adj.Close)) 
SP500.daily<- - diff(log(SP500$Adj.Close))



#calculate 2 years return
GRPN.return <- log(GRPN[2,7]/GRPN[253,7])
INTC.return <- log(INTC[2,7]/INTC[253,7])
ORCL.return <- log(ORCL[2,7]/ORCL[253,7])
BIDU.return <- log(BIDU[2,7]/BIDU[253,7])
FB.return <- log(FB[2,7]/FB[253,7])
GOOG.return <- log(GOOG[2,7]/GOOG[253,7])
TWTR.return <- log(TWTR[2,7]/TWTR[253,7])
JD.return <- log(JD[2,7]/JD[253,7])
MSFT.return <- log(MSFT[2,7]/MSFT[253,7])
AAPL.return <- log(AAPL[2,7]/AAPL[253,7])
SP500.return <- log(SP500[2,7]/SP500[253,7])


#Difference = B[1,]-B[2,]


# Create dataset for Shiny
DailyReturn1 <- data.frame( FB=FB.daily, TWTR= TWTR.daily, GOOG =GOOG.daily, GRPN =GRPN.daily, INTC=INTC.daily , JD=JD.daily , MSFT = MSFT.daily ,AAPL = AAPL.daily,
                           BIDU = BIDU.daily , ORCL =ORCL.daily, SP500=SP500.daily, Date=AAPL$Date[2:253] )

DailyReturn1$ID<-seq.int(nrow(DailyReturn1))
DailyReturn <- DailyReturn1[order(DailyReturn1$ID, decreasing = TRUE), ]
DailyReturn$DateSeq<-seq.int(nrow(DailyReturn))
DailyReturn$DateSeq <- as.numeric(DailyReturn$DateSeq)


# create empty array to hold x-axis and y-axis values
mean.array <- array(0, dim=c(10))
sd.array <- array(0, dim=c(10))
cor.array <- array(0,dim=c(10))

for (i in 1:10) 
{mean.array[i]=mean(DailyReturn[,i])
sd.array[i]=sd(DailyReturn[,i]) 
cor.array[i] = cor(DailyReturn[,i],DailyReturn[,11]) }



NormVar <- data.frame ( Stock = c("FB","TWTR","GOOG", "GRPN" ,"INTC","JD", "MSFT","AAPL","BIDU", "ORCL"),  mean = format(mean.array, digit = 4) , standard_Deviation = format(sd.array, digit = 4), Correlation_with_SP500=format(cor.array, digit = 4),
                        Return_over_1yr = format(c(GRPN.return, INTC.return, ORCL.return, BIDU.return, FB.return, GOOG.return, TWTR.return, JD.return, MSFT.return, AAPL.return),digit = 2))


shinyServer(function(input, output) {
 
   # summary tab 
   output$Summary <-renderTable({
    NormVar
  })
  
   # Data tab
  output$Data <-renderTable({
    colm  <-  as.numeric(input$stock)
    format(DailyReturn[c(colm,12)],digit = 4)
    
  })
  
  # Histogram tab
   output$histPlot <- renderPlot({
    colm  <-  as.numeric(input$stock)
   # stockReturn <- DailyReturn$colm
    bins <- seq(min(DailyReturn[,colm]), max(DailyReturn[,colm]),l = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    
    hist(DailyReturn[,colm], breaks = bins, col = input$color, border = 'white', main = "Historgram of daily return", xlab = names(DailyReturn[colm]))
  
    points(seq(min(DailyReturn[,colm]), max(DailyReturn[,colm]), length.out=100000),
          dnorm(seq(min(DailyReturn[,colm]), max(DailyReturn[,colm]), length.out=100000),
          mean = mean(DailyReturn[,colm]), sd = sd(DailyReturn[,colm])), type="l", col = "red")
     
    
  })
   
   # Statistics Tab
   output$samplemean <-renderText({
     colm  <-  as.numeric(input$stock)
     paste ("<br><br>")
     paste("Sample Mean is ", format (mean(DailyReturn[,colm]), digits = 3))
      
   })
   
   # Statistics Tab
   output$samplesd <-renderText({
     colm  <-  as.numeric(input$stock)
     paste ("<br><br>")
     paste("Sample Standard Deviation is ", format (sd(DailyReturn[,colm]), digits = 3))
   })
   
   # Statistics Tab, confidence interval
   output$interval <-renderText({
     colm  <-  as.numeric(input$stock)
     alpha <- as.numeric(input$confLevel)
     mean <- mean(DailyReturn[,colm])
     std <- sd(DailyReturn[,colm])
     n <- NROW(DailyReturn[,colm])
     error <- qt(alpha,df=n-1)*std/sqrt(n)
     left <- mean - error 
     right <- mean + error 
    paste ("<br>")
    paste("Confidence Interval of mean is (", format(left, digits=5)," , ", format(right, digits=5), ")")
    })   
  
   # Statistics Tab: confidence interval for variance
   output$sdinterval <-renderText({
     colm  <-  as.numeric(input$stock)
     alpha <- as.numeric(input$confLevel)
     mean <- mean(DailyReturn[,colm])
     std <- sd(DailyReturn[,colm])
     n <- NROW(DailyReturn[,colm])
     chisquareL <- qchisq(alpha, df=n-1)
     chisquareR <- qchisq(1-alpha, df=n-1)
     varleft <- ((n-1)*std^2) / chisquareL 
     varright <-((n-1)*std^2) / chisquareR
     paste ("<br>")
     paste("Confidence Interval of variance is (", format(varleft, digits=5)," , ", format(varright, digits=5), ")")
   }) 
   
   
   # Statistics Tab : correlation with SP&500
   output$cor <-renderText({
     colm  <-  as.numeric(input$stock)
     cor <- cor(DailyReturn[,colm],DailyReturn[,11]) 
     paste ("<br>")
     paste("Correlation with SP&500 is ", format(cor, digits=5))
   })  
   
   # Statistics Tab: hypothesis testing
   output$hp <-renderText({
     colm1  <-  as.numeric(input$stock)
     colm2  <-  as.numeric(input$Stock2)
     alpha <- as.numeric(input$SigLevel)
     mean1 <- mean(DailyReturn[,colm1])
     mean2 <- mean(DailyReturn[,colm2])
     std1 <- sd(DailyReturn[,colm1])
     std2 <- sd(DailyReturn[,colm2])
     n1 <- NROW(DailyReturn[,colm1])
     n2 <- NROW(DailyReturn[,colm2])
     testVar <- (std1^2)/n1 + (std2^2)/n2
     testStat <-  (mean1 - mean2)/sqrt(testVar) 
     Z.half.alpha <-  qnorm((1-alpha/2)) 
     p.value <- 2*(1-pnorm(abs(testStat)))
    
     
      str1 <- paste( "H0: mean of ", names(DailyReturn[colm1]), " = mean of ",names(DailyReturn[colm2]), ",    ") 
      str2 <- paste( "H1: mean of ", names(DailyReturn[colm1]), " <> mean of ",names(DailyReturn[colm2]), ";    ") 
      str3 <- paste( "Testing at Significance Level =  ", input$SigLevel,", two sided ;   ") 
      
      str4 <- paste("Test Statistics is  ", format (testStat, digit = 5)," ;   " )
      str5 <- paste("Z (alpha/2) is ", format(Z.half.alpha, digit = 5) , " and -z (alpha/2) is ", format (-Z.half.alpha, digit = 5) , " ;   ")
            
            if ((testStat > -Z.half.alpha) & (testStat < Z.half.alpha)){
              str6 <- paste("Fail to reject H0 ;")
            }        
            else
            {
              str6 <- paste("Enough Evidence to reject H0 ;")
            }
      
      str7 <- paste("P-value is  ", format (p.value, digit = 5)," ;   " )
            
      HTML(paste(str1, str2, str3, str4, str5, str6, str7, sep = "\n" ))      
     
   })
   
   # Regression Tab
   output$Reg2 <-renderPlot({
     colm1  <-  as.numeric(input$stock)
     colm2  <-  as.numeric(input$Stock2)
     
     y <- DailyReturn[,colm1]
     x <- DailyReturn[,colm2] # plot scatterplot and the regression line
     mod1 <- lm(y ~ x)
     plot(x, y, xlab = names(DailyReturn[colm2]), ylab = names(DailyReturn[colm1]), main ="Two Stock Regression")
     abline(lm(y~x))
     res <- signif(residuals(mod1), 5)
     pre <- predict(mod1) # plot distances between points and the regression line
     segments(x, y, x, pre, col="red")
     
     
   }) 
   
   output$Reg1 <-renderPlot({
     colm  <-  as.numeric(input$stock)
     y <- DailyReturn[,colm]
     x <- DailyReturn[,14]  # return on time
     mod1 <- lm(y~x)
     plot(x, y, xlab = "Time", ylab = names(DailyReturn[colm]), main ="One Stock Regression VS Time")
     abline(lm(y~x))
     res <- signif(residuals(mod1), 5)
     pre <- predict(mod1) # plot distances between points and the regression line
     segments(x, y, x, pre, col="red")
   }) 
   
   output$Reg1Sum <-renderText({
     colm  <-  as.numeric(input$stock)
     y <- DailyReturn[,colm]
     x <- DailyReturn[,14] 
     regSum <- summary(lm(y ~x))  
     paste("Regression Intercept estimate is", format(regSum$coefficients[1][1], digits=5), ";     ", 
           "Regression Slope estimate is", format(regSum$coefficients[2][1], digits=5), ";     " ,
           "Regression R^2 is", format(regSum$r.squared, digits=5),";     " ,
           "Regression Adjusted R^2 is", format(regSum$adj.r.squared, digits=5), ";     " 
           
           )
     })  
   
   # Statistics Tab: calculate statistics
   output$Reg2Sum <-renderText({
     colm1  <-  as.numeric(input$stock)
     colm2  <-  as.numeric(input$Stock2)
     
     y <- DailyReturn[,colm1]
     x <- DailyReturn[,colm2] # plot scatterplot and the regression line
     regSum <- summary(lm(y ~x))  
     paste ("<br>")
     paste("Correlation with SP&500 is ", format(cor, digits=5))
     paste("Regression Intercept estimate is", format(regSum$coefficients[1][1], digits=5), ";       " ,
           "Regression Slope estimate is", format(regSum$coefficients[2][1], digits=5), ";       " ,
           "Regression R^2 is", format(regSum$r.squared, digits=5),";       " ,
           "Regression Adjusted R^2 is", format(regSum$adj.r.squared, digits=5), ";       " 
           
     )
     
   })  
   
   output$lineSeries <-renderPlot({
         colm  <-  as.numeric(input$stock)
     plot.ts(DailyReturn[,colm], type="l",col=input$color,xlab = "Time" ,ylab = names(DailyReturn[colm]), main ="Time Series against S&P 500 over 1 year" )
     lines(DailyReturn[,11],col="black")
     legend("topright", c("S&P 500"),col = 1:3,lty=1:3 )
   }) 
   
   
})
