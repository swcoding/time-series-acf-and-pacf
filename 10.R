library(magrittr)
library(forecast)

5.60
{
  
  # y = 25 + 0.6y_1 + 0.25y_2 + e
  
  # a ####
  acf = ARMAacf(ar = c(0.6,0.25),lag.max=25)
  pacf = ARMAacf(ar = c(0.6,0.25),lag.max=25, pacf = T)
    barplot(acf,main = "theoretical ACF",
            xlab = "lag",
            ylab = "acf")

    barplot(pacf,main = "theoretical PACF",
            xlab = "lag",
            ylab = "pacf")
    
  # b #### #AR1
    y.b = c(y1,y2)
    for(i in 3:50){
      y.b %<>% append(., 25 + 0.6*y.b[i-1] + 0.25*y.b[i-2] + rnorm(1,0,1))
    }
    y.b
      
      tsdisplay(y.b)
      #acf長的一樣
      #pacf lag 2 的時候不一樣
  
  # c ####
    y.c = c(y1,y2)
    for(i in 3:200){
      y.c %<>% append(., 25 + 0.6*y.c[i-1] + 0.25*y.c[i-2] + rnorm(1,0,1))
    }
    y.c
    tsdisplay(y.c)

}

5.61
{
  # y = 40 + 0.4e_1 + e
  # e ~ N(0,2)
  
  # a ####
  acf = ARMAacf(ma = c(0.4),lag.max = 25)
  pacf = ARMAacf(ma = 0.4,lag.max = 25,pacf=T)
    barplot(acf,main = "theoretical ACF", xlab = "lag",
          ylab = "acf")
    barplot(pacf,main = "theoretical ACF", xlab = "lag",
            ylab = "pacf")
    
  # b ####
    y = c(rnorm(1,0,sqrt(2)))
    for(i in 2:50){
      y %<>% append(., 40 + 0.4*y[i-1] + rnorm(1,0,sqrt(2)))
    }
      y
      tsdisplay(y)
    
  # c ####
    y.b = c(rnorm(1,0,sqrt(2)))
    for(i in 2:200){
      y.b %<>% append(., 40 + 0.4*y.b[i-1] + rnorm(1,0,sqrt(2)))
    }
      y.b
      tsdisplay(y.b)

}

5.62
{
  # y = 50 - 0.7y_1 + 0.5e_1 + e
  # e ~ N(0,2)
  
  # a ####
    acf = ARMAacf(ar = -0.7, ma = 0.5, lag.max = 25)
    pacf = ARMAacf(ar = -0.7, ma = 0.5, lag.max = 25, pacf = T)
    barplot(acf,main = "theoretical ACF",
            xlab = "lag",
            ylab = "acf")

    barplot(pacf,main = "theoretical PACF",
            xlab = "lag",
            ylab = "pacf")
  # b ####
    y.b = c(1)
    e = c(rnorm(1,0,sqrt(2)))
    for(i in 2:50){
      e[i] = 0.5*e[i-1]
      y.b[i] = 50 - 0.7*y.b[i-1] + e[i] + rnorm(1,0,sqrt(2))
    }
      tsdisplay(y.b)
  # c ####
    y.c = c(1)
    e = c(rnorm(1,0,sqrt(2)))
    for(i in 2:200){
      e[i] = 0.5*e[i-1]
      y.c[i] = 50 - 0.7*y.c[i-1] + e[i] + rnorm(1,0,sqrt(2))
    }
      tsdisplay(y.c)
}