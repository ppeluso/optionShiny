library(shiny)
library(Rcpp)
library(plyr)
library(ggplot2)
library(plotly)
library(AmericanCallOpt)
library(fOptions)
Rcpp::sourceCpp('~/Desktop/gabr/SABR/sabr_rcpp.cpp')
shinyServer(function(input, output){
 
 
  output$price <- renderText({
   
    so <- seq(trunc(as.numeric(input$S)) - 20, trunc(as.numeric(input$S)) + 20, 1 )
    plot <- c()
    if(input$sel == "LC")
    {
      for(i in 1: length(so))
      {
        if(so[i] > as.numeric(input$K) )
        {
        plot[i] <- so[i] - as.numeric(input$K)
        }
        else
        {
          plot[i] <- 0 
        }
      }
      value <- callPriceMonteCarlo(1000000,as.numeric(input$S),as.numeric(input$K), as.numeric(input$T), as.numeric(input$r), as.numeric(input$sigma))
      
      value <- round_any(value, accuracy = .001, f = round)
      print(value)
      paste("Value of Call:",value )
    
    }
    else if(input$sel == "LP")
    { 
      
      value <- putPriceMonteCarlo(1000000,as.numeric(input$S),as.numeric(input$K), as.numeric(input$T), as.numeric(input$r), as.numeric(input$sigma))
      
     value <- round_any(value, accuracy = .001, f = round)
      print(value)
      paste("Value of Put:",value )
      
      
    }
    else if(input$sel == "SC")
    {
      value <- callPriceMonteCarlo(1000000,as.numeric(input$S),as.numeric(input$K), as.numeric(input$T), as.numeric(input$r), as.numeric(input$sigma))
      
      value <- round_any(value, accuracy = .001, f = round)
      print(value)
      paste("Value of Call:",value )
      
    }
    else if (input$sel == "SP")
    { 
      
      value <- putPriceMonteCarlo(1000000,as.numeric(input$S),as.numeric(input$K), as.numeric(input$T), as.numeric(input$r), as.numeric(input$sigma))
      
      value <- round_any(value, accuracy = .001, f = round)
      print(value)
      paste("Value of Put:",value )
    
    }
    else 
    {
      paste("Your", "Dumb")
    }
    
})
  
output$plotly <- renderPlotly({
  
  so <- seq(trunc(as.numeric(input$S)) - 40, trunc(as.numeric(input$S)) + 20, 1 )
  plot <- c()
  value <- callPriceMonteCarlo(1000000,as.numeric(input$S),as.numeric(input$K), as.numeric(input$T), as.numeric(input$r), as.numeric(input$sigma))
  current <- c()
  
  if(input$sel == "LC")
  {
    value <- callPriceMonteCarlo(1000000,as.numeric(input$S),as.numeric(input$K), as.numeric(input$T),as.numeric(input$r), as.numeric(input$sigma))
    
    for(i in 1: length(so))
    {
      currentVal <- callPriceMonteCarlo(1000000,as.numeric(so[i]),as.numeric(input$K), as.numeric(input$T), as.numeric(input$r), as.numeric(input$sigma)) * 100
      if(so[i] <= as.numeric(input$K)  )
      {
        plot[i] <- -value * 100
        current[i] <- currentVal - (value * 100)
        
        
      }
      else
      {
        plot[i] <- ((so[i] - as.numeric(input$K))*100) - value *100  
        current[i] <- currentVal - (value * 100)
        
      }
    }

    dat <- data.frame(so, plot, current)
   # ggplot() + geom_line(data = dat, aes(x = so, y = plot),colour = "red") + geom_hline(yintercept = 0, size = 1, colour = "black") + geom_line(data = dat, aes(x = so, y = current), colour="blue")  + xlab("Underlying Price") + ylab("Payoff") + ggtitle("Payoff of Long Call")
 p <- plot_ly(dat, x = so, y = plot, name = "Payoff at Expiration",marker = list(
   color = 'rgb(0,0,255)'
 ) )
 p %>% add_trace(x= so,y = current, name = "Payoff at Current Date", marker = list(
   color = 'rgb(233,16,16)'
 ))
    #ggplotly(hoverinfo="poop")
 layout(p = last_plot(), xaxis = list(title = "Underlying Price"), yaxis= list(title = "Payoff"))
 }
  
  else if(input$sel == "LP")
  {
    value <- putPriceMonteCarlo(1000000,as.numeric(input$S),as.numeric(input$K), as.numeric(input$T), as.numeric(input$r), as.numeric(input$sigma))
    
    for(i in 1: length(so))
    {
      currentVal <- putPriceMonteCarlo(1000000,as.numeric(so[i]),as.numeric(input$K), as.numeric(input$T), as.numeric(input$r), as.numeric(input$sigma)) * 100
      if(so[i] >= as.numeric(input$K)  )
      {
        plot[i] <- -value * 100
        current[i] <- currentVal - (value * 100)
        
      
      }
      else
      {
        plot[i] <- (( as.numeric(input$K) - so[i])*100) - value *100  
        current[i] <- currentVal - (value * 100)
        
      }
    }
    
    dat <- data.frame(so, plot, current)
    # ggplot() + geom_line(data = dat, aes(x = so, y = plot),colour = "red") + geom_hline(yintercept = 0, size = 1, colour = "black") + geom_line(data = dat, aes(x = so, y = current), colour="blue")  + xlab("Underlying Price") + ylab("Payoff") + ggtitle("Payoff of Long Call")
    p <- plot_ly(dat, x = so, y = plot, name = "Payoff at Expiration",marker = list(
      color = 'rgb(0,0,255)'
    ) )
    p %>% add_trace(x= so,y = current, name = "Payoff at Current Date", marker = list(
      color = 'rgb(233,16,16)'
    ))
    #ggplotly(hoverinfo="poop")
    layout(p = last_plot(), xaxis = list(title = "Underlying Price"), yaxis= list(title = "Payoff"))
  }
    

    
   
    

  else if(input$sel == "SC")
  {
    value <- callPriceMonteCarlo(1000000,as.numeric(input$S),as.numeric(input$K), as.numeric(input$T),as.numeric(input$r), as.numeric(input$sigma))
    
    for(i in 1: length(so))
    {
      currentVal <- callPriceMonteCarlo(1000000,as.numeric(so[i]),as.numeric(input$K), as.numeric(input$T), as.numeric(input$r), as.numeric(input$sigma)) * 100
      if(so[i] <= as.numeric(input$K) )
      {
        plot[i] <- value * 100
        current[i] <- (value * 100) - currentVal 
      }
      else
      {
        plot[i] <- ((as.numeric(input$K) - so[i] ) + value) * 100
        current[i] <-  (value * 100) -currentVal 
      }
    }
    
    dat <- data.frame(so, plot, current)
    #ggplot() + geom_line(data = dat, aes(x = so, y = plot),colour = "red") + geom_line(data = dat, aes(x = so, y = current), colour = "blue") + geom_hline(yintercept = 0, size = 1, colour = "black") + xlab("Underlying Price") + ylab("Payoff") + ggtitle("Payoff of Short Call")
    
    p <- plot_ly(dat, x = so, y = plot, name = "Payoff at Expiration",marker = list(
      color = 'rgb(0,0,255)'
    ))
    p %>% add_trace(x= so,y = current, name = "Payoff at Current Date",marker = list(
      color = 'rgb(233,16,16)'
    ))
    #ggplotly(hoverinfo="poop")
    layout(p = last_plot(), xaxis = list(title = "Underlying Price"), yaxis= list(title = "Payoff"))
    
  }
  else if (input$sel == "SP")
  { 
    
    value <- putPriceMonteCarlo(1000000,as.numeric(input$S),as.numeric(input$K), as.numeric(input$T), as.numeric(input$r), as.numeric(input$sigma))
    
    for(i in 1: length(so))
    {
      currentVal <- putPriceMonteCarlo(1000000,as.numeric(so[i]),as.numeric(input$K), as.numeric(input$T), as.numeric(input$r), as.numeric(input$sigma)) * 100
      if(so[i] <= as.numeric(input$K)  )
      {
        plot[i] <- value * 100
        current[i] <- currentVal - (value * 100)
        
        
      }
      else
      {
        plot[i] <- ((  as.numeric(input$K) -so[i])*100) 
        current[i] <- currentVal - (value * 100)
        
      }
    }
    
    dat <- data.frame(so, plot, current)
    # ggplot() + geom_line(data = dat, aes(x = so, y = plot),colour = "red") + geom_hline(yintercept = 0, size = 1, colour = "black") + geom_line(data = dat, aes(x = so, y = current), colour="blue")  + xlab("Underlying Price") + ylab("Payoff") + ggtitle("Payoff of Long Call")
    p <- plot_ly(dat, x = so, y = plot, name = "Payoff at Expiration",marker = list(
      color = 'rgb(0,0,255)'
    ) )
    p %>% add_trace(x= so,y = current, name = "Payoff at Current Date", marker = list(
      color = 'rgb(233,16,16)'
    ))
    #ggplotly(hoverinfo="poop")
    layout(p = last_plot(), xaxis = list(title = "Underlying Price"), yaxis= list(title = "Payoff"))
    
  }
  else 
  {
    paste("Your", "Dumb")
  }
})


output$amer_price <- renderText({
  
  so <- seq(trunc(as.numeric(input$S)) - 20, trunc(as.numeric(input$S)) + 20, 1 )
  plot <- c()
  if(input$select == "lc")
  {
    time <- as.numeric(input$T) / 252
    
    value<-BSAmericanApproxOption("c", as.numeric(input$S),as.numeric(input$K),time , as.numeric(input$r), as.numeric(input$b), as.numeric(input$sigma),
                                          title = NULL, description = NULL)
#     for(i in 1: length(so))
#     {
#       if(so[i] > as.numeric(input$K) )
#       {
#         plot[i] <- so[i] - as.numeric(input$K)
#       }
#       else
#       {
#         plot[i] <- 0 
#       }
#     }

  
    price <- round_any(value@price, accuracy = .001, f = round)
   
    paste("Value of Call:",price)
    
  }
  else if(input$select == "lp")
  { 
    time <- as.numeric(input$K) /252
    value<-BSAmericanApproxOption("p", as.numeric(input$S),as.numeric(input$K),time , as.numeric(input$r), as.numeric(input$b), as.numeric(input$sigma),
                                  title = NULL, description = NULL)
    
    price <- round_any(value@price, accuracy = .001, f = round)
    paste("Value of Put:",price )
    
    
  }
  else if(input$select == "sc")
  {
    time <- as.numeric(input$T) /252

    value<-BSAmericanApproxOption("c", as.numeric(input$S),as.numeric(input$K),time , as.numeric(input$r), as.numeric(input$b), as.numeric(input$sigma),
                                  title = NULL, description = NULL)
   
    price <- round_any(value@price, accuracy = .001, f = round)
    paste("Value of Call:", price)
    
  }
  else if (input$select == "sp")
  { 
    time <- as.numeric(input$K) /252
    value<-BSAmericanApproxOption("p", as.numeric(input$S),as.numeric(input$K),time , as.numeric(input$r), as.numeric(input$b), as.numeric(input$sigma),
                                  title = NULL, description = NULL)
    
    price <- round_any(value@price, accuracy = .001, f = round)
    paste("Value of Put:",price )
    
  }
  else 
  {
    paste("Your", "Dumb")
  }
  
})


output$carry <- renderText({
  time <- as.numeric(input$t) /252
  cost <- as.numeric(input$s)*exp((as.numeric(input$rate) + as.numeric(input$st) - as.numeric(input$c)) * time)
  tocary <- (cost / as.numeric(input$s)) - 1 
  tocary <- round_any(tocary, accuracy = .0001, f = round)
  paste("Cost to carry: ", tocary * 100, "%", sep = "")
})


output$amer_plot <- renderPlotly({
  
  so <- seq(trunc(as.numeric(input$S)) - 40, trunc(as.numeric(input$S)) + 20, 1 )
  plot <- c()

  current <- c()
  
  if(input$sel == "LC")
  {
    value <- BSAmericanApproxOption("c", as.numeric(input$S),as.numeric(input$K),time , as.numeric(input$r), as.numeric(input$b), as.numeric(input$sigma),
                                    title = NULL, description = NULL)
    value <- value@price
    for(i in 1: length(so))
    {
     currentVal <-  BSAmericanApproxOption("c", as.numeric(input$S),as.numeric(input$K),time , as.numeric(input$r), as.numeric(input$b), as.numeric(input$sigma),
                             title = NULL, description = NULL)
     currentVal <- currentVal@price
      if(so[i] <= as.numeric(input$K) )
      {
        plot[i] <- -value * 100
        current[i] <- currentVal - (value * 100)
        
        
      }
      else
      {
        plot[i] <- ((so[i] - as.numeric(input$K))*100) - value *100  
        current[i] <- currentVal - (value * 100)
        
      }
    }
    
    dat <- data.frame(so, plot, current)
    # ggplot() + geom_line(data = dat, aes(x = so, y = plot),colour = "red") + geom_hline(yintercept = 0, size = 1, colour = "black") + geom_line(data = dat, aes(x = so, y = current), colour="blue")  + xlab("Underlying Price") + ylab("Payoff") + ggtitle("Payoff of Long Call")
    p <- plot_ly(dat, x = so, y = plot, name = "Payoff at Expiration",marker = list(
      color = 'rgb(0,0,255)'
    ) )
    p %>% add_trace(x= so,y = current, name = "Payoff at Current Date", marker = list(
      color = 'rgb(233,16,16)'
    ))
    #ggplotly(hoverinfo="poop")
    layout(p = last_plot(), xaxis = list(title = "Underlying Price"), yaxis= list(title = "Payoff"))
  }
  
  else if(input$sel == "LP")
  {
    value <- putPriceMonteCarlo(1000000,as.numeric(input$S),as.numeric(input$K), as.numeric(input$T), as.numeric(input$r), as.numeric(input$sigma))
    
    for(i in 1: length(so))
    {
      currentVal <- putPriceMonteCarlo(1000000,as.numeric(so[i]),as.numeric(input$K), as.numeric(input$T), as.numeric(input$r), as.numeric(input$sigma)) * 100
      if(so[i] >= as.numeric(input$K)  )
      {
        plot[i] <- -value * 100
        current[i] <- currentVal - (value * 100)
        
        
      }
      else
      {
        plot[i] <- (( as.numeric(input$K) - so[i])*100) - value *100  
        current[i] <- currentVal - (value * 100)
        
      }
    }
    
    dat <- data.frame(so, plot, current)
    # ggplot() + geom_line(data = dat, aes(x = so, y = plot),colour = "red") + geom_hline(yintercept = 0, size = 1, colour = "black") + geom_line(data = dat, aes(x = so, y = current), colour="blue")  + xlab("Underlying Price") + ylab("Payoff") + ggtitle("Payoff of Long Call")
    p <- plot_ly(dat, x = so, y = plot, name = "Payoff at Expiration",marker = list(
      color = 'rgb(0,0,255)'
    ) )
    p %>% add_trace(x= so,y = current, name = "Payoff at Current Date", marker = list(
      color = 'rgb(233,16,16)'
    ))
    #ggplotly(hoverinfo="poop")
    layout(p = last_plot(), xaxis = list(title = "Underlying Price"), yaxis= list(title = "Payoff"))
  }
  
  
  
  
  
  
  else if(input$sel == "SC")
  {
    value <- callPriceMonteCarlo(1000000,as.numeric(input$S),as.numeric(input$K), as.numeric(input$T),as.numeric(input$r), as.numeric(input$sigma))
    
    for(i in 1: length(so))
    {
      currentVal <- callPriceMonteCarlo(1000000,as.numeric(so[i]),as.numeric(input$K), as.numeric(input$T), as.numeric(input$r), as.numeric(input$sigma)) * 100
      if(so[i] <= as.numeric(input$K) )
      {
        plot[i] <- value * 100
        current[i] <- (value * 100) - currentVal 
      }
      else
      {
        plot[i] <- ((as.numeric(input$K) - so[i] ) + value) * 100
        current[i] <-  (value * 100) -currentVal 
      }
    }
    
    dat <- data.frame(so, plot, current)
    #ggplot() + geom_line(data = dat, aes(x = so, y = plot),colour = "red") + geom_line(data = dat, aes(x = so, y = current), colour = "blue") + geom_hline(yintercept = 0, size = 1, colour = "black") + xlab("Underlying Price") + ylab("Payoff") + ggtitle("Payoff of Short Call")
    
    p <- plot_ly(dat, x = so, y = plot, name = "Payoff at Expiration",marker = list(
      color = 'rgb(0,0,255)'
    ))
    p %>% add_trace(x= so,y = current, name = "Payoff at Current Date",marker = list(
      color = 'rgb(233,16,16)'
    ))
    #ggplotly(hoverinfo="poop")
    layout(p = last_plot(), xaxis = list(title = "Underlying Price"), yaxis= list(title = "Payoff"))
    
  }
  else if (input$sel == "SP")
  { 
    
    value <- putPriceMonteCarlo(1000000,as.numeric(input$S),as.numeric(input$K), as.numeric(input$T), as.numeric(input$r), as.numeric(input$sigma))
    
    for(i in 1: length(so))
    {
      currentVal <- putPriceMonteCarlo(1000000,as.numeric(so[i]),as.numeric(input$K), as.numeric(input$T), as.numeric(input$r), as.numeric(input$sigma)) * 100
      if(so[i] <= as.numeric(input$K)  )
      {
        plot[i] <- value * 100
        current[i] <- currentVal - (value * 100)
        
        
      }
      else
      {
        plot[i] <- ((  as.numeric(input$K) -so[i])*100) 
        current[i] <- currentVal - (value * 100)
        
      }
    }
    
    dat <- data.frame(so, plot, current)
    # ggplot() + geom_line(data = dat, aes(x = so, y = plot),colour = "red") + geom_hline(yintercept = 0, size = 1, colour = "black") + geom_line(data = dat, aes(x = so, y = current), colour="blue")  + xlab("Underlying Price") + ylab("Payoff") + ggtitle("Payoff of Long Call")
    p <- plot_ly(dat, x = so, y = plot, name = "Payoff at Expiration",marker = list(
      color = 'rgb(0,0,255)'
    ) )
    p %>% add_trace(x= so,y = current, name = "Payoff at Current Date", marker = list(
      color = 'rgb(233,16,16)'
    ))
    #ggplotly(hoverinfo="poop")
    layout(p = last_plot(), xaxis = list(title = "Underlying Price"), yaxis= list(title = "Payoff"))
    
  }
  else 
  {
    paste("Your", "Dumb")
  }
})


})
