library(shiny)
library(Rcpp)
library(plyr)
library(ggplot2)
library(plotly)
library(AmericanCallOpt)
Rcpp::sourceCpp('~/Desktop/optionShiny/sabr_rcpp.cpp')


shinyUI( navbarPage("Options for All",
    tabPanel("European", h1("European"),
      sidebarPanel(
      selectInput("sel", label = h4("Type"), choices = list("Long Call" = "LC", "Short Call" = "SC","Long Put" = "LP", "Short Put" = "SP")),
       textInput("S",label = h4("Foward Price" ), value= 101),
       textInput("K",label = h4("Strike Price"), value= 100), 
       textInput("T",label = h4("Days to Expiration"),value= "365" ),
       textInput("r",label = h4("Interest Rate"), value= .02),
       textInput("sigma",label = h4("Volatility"), value= .2)
      ), 
      mainPanel(verbatimTextOutput("price"), plotlyOutput("plotly")
      )
      
             
      ), 
    navbarMenu("American",tabPanel("American",
             h1("American"),
             sidebarPanel(
               selectInput("select", label = h4("Type"), choices = list("Long Call" = "lc", "Short Call" = "sc","Long Put" = "lp", "Short Put" = "sp")),
               textInput("Sike",label = h4("Foward Price" ), value= 101),
               textInput("Kike",label = h4("Strike Price"), value= 100), 
               textInput("Time",label = h4("Days to Expiration"),value= "365" ),
               textInput("rike",label = h4("Interest Rate"), value= .02),
               textInput("sigike",label = h4("Volatility"), value= .2),
               textInput("bike",label = h4("Cost to Carry"), value = .01)
               ), 
             mainPanel(verbatimTextOutput("amer_price"), plotlyOutput("amer_plot")
             )
    
             ), tabPanel("Cost to Carry", h1("Cost to Carry Calculator"),withMathJax(), helpText("The cost to carry is a term used in the futures market. The cost to carry of a commodity is the cost to store and insure a physical commodity. Inthe interest rat market, it is the difference between the yield on a cash instruement and the cost of the funds necessary to buy the instruement. $$F = Se^{(r+s-c)t}$$ Where \\(F\\) is foward price, 
                                                                                                 \\(S\\) is spot price, \\(r\\) is risk-free rate, \\(s\\) is storage cost, \\(c\\) is convience rate, and \\(t\\) is time to future expiration."),
                         sidebarPanel(
                                      textInput("s", label = h4("Sport Price"), value = 1000),
                                      textInput("rate", label = h4("Interest Rate"), value =.02),
                                      textInput("st", label = h4("Storage Cost"), value = .005), 
                                      textInput("c", label = h4("Convenience Yield"), value = .0025), 
                                      textInput("t", label = h4("Days to Delivery"), value = 252)
                                      
                                      
                                      ), 
                         mainPanel(verbatimTextOutput("carry"))
                        )
             
             
             )
                    
      )
)
