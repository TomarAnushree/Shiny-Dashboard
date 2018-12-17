#Problem Statement
#Display data from https://coinmarketcap.com/gainers-losers/ to see which coins are on a real heater that day.

library(shiny)
library(tidyverse)
library(shinydashboard)
library(rvest)
library(DT)

get.data <- function(x){
  myurl <- read_html("https://coinmarketcap.com/gainers-losers/") # read our webpage as html
  myurl <- html_table(myurl)  # convert to an html table for ease of use
  to.parse <- myurl[[1]]  # pull the first item in the list
  to.parse$`% 1h` <- gsub("%","",to.parse$`% 1h`) # cleanup - remove non-characters
  to.parse$`% 1h`<- as.numeric(to.parse$`% 1h`) #cleanup - convert percentages column to numeric so we can sort
  to.parse$Symbol <- as.factor(to.parse$Symbol) # cleanup - convert coin symbol to factor
  to.parse$Symbol <- factor(to.parse$Symbol,
                            levels = to.parse$Symbol[order(to.parse$'% 1h')])  # sort by gain value
  to.parse  # return the finished data.frame
}

#The get.data function scrapes our website and returns a data frame that is useful to us.

get.infobox.val <- function(x){
  
  df1 <- get.data() # run the scraping function above and assign that data.frame to a variable
  df1 <- df1$`% 1h`[1]  # assign the first value of the % gain column to same variable
  df1   # return value
  
}
#pulls the highest value from the data frame above.
get.infobox.coin <- function(x){
  
  df <- get.data()  # run the scraping function above and assign that data.frame to a variable
  df <- df$Name[1]  # assign the first value of the name column to same variable
  df   # return value
  
}
#The last function is called get.infobox.coin, which returns the name of the top coin.

ui <- dashboardPage(
  
  
  # H E A D E R
  
  dashboardHeader(title = "Alt Coin Gainers"),
  # S I D E B A R
  
  dashboardSidebar(
    
    h5("A slightly interactive dashboard that pulls the top gainers from the last hour from
       coinmarketcap.com. Refreshes every 60 seconds."),
    
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    
    h6("Built in the R computing language 
       [  R Core Team (2018). R: A language and environment for statistical computing.
        URL https://www.R-project.org/]"),
    br(),
    h6("R version 3.4.4 (2018-03-15) 'Someone to Lean On'")
    
    
    ),
  
  # B O D Y
  dashboardBody(
    
    fluidRow(
      
      # InfoBox
      infoBoxOutput("top.coin",
                    width = 3),
      
      # InfoBox
      infoBoxOutput("top.name",
                    width = 3)
      
    ),
    
    fluidRow(
      column(
        # Datatable
        box(
          status = "primary",
          headerPanel("Data Table"),
          solidHeader = T,
          br(),
          DT::dataTableOutput("table", height = "350px"),
          width = 6,
          height = "560px"
        ),
        
        # Chart
        box(
          status = "primary",
          headerPanel("Chart"),
          solidHeader = T,
          br(),
          plotOutput("plot", height = "400px"),
          width = 6,
          height = "500px"
        ),
        width = 12
      )
      
    )
  )
)


  
server <- function(input, output) {
  # R E A C T I V E 
  liveish_data <- reactive({
    invalidateLater(60000)    # refresh the report every 60k milliseconds (60 seconds)
    get.data()                # call our function from above
  })
  
  
  live.infobox.val <- reactive({
    invalidateLater(60000)    # refresh the report every 60k milliseconds (60 seconds)
    get.infobox.val()         # call our function from above
  })
  
  
  live.infobox.coin <- reactive({
    invalidateLater(60000)    # refresh the report every 60k milliseconds (60 seconds)
    get.infobox.coin()        # call our function from above
  })

  # D A T A   T A B L E   O U T P U T
  output$table <- DT::renderDataTable(DT::datatable({
    data <- liveish_data()}))
  # P L O T   O U T P U T
  output$plot <- renderPlot({ (ggplot(data=liveish_data(),aes(x=Symbol, y=`% 1h`)) +
                                 geom_bar(stat="identity", fill = "springgreen3") +
                                 theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                                 ggtitle("Gainers from the Last Hour"))
  })
  # I N F O B O X   O U T P U T - V A L
  output$top.coin <- renderInfoBox({
    infoBox(
      "Gain in Last Hour",
      paste0(live.infobox.val(), "%"),
      icon = icon("signal"),
      color = "purple",
      fill = TRUE)
  })
  
  
  # I N F O B O X   O U T P U T - N A M E
  output$top.name <- renderInfoBox({
    infoBox(
      "Coin Name",
      live.infobox.coin(),
      icon = icon("bitcoin"),
      color = "purple",
      fill = TRUE)
  })
  
}

# Return a Shiny app objectshinyApp(ui = ui, server = server)

shinyApp(ui = ui, server = server)




