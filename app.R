# library
library(shiny)
library(bslib)
library(readxl)
library(ggplot2)
library(plotly)

library(ggiraph)
library(tidyverse)
library(highcharter) 


# Libraries
library(dplyr)
library(viridis)
library(hrbrthemes)

# Preamble ----

# Anything that doesn't rely on any user inputs,
# we can do this once at startup
# and then use the value throughout the lifetime of the app

### Load Data ##########################################################
Data2425 <- read_excel("Data.xlsx", 
                       col_types = c( "numeric", "text", 
                                      "date", "date", "text", "text", 
                                      "numeric", "text", "numeric", "numeric", 
                                      "numeric"), sheet = 8)


Data2324 <- read_excel("Data.xlsx", 
                       col_types = c( "numeric", "text", 
                                     "date", "date", "text", "text", 
                                     "numeric", "text", "numeric", "numeric", 
                                     "numeric"), sheet = 1)

Data2223 <- read_excel("Data.xlsx", 
                       col_types = c( "numeric", "text", 
                                      "date", "date", "text", "text", 
                                      "numeric", "text", "numeric", "numeric", 
                                      "numeric"), sheet = 2)
Data2122 <- read_excel("Data.xlsx", 
                       col_types = c( "numeric", "text", 
                                      "date", "date", "text", "text", 
                                      "numeric", "text", "numeric", "numeric", 
                                      "numeric"), sheet = 3)

Data1920 <- read_excel("Data.xlsx", 
                       col_types = c( "numeric", "text", 
                                      "date", "date", "text", "text", 
                                      "numeric", "text", "numeric", "numeric", 
                                      "numeric"), sheet = 4)

Data1819 <- read_excel("Data.xlsx", 
                       col_types = c( "numeric", "text", 
                                      "date", "date", "text", "text", 
                                      "numeric", "text", "numeric", "numeric", 
                                      "numeric"), sheet = 5)

Data1718 <- read_excel("Data.xlsx", 
                       col_types = c( "numeric", "text", 
                                      "date", "date", "text", "text", 
                                      "numeric", "text", "numeric", "numeric", 
                                      "numeric"), sheet = 6)

Data17 <- read_excel("Data.xlsx", 
                       col_types = c( "numeric", "text", 
                                      "date", "date", "text", "text", 
                                      "numeric", "text", "numeric", "numeric", 
                                      "numeric"), sheet = 7)

AvData <- read_excel("AvData.xlsx")
Year <- as.numeric(names(AvData)[-(1:2)])

#################################################################

### Fix colours for consistent plotting
team.colours <- c("Arsenal" = "red" , "Aston Villa" = "maroon" , "Birmingham" ="yellow", "Brighton" = "gold", "Bristol City" = "green" , "Chelsea" = "blue","Crystal Palace" = "darkmagenta", "Everton" = "brown" , 
                  "Leicester City" = "orange", "Liverpool" = "darkgreen", "Manchester City" = "skyblue" , "Manchester United" = "black" , "Reading" = "purple" , "Sunderland" = "cyan" , 
                  "Tottenham" = "darkblue" , "West Ham" =	"grey", "Yeovil" = "pink")
###
team.colours2425 <- c("Arsenal"="red", "Aston Villa" = "maroon" ,  "Brighton" = "gold",    "Chelsea" = "blue","Crystal Palace" = "darkmagenta",  "Everton" = "brown", "Leicester City" = "orange" , "Liverpool" = "darkgreen", "Manchester City" = "skyblue", "Manchester United" = "black", "Tottenham" = "darkblue", "West Ham"="grey" )

team.colours2324 <- c("Arsenal"="red", "Aston Villa" = "maroon" ,  "Brighton" = "gold",  "Bristol City"="green",  "Chelsea" = "blue",  "Everton" = "brown", "Leicester City" = "orange" , "Liverpool" = "darkgreen", "Manchester City" = "skyblue", "Manchester United" = "black", "Tottenham" = "darkblue", "West Ham"="grey" )
team.colours2223 <- c("Arsenal"="red", "Aston Villa" = "maroon" ,  "Brighton" = "gold",    "Chelsea" = "blue",  "Everton" = "brown", "Leicester City" = "orange" , "Liverpool" = "darkgreen", "Manchester City" = "skyblue", "Manchester United" = "black", "Reading" = "purple", "Tottenham" = "darkblue", "West Ham"="grey" )
team.colours2122 <- c("Arsenal"="red", "Aston Villa" = "maroon" , "Birmingham" ="yellow", "Brighton" = "gold",    "Chelsea" = "blue",  "Everton" = "brown", "Leicester City" = "orange" , "Manchester City" = "skyblue", "Manchester United" = "black",  "Reading" = "purple" ,  "Tottenham" = "darkblue", "West Ham"="grey" )
team.colours1920 <- c("Arsenal"="red", "Birmingham" ="yellow", "Brighton" = "gold",  "Bristol City" = "green" ,  "Chelsea" = "blue",  "Everton" = "brown", "Liverpool" = "darkgreen", "Manchester City" = "skyblue", "Manchester United" = "black","Reading" = "purple",  "Tottenham" = "darkblue", "West Ham"="grey" )
team.colours1819 <- c("Arsenal"="red", "Birmingham" ="yellow", "Brighton" = "gold",  "Bristol City" = "green" ,  "Chelsea" = "blue",  "Everton" = "brown", "Liverpool" = "darkgreen", "Manchester City" = "skyblue"  ,"Reading" = "purple", "West Ham"="grey" ,"Yeovil Town" = "pink"  )
team.colours1718 <- c("Arsenal"="red", "Birmingham" ="yellow",   "Bristol City" = "green" ,  "Chelsea" = "blue",  "Everton" = "brown", "Liverpool" = "darkgreen", "Manchester City" = "skyblue"  ,"Reading" = "purple", "Sunderland" = "cyan"  , "Yeovil Town" = "pink"  )
team.colours17 <- c("Arsenal"="red", "Birmingham" ="yellow",   "Bristol City" = "green" ,  "Chelsea" = "blue", "Liverpool" = "darkgreen", "Manchester City" = "skyblue"  ,"Reading" = "purple", "Sunderland" = "cyan"  ,"Yeovil Town" = "pink"  )


###########
# Define UI for  app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Exploring Home Attendance in the WSL"), ################ Change this
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(style = "position:fixed;width:inherit;", width=2,
                 
                 selectInput("season", "Season:",c("2024/25",
                                                   "2023/24",
                                                   "2022/23",
                                                   "2021/22",
                                                   "2019/20",
                                                   "2018/19",
                                                   "2017/18",
                                                   "2017"),
                             selected = "2023/24"),
                 
                 # Input: Selector for variable to plot against Group ----
                 selectInput("team", "Team:",
                             choices = NULL),
                 
                 uiOutput("image_table"),
                 
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(width=10, 
              
              ####
              # Output: Layout columns with cards w/ plot, summary, and table ----
              layout_columns(type = "tabs",
                          card(highchartOutput("AllPlot"),
                               # Output: Requested attendance plot ----
                               plotlyOutput("TeamPlot", height="auto", width="auto"),
                               plotlyOutput("AvPlot", height="auto", width="auto")
                               ),
                          
                          
                          ####
                          
                          
                          
              ),
    )
  ),
)

# Define server logic to plot various variables against Group ----
server <- function(input, output) {
  
  #image based on season
  
  output$image_table <- renderUI({
    if (input$season == "2023/24"){img(src="WSLTable24.png", height="100%", width="100%", align = "center")}
    
    else if(input$season == "2024/25"){img(src="WSLTable25.png", height="100%", width="100%", align = "center")}
    
    else if(input$season == "2022/23"){img(src="WSLTable23.png", height="100%", width="100%", align = "center")}
    else if(input$season == "2021/22"){img(src="WSLTable22.png", height="100%", width="100%", align = "center")}
    else if(input$season == "2019/20"){img(src="WSLTable20.png", height="100%", width="100%", align = "center")}
    else if(input$season == "2018/19"){img(src="WSLTable19.png", height="100%", width="100%", align = "center")}
    else if(input$season == "2017/18"){img(src="WSLTable18.png", height="100%", width="100%", align = "center")}
    else if(input$season == "2017"){img(src="WSLTable17.png", height="100%", width="100%", align = "center")}
  })
  
  # select data based on season
  data <- reactive({
    if (input$season == "2023/24"){data = Data2324}
    else if(input$season == "2024/25"){data = Data2425}
    
    else if(input$season == "2022/23"){data = Data2223}
    else if(input$season == "2021/22"){data = Data2122}
    else if(input$season == "2019/20"){data = Data1920}
    else if(input$season == "2018/19"){data = Data1819}
    else if(input$season == "2017/18"){data = Data1718}
    else if(input$season == "2017"){data = Data17}
  })
  
  #colours based on season
  colours <- reactive({
    if (input$season == "2023/24"){colours = team.colours2324}
    
    else if(input$season == "2024/25"){colours = team.colours2425}
    else if(input$season == "2022/23"){colours = team.colours2223}
    else if(input$season == "2021/22"){colours = team.colours2122}
    else if(input$season == "2019/20"){colours = team.colours1920}
    else if(input$season == "2018/19"){colours = team.colours1819}
    else if(input$season == "2017/18"){colours = team.colours1718}
    else if(input$season == "2017"){colours = team.colours17}
  })
  
  
  #observe data and update team choices
  observeEvent(data(), {
    choices_teams <- sort(unique(data()$Home))
    updateSelectInput(inputId = "team", choices = choices_teams)
  })
  
  # Create team subset  
  team <- reactive({
    input$team
  })
  
  teamdata <- reactive({
    subset(data(), Home==team()) 
  })
  
  ymin <- reactive({min(teamdata()$Attendance)})
  ymax <- reactive({max(max(teamdata()$Attendance), teamdata()$Capacity)})
  
  xmax <- reactive({max(data()$Wk)})
  
  ###################
  teamid <- reactive({which(AvData$Team == team())})
  Attendance <- reactive({t(AvData[teamid(),-(1:2)])})
  df <- reactive({data.frame(cbind(Year, Attendance())) })
  
  yymax <- reactive({max(df()$V2)/2})
  
  ### Create reactive average attendance plot
  q <- reactive({
    ggplot(df() , aes(x=Year, y=V2)) +
      xlim(2017, 2025) + 
      geom_vline(xintercept=2021, color = "#69b3a2", linetype="dashed") +
      #annotate("text", x=2021, y= yymax(), label="Some text", color="#69b3a2", angle = 45) +
      geom_vline(xintercept=2022.5, color = "gold", linetype="dashed") +
      geom_vline(xintercept=2023.5, color = "grey", linetype="dashed") +
      #geom_rect(aes(xmin=2020.5, xmax=2021.5, ymin=0, ymax=yymax()), fill = "#69b3a2", alpha = 0.2) +
    geom_line(data=df(), aes(x=Year,y=V2)) +
    geom_point(color='black', shape=21, size=2, fill= team.colours[teamid()][[1]]) + 
    ggtitle(paste(team(), " Average Annual Home Attendance" , sep="")) +
    ylab("Average Attendance")
  })
  ####################
  
  
  ###### Create reactive home attendance plot
  
  p <- reactive({
    ggplot(teamdata() ,
           aes(x=Wk, y=Attendance)) +
      geom_line(data=teamdata(),aes(x=Wk,y=Capacity), linetype="dashed", colour="darkgrey") +
      geom_line(data=teamdata(),aes(x=Wk,y=Attendance)) +
      geom_point(color='black', shape=21, size=2, aes(fill=factor(Away), text=paste("<br><b>Opponent:</b>", Away, "<br><b>Venue:</b>", Venue, "<br><b>Attendance:</b>",Attendance, "<br><b>Date:</b>",Date))) +
      ggtitle(paste(team(), " Home Match Attendance" , sep="")) +
      xlab("Match Week") +
      ylim(ymin(), ymax()) +
      xlim(1, xmax()) + 
      #geom_hline(yintercept=teamdata()$Capacity[1], linetype="dashed", color = "blue") +
      labs(fill='Opponent')+
      scale_fill_manual(values=colours())
      #scale_fill_manual(values= team.colours)
  })
  
########### Render plots ############################################  
  
  # Generate a plot of the requested team's attendance
  output$TeamPlot <- renderPlotly({
    #p()$x$data[[1]]$hoverinfo <- "none"
    ggplotly(p(),tooltip="text")
  })
  ######## Plot all attendances for requested season
  output$AllPlot <- renderHighchart({
    hc <- hchart(  data(), "line",   hcaes(x = Wk, y = Aprop, group = Home),  color = colours() ) |>
      hc_title(text = "Attendance as a Proportion of Stadium Capacity") |>
      hc_xAxis(title = list(text = "Match Week")) |>
      hc_yAxis(title = list(text = "Proportion"), max=1) |>
      hc_tooltip(crosshairs=TRUE, formatter = JS("function(){
                            return ('Team: ' + this.point.Home + '<br> Opponent: ' + this.point.Away + '<br> Venue: ' + this.point.Venue + ' <br> Attendance: ' + this.point.Attendance)
                            }"))
    hc
  })
  
  ########## Average attendance plot for a particular team
  
  output$AvPlot <- renderPlotly({
    ggplotly(q())
  })
 ############################ 
  
}

# Create Shiny app ----
shinyApp(ui, server)