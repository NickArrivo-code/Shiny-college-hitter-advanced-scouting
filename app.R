
library(shiny)
library(argonR)
library(argonDash)
library(shinyWidgets)
library(tidyr)
library(dplyr)
library(gt)
library(scales)
library(plotly)
library(tibble)
library(stats)

source("Functions.R")
ui <- fluidPage(
  h1("Shiny Player Lookup", align = "center"),
  h3("Created by Nick Arrivo", align = "center"),
  
  # use this in non dashboard app
  setBackgroundColor(color = "ghostwhite"),
  useArgonDash(),
  
  fluidRow(
    argonCard(
      selectizeInput('Name', "Player", choices = NULL),
      status = "primary",
      width = 4,
      title = "Player Select",
      hover_lift = FALSE,
      shadow = TRUE,
      icon = argonIcon("single-02"),
      icon_background = NULL,
      background_color = "info",
      gradient = !TRUE
      
      
    ),
    
    argonInfoCard(
      title = "Player Overview",
      value = textOutput("Name"),
      stat = NULL,
      icon = argonIcon("ungroup"),
      width = 4,
      description = textOutput("Team"),
      background_color = "primary",
      gradient = TRUE,
      hover_lift = FALSE
      
    ),
    
    argonInfoCard(
      title = "Whiff%",
      value = textOutput("Whiff"),
      stat = NULL,
      icon = argonIcon("ungroup"),
      width = 3,
      description = textOutput("Barrel"),
      background_color = "primary",
      gradient = TRUE,
      hover_lift = TRUE
      
    ),
  ),
  br(),
  #Change these cards into 2 larger cards with the rest of the info on the top line card
  fluidRow(
    argonInfoCard(
      value = textOutput("MaxEV"),
      title = "Max Exit Velo",
      stat = NULL,
      stat_icon =NULL,
      description = NULL,
      icon = argonIcon("trophy"),
      icon_background = "danger",
      background_color = "success",
      gradient = TRUE,
      hover_lift = TRUE,
      shadow = TRUE
    ),
    argonInfoCard(
      value = textOutput("RunsCreated"),
      title = "Runs Created",
      stat_icon =NULL,
      description = NULL,
      icon = argonIcon("chart-pie-35"),
      icon_background = "warning",
      background_color = "default",
      gradient = TRUE,
      shadow = TRUE,
      hover_lift = TRUE
    ),
    argonInfoCard(
      value = textOutput("BB") ,
      title = "Walk %",
      stat_icon = NULL,
      description = textOutput("K"),
      icon = argonIcon("bullet-list-67"),
      stat = NULL,
      icon_background =NULL,
      hover_lift = TRUE,
      gradient = TRUE,
      background_color = "info",
      shadow = TRUE
      
    ),
    argonInfoCard(
      value = textOutput("SlashLine"),
      title = "Slash Line",
      stat = NULL,
      stat_icon = "arrow-up",
      description = NULL,
      icon = argonIcon("basket"),
      icon_background = NULL,
      gradient = TRUE,
      background_color = "orange",
      hover_lift = TRUE
    )
  ),
  #put a title saying adjust the slider to show which range of players to show
  br(),
  fluidRow(
    argonCard(
      title = "sliderPlot",
      hover_lift = FALSE,
      icon = argonIcon("ui-04"),
      plotlyOutput("sliderPlot"),
      width = 20,
      sliderInput("sliderRuns", "Choose Runs Created range", min = 0.0, max = 100.0, value = c(60, 90), animate = TRUE, step = 1, width = '100%'),
      
      
      
    )
  ),
  
  br(),
  fluidRow(
    argonCard(
      title = "Table",
      hover_lift = FALSE,
      icon = argonIcon("archive-2"),
      uiOutput("Table"),
      width = 20,
      #Change table to adjust to slider plot values
    )
  )
)

server <- function(input, output, session) {
  updateSelectizeInput(session, 'Name', choices = Hitting_Leaders$playerFullName, server = TRUE)
  
  
  output$winsPercentile <- renderText(getPlayerWinsPercentile(input$Name))
  
  
  output$hitterCategory <- renderText(getHitterCategory(input$Name))
  
  output$CategoryDescription <- renderText(getHitterQualities(input$Name))
  
  output$progress <- renderUI({
    argonProgress(value = input$number, status = "danger", text = "Custom Text")
  })
  
  #Change this plot to find contact>discipline guys and also create new column in table
  
  output$sliderPlot <- renderPlotly({
    plotRunsRange(input$sliderRuns)
    
  })
  
  output$Team <- renderText(getPlayerTeam(input$Name))
  output$Name <- renderText(getPlayerName(input$Name))
  
  output$MaxEV <- renderText(getPlayerMaxEV(input$Name))
  output$RunsCreated <- renderText(getPlayerRunsCreated(input$Name))
  
  output$valueWins <- renderText(rangeOfWins(input$Name))
  
  output$K <- renderText(paste("Strikeout%: ", getPlayerK(input$Name)))
  output$BB <- renderText(getPlayerBB(input$Name))
  output$Whiff <- renderText(getPlayerIZWhiff(input$Name))
  output$Barrel <- renderText(paste("Barrel%: ", getPlayerBarrel(input$Name)))
  output$Table <- renderUI(getDataTable(input$Name))
  output$AVG <- renderText(getPlayerAVG(input$Name))
  output$OBP <- renderText(getPlayerOBP(input$Name))
  output$SLG <- renderText(getPlayerSLG(input$Name))
  output$OPS <- renderText(getPlayerOPS(input$Name))
  output$SlashLine <- renderText(paste0(getPlayerAVG(input$Name), "/", getPlayerOBP(input$Name), "/", getPlayerSLG(input$Name)))
  
  
  
  
  
  
  
}

shinyApp(ui, server)
