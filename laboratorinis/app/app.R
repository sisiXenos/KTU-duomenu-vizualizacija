library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyverse)
library(readr)
library(ggplot2)

# UI
ui = dashboardPage(skin = "black",
                    dashboardHeader(title = "Sodros duomenys(kodas:560000)",
                                    titleWidth = 340),
                   dashboardSidebar(
                     width = 340,
                     selectizeInput(inputId = "kodas", label="Imones kodas",
                                    choices= NULL, selected= NULL),
                     sidebarMenu(
                       menuItem("Main", tabName= "main", icon = icon("home")),
                       menuItem("Raw Data", tabName= "data", icon = icon("info"))
                     )
                   ),
                   dashboardBody(
                     tabItems(
                       tabItem(tabName = "main",
                       fluidRow(
                         infoBoxOutput("name", width = 6),
                         infoBoxOutput("apdraustieji", width = 6),
                         box(
                           title = "Vidutinio atlyginimo diagrama", status = "primary", 
                           plotOutput("plot1", height=250)),
                         box(
                           title = "Menesine apdraustuju diagrama", status = "primary",
                           plotOutput("plot2", height = 250)),
                         box(
                           title = "Mokesciu diagrama", status = "primary",
                           plotOutput("plot3", height = 250)),
                         infoBoxOutput("tax", width = 6)
                         
                       )#fluidrow
                       ),#tabitem1
                       tabItem(tabName="data",
                               tableOutput("table")
                               )#tabitem2
                     )#tab items
                     
                   )#dashsidebar
)#dashpage


# Server
server = function(input, output, session) {
  raw = read_csv("../data/lab_sodra.csv")
  data <- raw%>%
    filter(ecoActCode==560000)%>%
    mutate(month = as.numeric(substr(month,5,6)))
  
  #for select bar in the sidebar
  updateSelectizeInput(
    session, "kodas",
    choices = data$code,
    selected = NULL
  )
  
  # raw data table output
  output$table = renderTable(
    data %>%
      filter(code == input$kodas)%>%
      mutate_if(is.numeric, function(x) format(x, nsmall = 0))
    
  )
  
  # Render name info box
  output$name = renderInfoBox({
    name = unique(data$name[data$code == input$kodas])
    infoBox(
      title =HTML(paste("Imones pavadinimas:","<h4; font-size:16px;'>", name, "</h4>")),
      color = "blue",
      width = "100%"
    )
  })
  
  # plot1- vidutinis atlyginimas
  output$plot1 = renderPlot(
    data %>%
      filter(code == input$kodas) %>%
      ggplot(aes(x=month, y=avgWage))+
      geom_line(color = "blue")+
      theme_bw()+
      scale_x_continuous(breaks = 1:12, labels = month.name)+
      labs(x= "Month", y="Euros")
  )
  
  #plot2 - apdraustuju diagrama
  output$plot2 = renderPlot(
    data %>%
      filter(code == input$kodas) %>%
      ggplot(aes(x =month, y = numInsured))+
      geom_line(color = "green")+
      theme_bw()+
      scale_x_continuous(breaks = 1:12, labels = month.name)+
      labs(x= "Month", y="Count")
  )

  # apdraustieji info box
  output$apdraustieji= renderInfoBox({
    drausti = sum(data$numInsured[data$code == input$kodas], na.rm = T)
    infoBox(
      paste("Apdraustuju kiekis: ", drausti),
      icon = icon("users"), 
      color = "green"
    )
  })
  
  #mokesciai info box
  output$tax= renderInfoBox({
    tax = sum(data$tax[data$code == input$kodas], na.rm = T)
    infoBox(
      paste("Sumoketi mokesciai siais metais: ", tax),
      icon = icon("dollar-sign"), 
      color = "maroon"
    )
  })
  
  # plot3 - mokesciu diagrama
  output$plot3 = renderPlot(
    data %>%
      filter(code == input$kodas) %>%
      ggplot(aes(x =month, y = tax))+
      geom_bar(stat = "identity",just = 0.5, fill = "maroon")+
      theme_bw()+
      scale_x_continuous(breaks = 1:12, labels = month.name)+
      labs(x= "Month", y="Count")
  )
} # server

# Create Shiny app
shinyApp(ui, server)

