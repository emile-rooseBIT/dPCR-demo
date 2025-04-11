# app.R - Main entry point for the Shiny application

# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(dplyr)
library(shinyjs)

# Source global configuration, UI, and server logic
source("global.R")  # Load global variables and functions
source("ui.R")      # Load UI definition
source("server.R")  # Load server logic

# Run the application
shinyApp(ui = ui, server = server)


