# ui.R - Main UI structure for the application

ui <- dashboardPage(
  # Dashboard header
  dashboardHeader(
    title = "dPCR Analysis Suite",
    tags$li(
      class = "dropdown",
      tags$a(
        href = NULL,
        paste("Version:", APP_VERSION)
      )
    )
  ),
  
  # Dashboard sidebar with navigation menu
  dashboardSidebar(
    sidebarMenu(
      id = "mainNavbar",
      menuItem("1. Data Upload", tabName = "upload", icon = icon("upload")),
      menuItem("2. Preprocessing", tabName = "preprocess", icon = icon("sliders-h")),
      menuItem("3. App Selection", tabName = "app_select", icon = icon("th-large")),
      menuItem("4. Analysis", tabName = "analysis", icon = icon("chart-bar")),
      menuItem("5. Report", tabName = "report", icon = icon("file-alt"))
    )
  ),
  
  # Dashboard body content
  dashboardBody(
    # Use shinyjs
    useShinyjs(),
    
    # Include custom CSS
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    
    # Main tabbed interface
    tabItems(
      # Tab 1: Data Upload
      tabItem(
        tabName = "upload",
        h2("Step 1: Data Upload"),
        uploadUI("upload")
      ),
      
      # Tab 2: Preprocessing
      tabItem(
        tabName = "preprocess",
        h2("Step 2: Data Preprocessing"),
        preprocessUI("preprocess")
      ),
      
      # Tab 3: App Selection
      tabItem(
        tabName = "app_select",
        h2("Step 3: Analysis App Selection"),
        appSelectionUI("app_select")
      ),
      
      # Tab 4: Analysis (dynamically generated based on selected apps)
      tabItem(
        tabName = "analysis",
        h2("Step 4: Analysis"),
        uiOutput("analysisTabsUI")
      ),
      
      # Tab 5: Report Generation
      tabItem(
        tabName = "report",
        h2("Step 5: Report Generation"),
        reportUI("report")
      )
    )
  )
)