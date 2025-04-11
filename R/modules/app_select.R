# App Selection Module

appSelectionUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        width = 12,
        title = "Select Analysis Modules",
        status = "primary",
        solidHeader = TRUE,
        p("Choose one or more analysis modules to run on your preprocessed data:"),
        p("Each module performs a specific analysis task. You can select multiple modules to run in sequence.")
      )
    ),
    
    fluidRow(
      uiOutput(ns("available_apps_ui"))
    ),
    
    fluidRow(
      column(
        width = 12,
        align = "right",
        conditionalPanel(
          condition = paste0("input['", ns("has_selected_apps"), "'] == true"),
          actionButton(
            ns("next_step"),
            "Next Step: Run Analysis",
            icon = icon("arrow-right"),
            class = "btn-success"
          )
        )
      )
    )
  )
}

appSelection <- function(input, output, session, available_apps) {
  ns <- session$ns
  
  # Reactive value to store selected apps
  selected_apps <- reactiveVal(NULL)
  
  # Generate UI for available apps
  output$available_apps_ui <- renderUI({
    req(available_apps())
    
    app_cards <- lapply(available_apps(), function(app) {
      box(
        width = 4,
        title = app$name,
        status = "info",
        solidHeader = TRUE,
        
        p(app$description),
        p(tags$small(tags$a(href = app$url, target = "_blank", 
                           paste0("Publication: ", app$citation)))),
        
        checkboxInput(
          ns(paste0("select_app_", app$id)),
          "Select this module",
          value = FALSE
        )
      )
    })
    
    fluidRow(app_cards)
  })
  
  # Update selected_apps based on checkboxes
  observe({
    req(available_apps())
    
    # Get all selected app IDs
    selected <- sapply(available_apps(), function(app) {
      checkbox_id <- paste0("select_app_", app$id)
      if (!is.null(input[[checkbox_id]]) && input[[checkbox_id]]) {
        return(app$id)
      } else {
        return(NULL)
      }
    })
    
    # Filter out NULL values and store
    selected <- selected[!sapply(selected, is.null)]
    selected_apps(selected)
    
    # Update hidden input for conditional panel
    session$sendCustomMessage(type = "updateSelectStatus", 
                            message = list(
                              inputId = ns("has_selected_apps"),
                              value = length(selected) > 0
                            ))
  })
  
  # Handle navigation to next step
  observeEvent(input$next_step, {
    updateTabsetPanel(session$parentSession, "mainNav", selected = "4. Analysis")
  })
  
  # Hidden input for conditional panel
  output$has_selected_apps <- reactive({
    return(!is.null(selected_apps()) && length(selected_apps()) > 0)
  })
  outputOptions(output, "has_selected_apps", suspendWhenHidden = FALSE)
  
  # Return the selected apps
  return(selected_apps)
}