# server.R - Main server logic for the application

server <- function(input, output, session) {
  # Create a central reactive values object to store app state
  app_state <- reactiveValues(
    raw_data = NULL,
    preprocessed_data = NULL,
    selected_apps = NULL,
    app_results = list()
  )
  
  # Navigation control
  observe({
    # Disable tabs until data is loaded and processed
    if (is.null(app_state$raw_data)) {
      hideTab("mainNavbar", "preprocess")
      hideTab("mainNavbar", "app_select")
      hideTab("mainNavbar", "analysis")
      hideTab("mainNavbar", "report")
    } else {
      showTab("mainNavbar", "preprocess")
      
      if (!is.null(app_state$preprocessed_data)) {
        showTab("mainNavbar", "app_select")
        
        if (!is.null(app_state$selected_apps) && length(app_state$selected_apps) > 0) {
          showTab("mainNavbar", "analysis")
          
          if (length(app_state$app_results) > 0) {
            showTab("mainNavbar", "report")
          } else {
            hideTab("mainNavbar", "report")
          }
        } else {
          hideTab("mainNavbar", "analysis")
          hideTab("mainNavbar", "report")
        }
      } else {
        hideTab("mainNavbar", "app_select")
        hideTab("mainNavbar", "analysis")
        hideTab("mainNavbar", "report")
      }
    }
  })
  
  # Step 1: Data Upload
  upload_results <- callModule(
    upload, 
    "upload"
  )
  
  # Update app state when upload completes
  observe({
    data <- upload_results()
    if (!is.null(data)) {
      app_state$raw_data <- data
    }
  })
  
  # Step 2: Preprocessing
  preprocess_results <- callModule(
    preprocess, 
    "preprocess", 
    reactive(app_state$raw_data)
  )
  
  # Update app state when preprocessing completes
  observe({
    preproc_data <- preprocess_results()
    if (!is.null(preproc_data)) {
      app_state$preprocessed_data <- preproc_data
    }
  })
  
  # Step 3: App Selection
  selected_apps <- callModule(
    appSelection, 
    "app_select", 
    reactive(AVAILABLE_APPS)
  )
  
  # Update app state when apps are selected
  observe({
    apps <- selected_apps()
    if (!is.null(apps) && length(apps) > 0) {
      app_state$selected_apps <- apps
    }
  })
  
  # Step 4: Analysis
  # Dynamically generate UI for selected apps
  output$analysisTabsUI <- renderUI({
    req(app_state$selected_apps)
    
    tabBox(
      id = "analysisTabBox",
      width = 12,
      tabsetPanel(
        id = "analysisTabs",
        type = "pills",
        lapply(app_state$selected_apps, function(app_id) {
          app_info <- AVAILABLE_APPS[[app_id]]
          tabPanel(
            app_info$name,
            if (app_id == "threshold") {
              thresholdUI(app_id)
            } else if (app_id == "cnv") {
              cnvUI(app_id)
            }
          )
        })
      )
    )
  })
  
  # Initialize each selected app
  observe({
    req(app_state$selected_apps, app_state$preprocessed_data)
    
    # Clear previous results when apps change
    app_state$app_results <- list()
    
    # Initialize each app
    for (app_id in app_state$selected_apps) {
      if (app_id == "threshold") {
        threshold_result <- callModule(
          threshold, 
          app_id, 
          reactive(app_state$preprocessed_data)
        )
        
        # Store results when available
        observe({
          result <- threshold_result()
          if (!is.null(result)) {
            app_state$app_results[[app_id]] <- result
          }
        })
      } else if (app_id == "cnv") {
        cnv_result <- callModule(
          cnv, 
          app_id, 
          reactive(app_state$preprocessed_data)
        )
        
        # Store results when available
        observe({
          result <- cnv_result()
          if (!is.null(result)) {
            app_state$app_results[[app_id]] <- result
          }
        })
      }
    }
  })
  
  # Step 5: Report Generation
  callModule(
    report, 
    "report", 
    reactive(app_state$app_results),
    reactive(app_state$selected_apps)
  )
}