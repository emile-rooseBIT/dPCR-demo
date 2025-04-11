# Upload module for dPCR data

# UI Component
uploadUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(
      width = 6,
      box(
        width = NULL,
        title = "Upload Your dPCR Data",
        status = "primary",
        solidHeader = TRUE,
        fileInput(
          ns("file"), 
          "Choose a file to upload:",
          accept = c(".csv", ".xlsx", ".rds"),
          multiple = FALSE
        ),
        helpText(
          "Supported formats: CSV, Excel, RDS",
          br(),
          paste("Maximum file size:", MAX_UPLOAD_SIZE/1024^2, "MB")
        )
      )
    ),
    column(
      width = 6,
      box(
        width = NULL,
        title = "Example Datasets",
        status = "info",
        solidHeader = TRUE,
        selectInput(
          ns("example_dataset"),
          "Or load an example dataset:",
          choices = c(
            "Select an example" = "",
            "Standard dPCR example" = "example_data"
          )
        ),
        conditionalPanel(
          condition = paste0("input['", ns("example_dataset"), "'] !== ''"),
          actionButton(
            ns("load_example"),
            "Load Example",
            icon = icon("database"),
            class = "btn-info"
          )
        )
      )
    ),
    column(
      width = 12,
      box(
        width = NULL,
        title = "Data Preview",
        status = "primary",
        solidHeader = TRUE,
        DT::dataTableOutput(ns("preview"))
      )
    ),
    column(
      width = 12,
      box(
        width = NULL,
        title = "Summary Statistics",
        status = "info",
        solidHeader = TRUE,
        verbatimTextOutput(ns("summary"))
      )
    ),
    column(
      width = 12,
      align = "right",
      conditionalPanel(
        condition = paste0("output['", ns("data_loaded"), "']"),
        actionButton(
          ns("next_step"),
          "Next Step: Preprocessing",
          icon = icon("arrow-right"),
          class = "btn-success"
        )
      )
    )
  )
}

# Server Component
upload <- function(input, output, session) {
  # Namespace
  ns <- session$ns
  
  # Reactive values to store the data
  uploaded_data <- reactiveVal(NULL)
  
  # Handle file upload
  observeEvent(input$file, {
    req(input$file)
    
    # Get the file extension
    ext <- tools::file_ext(input$file$name)
    
    # Check if file type is supported
    if (!ext %in% SUPPORTED_FILE_TYPES) {
      showNotification(
        "Unsupported file type. Please upload a CSV, Excel, or RDS file.",
        type = "error"
      )
      return(NULL)
    }
    
    # Read the file based on its extension
    tryCatch({
      data <- switch(ext,
                    "csv" = read.csv(input$file$datapath, stringsAsFactors = FALSE),
                    "xlsx" = readxl::read_excel(input$file$datapath),
                    "rds" = readRDS(input$file$datapath),
                    NULL)
      
      # Basic validation (in real app, would be more complex)
      if (is.null(data) || ncol(data) < 2) {
        showNotification(
          "Invalid data format: File must contain at least two columns.",
          type = "error"
        )
        return(NULL)
      }
      
      # Store the data
      uploaded_data(data)
      showNotification("Data successfully loaded", type = "message")
    }, error = function(e) {
      showNotification(
        paste("Error reading file:", e$message),
        type = "error"
      )
    })
  })
  
  # Handle example dataset selection
  observeEvent(input$load_example, {
    req(input$example_dataset)
    
    tryCatch({
      # In a real app, this would load from /data directory
      # For the demo, we'll simulate dPCR data
      set.seed(123)
      num_rows <- 1000
      
      # Create mock dPCR data
      data <- data.frame(
        Channel1 = rnorm(num_rows, mean = 2000, sd = 500),
        Channel2 = rnorm(num_rows, mean = 3000, sd = 700),
        Droplet_ID = paste0("D", 1:num_rows),
        Well = rep(paste0("Well", 1:8), length.out = num_rows),
        Sample = rep(paste0("Sample", 1:4), length.out = num_rows)
      )
      
      # Add classification column with some positive droplets
      data$Classification <- ifelse(data$Channel1 > 2300 & data$Channel2 > 3500, 
                                 "Positive", "Negative")
      
      # Store the data
      uploaded_data(data)
      showNotification("Example dataset loaded successfully", type = "message")
    }, error = function(e) {
      showNotification(
        paste("Error loading example dataset:", e$message),
        type = "error"
      )
    })
  })
  
  # Data preview
  output$preview <- DT::renderDataTable({
    req(uploaded_data())
    DT::datatable(
      head(uploaded_data(), 15),
      options = list(
        pageLength = 5,
        scrollX = TRUE,
        dom = 'Bfrtip'
      )
    )
  })
  
  # Summary statistics
  output$summary <- renderPrint({
    req(uploaded_data())
    
    # Get basic summary
    cat("Dataset Summary:\n")
    cat("----------------\n")
    cat("Number of rows:", nrow(uploaded_data()), "\n")
    cat("Number of columns:", ncol(uploaded_data()), "\n\n")
    
    # If we have a Classification column, show class distribution
    if ("Classification" %in% colnames(uploaded_data())) {
      counts <- table(uploaded_data()$Classification)
      cat("Droplet Classification:\n")
      print(counts)
      cat("\n")
    }
    
    # Show numeric column summaries
    numeric_cols <- sapply(uploaded_data(), is.numeric)
    if (any(numeric_cols)) {
      cat("Numeric Channel Statistics:\n")
      print(summary(uploaded_data()[, numeric_cols, drop = FALSE]))
    }
  })
  
  # Flag indicating if data is loaded
  output$data_loaded <- reactive({
    return(!is.null(uploaded_data()))
  })
  outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)
  
  # Handle navigation to next step
  observeEvent(input$next_step, {
    updateTabItems(session, "mainNavbar", "preprocess")
  })
  
  # Return the uploaded data
  return(uploaded_data)
}