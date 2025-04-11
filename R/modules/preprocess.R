# Preprocessing module for dPCR data

# UI Component
preprocessUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(
      width = 4,
      box(
        width = NULL,
        title = "Preprocessing Options",
        status = "primary",
        solidHeader = TRUE,
        checkboxInput(
          ns("normalize"), 
          "Normalize Channel Intensities",
          value = TRUE
        ),
        checkboxInput(
          ns("remove_outliers"), 
          "Remove Outliers",
          value = TRUE
        ),
        numericInput(
          ns("outlier_threshold"),
          "Outlier Threshold (SD)",
          value = 3,
          min = 1,
          max = 10,
          step = 0.1
        ),
        checkboxInput(
          ns("remove_rain"), 
          "Remove Rain Droplets",
          value = TRUE
        ),
        actionButton(
          ns("preprocess"),
          "Run Preprocessing",
          icon = icon("play"),
          class = "btn-primary"
        ),
        br(), br(),
        tags$div(
          class = "info-box",
          tags$h4("About Preprocessing"),
          tags$p("Preprocessing improves data quality and ensures robust analysis results."),
          tags$ul(
            tags$li(tags$strong("Normalization:"), " Scales intensity values to correct for channel-specific biases."),
            tags$li(tags$strong("Outlier Removal:"), " Removes extreme values that could skew thresholding."),
            tags$li(tags$strong("Rain Removal:"), " Filters out intermediate droplets that don't clearly classify as positive or negative.")
          ),
          tags$p("For more details, see: ", 
                tags$a(href = "https://example.com/preprocessing", "Best practices in dPCR preprocessing", target = "_blank"))
        )
      )
    ),
    column(
      width = 8,
      tabBox(
        width = NULL,
        tabPanel(
          "Before vs After",
          plotOutput(ns("comparison_plot"), height = "400px")
        ),
        tabPanel(
          "Data Summary",
          verbatimTextOutput(ns("summary"))
        ),
        tabPanel(
          "Preprocessing Log",
          verbatimTextOutput(ns("log"))
        )
      )
    ),
    column(
      width = 12,
      align = "right",
      conditionalPanel(
        condition = paste0("output['", ns("processed"), "']"),
        actionButton(
          ns("next_step"),
          "Next Step: App Selection",
          icon = icon("arrow-right"),
          class = "btn-success"
        )
      )
    )
  )
}

# Server Component
preprocess <- function(input, output, session, raw_data) {
  # Namespace
  ns <- session$ns
  
  # Reactive values to store preprocessing results
  preprocessed_data <- reactiveVal(NULL)
  preprocess_log <- reactiveVal("")
  
  # Run preprocessing when the button is clicked
  observeEvent(input$preprocess, {
    req(raw_data())
    
    # Start with raw data
    data <- raw_data()
    current_log <- "Preprocessing Log:\n"
    
    # Get numeric columns (assumed to be channels)
    numeric_cols <- sapply(data, is.numeric)
    channel_cols <- which(numeric_cols)
    
    # Normalize if selected
    if (input$normalize) {
      current_log <- paste0(current_log, "- Normalizing channel intensities\n")
      for (col in channel_cols) {
        col_name <- names(data)[col]
        data[[col_name]] <- scale(data[[col_name]])
        current_log <- paste0(current_log, "  * Normalized ", col_name, "\n")
      }
    }
    
    # Remove outliers if selected
    if (input$remove_outliers) {
      threshold <- input$outlier_threshold
      current_log <- paste0(current_log, "- Removing outliers (threshold: ", threshold, " SD)\n")
      
      initial_rows <- nrow(data)
      
      for (col in channel_cols) {
        col_name <- names(data)[col]
        col_data <- data[[col_name]]
        col_mean <- mean(col_data, na.rm = TRUE)
        col_sd <- sd(col_data, na.rm = TRUE)
        
        # Mark outliers
        outliers <- abs(col_data - col_mean) > threshold * col_sd
        
        # Remove outliers from dataset
        if (any(outliers, na.rm = TRUE)) {
          data <- data[!outliers, ]
          current_log <- paste0(current_log, "  * Removed ", sum(outliers, na.rm = TRUE), 
                               " outliers from ", col_name, "\n")
        }
      }
      
      current_log <- paste0(current_log, "  * Removed ", initial_rows - nrow(data), 
                           " rows in total (", round((initial_rows - nrow(data))/initial_rows * 100, 2), 
                           "% of data)\n")
    }
    
    # Remove rain droplets if selected
    if (input$remove_rain && "Classification" %in% colnames(data)) {
      current_log <- paste0(current_log, "- Identifying and removing rain droplets\n")
      
      # This is just a placeholder - in a real app, this would use a more sophisticated algorithm
      # For demo purposes, we'll simply mark droplets close to the threshold as "Rain"
      if (all(c("Channel1", "Channel2") %in% colnames(data))) {
        initial_rows <- nrow(data)
        
        # Define a simple rule to identify rain
        rain_mask <- data$Channel1 > 2100 & data$Channel1 < 2400 & 
                    data$Channel2 > 3200 & data$Channel2 < 3700
        
        if (any(rain_mask)) {
          data$Classification[rain_mask] <- "Rain"
          
          # Remove rain droplets
          data <- data[data$Classification != "Rain", ]
          
          current_log <- paste0(current_log, "  * Identified ", sum(rain_mask), 
                               " rain droplets\n")
          current_log <- paste0(current_log, "  * Final dataset has ", nrow(data), 
                               " droplets (removed ", initial_rows - nrow(data), " rain droplets)\n")
        } else {
          current_log <- paste0(current_log, "  * No rain droplets identified\n")
        }
      } else {
        current_log <- paste0(current_log, "  * Cannot identify rain droplets: required columns not found\n")
      }
    }
    
    # Complete preprocessing
    current_log <- paste0(current_log, "\nPreprocessing complete.\n")
    
    # Update reactive values
    preprocessed_data(data)
    preprocess_log(current_log)
    
    showNotification("Preprocessing completed successfully", type = "message")
  })
  
  # Comparison plot
  output$comparison_plot <- renderPlot({
    req(raw_data())
    
    # If preprocessed data is available, show comparison
    if (!is.null(preprocessed_data())) {
      par(mfrow = c(1, 2))
      
      # Before plot
      if (all(c("Channel1", "Channel2") %in% colnames(raw_data()))) {
        plot(raw_data()$Channel1, raw_data()$Channel2, 
             main = "Before Preprocessing",
             xlab = "Channel 1", ylab = "Channel 2",
             pch = 19, cex = 0.5, col = "darkgray")
      }
      
      # After plot
      if (all(c("Channel1", "Channel2") %in% colnames(preprocessed_data()))) {
        processed_data <- preprocessed_data()
        plot(processed_data$Channel1, processed_data$Channel2, 
             main = "After Preprocessing",
             xlab = "Channel 1", ylab = "Channel 2",
             pch = 19, cex = 0.5, 
             col = ifelse(processed_data$Classification == "Positive", "red", "blue"))
        legend("topright", legend = c("Positive", "Negative"), 
               col = c("red", "blue"), pch = 19, cex = 0.8)
      }
    } else {
      # Just show raw data
      if (all(c("Channel1", "Channel2") %in% colnames(raw_data()))) {
        plot(raw_data()$Channel1, raw_data()$Channel2, 
             main = "Raw Data (Before Preprocessing)",
             xlab = "Channel 1", ylab = "Channel 2",
             pch = 19, cex = 0.5, col = "darkgray")
      }
    }
  })
  
  # Data summary
  output$summary <- renderPrint({
    # If we have processed data, show a summary
    if (!is.null(preprocessed_data())) {
      cat("Preprocessed Data Summary:\n")
      cat("-------------------------\n")
      cat("Number of rows:", nrow(preprocessed_data()), "\n")
      cat("Number of columns:", ncol(preprocessed_data()), "\n\n")
      
      # If we have a Classification column, show class distribution
      if ("Classification" %in% colnames(preprocessed_data())) {
        counts <- table(preprocessed_data()$Classification)
        cat("Droplet Classification:\n")
        print(counts)
        cat("\n")
      }
      
      # Show numeric column summaries
      numeric_cols <- sapply(preprocessed_data(), is.numeric)
      if (any(numeric_cols)) {
        cat("Numeric Channel Statistics:\n")
        print(summary(preprocessed_data()[, numeric_cols, drop = FALSE]))
      }
    } else {
      cat("Run preprocessing to see data summary.")
    }
  })
  
  # Preprocessing log
  output$log <- renderPrint({
    if (preprocess_log() != "") {
      cat(preprocess_log())
    } else {
      cat("Run preprocessing to see log details.")
    }
  })
  
  # Flag indicating if preprocessing is done
  output$processed <- reactive({
    return(!is.null(preprocessed_data()))
  })
  outputOptions(output, "processed", suspendWhenHidden = FALSE)
  
  # Handle navigation to next step
  observeEvent(input$next_step, {
    updateTabItems(session, "mainNavbar", "app_select")
  })
  
  # Return the preprocessed data
  return(preprocessed_data)
}