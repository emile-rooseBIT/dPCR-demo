# Threshold Determination Module

thresholdUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 4,
        box(
          width = NULL,
          title = "Threshold Settings",
          status = "primary",
          solidHeader = TRUE,
          
          selectInput(
            ns("threshold_method"),
            "Threshold Method:",
            choices = c(
              "Automatic (K-means)" = "kmeans",
              "Manual Setting" = "manual",
              "Percentile Based" = "percentile"
            ),
            selected = "kmeans"
          ),
          
          conditionalPanel(
            condition = paste0("input['", ns("threshold_method"), "'] == 'manual'"),
            sliderInput(
              ns("manual_threshold"),
              "Manual Threshold Value:",
              min = 0,
              max = 10000,
              value = 2000,
              step = 100
            )
          ),
          
          conditionalPanel(
            condition = paste0("input['", ns("threshold_method"), "'] == 'percentile'"),
            sliderInput(
              ns("percentile_value"),
              "Percentile (%):",
              min = 1,
              max = 99,
              value = 50,
              step = 1
            )
          ),
          
          actionButton(
            ns("apply_threshold"),
            "Apply Threshold",
            icon = icon("check"),
            class = "btn-success"
          )
        )
      ),
      
      column(
        width = 8,
        box(
          width = NULL,
          title = "Threshold Visualization",
          status = "primary",
          solidHeader = TRUE,
          plotOutput(ns("threshold_plot"), height = "400px")
        )
      )
    ),
    
    fluidRow(
      box(
        width = 12,
        title = "Threshold Results",
        status = "info",
        solidHeader = TRUE,
        
        verbatimTextOutput(ns("threshold_summary")),
        
        downloadButton(
          ns("download_results"),
          "Download Results",
          class = "btn-info"
        )
      )
    )
  )
}

threshold <- function(input, output, session, preprocessed_data) {
  ns <- session$ns
  
  # Reactive values
  threshold_result <- reactiveVal(NULL)
  current_threshold <- reactiveVal(NULL)
  
  # Calculate threshold when apply button is clicked
  observeEvent(input$apply_threshold, {
    req(preprocessed_data())
    
    data <- preprocessed_data()
    
    # Calculate threshold based on selected method
    threshold_value <- switch(
      input$threshold_method,
      "kmeans" = calculateKmeansThreshold(data),
      "manual" = input$manual_threshold,
      "percentile" = calculatePercentileThreshold(data, input$percentile_value),
      2000  # Default fallback
    )
    
    # Store the current threshold
    current_threshold(threshold_value)
    
    # Apply threshold to data
    result <- applyThreshold(data, threshold_value)
    
    # Store results
    threshold_result(result)
    
    # Show notification
    showNotification("Threshold applied successfully", type = "message")
  })
  
  # Helper function for k-means threshold
  calculateKmeansThreshold <- function(data) {
    # Extract fluorescence intensity values
    intensity <- data$fluorescence_intensity
    
    # Apply k-means clustering with 2 centers
    kmeans_result <- kmeans(intensity, centers = 2)
    
    # Get the centroids and calculate midpoint
    centroids <- kmeans_result$centers
    threshold <- mean(centroids)
    
    return(threshold)
  }
  
  # Helper function for percentile threshold
  calculatePercentileThreshold <- function(data, percentile) {
    # Extract fluorescence intensity values
    intensity <- data$fluorescence_intensity
    
    # Calculate the percentile
    threshold <- quantile(intensity, percentile/100)
    
    return(threshold)
  }
  
  # Helper function to apply threshold
  applyThreshold <- function(data, threshold) {
    # Classify droplets as positive or negative
    data$classification <- ifelse(data$fluorescence_intensity > threshold, 
                                "positive", 
                                "negative")
    
    # Calculate the number of positive and negative droplets
    positive_count <- sum(data$classification == "positive")
    negative_count <- sum(data$classification == "negative")
    total_count <- nrow(data)
    
    # Calculate the concentration
    concentration <- -log(negative_count / total_count) / 0.000851  # Assuming volume of 0.851 nL
    
    # Return results
    return(list(
      data = data,
      threshold = threshold,
      positive_count = positive_count,
      negative_count = negative_count,
      total_count = total_count,
      concentration = concentration
    ))
  }
  
  # Render threshold plot
  output$threshold_plot <- renderPlot({
    req(preprocessed_data())
    
    # Get the preprocessed data
    data <- preprocessed_data()
    
    # Create histogram of fluorescence intensity
    p <- ggplot(data, aes(x = fluorescence_intensity)) +
      geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7) +
      theme_minimal() +
      labs(
        title = "Fluorescence Intensity Distribution",
        x = "Fluorescence Intensity",
        y = "Count"
      )
    
    # Add threshold line if available
    if (!is.null(current_threshold())) {
      p <- p + 
        geom_vline(xintercept = current_threshold(), 
                  color = "red", 
                  linetype = "dashed", 
                  size = 1) +
        annotate("text", 
                x = current_threshold() * 1.1, 
                y = max(ggplot_build(p)$data[[1]]$count) * 0.9,
                label = paste("Threshold =", round(current_threshold(), 2)),
                color = "red")
    }
    
    return(p)
  })
  
  # Render threshold summary
  output$threshold_summary <- renderPrint({
    req(threshold_result())
    
    result <- threshold_result()
    
    cat("Threshold Value:", round(result$threshold, 2), "\n")
    cat("Total Droplets:", result$total_count, "\n")
    cat("Positive Droplets:", result$positive_count, " (", 
        round(result$positive_count/result$total_count*100, 2), "%)\n", sep="")
    cat("Negative Droplets:", result$negative_count, " (", 
        round(result$negative_count/result$total_count*100, 2), "%)\n", sep="")
    cat("Estimated Concentration:", round(result$concentration, 2), "copies/μL\n")
  })
  
  # Download handler for results
  output$download_results <- downloadHandler(
    filename = function() {
      paste("threshold-results-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      req(threshold_result())
      write.csv(threshold_result()$data, file, row.names = FALSE)
    }
  )
  
  # Return threshold results
  return(threshold_result)
}