# CNV Analysis Module

cnvUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 4,
        box(
          width = NULL,
          title = "CNV Analysis Settings",
          status = "primary",
          solidHeader = TRUE,
          
          selectInput(
            ns("target_column"),
            "Target Column:",
            choices = NULL
          ),
          
          selectInput(
            ns("reference_column"),
            "Reference Column:",
            choices = NULL
          ),
          
          numericInput(
            ns("reference_copies"),
            "Reference Copy Number:",
            value = 2,
            min = 1,
            max = 10,
            step = 1
          ),
          
          actionButton(
            ns("run_cnv_analysis"),
            "Run CNV Analysis",
            icon = icon("play"),
            class = "btn-success"
          )
        )
      ),
      
      column(
        width = 8,
        box(
          width = NULL,
          title = "CNV Visualization",
          status = "primary",
          solidHeader = TRUE,
          plotOutput(ns("cnv_plot"), height = "400px")
        )
      )
    ),
    
    fluidRow(
      box(
        width = 12,
        title = "CNV Results",
        status = "info",
        solidHeader = TRUE,
        
        verbatimTextOutput(ns("cnv_summary")),
        
        downloadButton(
          ns("download_cnv_results"),
          "Download Results",
          class = "btn-info"
        )
      )
    )
  )
}

cnv <- function(input, output, session, preprocessed_data) {
  ns <- session$ns
  
  # Reactive values
  cnv_result <- reactiveVal(NULL)
  
  # Update column choices when data changes
  observe({
    req(preprocessed_data())
    
    # Get column names from data
    columns <- colnames(preprocessed_data())
    
    # Update select inputs
    updateSelectInput(session, "target_column", choices = columns)
    updateSelectInput(session, "reference_column", choices = columns)
  })
  
  # Run CNV analysis when button is clicked
  observeEvent(input$run_cnv_analysis, {
    req(preprocessed_data(), input$target_column, input$reference_column)
    
    # Validate inputs
    if (input$target_column == input$reference_column) {
      showNotification("Target and reference columns must be different", type = "error")
      return(NULL)
    }
    
    # Get data
    data <- preprocessed_data()
    
    # Run CNV analysis
    result <- calculateCNV(
      data = data,
      target_col = input$target_column,
      reference_col = input$reference_column,
      reference_copies = input$reference_copies
    )
    
    # Store results
    cnv_result(result)
    
    # Show notification
    showNotification("CNV analysis completed successfully", type = "message")
  })
  
  # Helper function to calculate CNV
  calculateCNV <- function(data, target_col, reference_col, reference_copies) {
    # Extract target and reference data
    target_data <- data[[target_col]]
    reference_data <- data[[reference_col]]
    
    # Calculate concentration for target and reference
    target_pos <- sum(target_data > 0, na.rm = TRUE)
    target_total <- length(target_data)
    target_concentration <- -log(1 - (target_pos / target_total)) / 0.000851
    
    reference_pos <- sum(reference_data > 0, na.rm = TRUE)
    reference_total <- length(reference_data)
    reference_concentration <- -log(1 - (reference_pos / reference_total)) / 0.000851
    
    # Calculate CNV
    cnv_ratio <- target_concentration / reference_concentration
    estimated_copies <- cnv_ratio * reference_copies
    
    # Calculate confidence interval using Poisson statistics
    ci_lower <- qchisq(0.025, 2 * target_pos) / (2 * target_total) / 
                (reference_pos / reference_total) * reference_copies
    
    ci_upper <- qchisq(0.975, 2 * (target_pos + 1)) / (2 * target_total) / 
                (reference_pos / reference_total) * reference_copies
    
    # Return results
    return(list(
      target_column = target_col,
      reference_column = reference_col,
      target_pos = target_pos,
      target_total = target_total,
      reference_pos = reference_pos,
      reference_total = reference_total,
      target_concentration = target_concentration,
      reference_concentration = reference_concentration,
      cnv_ratio = cnv_ratio,
      estimated_copies = estimated_copies,
      reference_copies = reference_copies,
      ci_lower = ci_lower,
      ci_upper = ci_upper
    ))
  }
  
  # Render CNV plot
  output$cnv_plot <- renderPlot({
    req(cnv_result())
    
    result <- cnv_result()
    
    # Create data frame for plotting
    plot_data <- data.frame(
      Sample = "Sample 1",
      CNV = result$estimated_copies,
      Lower_CI = result$ci_lower,
      Upper_CI = result$ci_upper
    )
    
    # Create bar plot with error bars
    ggplot(plot_data, aes(x = Sample, y = CNV)) +
      geom_bar(stat = "identity", fill = "steelblue", width = 0.5) +
      geom_errorbar(
        aes(ymin = Lower_CI, ymax = Upper_CI),
        width = 0.2,
        color = "red",
        size = 1
      ) +
      geom_hline(yintercept = result$reference_copies, linetype = "dashed") +
      labs(
        title = "Copy Number Variation Analysis",
        y = "Estimated Copy Number",
        x = NULL
      ) +
      theme_minimal() +
      ylim(0, max(plot_data$Upper_CI) * 1.1)
  })
  
  # Render CNV summary
  output$cnv_summary <- renderPrint({
    req(cnv_result())
    
    result <- cnv_result()
    
    cat("CNV Analysis Results:\n\n")
    cat("Target:", result$target_column, "\n")
    cat("Reference:", result$reference_column, "\n\n")
    cat("Target Concentration:", round(result$target_concentration, 2), "copies/μL\n")
    cat("Reference Concentration:", round(result$reference_concentration, 2), "copies/μL\n")
    cat("CNV Ratio:", round(result$cnv_ratio, 4), "\n\n")
    cat("Reference Copy Number:", result$reference_copies, "\n")
    cat("Estimated Copy Number:", round(result$estimated_copies, 2), "\n")
    cat("95% Confidence Interval:", round(result$ci_lower, 2), "-", round(result$ci_upper, 2), "\n")
  })
  
  # Download handler for results
  output$download_cnv_results <- downloadHandler(
    filename = function() {
      paste("cnv-analysis-results-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      req(cnv_result())
      
      result <- cnv_result()
      
      # Create data frame for export
      export_data <- data.frame(
        Analysis = "CNV",
        Target = result$target_column,
        Reference = result$reference_column,
        Target_Concentration = result$target_concentration,
        Reference_Concentration = result$reference_concentration,
        CNV_Ratio = result$cnv_ratio,
        Reference_Copies = result$reference_copies,
        Estimated_Copies = result$estimated_copies,
        CI_Lower = result$ci_lower,
        CI_Upper = result$ci_upper
      )
      
      write.csv(export_data, file, row.names = FALSE)
    }
  )
  
  # Return CNV results
  return(cnv_result)
}