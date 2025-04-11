# Report Generation Module

reportUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 4,
        box(
          width = NULL,
          title = "Report Settings",
          status = "primary",
          solidHeader = TRUE,
          
          textInput(
            ns("report_title"),
            "Report Title:",
            value = "dPCR Analysis Report"
          ),
          
          textInput(
            ns("author_name"),
            "Author Name:",
            value = ""
          ),
          
          dateInput(
            ns("report_date"),
            "Report Date:",
            value = Sys.Date()
          ),
          
          textAreaInput(
            ns("report_notes"),
            "Additional Notes:",
            rows = 3
          ),
          
          selectInput(
            ns("report_format"),
            "Report Format:",
            choices = c(
              "HTML" = "html",
              "PDF" = "pdf",
              "Word" = "docx"
            ),
            selected = "html"
          )
        ),
        
        box(
          width = NULL,
          title = "Content Selection",
          status = "primary",
          solidHeader = TRUE,
          
          uiOutput(ns("content_selection_ui")),
          
          actionButton(
            ns("generate_report"),
            "Generate Report",
            icon = icon("file-alt"),
            class = "btn-success"
          )
        )
      ),
      
      column(
        width = 8,
        box(
          width = NULL,
          title = "Report Preview",
          status = "primary",
          solidHeader = TRUE,
          
          htmlOutput(ns("report_preview"))
        )
      )
    )
  )
}

report <- function(input, output, session, app_results) {
  ns <- session$ns
  
  # Reactive value for report content
  report_content <- reactiveVal(NULL)
  
  # Generate content selection UI
  output$content_selection_ui <- renderUI({
    req(app_results())
    
    results <- app_results()
    
    if (length(results) == 0) {
      return(p("No analysis results available for reporting."))
    }
    
    # Create UI elements for each available result
    selection_elements <- lapply(names(results), function(app_id) {
      # Determine which figures and tables are available for this app
      if (app_id == "threshold") {
        checkboxGroupInput(
          ns(paste0("include_", app_id)),
          paste0("Include from ", app_id, " analysis:"),
          choices = c(
            "Threshold Plot" = "plot",
            "Threshold Summary" = "summary"
          ),
          selected = c("plot", "summary")
        )
      } else if (app_id == "cnv") {
        checkboxGroupInput(
          ns(paste0("include_", app_id)),
          paste0("Include from ", app_id, " analysis:"),
          choices = c(
            "CNV Plot" = "plot",
            "CNV Summary" = "summary"
          ),
          selected = c("plot", "summary")
        )
      } else {
        # Generic selection for other app types
        checkboxGroupInput(
          ns(paste0("include_", app_id)),
          paste0("Include from ", app_id, " analysis:"),
          choices = c(
            "Plots" = "plots",
            "Summary" = "summary"
          ),
          selected = c("plots", "summary")
        )
      }
    })
    
    tagList(selection_elements)
  })
  
  # Generate report when button is clicked
  observeEvent(input$generate_report, {
    req(app_results())
    
    results <- app_results()
    
    if (length(results) == 0) {
      showNotification("No analysis results available for reporting", type = "error")
      return(NULL)
    }
    
    # Collect selected content for each app
    selected_content <- lapply(names(results), function(app_id) {
      selection_id <- paste0("include_", app_id)
      if (!is.null(input[[selection_id]])) {
        return(list(
          app_id = app_id,
          selections = input[[selection_id]]
        ))
      } else {
        return(NULL)
      }
    })
    
    # Filter out NULL values
    selected_content <- selected_content[!sapply(selected_content, is.null)]
    
    # Generate report content
    report <- generateReportContent(
      results = results,
      selections = selected_content,
      title = input$report_title,
      author = input$author_name,
      date = input$report_date,
      notes = input$report_notes
    )
    
    # Store report content
    report_content(report)
    
    # Show notification
    showNotification("Report generated successfully", type = "message")
    
    # Trigger download
    downloadReport()
  })
  
  # Helper function to generate report content
  generateReportContent <- function(results, selections, title, author, date, notes) {
    # Create report header
    header <- paste0(
      "# ", title, "\n\n",
      "**Author:** ", author, "\n\n",
      "**Date:** ", format(date, "%B %d, %Y"), "\n\n",
      if (nchar(notes) > 0) paste0("**Notes:** ", notes, "\n\n") else ""
    )
    
    # Create content for each selected app
    content_sections <- lapply(selections, function(selection) {
      app_id <- selection$app_id
      app_selections <- selection$selections
      
      # Get the result for this app
      app_result <- results[[app_id]]
      
      # Create section header
      section <- paste0("## ", toupper(substr(app_id, 1, 1)), substr(app_id, 2, nchar(app_id)), " Analysis\n\n")
      
      if (app_id == "threshold") {
        if ("summary" %in% app_selections) {
          section <- paste0(section, "### Summary\n\n")
          section <- paste0(section, "Threshold Value: ", round(app_result()$threshold, 2), "\n\n")
          section <- paste0(section, "Total Droplets: ", app_result()$total_count, "\n\n")
          section <- paste0(section, "Positive Droplets: ", app_result()$positive_count, 
                           " (", round(app_result()$positive_count/app_result()$total_count*100, 2), "%)\n\n")
          section <- paste0(section, "Negative Droplets: ", app_result()$negative_count, 
                           " (", round(app_result()$negative_count/app_result()$total_count*100, 2), "%)\n\n")
          section <- paste0(section, "Estimated Concentration: ", round(app_result()$concentration, 2), " copies/μL\n\n")
        }
        
        if ("plot" %in% app_selections) {
          section <- paste0(section, "### Threshold Plot\n\n")
          section <- paste0(section, "![Threshold Plot](threshold_plot.png)\n\n")
        }
      } else if (app_id == "cnv") {
        if ("summary" %in% app_selections) {
          section <- paste0(section, "### Summary\n\n")
          section <- paste0(section, "Target: ", app_result()$target_column, "\n\n")
          section <- paste0(section, "Reference: ", app_result()$reference_column, "\n\n")
          section <- paste0(section, "Target Concentration: ", round(app_result()$target_concentration, 2), " copies/μL\n\n")
          section <- paste0(section, "Reference Concentration: ", round(app_result()$reference_concentration, 2), " copies/μL\n\n")
          section <- paste0(section, "CNV Ratio: ", round(app_result()$cnv_ratio, 4), "\n\n")
          section <- paste0(section, "Reference Copy Number: ", app_result()$reference_copies, "\n\n")
          section <- paste0(section, "Estimated Copy Number: ", round(app_result()$estimated_copies, 2), "\n\n")
          section <- paste0(section, "95% Confidence Interval: ", round(app_result()$ci_lower, 2), " - ", 
                           round(app_result()$ci_upper, 2), "\n\n")
        }
        
        if ("plot" %in% app_selections) {
          section <- paste0(section, "### CNV Plot\n\n")
          section <- paste0(section, "![CNV Plot](cnv_plot.png)\n\n")
        }
      } else {
        # Generic handling for other app types
        section <- paste0(section, "Results from ", app_id, " analysis are included in this report.\n\n")
      }
      
      return(section)
    })
    
    # Combine all sections
    full_report <- paste0(
      header,
      paste(unlist(content_sections), collapse = "\n")
    )
    
    return(full_report)
  }
  
  # Render report preview
  output$report_preview <- renderUI({
    req(report_content())
    
    # Convert markdown to HTML for preview
    html_content <- markdown::markdownToHTML(
      text = report_content(),
      fragment.only = TRUE
    )
    
    HTML(html_content)
  })
  
  # Download handler for report
  downloadReport <- function() {
    req(report_content(), input$report_format)
    
    # Generate temporary file path
    temp_dir <- tempdir()
    temp_md <- file.path(temp_dir, "report.md")
    
    # Write markdown content to file
    writeLines(report_content(), temp_md)
    
    # Determine output format
    output_format <- switch(
      input$report_format,
      "html" = "html_document",
      "pdf" = "pdf_document",
      "docx" = "word_document"
    )
    
    # Render the report
    output_file <- file.path(temp_dir, paste0("report.", input$report_format))
    
    tryCatch({
      rmarkdown::render(
        input = temp_md,
        output_format = output_format,
        output_file = output_file,
        quiet = TRUE
      )
      
      # Create download link
      showModal(modalDialog(
        title = "Report Generated",
        p("Your report has been generated successfully."),
        downloadButton(
          ns("download_report_file"),
          "Download Report",
          class = "btn-primary"
        ),
        easyClose = TRUE
      ))
    }, error = function(e) {
      showNotification(paste("Error generating report:", e$message), type = "error")
    })
  }
  
  # Download handler for report file
  output$download_report_file <- downloadHandler(
    filename = function() {
      paste0(
        gsub(" ", "_", input$report_title), "_",
        format(input$report_date, "%Y%m%d"), ".",
        input$report_format
      )
    },
    content = function(file) {
      # Get the temporary file path
      temp_dir <- tempdir()
      temp_file <- file.path(temp_dir, paste0("report.", input$report_format))
      
      # Copy the file
      file.copy(temp_file, file)
    }
  )
  
  # Return the report content
  return(report_content)
}