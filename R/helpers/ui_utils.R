# UI utility functions for the dPCR Analysis app

#' Create a box with a help icon and tooltip
#'
#' @param title Box title
#' @param status Box status color
#' @param solidHeader Whether to use solid header
#' @param width Box width
#' @param tooltip_text Text to show in tooltip
#' @param ... Additional elements to include in the box
#'
#' @return A shinydashboard box with help tooltip
createBoxWithTooltip <- function(title, status = "primary", solidHeader = TRUE, 
                                width = NULL, tooltip_text = NULL, ...) {
  box_title <- div(
    title,
    if (!is.null(tooltip_text)) {
      span(
        icon("question-circle"), 
        style = "margin-left: 5px; cursor: help;",
        title = tooltip_text
      )
    }
  )
  
  shinydashboard::box(
    title = box_title,
    status = status,
    solidHeader = solidHeader,
    width = width,
    ...
  )
}

#' Create a citation block with optional link
#'
#' @param citation_text Citation text
#' @param url Optional URL to the publication
#'
#' @return A div containing the citation
createCitation <- function(citation_text, url = NULL) {
  if (!is.null(url)) {
    div(
      class = "citation",
      "Citation: ", 
      a(citation_text, href = url, target = "_blank")
    )
  } else {
    div(
      class = "citation",
      "Citation: ", citation_text
    )
  }
}

#' Create an app selection card
#'
#' @param id The ID of the app
#' @param name The name of the app
#' @param description A brief description of the app
#' @param citation Citation information
#' @param url URL to publication
#' @param selected Whether the app is selected
#'
#' @return A div representing an app card
createAppCard <- function(id, name, description, citation, url, selected = FALSE) {
  div(
    class = paste("app-card", if(selected) "selected" else ""),
    id = paste0("app-card-", id),
    div(class = "app-card-title", name),
    div(class = "app-card-description", description),
    createCitation(citation, url),
    div(
      style = "text-align: right; margin-top: 10px;",
      checkboxInput(
        inputId = paste0("select-app-", id),
        label = "Select",
        value = selected
      )
    )
  )
}

#' Create a loading spinner
#'
#' @param text Text to display with the spinner
#'
#' @return A div containing a spinner and message
createLoadingSpinner <- function(text = "Processing...") {
  div(
    class = "loading-spinner",
    tags$i(class = "fa fa-spinner fa-spin fa-3x"),
    div(text)
  )
}

#' Generate a formatted plot download panel
#'
#' @param id The ID to use for the UI elements
#' @param ns The namespace function
#'
#' @return A div containing plot download options
createPlotDownloadPanel <- function(id, ns) {
  div(
    style = "margin: 10px 0; padding: 10px; background-color: #f8f9fa; border-radius: 5px;",
    div(
      style = "display: flex; align-items: center; justify-content: space-between;",
      div(
        "Download Plot:",
        style = "font-weight: bold;"
      ),
      div(
        style = "display: flex; gap: 10px;",
        downloadButton(ns(paste0(id, "_download_png")), "PNG"),
        downloadButton(ns(paste0(id, "_download_pdf")), "PDF")
      )
    )
  )
}

#' Create a collapsible help section
#'
#' @param title Section title
#' @param content Help content (HTML)
#' @param id Unique ID for the section
#'
#' @return A div containing a collapsible help section
createHelpSection <- function(title, content, id) {
  div(
    div(
      style = "cursor: pointer; padding: 10px; background-color: #f8f9fa; 
              border: 1px solid #e9ecef; border-radius: 5px; margin-bottom: 5px;",
      onclick = paste0("$('#", id, "').toggle()"),
      icon("question-circle"), 
      strong(title),
      icon("chevron-down", style = "float: right;")
    ),
    div(
      id = id,
      style = "display: none; padding: 15px; border: 1px solid #e9ecef; 
              border-radius: 5px; margin-bottom: 15px; background-color: white;",
      HTML(content)
    )
  )
}

#' Create a responsive two-column layout
#'
#' @param left_content Content for the left column
#' @param right_content Content for the right column
#' @param left_width Width of the left column (1-11)
#'
#' @return A fluidRow with responsive columns
createTwoColumnLayout <- function(left_content, right_content, left_width = 4) {
  right_width <- 12 - left_width
  fluidRow(
    column(width = left_width, left_content),
    column(width = right_width, right_content)
  )
}

#' Create a tabbed results panel
#'
#' @param id Base ID for the tabs
#' @param tabs List of tab specifications
#' @param ns Namespace function
#'
#' @return A tabsetPanel with the specified tabs
createResultsTabs <- function(id, tabs, ns) {
  # Example tabs structure:
  # tabs = list(
  #   list(id = "tab1", title = "Tab 1", content = plotOutput(ns("plot1"))),
  #   list(id = "tab2", title = "Tab 2", content = tableOutput(ns("table1")))
  # )
  
  tabsetPanel(
    id = ns(paste0(id, "_tabs")),
    lapply(tabs, function(tab) {
      tabPanel(
        title = tab$title,
        tab$content
      )
    })
  )
}

#' Create a navigation button bar
#'
#' @param prev_label Label for previous button (NULL to hide)
#' @param next_label Label for next button (NULL to hide)
#' @param prev_id ID for previous button
#' @param next_id ID for next button
#' @param ns Namespace function
#'
#' @return A div containing navigation buttons
createNavButtons <- function(prev_label = "Previous", next_label = "Next", 
                            prev_id = "prev_button", next_id = "next_button", ns) {
  div(
    style = "display: flex; justify-content: space-between; margin-top: 20px;",
    if (!is.null(prev_label)) {
      actionButton(
        ns(prev_id),
        prev_label,
        icon = icon("arrow-left"),
        class = "btn-default"
      )
    } else {
      div() # Empty div as placeholder
    },
    if (!is.null(next_label)) {
      actionButton(
        ns(next_id),
        next_label,
        icon = icon("arrow-right"),
        class = "btn-success"
      )
    } else {
      div() # Empty div as placeholder
    }
  )
}