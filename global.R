# Add error handling and logging at the start
options(shiny.error = browser)
options(shiny.fullstacktrace = TRUE)

# Install required packages if not already installed
required_packages <- c(
  "shiny",
  "shinydashboard",
  "DT",
  "ggplot2",
  "dplyr",
  "shinyjs",
  "rmarkdown",
  "writexl",
  "markdown",
  "tidyr"  
)

# More robust package loading with error checking
for (pkg in required_packages) {
  tryCatch({
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg)
    }
    library(pkg, character.only = TRUE)
  }, error = function(e) {
    message(sprintf("Error loading package %s: %s", pkg, e$message))
  })
}

# Source global configuration, UI, and server logic
source("R/helpers/data_utils.R")  # Load helper functions
source("R/helpers/ui_utils.R")    # Load UI utilities
source("R/modules/upload.R")      # Load upload module
source("R/modules/preprocess.R")  # Load preprocessing module
source("R/modules/app_select.R")  # Load app selection module
source("R/modules/threshold.R")   # Load threshold module
source("R/modules/cnv.R")        # Load CNV module
source("R/modules/report.R")     # Load report module

# Define global constants
APP_VERSION <- "1.0.0 (Demo)"
SUPPORTED_FILE_TYPES <- c("csv", "xlsx", "rds")
MAX_UPLOAD_SIZE <- 50 * 1024^2  # 50 MB

# Define available analysis apps
AVAILABLE_APPS <- list(
  threshold = list(
    id = "threshold",
    name = "Threshold Determination",
    description = "Establish optimal fluorescence thresholds to differentiate positive from negative droplets.",
    citation = "Smith et al. (2020)",
    url = "https://example.com/threshold-paper",
    icon = "chart-line"
  ),
  cnv = list(
    id = "cnv",
    name = "CNV Analysis",
    description = "Quantify copy number variation using target vs. reference partition data.",
    citation = "Jones et al. (2019)",
    url = "https://example.com/cnv-analysis",
    icon = "dna"
  )
)