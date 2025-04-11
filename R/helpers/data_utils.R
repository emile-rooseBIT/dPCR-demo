#' data_utils.R
#' Utility functions for dPCR data processing, validation, and manipulation
#' These functions support the integrated dPCR analysis application

# Load required packages
library(dplyr)
library(tidyr)
library(stats)

#' Validate uploaded dPCR data format
#' @param data A data frame containing the uploaded dPCR data
#' @return A list with $valid logical value and $message describing any issues
validateDataFormat <- function(data) {
  # Initialize return values
  result <- list(valid = FALSE, message = "")
  
  # Check if data is a data frame
  if (!is.data.frame(data)) {
    result$message <- "Uploaded file is not a valid data frame"
    return(result)
  }
  
  # Check for minimum required columns (adjust based on your specific requirements)
  required_cols <- c("well", "droplet_id", "fluorescence_ch1", "fluorescence_ch2")
  missing_cols <- required_cols[!required_cols %in% colnames(data)]
  
  if (length(missing_cols) > 0) {
    result$message <- paste("Missing required columns:", paste(missing_cols, collapse = ", "))
    return(result)
  }
  
  # Check if there's enough data points
  if (nrow(data) < 10) {
    result$message <- "Dataset too small, need at least 10 data points"
    return(result)
  }
  
  # Validate data types
  if (!is.numeric(data$fluorescence_ch1) || !is.numeric(data$fluorescence_ch2)) {
    result$message <- "Fluorescence values must be numeric"
    return(result)
  }
  
  # Check for extreme values
  if (max(data$fluorescence_ch1, na.rm = TRUE) > 100000 || 
      max(data$fluorescence_ch2, na.rm = TRUE) > 100000) {
    result$message <- "Warning: Dataset contains extremely high fluorescence values"
    # Still valid, but with warning
  }
  
  # Data passed all checks
  result$valid <- TRUE
  
  # If there was a warning, keep the message, otherwise indicate success
  if (result$message == "") {
    result$message <- "Data format validated successfully"
  }
  
  return(result)
}

#' Preprocess dPCR data
#' @param data A data frame containing the raw dPCR data
#' @param normalize Logical, whether to normalize fluorescence values
#' @param remove_outliers Logical, whether to remove outliers
#' @param trim_percentile Numeric, percentile of data to trim (0-1)
#' @return Preprocessed data frame
preprocessDPCRData <- function(data, normalize = TRUE, remove_outliers = TRUE, trim_percentile = 0.1) {
  # Make a copy of the data to avoid modifying the original
  processed_data <- data
  
  # Normalize fluorescence values (scale to 0-1 range per channel)
  if (normalize) {
    processed_data <- processed_data %>%
      group_by(well) %>%
      mutate(
        fluorescence_ch1_norm = (fluorescence_ch1 - min(fluorescence_ch1, na.rm = TRUE)) / 
                               (max(fluorescence_ch1, na.rm = TRUE) - min(fluorescence_ch1, na.rm = TRUE)),
        fluorescence_ch2_norm = (fluorescence_ch2 - min(fluorescence_ch2, na.rm = TRUE)) / 
                               (max(fluorescence_ch2, na.rm = TRUE) - min(fluorescence_ch2, na.rm = TRUE))
      ) %>%
      ungroup()
  }
  
  # Remove outliers (using IQR method)
  if (remove_outliers) {
    processed_data <- processed_data %>%
      group_by(well) %>%
      mutate(
        # Calculate IQR for each channel
        q1_ch1 = quantile(fluorescence_ch1, 0.25, na.rm = TRUE),
        q3_ch1 = quantile(fluorescence_ch1, 0.75, na.rm = TRUE),
        iqr_ch1 = q3_ch1 - q1_ch1,
        lower_bound_ch1 = q1_ch1 - 1.5 * iqr_ch1,
        upper_bound_ch1 = q3_ch1 + 1.5 * iqr_ch1,
        
        q1_ch2 = quantile(fluorescence_ch2, 0.25, na.rm = TRUE),
        q3_ch2 = quantile(fluorescence_ch2, 0.75, na.rm = TRUE),
        iqr_ch2 = q3_ch2 - q1_ch2,
        lower_bound_ch2 = q1_ch2 - 1.5 * iqr_ch2,
        upper_bound_ch2 = q3_ch2 + 1.5 * iqr_ch2,
        
        # Flag outliers
        is_outlier = fluorescence_ch1 < lower_bound_ch1 | 
                    fluorescence_ch1 > upper_bound_ch1 |
                    fluorescence_ch2 < lower_bound_ch2 | 
                    fluorescence_ch2 > upper_bound_ch2
      ) %>%
      filter(!is_outlier) %>%
      select(-q1_ch1, -q3_ch1, -iqr_ch1, -lower_bound_ch1, -upper_bound_ch1,
             -q1_ch2, -q3_ch2, -iqr_ch2, -lower_bound_ch2, -upper_bound_ch2, -is_outlier) %>%
      ungroup()
  }
  
  # Trim extreme percentiles to stabilize range estimates
  if (trim_percentile > 0 && trim_percentile < 0.5) {
    processed_data <- processed_data %>%
      group_by(well) %>%
      mutate(
        # Calculate percentiles for trimming
        lower_pct_ch1 = quantile(fluorescence_ch1, trim_percentile, na.rm = TRUE),
        upper_pct_ch1 = quantile(fluorescence_ch1, 1 - trim_percentile, na.rm = TRUE),
        lower_pct_ch2 = quantile(fluorescence_ch2, trim_percentile, na.rm = TRUE),
        upper_pct_ch2 = quantile(fluorescence_ch2, 1 - trim_percentile, na.rm = TRUE),
        
        # Flag data points to keep
        keep_datapoint = fluorescence_ch1 >= lower_pct_ch1 & 
                        fluorescence_ch1 <= upper_pct_ch1 &
                        fluorescence_ch2 >= lower_pct_ch2 & 
                        fluorescence_ch2 <= upper_pct_ch2
      ) %>%
      filter(keep_datapoint) %>%
      select(-lower_pct_ch1, -upper_pct_ch1, -lower_pct_ch2, -upper_pct_ch2, -keep_datapoint) %>%
      ungroup()
  }
  
  return(processed_data)
}

#' Calculate basic QC metrics for dPCR data
#' @param data A data frame containing the preprocessed dPCR data
#' @param threshold_ch1 Numeric, threshold for positive/negative in channel 1
#' @param threshold_ch2 Numeric, threshold for positive/negative in channel 2
#' @return A data frame with QC metrics
calculateQCMetrics <- function(data, threshold_ch1 = NULL, threshold_ch2 = NULL) {
  # If thresholds are not provided, calculate them using k-means clustering
  if (is.null(threshold_ch1)) {
    # Simple k-means with 2 clusters to separate positive and negative droplets
    kmeans_result <- kmeans(data$fluorescence_ch1, centers = 2)
    centers <- sort(kmeans_result$centers)
    threshold_ch1 <- mean(centers)
  }
  
  if (is.null(threshold_ch2)) {
    kmeans_result <- kmeans(data$fluorescence_ch2, centers = 2)
    centers <- sort(kmeans_result$centers)
    threshold_ch2 <- mean(centers)
  }
  
  # Calculate QC metrics per well
  qc_metrics <- data %>%
    group_by(well) %>%
    summarize(
      total_droplets = n(),
      
      # Channel 1 metrics
      ch1_positive = sum(fluorescence_ch1 > threshold_ch1, na.rm = TRUE),
      ch1_negative = sum(fluorescence_ch1 <= threshold_ch1, na.rm = TRUE),
      ch1_percent_positive = ch1_positive / total_droplets * 100,
      
      # Channel 2 metrics
      ch2_positive = sum(fluorescence_ch2 > threshold_ch2, na.rm = TRUE),
      ch2_negative = sum(fluorescence_ch2 <= threshold_ch2, na.rm = TRUE),
      ch2_percent_positive = ch2_positive / total_droplets * 100,
      
      # Four quadrant counts for dual channel analysis
      q1_count = sum(fluorescence_ch1 <= threshold_ch1 & fluorescence_ch2 <= threshold_ch2, na.rm = TRUE),
      q2_count = sum(fluorescence_ch1 > threshold_ch1 & fluorescence_ch2 <= threshold_ch2, na.rm = TRUE),
      q3_count = sum(fluorescence_ch1 <= threshold_ch1 & fluorescence_ch2 > threshold_ch2, na.rm = TRUE),
      q4_count = sum(fluorescence_ch1 > threshold_ch1 & fluorescence_ch2 > threshold_ch2, na.rm = TRUE),
      
      # Flag wells with low droplet counts
      low_droplet_flag = total_droplets < 10000,
      
      # Calculate Poisson statistics for concentration estimates
      lambda_ch1 = -log(1 - (ch1_positive / total_droplets)),
      lambda_ch2 = -log(1 - (ch2_positive / total_droplets)),
      
      # Calculate copies per μL (example - adjust based on your specific volume)
      copies_per_ul_ch1 = lambda_ch1 * 20000 / 20, # Assuming 20,000 droplets per 20 μL
      copies_per_ul_ch2 = lambda_ch2 * 20000 / 20
    )
  
  return(qc_metrics)
}

#' Determine optimal fluorescence threshold
#' @param data A data frame containing the dPCR data
#' @param channel Column name of the channel to analyze
#' @param method Method for threshold determination: "kmeans", "otsu", or "manual"
#' @param manual_threshold Numeric, manual threshold value (used when method="manual")
#' @return Optimal threshold value
determineThreshold <- function(data, channel = "fluorescence_ch1", 
                              method = "kmeans", manual_threshold = NULL) {
  # Extract the fluorescence values
  values <- data[[channel]]
  values <- values[!is.na(values)]
  
  if (method == "manual" && !is.null(manual_threshold)) {
    return(manual_threshold)
  }
  
  else if (method == "kmeans") {
    # K-means clustering to find positive and negative populations
    kmeans_result <- kmeans(values, centers = 2)
    centers <- sort(kmeans_result$centers)
    threshold <- mean(centers)
    return(threshold)
  }
  
  else if (method == "otsu") {
    # Otsu's method for thresholding
    # First, create a histogram of the data
    hist_data <- hist(values, breaks = 100, plot = FALSE)
    
    # Get counts and mids (bin centers)
    counts <- hist_data$counts
    mids <- hist_data$mids
    
    # Find threshold that maximizes between-class variance
    max_variance <- 0
    best_threshold <- mids[1]
    
    for (i in 1:(length(mids)-1)) {
      # Calculate probabilities and means for the two classes
      w1 <- sum(counts[1:i]) / sum(counts)
      w2 <- 1 - w1
      
      if (w1 == 0 || w2 == 0) next
      
      # Calculate means for the two classes
      m1 <- sum(mids[1:i] * counts[1:i]) / sum(counts[1:i])
      m2 <- sum(mids[(i+1):length(mids)] * counts[(i+1):length(mids)]) / 
             sum(counts[(i+1):length(mids)])
      
      # Calculate between-class variance
      variance <- w1 * w2 * (m1 - m2)^2
      
      # Update if we found a better threshold
      if (variance > max_variance) {
        max_variance <- variance
        best_threshold <- mids[i]
      }
    }
    
    return(best_threshold)
  }
  
  else {
    # Default to median if no valid method
    warning("Invalid threshold method specified. Using median as threshold.")
    return(median(values))
  }
}

#' Calculate copy number variation (CNV)
#' @param data A data frame containing the dPCR QC metrics
#' @param target_column Column name for the target concentration
#' @param reference_column Column name for the reference concentration
#' @return CNV ratio
calculateCNV <- function(data, target_column = "copies_per_ul_ch1", 
                        reference_column = "copies_per_ul_ch2") {
  # Calculate CNV ratios
  data$cnv_ratio <- data[[target_column]] / data[[reference_column]]
  
  return(data)
}

#' Export dPCR results to various formats
#' @param data A data frame containing the dPCR results
#' @param filename Base filename without extension
#' @param formats Vector of export formats ("csv", "xlsx", "rds")
#' @return Logical indicating success
exportResults <- function(data, filename = "dpcr_results", formats = c("csv")) {
  success <- TRUE
  
  for (format in formats) {
    tryCatch({
      if (format == "csv") {
        write.csv(data, paste0(filename, ".csv"), row.names = FALSE)
      } 
      else if (format == "xlsx") {
        if (!requireNamespace("writexl", quietly = TRUE)) {
          warning("Package 'writexl' is required to export to Excel format.")
          next
        }
        writexl::write_xlsx(data, paste0(filename, ".xlsx"))
      } 
      else if (format == "rds") {
        saveRDS(data, paste0(filename, ".rds"))
      }
    }, error = function(e) {
      warning(paste("Failed to export data in", format, "format:", e$message))
      success <- FALSE
    })
  }
  
  return(success)
}

#' Generate sample data for dPCR
#' @param n_wells Number of wells to simulate
#' @param n_droplets Number of droplets per well
#' @param mut_freq Mutation frequency for simulation
#' @return Simulated dPCR data frame
generateSampleData <- function(n_wells = 8, n_droplets = 15000, mut_freq = 0.01) {
  # Initialize empty data frame
  sample_data <- data.frame()
  
  for (well_id in 1:n_wells) {
    # Generate well name (A1, A2, etc.)
    row_letter <- LETTERS[ceiling(well_id/12)]
    col_number <- well_id %% 12
    if (col_number == 0) col_number <- 12
    well_name <- paste0(row_letter, col_number)
    
    # Generate random droplet data
    # Channel 1: Target gene (with some positive signals)
    # Channel 2: Reference gene (more positive signals)
    
    # Create random positive/negative assignments based on probabilities
    ch1_positive_prob <- runif(1, 0.1, 0.3)  # 10-30% positive for ch1
    ch2_positive_prob <- runif(1, 0.5, 0.7)  # 50-70% positive for ch2
    
    ch1_is_positive <- rbinom(n_droplets, 1, ch1_positive_prob)
    ch2_is_positive <- rbinom(n_droplets, 1, ch2_positive_prob)
    
    # Generate fluorescence values
    # Negative droplets: low fluorescence with some noise
    # Positive droplets: high fluorescence with some noise
    
    fluorescence_ch1 <- numeric(n_droplets)
    fluorescence_ch2 <- numeric(n_droplets)
    
    # Set baseline and positive signal levels with some variation between wells
    baseline_ch1 <- runif(1, 1000, 1500)
    baseline_ch2 <- runif(1, 1000, 1500)
    positive_ch1 <- runif(1, 8000, 12000)
    positive_ch2 <- runif(1, 8000, 12000)
    
    # Generate fluorescence values with noise
    for (i in 1:n_droplets) {
      if (ch1_is_positive[i] == 1) {
        fluorescence_ch1[i] <- rnorm(1, positive_ch1, positive_ch1 * 0.1)
      } else {
        fluorescence_ch1[i] <- rnorm(1, baseline_ch1, baseline_ch1 * 0.1)
      }
      
      if (ch2_is_positive[i] == 1) {
        fluorescence_ch2[i] <- rnorm(1, positive_ch2, positive_ch2 * 0.1)
      } else {
        fluorescence_ch2[i] <- rnorm(1, baseline_ch2, baseline_ch2 * 0.1)
      }
    }
    
    # Add some "rain" (intermediate values) between positive and negative clusters
    rain_count <- round(n_droplets * 0.05)  # 5% of droplets are "rain"
    rain_indices <- sample(1:n_droplets, rain_count)
    
    for (idx in rain_indices) {
      # For "rain" droplets, set values between baseline and positive
      if (ch1_is_positive[idx] == 1) {
        fluorescence_ch1[idx] <- runif(1, baseline_ch1 * 1.5, positive_ch1 * 0.8)
      }
      if (ch2_is_positive[idx] == 1) {
        fluorescence_ch2[idx] <- runif(1, baseline_ch2 * 1.5, positive_ch2 * 0.8)
      }
    }
    
    # Create well data
    well_data <- data.frame(
      well = rep(well_name, n_droplets),
      droplet_id = 1:n_droplets,
      fluorescence_ch1 = fluorescence_ch1,
      fluorescence_ch2 = fluorescence_ch2,
      ch1_status = ch1_is_positive,
      ch2_status = ch2_is_positive
    )
    
    # Add metadata for the well
    well_data$sample_id <- paste0("Sample", ceiling(well_id/2))
    well_data$sample_type <- ifelse(well_id %% 2 == 1, "Test", "Control")
    
    # Combine with the full dataset
    sample_data <- rbind(sample_data, well_data)
  }
  
  return(sample_data)
}