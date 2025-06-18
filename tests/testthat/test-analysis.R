# Load the testthat library
library(testthat)
library(devtools)
# document()
# build()
# install()
library(gwArsenicR)

# Source the helper function to create dummy data
source("helper-create_dummy_data.R")

# Define the test suite for the main analysis function
context("Test perform_sensitivity_analysis")

test_that("perform_sensitivity_analysis runs and produces correctly structured output", {

  # --- 1. Setup: Create a temporary directory and dummy data ---
  
  # Create a temporary directory for test artifacts
  temp_dir <- tempfile(pattern = "test_analysis_")
  dir.create(temp_dir)
  
  # Generate dummy data files within the temporary directory
  dummy_files <- create_dummy_data(file_dir = temp_dir)

  # --- 2. Define Parameters for the Analysis ---
  
  ndraws <- 2 # Use a small number of draws for fast testing
  output_dir <- file.path(temp_dir, "analysis_results")
  targets <- c("BWT", "OEGEST")
  as_level_col <- "AsLevel"
  
  # Define a simple regression formula for testing purposes
  regression_formula <- paste0(
    "~ as.factor(", as_level_col, ") + ",
    "MAGE_R + smoke + (1 | MRSTATE)"
  )

  # --- 3. Execution: Run the main analysis function ---
  
  # Use expect_no_error to ensure the function completes without failing
  results <- expect_no_error(
    perform_sensitivity_analysis(
      ndraws = ndraws,
      as_usgs_prob_csv = dummy_files$usgs,
      as_epa_prob_csv = dummy_files$epa,
      birth_data_txt = dummy_files$births,
      regression_formula = regression_formula,
      output_dir = output_dir,
      targets = targets,
      epa_lognormal_sdlog = 1.2, # Test the user-specified sdlog
    )
  )

  # --- 4. Validation: Check the structure and content of the results ---

  # Print the results to the console for visual inspection during testing
  cat("\n--- Pooled Analysis Results ---\n")
  print(results)
  cat("-----------------------------\n")

  # Check if the main result is a list
  expect_type(results, "list")

  # Check if the list is named after the targets
  expect_named(results, targets, ignore.order = TRUE)

  # Check the structure of the results for the all targets
  for (target in targets) {
    print(paste("Checking results for target:", target))
    expect_s3_class(results[[target]], "data.frame")
    
    # Check if the expected columns are present
    expected_cols <- c("q.mi", "se.mi", "statistic", "conf.low", "conf.high", "p.value")
    expect_true(all(expected_cols %in% names(results[[target]])))
    
    # Check that the number of rows corresponds to the number of estimated effects
    # (As5-10 and As10+, since As<5 is the reference)
    expect_equal(nrow(results[[target]]), 2)
    # Check that the output CSV file was created
    expected_csv_path <- file.path(output_dir, paste0(target, "_pooled_results.csv"))
    expect_true(file.exists(expected_csv_path))

     # Check for any NA, NaN, or Inf values in the key numeric columns.
    # This helps detect potential issues with model fitting or pooling.
    expect_false(any(is.na(results[[target]]$q.mi)), "Pooled estimates (q.mi) should not be NA")
    expect_false(any(is.na(results[[target]]$se.mi)), "Pooled standard errors (se.mi) should not be NA")
    expect_false(any(is.na(results[[target]]$p.value)), "P-values should not be NA")

    expect_true(all(is.finite(results[[target]]$q.mi)), "Pooled estimates (q.mi) must be finite")
    expect_true(all(is.finite(results[[target]]$se.mi)), "Pooled standard errors (se.mi) must be finite")
  }
 
  # --- 5. Teardown: Clean up the temporary directory ---
  
  unlink(temp_dir, recursive = TRUE)
})

