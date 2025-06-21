# Load the testthat library
library(testthat)
library(gwArsenicR)

# Source the helper function to create dummy data
source("helper-create_dummy_data.R")

# Define the test suite for the main analysis function
context("Test perform_sensitivity_analysis with covariate imputation")

test_that("Function handles missing covariates and produces valid output", {

  # --- 1. Setup: Create data with missing values ---
  temp_dir <- tempfile(pattern = "test_mice_")
  dir.create(temp_dir)
  # Generate dummy data files
  dummy_files <- create_dummy_data(file_dir = temp_dir, introduce_nas = TRUE)
  # Load the birth data
  birth_data <- data.table::fread(
    dummy_files$births, header = TRUE,
    nThread = parallel::detectCores()
  )
  # Verify that NAs were introduced
  expect_true(any(is.na(birth_data$MAGE_R)))
  expect_true(any(is.na(birth_data$smoke)))

  # --- 2. Define Parameters for the Analysis ---
  ndraws <- 5
  targets <- c("BWT", "OEGEST")
  # Variables to impute (in addition to AsLevel)
  impute_vars <- c("MAGE_R", "smoke")
  as_level_col <- "AsLevel"
  # These should not be part of the regression formula
  # but are used for imputation
  mice_covs <- c(
    "RUCC", "pm",
    "DMAR_1", "MEDUC_2",
    "MEDUC_3", "MEDUC_4"
  )
  # check if mice_covs are in the birth_data
  if (!all(mice_covs %in% names(birth_data))) {
    stop("Some mice_covs are not present in the birth_data.")
  }
  # Define a simple regression formula for testing purposes
  regression_formula <- paste0(
    "~ as.factor(", as_level_col, ") + ",
    "MAGE_R + smoke + (1 | MRSTATE)"
  )
  output_dir <- "test_output"
  # Ensure the output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # --- 3. Execution: Run the analysis ---
  results <- expect_no_error(
    perform_sensitivity_analysis(
      birth_data_txt = dummy_files$births,
      as_usgs_prob_csv = dummy_files$usgs,
      as_epa_prob_csv = dummy_files$epa,
      ndraws = ndraws,
      regression_formula = regression_formula,
      impute_vars = impute_vars,
      targets = targets,
      output_dir = output_dir,
      mice_covs = mice_covs,
      as_level_col = as_level_col
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
    expected_cols <- c(
      "q.mi", "se.mi", "statistic", "conf.low", "conf.high", "p.value"
    )
    expect_true(all(expected_cols %in% names(results[[target]])))

    # Check that the number of rows corresponds to the number of
    # estimated effects
    # (As5-10 and As10+, since As<5 is the reference)
    expect_equal(nrow(results[[target]]), 2)
    # Check that the output CSV file was created
    expected_csv_path <- file.path(
      output_dir, paste0(target, "_pooled_results.csv")
    )
    expect_true(file.exists(expected_csv_path))

    # Check for any NA, NaN, or Inf values in the key numeric columns.
    # This helps detect potential issues with model fitting or pooling.
    expect_false(
      any(is.na(results[[target]]$q.mi)),
      "Pooled estimates (q.mi) should not be NA"
    )
    expect_false(
      any(is.na(results[[target]]$se.mi)),
      "Pooled standard errors (se.mi) should not be NA"
    )
    expect_false(
      any(is.na(results[[target]]$p.value)),
      "P-values should not be NA"
    )

    expect_true(
      all(is.finite(results[[target]]$q.mi)),
      "Pooled estimates (q.mi) must be finite"
    )
    expect_true(
      all(is.finite(results[[target]]$se.mi)),
      "Pooled standard errors (se.mi) must be finite"
    )
  }

  # --- 5. Teardown: Clean up the temporary directory ---
  unlink(temp_dir, recursive = TRUE)
})
