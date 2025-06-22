#' Format Geographic Identifiers
#'
#' @param data_frame Data frame to format
#' @param geod_col Column name for geographic identifier
#' @return Data frame with formatted geographic IDs
#'
#' @keywords internal
format_geographic_ids <- function(data_frame, geod_col) {
  geod_numeric <- as.numeric(data_frame[[geod_col]])
  data_frame[[geod_col]] <- sprintf("%05d", geod_numeric)
  return(data_frame)
}

#' Validate Input Parameters for Sensitivity Analysis
#'
#' @description
#' Internal function to validate all input parameters for the sensitivity
#' analysis.
#' This function checks for required parameters, validates data types,
#' ranges, and ensures file paths exist.
#'
#' @param birth_data_txt A file path to the text file containing the birth data.
#' @param as_usgs_prob_csv A file path to the CSV containing USGS model-based
#'   multinomial probabilities of arsenic levels for private wells.
#' @param as_epa_prob_csv A file path to the CSV containing EPA model-based
#'   lognormal parameters for public wells.
#' @param ndraws An integer specifying the number of imputed datasets to
#'   generate.
#' @param regression_formula A string or formula object for the regression
#'   model.
#' @param targets A character vector of the dependent variables.
#' @param impute_vars A character vector of column names to be imputed
#'   using MICE.
#' @param output_dir A file path to the directory where output files will
#'   be saved.
#' @param mice_m An integer for the number of multiple imputations.
#' @param mice_maxit An integer for the maximum number of iterations for mice.
#' @param mice_method A character string specifying the imputation method.
#' @param epa_lognormal_sdlog A numeric value for the standard deviation.
#' @param seed An integer for setting the random number generator seed.
#' @param as_cat_label A character vector of labels for arsenic categories.
#' @param drop_as_cat_label_reg A character vector of reference categories.
#' @param as_level_col A character string for the name of the imputed arsenic
#'   level column.
#'
#' @return NULL (throws error if validation fails)
#' @keywords internal
validate_inputs <- function(
  birth_data_txt = NULL,
  as_usgs_prob_csv = NULL,
  as_epa_prob_csv = NULL,
  ndraws = NULL,
  regression_formula = NULL,
  targets = NULL,
  impute_vars = NULL,
  output_dir = NULL,
  mice_m = NULL,
  mice_maxit = NULL,
  mice_method = NULL,
  epa_lognormal_sdlog = NULL,
  seed = NULL,
  as_cat_label = NULL,
  drop_as_cat_label_reg = NULL,
  as_level_col = "AsLevel"
) {

  # --- Validate Required Parameters ---
  if (is.null(birth_data_txt)) {
    stop("birth_data_txt is required and cannot be NULL")
  }

  if (is.null(as_usgs_prob_csv)) {
    stop("as_usgs_prob_csv is required and cannot be NULL")
  }

  if (is.null(as_epa_prob_csv)) {
    stop("as_epa_prob_csv is required and cannot be NULL")
  }

  # --- Validate File Paths ---
  if (!is.character(birth_data_txt) || length(birth_data_txt) != 1) {
    stop("birth_data_txt must be a single character string")
  }

  if (!is.character(as_usgs_prob_csv) || length(as_usgs_prob_csv) != 1) {
    stop("as_usgs_prob_csv must be a single character string")
  }

  if (!is.character(as_epa_prob_csv) || length(as_epa_prob_csv) != 1) {
    stop("as_epa_prob_csv must be a single character string")
  }

  # Check if files exist
  if (!file.exists(birth_data_txt)) {
    stop("File does not exist: ", birth_data_txt)
  }

  if (!file.exists(as_usgs_prob_csv)) {
    stop("File does not exist: ", as_usgs_prob_csv)
  }

  if (!file.exists(as_epa_prob_csv)) {
    stop("File does not exist: ", as_epa_prob_csv)
  }

  # --- Validate ndraws ---
  if (!is.null(ndraws)) {
    if (!is.numeric(ndraws) || length(ndraws) != 1) {
      stop("ndraws must be a single numeric value")
    }

    if (ndraws <= 0 || ndraws != as.integer(ndraws)) {
      stop("ndraws must be a positive integer")
    }

    if (ndraws > 10000) {
      warning(
        "ndraws is very large (", ndraws, 
        "). This may take a long time to compute."
      )
    }
  }

  # --- Validate regression_formula ---
  if (!is.null(regression_formula)) {
    if (
      !is.character(regression_formula) &&
        !inherits(regression_formula, "formula")
    ) {
      stop("regression_formula must be a character string or formula object")
    }

    if (is.character(regression_formula)) {
      if (length(regression_formula) != 1) {
        stop("regression_formula must be a single character string")
      }

      # Check if the string is empty or contains only whitespace
      if (nchar(trimws(regression_formula)) == 0) {
        stop("regression_formula cannot be empty")
      }

      # Try to convert to formula to check validity
      tryCatch(
        as.formula(regression_formula),
        error = function(e) {
          stop("Invalid regression_formula: ", e$message)
        }
      )

      if (
        !grepl(
          paste0(
            "as\\.factor\\(", as_level_col,
            "\\)|factor\\(", as_level_col, "\\)"
          ),
          regression_formula
        )
      ) {
        stop(
          "regression_formula must include '", as_level_col,
          "' as a factor variable "
        )
      }
    }
  }

  # --- Validate targets ---
  if (!is.null(targets)) {
    if (!is.character(targets)) {
      stop("targets must be a character vector")
    }

    if (length(targets) == 0) {
      stop("targets cannot be empty")
    }

    if (any(is.na(targets)) || any(nchar(trimws(targets)) == 0)) {
      stop("targets cannot contain NA or empty values")
    }

    if (length(unique(targets)) != length(targets)) {
      stop("targets cannot contain duplicate values")
    }
  }

  # --- Validate impute_vars ---
  if (!is.null(impute_vars)) {
    if (!is.character(impute_vars)) {
      stop("impute_vars must be a character vector or NULL")
    }

    if (length(impute_vars) == 0) {
      stop("impute_vars cannot be empty (use NULL instead)")
    }

    if (any(is.na(impute_vars)) || any(nchar(trimws(impute_vars)) == 0)) {
      stop("impute_vars cannot contain NA or empty values")
    }
  }

  # --- Validate output_dir ---
  if (!is.null(output_dir)) {
    if (!is.character(output_dir) || length(output_dir) != 1) {
      stop("output_dir must be a single character string")
    }

    if (nchar(trimws(output_dir)) == 0) {
      stop("output_dir cannot be empty")
    }

    # Check if parent directory exists (if output_dir doesn't exist yet)
    if (!dir.exists(output_dir)) {
      parent_dir <- dirname(output_dir)
      if (!dir.exists(parent_dir)) {
        stop("Parent directory does not exist: ", parent_dir)
      }
    }
  }

  # --- Validate MICE parameters ---
  if (!is.null(mice_m)) {
    if (!is.numeric(mice_m) || length(mice_m) != 1) {
      stop("mice_m must be a single numeric value")
    }

    if (mice_m <= 0 || mice_m != as.integer(mice_m)) {
      stop("mice_m must be a positive integer")
    }

    if (mice_m > 100) {
      warning(
        "mice_m is very large (", mice_m, 
        "). This may take a long time to compute."
      )
    }
  }

  if (!is.null(mice_maxit)) {
    if (!is.numeric(mice_maxit) || length(mice_maxit) != 1) {
      stop("mice_maxit must be a single numeric value")
    }

    if (mice_maxit <= 0 || mice_maxit != as.integer(mice_maxit)) {
      stop("mice_maxit must be a positive integer")
    }
  }

  if (!is.null(mice_method)) {
    if (!is.character(mice_method) || length(mice_method) != 1) {
      stop("mice_method must be a single character string")
    }

    valid_methods <- c(
      "pmm", "logreg", "polyreg", "polr", "mean", "norm", "norm.nob",
      "norm.boot", "norm.predict"
    )
    if (!mice_method %in% valid_methods) {
      stop(
        "mice_method must be one of: ",
        paste(valid_methods, collapse = ", ")
      )
    }
  }

  # --- Validate numeric parameters ---
  if (!is.null(epa_lognormal_sdlog)) {
    if (!is.numeric(epa_lognormal_sdlog) || length(epa_lognormal_sdlog) != 1) {
      stop("epa_lognormal_sdlog must be a single numeric value")
    }
    if (epa_lognormal_sdlog <= 0) {
      stop("epa_lognormal_sdlog must be positive")
    }

    if (epa_lognormal_sdlog > 10) {
      warning(
        "epa_lognormal_sdlog is very large (", 
        epa_lognormal_sdlog, 
        "). Check if this is correct."
      )
    }
  }

  if (!is.null(seed)) {
    if (!is.numeric(seed) || length(seed) != 1) {
      stop("seed must be a single numeric value")
    }

    if (seed != as.integer(seed)) {
      stop("seed must be an integer")
    }
  }
  # --- Validate arsenic category labels ---
  if (!is.null(as_cat_label)) {
    if (!is.character(as_cat_label)) {
      stop("as_cat_label must be a character vector")
    }

    if (length(as_cat_label) == 0) {
      stop("as_cat_label cannot be empty")
    }

    if (any(is.na(as_cat_label)) || any(nchar(trimws(as_cat_label)) == 0)) {
      stop("as_cat_label cannot contain NA or empty values")
    }

    if (length(unique(as_cat_label)) != length(as_cat_label)) {
      stop("as_cat_label cannot contain duplicate values")
    }
  }
  if (!is.null(drop_as_cat_label_reg)) {
    if (!is.character(drop_as_cat_label_reg)) {
      stop("drop_as_cat_label_reg must be a character vector")
    }
    # Check that drop_as_cat_label_reg values are in as_cat_label
    if (!is.null(as_cat_label)) {
      if (!all(drop_as_cat_label_reg %in% as_cat_label)) {
        stop(paste0(
          "All values in drop_as_cat_label_reg must be present in as_cat_label"
        ))
      }

      # Check that we don't drop all categories
      remaining_cats <- setdiff(as_cat_label, drop_as_cat_label_reg)
      if (length(remaining_cats) == 0) {
        stop("Cannot drop all arsenic categories as reference levels")
      }
    }
  }

  # If we get here, all validations passed
  invisible(NULL)
}
