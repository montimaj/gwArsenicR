#' Perform Sensitivity Analysis for Arsenic Exposure
#'
#' @description
#' This function performs a sensitivity analysis to assess the impact of arsenic
#' exposure from private wells on specified health outcomes. It integrates data
#' from USGS (private wells) and EPA (public wells) models to create a
#' probabilistic model of arsenic exposure, weighted by the proportion of the
#' population using each water source. It then uses multiple imputation to
#' generate a series of datasets with imputed arsenic levels.
#'
#' @param ndraws An integer specifying the number of imputed datasets to
#'   generate.
#' @param as_usgs_prob_csv A file path to the CSV containing USGS model-based
#'   multinomial probabilities of arsenic levels for private wells.
#' @param as_epa_prob_csv A file path to the CSV containing EPA model-based
#'   lognormal parameters for public wells and the percentage of private well
#'   users.
#' @param birth_data_txt A file path to the text file containing the birth data.
#' @param regression_formula A string or formula object for the regression
#'   model.
#' @param output_dir A file path to the directory where output files will be
#'   saved.
#' @param targets A character vector of the dependent variables (health
#'   outcomes) for the regression analysis. Defaults to c("OEGEST", "BWT").
#' @param impute_vars A character vector of column names to be imputed using
#'   MICE. If NULL, no additional imputation is performed. Defaults to NULL.
#' @param as_cat_label A character vector of labels for the arsenic
#'   concentration categories. Defaults to c("As<5", "As5-10", "As10+").
#' @param drop_as_cat_label_reg A character vector of arsenic categories to be
#'   used as the reference level in the regression. Defaults to c("As<5").
#' @param columns_to_select A character vector of column names to be selected
#'   from the birth data. If NULL, all columns are used.
#' @param rucc_col A character string specifying the column name for the
#'   Rural-Urban Continuum Code. Defaults to "RUCC".
#' @param geod_col A character string specifying the column name for the
#'   Geographic Identifier (e.g., county FIPS code) in the arsenic data.
#'   Defaults to "GEOID10".
#' @param as_conc_cols A character vector of column names from the USGS data
#'   for the arsenic concentration probabilities. Defaults to 
#'   c("RFC3_C1v2", "RFC3_C2v2", "RFC3_C3v2").
#' @param pop_well_col A character string specifying the column name from the
#'   USGS data for the population of well users. Defaults to "Wells_2010".
#' @param birth_county_col A character string specifying the column name for
#'   the birth county FIPS code in the birth data. Defaults to "FIPS".
#' @param as_level_col A character string for the name of the imputed arsenic
#'   level column. Defaults to "AsLevel".
#' @param epa_as_col A character string for the column name of the EPA arsenic
#'   lognormal meanlog parameter. Defaults to "EPA_AS_meanlog".
#' @param epa_lognormal_sdlog A numeric value for the standard deviation of the
#'   lognormal distribution for the EPA data. Defaults to 1.0.
#' @param epa_pwell_col A character string for the column name of the
#'   percentage of private well users. Defaults to "PWELL_private_pct".
#' @param seed An integer for setting the random number generator seed for
#'   reproducibility. Defaults to 12345.
#' @param mice_m An integer for the number of multiple imputations for
#'   covariates. Defaults to 1.
#' @param mice_maxit An integer for the maximum number of iterations for mice.
#'   Defaults to 5.
#' @param mice_method A character string specifying the imputation method for
#'   covariates. Defaults to "pmm" (predictive mean matching).
#' @param mice_covs A character vector of column names to be used as
#'   covariates in the mice imputation process. Defaults to c("RUCC", "pm").
#'   Make sure these columns exist in the birth data.
#'
#' @return A list of data frames, each containing the pooled regression results
#'   for a target health outcome.
#'
#' @examples
#' \dontrun{
#' # Basic usage with minimal parameters
#' results <- perform_sensitivity_analysis(
#'   ndraws = 10,
#'   as_usgs_prob_csv = "usgs_arsenic_probs.csv",
#'   as_epa_prob_csv = "epa_arsenic_params.csv",
#'   birth_data_txt = "birth_outcomes.txt",
#'   regression_formula = "~ AsLevel + maternal_age + (1|county)",
#'   output_dir = "results/",
#'   targets = c("birth_weight", "gestational_age")
#' )
#' 
#' # Advanced usage with custom parameters and MICE imputation
#' results <- perform_sensitivity_analysis(
#'   ndraws = 100,
#'   as_usgs_prob_csv = "data/usgs_probs.csv",
#'   as_epa_prob_csv = "data/epa_params.csv",
#'   birth_data_txt = "data/births.txt",
#'   regression_formula = "~ AsLevel + MAGE_R + rural + (1|FIPS)",
#'   output_dir = "sensitivity_results/",
#'   targets = c("OEGEST", "BWT"),
#'   impute_vars = c("MAGE_R", "education"),
#'   mice_m = 5,
#'   mice_maxit = 10,
#'   seed = 42
#' )
#' }
#'
#' @export
perform_sensitivity_analysis <- function(
  ndraws = 10,
  as_usgs_prob_csv = NULL,
  as_epa_prob_csv = NULL,
  birth_data_txt = NULL,
  regression_formula = NULL,
  output_dir = "sensitivity_results/",
  targets = c("OEGEST", "BWT"),
  impute_vars = NULL,
  as_cat_label = c("As<5", "As5-10", "As10+"),
  drop_as_cat_label_reg = c("As<5"),
  columns_to_select = NULL,
  rucc_col = "RUCC",
  geod_col = "GEOID10",
  as_conc_cols = c("RFC3_C1v2", "RFC3_C2v2", "RFC3_C3v2"),
  pop_well_col = "Wells_2010",
  birth_county_col = "FIPS",
  as_level_col = "AsLevel",
  epa_as_col = "EPA_AS_meanlog",
  epa_lognormal_sdlog = 1.0,
  epa_pwell_col = "PWELL_private_pct",
  seed = 12345,
  mice_m = 1,
  mice_maxit = 5,
  mice_method = "pmm",
  mice_covs = c("RUCC", "pm")
) {

  # --- 0. Validate all input parameters ---
  validate_inputs(
    birth_data_txt = birth_data_txt,
    as_usgs_prob_csv = as_usgs_prob_csv,
    as_epa_prob_csv = as_epa_prob_csv,
    ndraws = ndraws,
    regression_formula = regression_formula,
    targets = targets,
    impute_vars = impute_vars,
    output_dir = output_dir,
    mice_m = mice_m,
    mice_maxit = mice_maxit,
    mice_method = mice_method,
    epa_lognormal_sdlog = epa_lognormal_sdlog,
    seed = seed,
    as_cat_label = as_cat_label,
    as_level_col = as_level_col,
    drop_as_cat_label_reg = drop_as_cat_label_reg
  )

  set.seed(seed)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # --- 1. Load and Process Arsenic Data ---
  arsenic_data <- load_and_process_arsenic_data(
    as_usgs_prob_csv = as_usgs_prob_csv,
    as_epa_prob_csv = as_epa_prob_csv,
    as_conc_cols = as_conc_cols,
    pop_well_col = pop_well_col,
    epa_as_col = epa_as_col,
    epa_lognormal_sdlog = epa_lognormal_sdlog,
    epa_pwell_col = epa_pwell_col,
    geod_col = geod_col
  )

  # --- 2. Load and Process Birth Data ---
  births <- load_and_process_birth_data(
    birth_data_txt = birth_data_txt,
    columns_to_select = columns_to_select,
    rucc_col = rucc_col,
    birth_county_col = birth_county_col
  )

  # --- 3. Multiple Imputation ---
  imputed_datasets <- impute_arsenic_exposure(
    datmatx_usgs = arsenic_data$datmatx_usgs,
    rasterprob_combined = arsenic_data$rasterprob_combined,
    cellpop = arsenic_data$cellpop,
    births = births,
    ndraws = ndraws,
    as_cat_label = as_cat_label,
    geod_col = geod_col,
    birth_county_col = birth_county_col,
    as_level_col = as_level_col
  )
  # Impute additional variables if specified
  if (!is.null(impute_vars)) {
    imputed_datasets <- impute_additional_variables(
      imputed_datasets = imputed_datasets,
      impute_vars = impute_vars,
      births = births,
      mice_covs = mice_covs,
      mice_m = mice_m,
      mice_maxit = mice_maxit,
      mice_method = mice_method,
      seed = seed
    )
  }
  # Validate imputed datasets
  validate_imputed_datasets(imputed_datasets, as_level_col, as_cat_label)

  # --- 4. Regression and Pooling ---

  combined_births <- data.table::rbindlist(imputed_datasets, idcol = ".id")
  # Ensure the reference level is set correctly
  filtered_as_cat_label <- base::setdiff(as_cat_label, drop_as_cat_label_reg)
  if (length(filtered_as_cat_label) == 0) {
    stop("No arsenic categories left to analyze after dropping reference.")
  }
  combined_births[[as_level_col]] <- stats::relevel(
    combined_births[[as_level_col]],
    ref = drop_as_cat_label_reg
  )
  res <- regression_analysis(
    births = combined_births,
    targets = targets,
    as_cat_label = filtered_as_cat_label,
    as_level_col = as_level_col,
    regression_formula = regression_formula,
    output_dir = output_dir
  )
  return(res)
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
    if (length(drop_as_cat_label_reg) == 0) {
      stop("drop_as_cat_label_reg cannot be empty")
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