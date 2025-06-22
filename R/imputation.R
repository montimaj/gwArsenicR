#' Impute Arsenic Exposure Levels
#'
#' Internal function to create multiple imputed datasets with arsenic
#' exposure levels based on county-level probability distributions.
#'
#' @param datmatx_usgs Data frame containing USGS data with geographic
#'   identifiers
#' @param rasterprob_combined Matrix of combined probability distributions
#' @param cellpop Vector of cell population weights
#' @param births Data frame containing birth records
#' @param ndraws Number of imputed datasets to create
#' @param as_cat_label Character vector of arsenic category labels
#' @param geod_col Column name for geographic identifier in USGS data
#' @param birth_county_col Column name for county identifier in birth data
#' @param as_level_col Column name for arsenic level variable
#' @return List of imputed datasets
#'
#' @keywords internal
impute_arsenic_exposure <- function(
  datmatx_usgs,
  rasterprob_combined,
  cellpop,
  births,
  ndraws,
  as_cat_label,
  geod_col,
  birth_county_col,
  as_level_col
) {
  # Get unique counties and setup
  county_id_list <- na.omit(unique(datmatx_usgs[[geod_col]]))
  n_counties <- length(county_id_list)
  ncats <- length(as_cat_label)
  # Pre-allocate matrix with appropriate type
  as_temp <- matrix(NA_integer_, nrow = n_counties, ncol = ndraws)
  # Create efficient county lookup using split()
  county_lookup <- split(
    seq_len(nrow(datmatx_usgs)),
    datmatx_usgs[[geod_col]]
  )
  # Vectorized probability calculation and sampling
  for (i in seq_len(n_counties)) {
    county_id <- county_id_list[i]
    cells_i <- county_lookup[[county_id]]
    if (is.null(cells_i) || length(cells_i) == 0) next
    # Optimized probability calculation
    if (length(cells_i) == 1) {
      # Single cell - no aggregation needed
      prob_values <- rasterprob_combined[cells_i, ]
    } else {
      # Multiple cells - use efficient weighted mean
      weights <- cellpop[cells_i]
      if (sum(weights) > 0) {
        prob_values <- colSums(
          rasterprob_combined[cells_i, , drop = FALSE] * weights
        ) / sum(weights)
      } else {
        prob_values <- colMeans(rasterprob_combined[cells_i, , drop = FALSE])
      }
    }
    # Normalize probabilities (handle numerical precision)
    prob_sum <- sum(prob_values)
    if (prob_sum > 0) {
      prob_values <- prob_values / prob_sum
      # Sample all draws at once for this county
      as_temp[i, ] <- sample.int(
        n = ncats,
        size = ndraws,
        replace = TRUE,
        prob = prob_values
      )
    }
  }
  # Create county reference data frame
  # Pre-allocate results list
  imputed_datasets <- vector("list", ndraws)
  # Create imputed datasets efficiently
  for (j in seq_len(ndraws)) {
    # Create arsenic assignments for this draw
    as_draw_df <- data.frame(
      countyID = county_id_list,
      temp_as_level = factor(
        as_temp[, j],
        levels = seq_len(ncats),
        labels = as_cat_label
      ),
      stringsAsFactors = FALSE
    )
    # Set proper column name
    names(as_draw_df)[2] <- as_level_col
    # Merge with births data
    imputed_datasets[[j]] <- merge(
      births,
      as_draw_df,
      by.x = birth_county_col,
      by.y = "countyID",
      all.x = TRUE,
      sort = FALSE
    )
  }
  return(imputed_datasets)
}

#' Impute Additional Variables Using MICE
#'
#' Internal function to impute missing values in additional variables using
#' MICE.
#'
#' @param imputed_datasets List of datasets with arsenic exposure already
#'   imputed
#' @param impute_vars Character vector of variables to impute
#' @param births Original birth data frame
#' @param mice_covs Character vector of covariates for MICE
#' @param mice_m Number of MICE imputations
#' @param mice_maxit Maximum MICE iterations
#' @param mice_method MICE imputation method
#' @param seed Random seed
#' @return List of datasets with additional variables imputed
#'
#' @keywords internal
impute_additional_variables <- function(
  imputed_datasets,
  impute_vars,
  births,
  mice_covs,
  mice_m,
  mice_maxit,
  mice_method,
  seed
) {
  # Validate input variables
  missing_vars <- setdiff(impute_vars, names(births))
  if (length(missing_vars) > 0) {
    stop(
      "Variables not found in birth data: ",
      paste(missing_vars, collapse = ", ")
    )
  }

  # Process each variable to impute
  for (var in impute_vars) {
    for (j in seq_along(imputed_datasets)) {
      # Check if covariates exist in current dataset
      available_covs <- intersect(mice_covs, names(imputed_datasets[[j]]))
      if (length(available_covs) == 0) {
        warning(
          "No MICE covariates available for variable ", var,
          " in draw ", j
        )
        next
      }

      # Create subset for imputation - fix the data.table issue
      var_data <- as.data.frame(imputed_datasets[[j]])[c(var, available_covs)]
      # Only impute if there are missing values
      if (any(is.na(var_data[[var]]))) {
        # Ensure we have at least 2 columns for MICE
        if (ncol(var_data) < 2) {
          warning(
            "Insufficient variables for MICE imputation of ", var,
            " in draw ", j
          )
          next
        }
        # Run MICE imputation with error handling
        mice_result <- tryCatch({
          suppressWarnings(
            mice::mice(
              as.data.frame(var_data),
              m = mice_m,
              maxit = mice_maxit,
              method = mice_method,
              seed = seed,
              printFlag = FALSE
            )
          )
        }, error = function(e) {
          warning(
            "MICE failed for variable ", var,
            " in draw ", j, ": ", e$message
          )
          return(NULL)
        })
        # Extract imputed values if successful
        if (!is.null(mice_result)) {
          imputed_datasets[[j]][[var]] <- mice::complete(mice_result)[[var]]
        }
      }
    }
  }
  return(imputed_datasets)
}

#' Validate Imputed Datasets
#'
#' Internal function to validate that arsenic exposure imputation was
#' successful.
#'
#' @param imputed_datasets List of imputed datasets
#' @param as_level_col Column name for arsenic level variable
#' @param as_cat_label Character vector of valid arsenic category labels
#'
#' @keywords internal
validate_imputed_datasets <- function(
  imputed_datasets,
  as_level_col,
  as_cat_label
) {
  for (j in seq_along(imputed_datasets)) {
    # Check for missing arsenic levels
    if (any(is.na(imputed_datasets[[j]][[as_level_col]]))) {
      stop("Missing values found in arsenic level column in draw ", j)
    }
    # Check for invalid arsenic levels
    dataset_levels <- as.character(imputed_datasets[[j]][[as_level_col]])
    if (!all(dataset_levels %in% as_cat_label)) {
      invalid_levels <- setdiff(dataset_levels, as_cat_label)
      stop(
        "Invalid arsenic levels found in draw ", j, ": ",
        paste(invalid_levels, collapse = ", ")
      )
    }
  }
  message(
    "All ", length(imputed_datasets),
    " imputed datasets validated successfully"
  )
}
