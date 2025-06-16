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
#' @param ndraws An integer specifying the number of imputed datasets to generate.
#' @param as_usgs_prob_csv A file path to the CSV containing USGS model-based
#'   multinomial probabilities of arsenic levels for private wells.
#' @param as_epa_prob_csv A file path to the CSV containing EPA model-based
#'   lognormal parameters for public wells and the percentage of private well users.
#' @param birth_data_txt A file path to the text file containing the birth data.
#' @param regression_formula A string or formula object for the regression model.
#' @param output_dir A file path to the directory where output files will be saved.
#' @param targets A character vector of the dependent variables (health outcomes)
#'   for the regression analysis.
#' @param as_cat_label A character vector of labels for the arsenic concentration categories.
#' @param drop_as_cat_label_reg A character vector of arsenic categories to be
#'   used as the reference level in the regression.
#' @param columns_to_select A character vector of column names to be selected
#'   from the birth data. If NULL, all columns are used.
#' @param rucc_col A character string specifying the column name for the
#'   Rural-Urban Continuum Code.
#' @param geod_col A character string specifying the column name for the
#'   Geographic Identifier (e.g., county FIPS code) in the arsenic data.
#' @param as_conc_cols A character vector of column names from the USGS data for
#'   the arsenic concentration probabilities.
#' @param pop_well_col A character string specifying the column name from the USGS
#'   data for the population of well users.
#' @param birth_county_col A character string specifying the column name for the
#'   birth county FIPS code in the birth data.
#' @param as_level_col A character string for the name of the imputed arsenic
#'   level column.
#' @param epa_as_col A character string for the column name of the EPA arsenic
#'   lognormal meanlog parameter.
#' @param epa_lognormal_sdlog A numeric value for the standard deviation of the
#'   lognormal distribution for the EPA data. Defaults to 1.0.
#' @param epa_pwell_col A character string for the column name of the percentage of
#'   private well users.
#' @param seed An integer for setting the random number generator seed for
#'   reproducibility.
#'
#' @return A list of data frames, each containing the pooled regression results for a
#'   target health outcome.
#'
#' @export

perform_sensitivity_analysis <- function(
  ndraws = 10,
  as_usgs_prob_csv,
  as_epa_prob_csv,
  birth_data_txt,
  regression_formula,
  output_dir,
  targets = c("OEGEST", "BWT"),
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
  seed = 12345
) {
  set.seed(seed)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # --- 1. Load and Process Arsenic Data ---

  # Load USGS (private well) multinomial probabilities
  datmatx_usgs <- data.table::fread(
    as_usgs_prob_csv, header = TRUE,
    showProgress = FALSE, nThread = parallel::detectCores()
  )
  # use cbind to create a matrix of probabilities
  missing_cols <- setdiff(as_conc_cols, names(datmatx_usgs))
  if (length(missing_cols) > 0) {
    stop(paste("The following columns are missing in USGS data:", paste(missing_cols, collapse = ", ")))
  }
  rasterprob_usgs <- as.matrix(datmatx_usgs[, ..as_conc_cols])
  
  cellpop <- datmatx_usgs[[pop_well_col]]

  # Load EPA (public well) lognormal data and private well percentages
  datmatx_epa <- data.table::fread(
    as_epa_prob_csv, header = TRUE,
    showProgress = FALSE, nThread = parallel::detectCores()
  )

  # Align data - assuming the CSVs have corresponding rows (e.g., by GEOID10)
  # For this example, we assume perfect row alignment. In a real scenario,
  # a merge/join on geod_col might be necessary.
  if (nrow(datmatx_usgs) != nrow(datmatx_epa)) {
    stop("USGS and EPA data files have different numbers of rows.")
  }

  # --- 2. Convert EPA Lognormal to Multinomial ---
  
  # Define cutoffs for arsenic categories (in µg/L)
  cutoffs <- c(5, 10)
  
  # Calculate cumulative probabilities for each cutoff
  p_le5 <- stats::plnorm(cutoffs[1], meanlog = datmatx_epa[[epa_as_col]], sdlog = epa_lognormal_sdlog)
  p_le10 <- stats::plnorm(cutoffs[2], meanlog = datmatx_epa[[epa_as_col]], sdlog = epa_lognormal_sdlog)

  # Calculate multinomial probabilities for the three categories
  rasterprob_epa <- cbind(
    p_le5,          # P(As < 5)
    p_le10 - p_le5, # P(5 <= As < 10)
    1 - p_le10      # P(As >= 10)
  )
  # Normalize to ensure rows sum to 1, handling potential floating point inaccuracies
  rasterprob_epa <- rasterprob_epa / rowSums(rasterprob_epa)

  # --- 3. Create Weighted, Combined Probability Matrix ---

  w_private <- datmatx_epa[[epa_pwell_col]] / 100
  w_public <- 1 - w_private

  # Element-wise multiplication and addition for the final weighted probability
  rasterprob_combined <- (rasterprob_usgs * w_private) + (rasterprob_epa * w_public)

  # --- 4. Load and Process Birth Data ---
  
  births <- data.table::fread(birth_data_txt, header = TRUE)
  if (!is.null(columns_to_select)) {
    births <- births %>% dplyr::select(dplyr::all_of(columns_to_select))
  }
  births <- births %>%
    dplyr::filter(complete.cases(.)) %>%
    dplyr::mutate(rural = .data[[rucc_col]] > 3)
  births[[birth_county_col]] <- sprintf("%05d", as.numeric(births[[birth_county_col]]))
  datmatx_usgs[[geod_col]] <- sprintf("%05d", as.numeric(datmatx_usgs[[geod_col]]))

  # --- 5. Multiple Imputation ---
  
  county_id_list <- na.omit(unique(datmatx_usgs[[geod_col]]))
  n_counties <- length(county_id_list)
  countydf <- data.frame(countyID = county_id_list)
  
  as_temp <- matrix(NA, n_counties, ndraws)
  ncats <- length(as_cat_label)
  
  for (i in 1:n_counties) {
    cells_i <- which(datmatx_usgs[[geod_col]] == county_id_list[i])
    if (length(cells_i) == 0) next
    
    # Calculate county-level mean probability, weighted by raster cell population
    prob_values <- apply(
      rasterprob_combined[cells_i, , drop = FALSE],
      2,
      stats::weighted.mean,
      w = cellpop[cells_i],
      na.rm = TRUE
    )
    # Ensure probabilities sum to 1
    prob_values <- prob_values / sum(prob_values)
    
    # Sample arsenic category for the county for each draw
    as_temp[i, ] <- sample(
      1:ncats, size = ndraws, replace = TRUE, prob = prob_values
    )
  }
  
  imputed_datasets <- vector("list", ndraws)
  for (j in 1:ndraws) {
    as_draw_df <- countydf
    as_draw_df[[as_level_col]] <- factor(as_temp[, j], levels = 1:ncats, labels = as_cat_label)
    imputed_datasets[[j]] <- merge(
      births, as_draw_df, by.x = birth_county_col, by.y = "countyID"
    )
  }

  # --- 6. Regression and Pooling ---

  combined_births <- data.table::rbindlist(imputed_datasets, idcol = ".id")
  
  # Ensure the reference level is set correctly
  filtered_as_cat_label <- base::setdiff(as_cat_label, drop_as_cat_label_reg)
  if (length(filtered_as_cat_label) == 0) {
    stop("No arsenic categories left to analyze after dropping reference.")
  }
  combined_births[[as_level_col]] <- stats::relevel(combined_births[[as_level_col]], ref = drop_as_cat_label_reg)

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

#' Perform Regression Analysis
#'
#' This function performs regression analysis on NAPHSIS data.
#'
#' @param births A data frame containing the birth data.
#' @param targets A vector of target variables.
#' @param as_cat_label A vector of arsenic categories generated by the random
#' forest model.
#' @param as_level_col The column name for the arsenic level.
#' @param regression_formula The regression formula.
#' @param output_dir The output directory.
#' @return A list of data frames with the regression results.
regression_analysis <- function(
  births,
  targets,
  as_cat_label,
  as_level_col,
  regression_formula,
  output_dir
) {
  # Fit a regression model within each of the "simulated arsenic category
  # worlds".
  # Append the results into the results matrices of coefficients and their
  # standard errors (SE).
  # This process involves grouping the data by the simulated arsenic levels
  # and fitting a linear mixed-effects model.
  # The model includes various covariates such as maternal age,
  # race/ethnicity, education, smoking status, and rural/urban status.
  # The random effects structure accounts for the hierarchical nature of the
  # data, with random intercepts for states and counties.

  results <- list()
  for (var in targets) {
    var_adj_overall_rfc_se <- births %>%
      dplyr::group_by(.id) %>%
      dplyr::do(
        fitmodel = lme4::lmer(
          as.formula(paste(
            var, regression_formula
          )),
          data = .
        )
      )

    # Define the terms to filter by
    terms_to_filter <- paste0("as.factor(", as_level_col, ")", as_cat_label)

    # Initialize an empty data frame to store the combined results
    var_combined_results <- data.frame()

    # Loop through each model and filter by terms
    for (i in seq_along(var_adj_overall_rfc_se$fitmodel)) {
      model_results <- broom.mixed::tidy(var_adj_overall_rfc_se$fitmodel[[i]])
      for (filtered_term in terms_to_filter) {
        filtered_results <- model_results %>%
          dplyr::filter(.data$term == filtered_term)
        var_combined_results <- dplyr::bind_rows(
          var_combined_results, filtered_results
        )
      }
    }
    deg_freedom <- as.integer(
      broom.mixed::glance(var_adj_overall_rfc_se$fitmodel[[1]])$df.residual
    )

    var_combined_results_terms <- data.frame()
    for (filtered_term in terms_to_filter) {
      var_combined_results_term <- var_combined_results %>%
        dplyr::filter(.data$term == filtered_term)
      var_combined_results_term <- as.data.frame(
        Amelia::mi.meld(
          q = as.matrix(var_combined_results_term$estimate),
          se = as.matrix(var_combined_results_term$std.error)
        )
      )
      var_combined_results_term <- var_combined_results_term %>%
        dplyr::mutate(
          statistic = .data$q.mi / .data$se.mi,
          conf.low = .data$q.mi + .data$se.mi * qt(0.025, deg_freedom),
          conf.high = .data$q.mi + .data$se.mi * qt(0.975, deg_freedom),
          p.value = 2 * pt(
            abs(.data$statistic), deg_freedom,
            lower.tail = FALSE
          )
        )
      var_combined_results_terms <- dplyr::bind_rows(
        var_combined_results_terms,
        var_combined_results_term
      )
    }
    data.table::fwrite(
      var_combined_results_terms,
      file.path(output_dir, paste0(var, "_pooled_results.csv"))
    )
    results[[var]] <- var_combined_results_terms
  }
  return(results)
}

