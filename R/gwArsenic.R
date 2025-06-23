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
