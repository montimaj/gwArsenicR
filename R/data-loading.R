#' Load and Process Arsenic Data
#'
#' Internal function to load USGS and EPA arsenic data, convert EPA lognormal
#' to multinomial, and create weighted combined probability matrix.
#'
#' This function is used within the sensitivity analysis workflow.
#' @param as_usgs_prob_csv Path to USGS probability CSV
#' @param as_epa_prob_csv Path to EPA probability CSV
#' @param as_conc_cols Column names for arsenic concentration probabilities
#' @param pop_well_col Column name for population weights
#' @param epa_as_col Column name for EPA arsenic meanlog parameter
#' @param epa_lognormal_sdlog Standard deviation for lognormal distribution
#' @param epa_pwell_col Column name for private well percentage
#' @param geod_col Column name for geographic identifier
#' @return List containing processed arsenic data
#'
#' @keywords internal
load_and_process_arsenic_data <- function(
  as_usgs_prob_csv,
  as_epa_prob_csv,
  as_conc_cols,
  pop_well_col,
  epa_as_col,
  epa_lognormal_sdlog,
  epa_pwell_col,
  geod_col
) {
  # Load USGS data (returns a list)
  usgs_data <- load_usgs_data(as_usgs_prob_csv, as_conc_cols, pop_well_col)

  # Load EPA data
  datmatx_epa <- data.table::fread(
    as_epa_prob_csv,
    header = TRUE,
    showProgress = FALSE,
    nThread = parallel::detectCores()
  )

  # Validate data alignment - use the actual data frames
  validate_data_alignment(usgs_data$datmatx_usgs, datmatx_epa)

  # Convert EPA lognormal to multinomial
  rasterprob_epa <- convert_epa_to_multinomial(
    datmatx_epa = datmatx_epa,
    epa_as_col = epa_as_col,
    epa_lognormal_sdlog = epa_lognormal_sdlog
  )

  # Create weighted combined probability matrix
  rasterprob_combined <- create_weighted_prob_matrix(
    rasterprob_usgs = usgs_data$rasterprob_usgs,  # Use the extracted matrix
    rasterprob_epa = rasterprob_epa,
    datmatx_epa = datmatx_epa,
    epa_pwell_col = epa_pwell_col
  )

  # Format geographic identifiers
  formatted_usgs <- format_geographic_ids(
    usgs_data$datmatx_usgs,
    geod_col
  )

  return(list(
    datmatx_usgs = formatted_usgs,
    rasterprob_combined = rasterprob_combined,
    cellpop = usgs_data$cellpop
  ))
}

#' Load USGS Data
#'
#' @param as_usgs_prob_csv Path to USGS CSV file
#' @param as_conc_cols Column names for concentration probabilities
#' @param pop_well_col Column name for population weights
#' @return List with USGS data, probability matrix, and cell populations
#'
#' @keywords internal
load_usgs_data <- function(as_usgs_prob_csv, as_conc_cols, pop_well_col) {
  datmatx_usgs <- data.table::fread(
    as_usgs_prob_csv,
    header = TRUE,
    showProgress = FALSE,
    nThread = parallel::detectCores()
  )

  # Validate required columns
  missing_cols <- setdiff(as_conc_cols, names(datmatx_usgs))
  if (length(missing_cols) > 0) {
    stop(
      "The following columns are missing in USGS data: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  # Extract probability matrix and population weights
  rasterprob_usgs <- as.matrix(datmatx_usgs[, as_conc_cols, with = FALSE])
  cellpop <- datmatx_usgs[[pop_well_col]]

  return(list(
    datmatx_usgs = datmatx_usgs,
    rasterprob_usgs = rasterprob_usgs,
    cellpop = cellpop
  ))
}

#' Load and Process Birth Data
#'
#' Internal function to load birth data, select columns, create rural indicator,
#' and format county identifiers.
#'
#' @param birth_data_txt Path to birth data file
#' @param columns_to_select Column names to select (NULL for all)
#' @param rucc_col Column name for Rural-Urban Continuum Code
#' @param birth_county_col Column name for birth county identifier
#' @return Processed birth data frame
#'
#' @keywords internal
load_and_process_birth_data <- function(
  birth_data_txt,
  columns_to_select,
  rucc_col,
  birth_county_col
) {
  # Load birth data
  births <- data.table::fread(
    birth_data_txt,
    header = TRUE,
    nThread = parallel::detectCores()
  )

  # Select specified columns if provided
  if (!is.null(columns_to_select)) {
    births <- births %>% dplyr::select(dplyr::all_of(columns_to_select))
  }

  # Create rural indicator if RUCC column exists
  if (rucc_col %in% names(births)) {
    births <- births %>%
      dplyr::mutate(rural = .data[[rucc_col]] > 3)
  }

  # Format county identifiers
  births_county_numeric <- as.numeric(births[[birth_county_col]])
  births[[birth_county_col]] <- sprintf("%05d", births_county_numeric)

  return(births)
}

#' Validate Data Alignment
#'
#' @param datmatx_usgs USGS data frame
#' @param datmatx_epa EPA data frame
#'
#' @keywords internal
validate_data_alignment <- function(datmatx_usgs, datmatx_epa) {
  if (nrow(datmatx_usgs) != nrow(datmatx_epa)) {
    stop("USGS and EPA data files have different numbers of rows.")
  }
}

#' Convert EPA Lognormal to Multinomial
#'
#' @param datmatx_epa EPA data frame
#' @param epa_as_col Column name for EPA arsenic meanlog
#' @param epa_lognormal_sdlog Standard deviation for lognormal distribution
#' @return Matrix of multinomial probabilities
#'
#' @keywords internal
convert_epa_to_multinomial <- function(
  datmatx_epa,
  epa_as_col,
  epa_lognormal_sdlog
) {
  # Define cutoffs for arsenic categories (in µg/L)
  cutoffs <- c(5, 10)

  # Calculate cumulative probabilities for each cutoff
  p_le5 <- stats::plnorm(
    cutoffs[1],
    meanlog = datmatx_epa[[epa_as_col]],
    sdlog = epa_lognormal_sdlog
  )
  p_le10 <- stats::plnorm(
    cutoffs[2],
    meanlog = datmatx_epa[[epa_as_col]],
    sdlog = epa_lognormal_sdlog
  )

  # Calculate multinomial probabilities for the three categories
  rasterprob_epa <- cbind(
    p_le5,          # P(As < 5)
    p_le10 - p_le5, # P(5 <= As < 10)
    1 - p_le10      # P(As >= 10)
  )

  # Normalize to ensure rows sum to 1
  rasterprob_epa / rowSums(rasterprob_epa)
}

#' Create Weighted Probability Matrix
#'
#' @param rasterprob_usgs USGS probability matrix
#' @param rasterprob_epa EPA probability matrix
#' @param datmatx_epa EPA data frame
#' @param epa_pwell_col Column name for private well percentage
#' @return Combined weighted probability matrix
#'
#' @keywords internal
create_weighted_prob_matrix <- function(
  rasterprob_usgs,
  rasterprob_epa,
  datmatx_epa,
  epa_pwell_col
) {
  w_private <- datmatx_epa[[epa_pwell_col]] / 100
  w_public <- 1 - w_private

  # Element-wise multiplication and addition for final weighted probability
  (rasterprob_usgs * w_private) + (rasterprob_epa * w_public)
}
