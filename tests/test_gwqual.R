# Load necessary library
library(dplyr)

#' Create Dummy Data for Testing
#'
#' This function generates dummy data for testing purposes, including a CSV file
#' with probability values and a TXT file with birth data. The generated data
#' is saved to the specified directory.
#'
#' @param file_dir The directory where the dummy data files will be saved.
#' Default is "tests/data".
#' @param num_rows The number of rows of data to generate. Default is 25,000,
#' which assumes 500 X 500 raster cells of 1 km^2 spatial resolution.
#' @param seed The seed for the random number generator to ensure
#' reproducibility. Default is 12345.
#' @return A list containing the paths to the generated dummy probability CSV
#' file and the dummy births TXT file.
#' @examples
#' dummy_data_list <- create_dummy_data(num_rows = 50000)
#' dummy_prob_csv <- dummy_data_list[[1]]
#' dummy_births_txt <- dummy_data_list[[2]]
create_dummy_data <- function(
  file_dir = "tests/data",
  num_rows = 25000,
  seed = 12345
) {
  # Set seed for reproducibility
  set.seed(seed)

  # Generate probability values that add up to 1
  prob_values <- t(apply(
    matrix(runif(num_rows * 3), ncol = 3),
    1,
    function(x) x / sum(x)
  ))

  # Create the output directory if it does not exist
  if (!dir.exists(file_dir)) {
    dir.create(file_dir, recursive = TRUE)
  }

  # Generate integer values for Wells_2010 and GEOID10
  wells_2010 <- sample(1:200, num_rows, replace = TRUE)
  geoid10 <- sprintf("%05d", sample(1:5, num_rows, replace = TRUE))

  # Create the dataframe
  prob_df <- data.frame(
    RFC3_C1v2 = prob_values[, 1],
    RFC3_C2v2 = prob_values[, 2],
    RFC3_C3v2 = prob_values[, 3],
    Wells_2010 = wells_2010,
    GEOID10 = geoid10
  )
  dummy_prob_csv <- file.path(file_dir, "dummy_prob.csv")
  # Save the dataframe to a CSV file
  write.csv(prob_df, dummy_prob_csv, row.names = FALSE)

  # Generate the birth data
  # Generate probability values that add up to 1
  prob_values <- t(apply(
    matrix(runif(num_rows * 3), ncol = 3),
    1,
    function(x) x / sum(x)
  ))

  # Generate values for other columns
  mage_r <- sample(18:45, num_rows, replace = TRUE)
  mracehisp <- sample(1:5, num_rows, replace = TRUE)
  mracehisp_f <- sample(1:5, num_rows, replace = TRUE)
  meduc_2 <- sample(0:1, num_rows, replace = TRUE)
  meduc_3 <- sample(0:1, num_rows, replace = TRUE)
  meduc_4 <- sample(0:1, num_rows, replace = TRUE)
  smoke <- sample(0:1, num_rows, replace = TRUE)
  rucc <- sample(1:100, num_rows, replace = TRUE)
  rucc_f <- sample(1:100, num_rows, replace = TRUE)
  pm <- sample(1:100, num_rows, replace = TRUE)
  dmar_1 <- sample(0:1, num_rows, replace = TRUE)
  fips <- sprintf("%05d", sample(1:5, num_rows, replace = TRUE))
  mrstate <- sample(0:1, num_rows, replace = TRUE)
  bwt <- sample(2500:4500, num_rows, replace = TRUE)
  oegest_r3 <- sample(35:42, num_rows, replace = TRUE)
  oegest <- sample(35:42, num_rows, replace = TRUE)

  # Create the data table
  dt <- data.table::data.table(
    RFCprobC1 = prob_values[, 1],
    RFCprobC2 = prob_values[, 2],
    RFCprobC3 = prob_values[, 3],
    MAGE_R = mage_r,
    MRACEHISP = mracehisp,
    MRACEHISP_F = mracehisp_f,
    MEDUC_2 = meduc_2,
    MEDUC_3 = meduc_3,
    MEDUC_4 = meduc_4,
    smoke = smoke,
    RUCC = rucc,
    RUCC_F = rucc_f,
    pm = pm,
    DMAR_1 = dmar_1,
    FIPS = fips,
    MRSTATE = mrstate,
    BWT = bwt,
    OEGEST_R3 = oegest_r3,
    OEGEST = oegest
  )
  dummy_births_txt <- file.path(file_dir, "dummy_births.txt")
  data.table::fwrite(dt, dummy_births_txt, sep = "\t")
  return(list(dummy_prob_csv, dummy_births_txt))
}

dummy_data_list <- create_dummy_data(num_rows = 50000)
dummy_prob_csv <- dummy_data_list[[1]]
dummy_births_txt <- dummy_data_list[[2]]

library(devtools)
build()
install()
library(gwqual)

# Define the input parameters
ndraws <- 10
output_dir <- "tests/outputs"
targets <- c("OEGEST", "BWT")
as_cat_label <- c("As<5", "As5-10", "As10+")
drop_as_cat_label_reg <- c("As<5")
seed <- 12345

# Run the sensitivity analysis
results <- perform_sensitivity_analysis(
  ndraws = ndraws,
  as_prob_csv = dummy_prob_csv,
  birth_data_txt = dummy_births_txt,
  output_dir = output_dir,
  targets = targets,
  as_cat_label = as_cat_label,
  drop_as_cat_label_reg = drop_as_cat_label_reg,
  seed = seed
)

# Print the result
cat("\n\nSensitivity Analysis Results:\n")
print(results)