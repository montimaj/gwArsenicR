# An R package for modeling and estimating arsenic exposure from groundwater, based on epidemiological studies and existing concentration models.

**Principal Investigator**: [Dr. Matthew O. Gribble](https://profiles.ucsf.edu/matthew.gribble) [matt.gribble@ucsf.edu]

**Co-Investigators**: [Dr. Sayantan Majumdar](https://www.dri.edu/directory/sayantan-majumdar/) [sayantan.majumdar@dri.edu], [Dr. Ryan G. Smith](https://www.engr.colostate.edu/ce/ryan-g-smith/) [ryan.g.smith@colostate.edu]

## Abstract
The exposure of fetuses to arsenic concentrations greater than 50 μg/L in drinking water during pregnancy is linked to unfavorable birth outcomes, while the evidence for concentrations below 50 μg/L is not conclusive. To investigate this issue, [Lombard et al. (2021)](https://doi.org/10.1021/acs.est.0c05239) utilized machine learning model estimates to determine the arsenic concentrations in private wells, which are not regulated by the federal government for drinking water contaminants. The study then assessed the relationship between these arsenic concentrations and birth outcomes across the contiguous United States.

As part of this project, we will develop an R package [(Wickham & Bryan, 2023)](https://r-pkgs.org/) based on previous epidemiological studies by Dr. Matt Gribble. The package will estimate a range of arsenic exposure outcomes based on the existing model of arsenic concentrations in groundwater ([Bulka et al., 2022](https://doi.org/10.1016/j.envint.2022.107176); [Lombard et al., 2021](https://doi.org/10.1021/acs.est.0c05239)). The R package will enable easier implementation of the arsenic exposure model by other epidemiologists.

Link to notes: [Google Docs](https://docs.google.com/document/d/1xjsfX_Sx-nYJIrSw1oVTyT-VNxg69WukXNVlj-hoL1s/edit?usp=sharing)

<img src="Readme_Figures/UCSF_Logo_21_Navy_300dpi_RGB.png" height="55"/> &nbsp; <img src="Readme_Figures/DRITaglineLogoTransparentBackground.png" height="55"/> &nbsp; <img src="Readme_Figures/CSU-Signature-C-357.png" height="65"/> &nbsp; <img src="Readme_Figures/NIH_Master_Logo_Vertical_2Color.png" height="55"/>


## Installing gwArsenicR

Install [R](https://www.r-project.org/) and run the following codes from the terminal or your preferred IDE ([RStudio](https://posit.co/download/rstudio-desktop/), [VS Code](https://code.visualstudio.com/), etc.) 

```r 
# Set the working directory to your package directory
setwd(<path to gwqual>)

# Load devtools
library(devtools)

# Generate documentation
document()

# Build the package
build()

# Install the package
install()
```

## Example use of gwArsenicR

```r
# Load the gwArsenicR package
library(gwarsenicr)

# Define the input parameters
ndraws <- 10
as_prob_csv <- "path/to/As_update_122019.csv"
birth_data_txt <- "path/to/births_2016_updated08142020.txt"
output_dir <- "path/to/output"
targets <- c("OEGEST", "BWT")
as_cat_label <- c("As<5", "As5-10", "As10+")
drop_as_cat_label_reg <- c("As<5")
seed <- 12345

# Run the sensitivity analysis
result <- perform_sensitivity_analysis(
  ndraws = ndraws, 
  as_prob_csv = as_prob_csv, 
  birth_data_txt = birth_data_txt, 
  output_dir = output_dir, 
  targets = targets,
  as_cat_label = as_cat_label,
  drop_as_cat_label_reg = drop_as_cat_label_reg,
  seed = seed
)

# Print the result
print(result)
```

### Example Data

To run the example, you will need to replace the placeholder file paths (`"path/to/As_update_122019.csv"`, `"path/to/births_2016_updated08142020.txt"`, and `"path/to/output"`) with the actual paths to your data files and output directory.

- The following columns must be present in `path/to/As_update_122019.csv`: 

    * ***RFC3_C1v2, RFC3_C2v2, RFC3_C3v2*** (multinomial probability vector for each raster cell), 
    * ***Wells_2010*** (population of well users for each raster cell in 2010), 
    * ***GEOID10*** (county ID)

- The following columns must be present in `path/to/births_2016_updated08142020.txt`: 

    * ***RFCprobC2, RFCprobC3, MAGE_R, MRACEHISP, MRACEHISP_F, MEDUC_2, MEDUC_3, MEDUC_4, smoke, RUCC, RUCC_F, pm, DMAR_1, FIPS, MRSTATE, BWT, OEGEST_R3, OEGEST***

- See [Bulka et al. (2022)](https://doi.org/10.1016/j.envint.2022.107176) and [Lombard et al. (2021)](https://doi.org/10.1021/acs.est.0c05239) for details on these covariates.

- See [tests/test_gwqal.R](tests/test_gwqual.R) for creating dummy data, building and installing gwqual, and using gwqual as an R package to get results.


## References
Bulka, C. M., Bryan, M. S., Lombard, M. A., Bartell, S. M., Jones, D. K., Bradley, P. M., ... & Argos, M. (2022). Arsenic in private well water and birth outcomes in the United States. Environment International, 163, 107176. https://doi.org/10.1016/j.envint.2022.107176

Lombard, M. A., Bryan, M. S., Jones, D. K., Bulka, C., Bradley, P. M., Backer, L. C., ... & Ayotte, J. D. (2021). Machine learning models of arsenic in private wells throughout the conterminous United States as a tool for exposure assessment in human health studies. Environmental science & technology, 55(8), 5012-5023. https://doi.org/10.1021/acs.est.0c05239

Wickham, H., & Bryan, J. (2023). R Packages (2nd ed.). O'Reilly Media. https://r-pkgs.org/