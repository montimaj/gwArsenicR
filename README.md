# gwArsenicR
An R package for modeling and estimating arsenic exposure from groundwater, based on epidemiological studies and existing concentration models.

**Principal Investigator**: [Dr. Matthew O. Gribble](https://profiles.ucsf.edu/matthew.gribble) [matt.gribble@ucsf.edu]

**Co-Investigators**: [Dr. Sayantan Majumdar](https://www.dri.edu/directory/sayantan-majumdar/) [sayantan.majumdar@dri.edu], [Dr. Ryan G. Smith](https://www.engr.colostate.edu/ce/ryan-g-smith/) [ryan.g.smith@colostate.edu]

## Statement of Need
Chronic exposure to arsenic in drinking water is a significant public health concern, linked to numerous adverse health outcomes. Epidemiological studies investigating these effects often face challenges in accurately assessing exposure, particularly for populations relying on unregulated private wells. The underlying statistical models and data integration processes from foundational studies like _Bulka et al. (2022)_ and _Lombard et al. (2021)_ are robust but complex to implement.

The gwArsenicR package addresses this challenge by providing a streamlined, accessible, and reproducible workflow for researchers. It encapsulates the complex methods of multiple imputation and mixed-effects regression modeling into a single, user-friendly function. This enables epidemiologists and public health scientists to:
- Estimate arsenic exposure probabilities by integrating USGS and EPA models.
- Account for uncertainty in exposure estimates through a multiple imputation framework.
- Assess the relationship between arsenic exposure and various health outcomes using robust statistical models.

By lowering the barrier to entry for this type of analysis, gwArsenicR promotes wider adoption of these methods, enhances the reproducibility of research findings, and facilitates more rigorous investigation into the public health impacts of arsenic in private well water.

## Installation
### System Dependencies: Pandoc

To build the package vignettes, which provide detailed documentation and examples, you need to have Pandoc installed. RStudio bundles a version of Pandoc, so if you are an RStudio user, you may not need to install it separately.

If you do not have Pandoc, you can download and install it from the official website: [pandoc.org/installing.html](https://pandoc.org/installing.html).

### R Package Installation
Install [R](https://www.r-project.org/) and run the following codes from the terminal or your preferred IDE ([RStudio](https://posit.co/download/rstudio-desktop/), [VS Code](https://code.visualstudio.com/), etc.) 
```
if (!require("devtools")) install.packages("devtools")
devtools::install_github("montimaj/gwArsenicR")
```

### Modifying the R Package
If you want to customize this package, run the following using R after you have modified the codes.
```
# Set the working directory to your package directory
setwd(<path to gwArsenicR>)

# Load devtools
library(devtools)

# Generate documentation
document()

# Build the package
build()

# Install the package
install()
```

## Package Structure
The gwArsenicR package follows the standard structure for R packages:
```
gwArsenicR/
├── R/
│   └── gwArsenic.R
├── man/
│   └── gwArsenic.Rd
├── tests/
│   ├── testthat/
│   │   ├── helper-create_dummy_data.R
│   │   └── test-analysis.R
│   └── testthat.R
├── vignettes/
│   └── gwArsenicR-vignette.Rmd
├── DESCRIPTION
├── NAMESPACE
├── LICENSE.md
└── README.md
```

## Core Functions
The package's primary functionality is exposed through a single main function:

```perform_sensitivity_analysis()```: This is the main function of the package. It orchestrates the entire workflow, from loading and processing the data to performing the multiple imputation and running the final regression analysis. It calls the internal ```regression_analysis()``` helper function to fit the models.

## Usage
See the [vignettes/gwArsenicR-vignette.Rmd](vignettes/gwArsenicR-vignette.Rmd) for example usage using dummy data. You can run this Rmd file using the following command.
```Rscript -e "rmarkdown::render('vignettes/gwArsenicR-vignette.Rmd')"``` or use VS Code preview.


## Citation
If you use gwArsenicR in your research, please cite it. A paper describing the package is currently under review at the Journal of Open Source Software (JOSS).

```
@Misc{,
  title = {gwArsenicR: An R package for modeling and estimating arsenic exposure from groundwater},
  author = {Sayantan Majumdar and Matthew O. Gribble and Ryan G. Smith and Melissa A. Lombard and Scott M. Bartell},
  year = {2025},
  note = {R package version 0.1.0. Under review in the Journal of Open Source Software.},
  url = {https://github.com/montimaj/gwArsenicR},
}
```
<img src="Readme_Figures/UCSF_Logo_21_Navy_300dpi_RGB.png" height="55"/> &nbsp; <img src="Readme_Figures/DRITaglineLogoTransparentBackground.png" height="55"/> &nbsp; <img src="Readme_Figures/CSU-Signature-C-357.png" height="65"/> &nbsp;  <img src="Readme_Figures/USGS_logo.png" height="55"/> &nbsp; <img src="Readme_Figures/brand-uci-edu-primarylogo-UC-Irvine.svg" height="55"/> &nbsp; <img src="Readme_Figures/NIH_Master_Logo_Vertical_2Color.png" height="55"/>

## References
Bulka, C. M., Bryan, M. S., Lombard, M. A., Bartell, S. M., Jones, D. K., Bradley, P. M., ... & Argos, M. (2022). Arsenic in private well water and birth outcomes in the United States. _Environment International, 163_, 107176. https://doi.org/10.1016/j.envint.2022.107176

Lombard, M. A., Bryan, M. S., Jones, D. K., Bulka, C., Bradley, P. M., Backer, L. C., ... & Ayotte, J. D. (2021). Machine learning models of arsenic in private wells throughout the conterminous United States as a tool for exposure assessment in human health studies. _Environmental science & technology, 55_(8), 5012-5023. https://doi.org/10.1021/acs.est.0c05239

Wickham, H., & Bryan, J. (2023). R Packages (2nd ed.). O'Reilly Media. https://r-pkgs.org/
