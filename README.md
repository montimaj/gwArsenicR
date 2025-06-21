# gwArsenicR
An R package for modeling and estimating arsenic exposure from groundwater, based on epidemiological studies and existing concentration models.

[![Version](https://img.shields.io/badge/version-0.1.0-blue.svg)](https://github.com/montimaj/gwArsenicR)
[![R](https://img.shields.io/badge/R-%E2%89%A5%204.1.0-blue.svg)](https://www.r-project.org/)
[![Build and Test](https://github.com/montimaj/gwArsenicR/actions/workflows/r.yml/badge.svg)](https://github.com/montimaj/gwArsenicR/actions/workflows/r.yml)

**Principal Investigator**: [Dr. Matthew O. Gribble](https://profiles.ucsf.edu/matthew.gribble) [matt.gribble@ucsf.edu]

**Co-Investigators**: [Dr. Sayantan Majumdar](https://www.dri.edu/directory/sayantan-majumdar/) [sayantan.majumdar@dri.edu], [Dr. Ryan G. Smith](https://www.engr.colostate.edu/ce/ryan-g-smith/) [ryan.g.smith@colostate.edu]

## Table of Contents
- [Statement of Need](#statement-of-need)
- [Installation](#installation)
- [Quick Start](#quick-start)
- [Data Requirements](#data-requirements)
- [Package Structure](#package-structure)
- [Core Functions](#core-functions)
- [Key Features](#key-features)
- [Documentation](#documentation)
- [Running Tests](#running-tests)
- [Modifying the R Package](#modifying-the-r-package)
- [Contributing](#contributing)
- [Citation](#citation)
- [License](#license)

## Statement of Need

Chronic arsenic exposure from drinking water affects millions globally and is linked to cancer, cardiovascular disease, and adverse birth outcomes. **The challenge**: Accurately estimating exposure for populations using unregulated private wells, where arsenic concentrations are unknown.

Existing methods from studies like _Bulka et al. (2022)_ and _Lombard et al. (2021)_ provide robust statistical frameworks but require complex implementation involving:
- Integration of multiple probabilistic models (USGS and EPA)
- Multiple imputation techniques for uncertain exposures
- Mixed-effects regression with proper pooling of results

**gwArsenicR solves this** by packaging these sophisticated methods into a single, user-friendly function. This enables researchers to:
- 🔬 **Focus on science**, not statistical programming
- 📊 **Ensure reproducibility** across studies  
- ⚡ **Accelerate research** on arsenic health effects
- 🎯 **Apply best practices** for uncertainty quantification

By democratizing access to these methods, gwArsenicR promotes more rigorous and comparable arsenic exposure research.

## Installation
### System Dependencies: Pandoc

To build the package vignettes, which provide detailed documentation and examples, you need to have Pandoc installed. RStudio bundles a version of Pandoc, so if you are an RStudio user, you may not need to install it separately.

If you do not have Pandoc, you can download and install it from the official website: [pandoc.org/installing.html](https://pandoc.org/installing.html).

### R Package Installation

**Prerequisites:** R (≥ 4.1.0) and [Pandoc](https://pandoc.org/installing.html) for building vignettes.

**Install from GitHub:**
```r
# Install devtools if not already installed
if (!require("devtools")) install.packages("devtools")

# Install gwArsenicR
devtools::install_github("montimaj/gwArsenicR", build_vignettes = TRUE)

# Load the package
library(gwArsenicR)
```

**Verify installation:**
```r
# Check package version
packageVersion("gwArsenicR")

# View help
?perform_sensitivity_analysis
```

**Package Documentation Website:**

The complete package documentation, including detailed function references, vignettes, and examples, is available at: **https://montimaj.github.io/gwArsenicR**

This website is automatically generated and updated whenever changes are made to the main branch, ensuring you always have access to the latest documentation.

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

## Quick Start

```r
library(gwArsenicR)

# Run sensitivity analysis with your data
results <- perform_sensitivity_analysis(
  ndraws = 10,
  as_usgs_prob_csv = "path/to/usgs_data.csv",
  as_epa_prob_csv = "path/to/epa_data.csv", 
  birth_data_txt = "path/to/birth_data.txt",
  regression_formula = "~ AsLevel + MAGE_R + rural + (1|FIPS)",
  output_dir = "results/",
  targets = c("OEGEST", "BWT")
)

# View results
print(results)
```

For detailed examples with dummy data, see the [vignette](vignettes/gwArsenicR-vignette.Rmd).

## Package Structure
The gwArsenicR package follows the standard structure for R packages:
```
gwArsenicR/
├── R/
│   ├── gwArsenic.R           # Main exported function
│   ├── data-loading.R        # Data loading and processing functions
│   ├── imputation.R          # Multiple imputation functions
│   ├── regression.R          # Regression analysis and pooling functions
│   └── utils.R               # Utility functions
├── man/
│   ├── perform_sensitivity_analysis.Rd
│   └── [other function documentation]
├── tests/
│   ├── testthat/
│   │   ├── helper-create_dummy_data.R
│   │   └── test-analysis.R
│   └── testthat.R
├── vignettes/
│   └── gwArsenicR-vignette.Rmd
├── .github/
│   └── workflows/
│       └── r.yml             # GitHub Actions CI/CD workflow
├── CONTRIBUTING.md           # Contribution guidelines and development setup
├── DESCRIPTION              # Package metadata and dependencies
├── NAMESPACE               # Package exports and imports
├── LICENSE.md             # Package license
├── NEWS.md                 # Change log and version history
└── README.md             # This file
```

### R/ Directory Structure

The R code is organized into modular files for better maintainability:

- **`gwArsenic.R`**: Contains the main exported function `perform_sensitivity_analysis()` that orchestrates the entire workflow
- **`data-loading.R`**: Internal functions for loading and processing USGS and EPA arsenic data, and birth/health outcome data
- **`imputation.R`**: Internal functions for multiple imputation of arsenic exposure levels and additional covariates using MICE
- **`regression.R`**: Internal functions for mixed-effects regression analysis and pooling results using Rubin's Rules
- **`utils.R`**: Utility functions for data formatting, validation, and other helper operations

### Key Internal Functions

While users primarily interact with `perform_sensitivity_analysis()`, the package includes several internal functions organized by functionality:

**Data Loading (`data-loading.R`):**
- `load_and_process_arsenic_data()`: Orchestrates USGS and EPA data loading
- `load_usgs_data()`: Loads USGS probability data
- `convert_epa_to_multinomial()`: Converts EPA lognormal to multinomial probabilities
- `create_weighted_prob_matrix()`: Creates weighted combined probability matrix
- `load_and_process_birth_data()`: Loads and processes birth/health outcome data

**Multiple Imputation (`imputation.R`):**
- `impute_arsenic_exposure()`: Creates multiple imputed datasets with arsenic exposure
- `impute_additional_variables()`: Imputes missing covariates using MICE
- `validate_imputed_datasets()`: Validates imputation results

**Regression Analysis (`regression.R`):**
- `regression_analysis()`: Performs mixed-effects regression on imputed data
- `pool_estimates_by_term()`: Pools regression estimates using Rubin's Rules
- `pool_single_estimate()`: Applies Rubin's Rules to individual parameters

**Utilities (`utils.R`):**
- `format_geographic_ids()`: Formats FIPS codes and geographic identifiers
- Additional helper functions for data validation and formatting

## Core Functions
The package's primary functionality is exposed through a single main function:

**`perform_sensitivity_analysis()`**: This is the main exported function of the package. It orchestrates the entire workflow from data loading through final analysis:

1. **Data Loading**: Loads and processes USGS probability data, EPA lognormal parameters, and birth/health outcome data
2. **Probability Integration**: Combines USGS and EPA models using weighted averages based on private well usage
3. **Multiple Imputation**: Creates multiple datasets with probabilistically assigned arsenic exposure levels
4. **Covariate Imputation**: Optionally imputes missing values in additional covariates using MICE
5. **Statistical Analysis**: Fits mixed-effects regression models to assess exposure-outcome relationships
6. **Results Pooling**: Applies Rubin's Rules to pool results across imputed datasets
7. **Output Generation**: Saves results and returns structured analysis output

The function is designed to handle the complex statistical methodology while providing a simple, user-friendly interface that requires minimal statistical programming expertise.

## Key Features

- ✅ **Automated workflow**: Single function handles entire analysis pipeline
- ✅ **Multiple imputation**: Accounts for uncertainty in arsenic exposure assignment
- ✅ **Flexible modeling**: Supports custom regression formulas and multiple outcomes
- ✅ **Robust statistics**: Implements Rubin's Rules for proper inference
- ✅ **Data integration**: Combines USGS and EPA models with population weighting
- ✅ **Missing data handling**: Optional MICE imputation for covariates
- ✅ **Reproducible results**: Seed control and comprehensive output saving
- ✅ **Extensive testing**: >95% code coverage with automated CI/CD

## Documentation

### Online Documentation
📖 **Complete documentation website**: https://montimaj.github.io/gwArsenicR

The documentation website includes:
- **Function Reference**: Detailed documentation for all package functions
- **Vignettes**: Step-by-step tutorials with working examples
- **Getting Started Guide**: Quick introduction to package usage
- **Methodology**: Statistical background and implementation details
- **FAQ**: Common questions and troubleshooting

### Local Documentation
```r
# View package help
help(package = "gwArsenicR")

# View main function documentation
?perform_sensitivity_analysis

# Browse vignettes
browseVignettes("gwArsenicR")
```

## Data Requirements

The package requires three input files:

1. **USGS Probability Data** (`as_usgs_prob_csv`): CSV with arsenic concentration probabilities and geographic identifiers
2. **EPA Parameters** (`as_epa_prob_csv`): CSV with EPA lognormal distribution parameters  
3. **Health Outcome Data** (`birth_data_txt`): Text file with health outcomes and county identifiers

**Expected columns:**
- USGS data: `GEOID10`, `RFC3_C1v2`, `RFC3_C2v2`, `RFC3_C3v2`, `Wells_2010`
- EPA data: `EPA_AS_meanlog`, `PWELL_private_pct`
- Health data: `FIPS`, outcome variables (e.g., `BWT`, `OEGEST`), covariates

See the [vignette](vignettes/gwArsenicR-vignette.Rmd) for detailed data format specifications.

## Usage
See the [vignettes/gwArsenicR-vignette.Rmd](vignettes/gwArsenicR-vignette.Rmd) for example usage using dummy data. You can run this Rmd file using the following command.
```Rscript -e "rmarkdown::render('vignettes/gwArsenicR-vignette.Rmd')"``` or use VS Code preview.

### Running Tests

The package includes comprehensive tests to ensure reliability and correctness. All tests use synthetic dummy data and run quickly for efficient development and CI/CD workflows.

**Run all tests:**
```bash
cd /path/to/gwArsenicR
Rscript -e "devtools::test()"
```

**Run specific test file:**
```bash
Rscript -e "testthat::test_file('tests/testthat/test-analysis.R')"
```

**Run tests during development (faster - no package rebuild):**
```bash
Rscript -e "devtools::load_all(); devtools::test()"
```

**Run tests with clean output (suppress MICE warnings):**
```bash
Rscript -e "suppressWarnings(devtools::test())"
```

**Check package integrity:**
```bash
Rscript -e "devtools::check()"
```

**Interactive testing in R/RStudio:**
```r
# Load package for development
devtools::load_all()

# Run all tests
devtools::test()

# Run specific test with detailed output
testthat::test_file("tests/testthat/test-analysis.R", reporter = "progress")

# Check test coverage
covr::package_coverage()
```

### Testing Configuration

Tests are configured for speed and reliability:
- **Fast execution**: Uses `ndraws = 2` for quick multiple imputation
- **Synthetic data**: All tests use generated dummy data, no external dependencies
- **Comprehensive coverage**: Tests data loading, imputation, regression, and output generation
- **Warning suppression**: Expected MICE convergence warnings are suppressed for clean output
- **Automatic cleanup**: Temporary files are automatically removed after each test

### Expected Test Output

A successful test run will show:
```
══ Testing test-analysis.R ══════════════════════════════════════════════════════════════════
✅ [ FAIL 0 | WARN 0 | SKIP 0 | PASS 3 ]
--- Pooled Analysis Results ---
$BWT
    term      q.mi    se.mi  statistic  conf.low conf.high   p.value
1 As5-10 13.555199 43.50701  0.3115636 -86.29198 113.40237 0.7631215
2  As10+ -8.176346 37.90346 -0.2157150 -89.49962  73.14693 0.8323325

$OEGEST
    term       q.mi     se.mi statistic   conf.low conf.high   p.value
1 As5-10 0.08924988 0.1643782 0.5429544 -0.2702451 0.4487449 0.5974323
2  As10+ 0.07891698 0.1640922 0.4809308 -0.2709718 0.4288058 0.6375356

-----------------------------
✅ [ FAIL 0 | WARN 0 | SKIP 0 | PASS 5 ][1] "Checking results for target: BWT"
✅ [ FAIL 0 | WARN 0 | SKIP 0 | PASS 14 ][1] "Checking results for target: OEGEST"
✅ [ FAIL 0 | WARN 0 | SKIP 0 | PASS 23 ] Done!
```

### Continuous Integration

The package includes automated testing via GitHub Actions:
- **R CMD check**: Validates package structure and dependencies
- **Multiple R versions**: Tests on R 4.1+ across different operating systems
- **Dependency validation**: Ensures all required packages are properly declared
- **Documentation checks**: Validates roxygen2 documentation completeness

[![Build and Test](https://github.com/montimaj/gwArsenicR/actions/workflows/r.yml/badge.svg)](https://github.com/montimaj/gwArsenicR/actions/workflows/r.yml)

### Troubleshooting Tests

**If tests fail with data.table errors:**
```bash
# Ensure data.table is properly loaded
Rscript -e "library(data.table); devtools::test()"
```

**If MICE warnings are excessive:**
```bash
# Run with warning suppression
Rscript -e "suppressWarnings(devtools::test())"
```

**If tests timeout:**
```bash
# Check system resources and reduce ndraws in test files if needed
# Default test configuration uses ndraws = 2 for speed
```

**Memory issues:**
```bash
# Clear R environment and restart
Rscript -e "rm(list=ls()); gc(); devtools::test()"
```

### Testing Best Practices

When developing or modifying the package:

1. **Run tests frequently** during development
2. **Add new tests** for new functionality
3. **Use small datasets** in tests for speed (ndraws = 2)
4. **Test edge cases** such as missing data or unusual parameter values
5. **Validate statistical correctness** of pooled results

## Contributing

We welcome contributions! Please see our [contribution guidelines](CONTRIBUTING.md) for details.

**Ways to contribute:**
- 🐛 Report bugs via [GitHub issues](https://github.com/montimaj/gwArsenicR/issues)
- 💡 Suggest features or improvements
- 📖 Improve documentation
- 🧪 Add tests or examples
- 🔧 Submit pull requests

**Development setup:**
```bash
git clone https://github.com/montimaj/gwArsenicR.git
cd gwArsenicR
Rscript -e "devtools::load_all(); devtools::test()"
```

## Citation

If you use gwArsenicR in your research, please cite:

> Majumdar, S., Bartell, S. M., Lombard, M. A., Smith, R. G., & Gribble, M. O. (2025). 
> gwArsenicR: An R package for modeling and estimating arsenic exposure from groundwater. 
> *Journal of Open Source Software* (under review).

**BibTeX:**
```bibtex
@article{majumdar2025gwarsenic,
  title={gwArsenicR: An R package for modeling and estimating arsenic exposure from groundwater},
  author={Majumdar, Sayantan and Bartell, Scott M and Lombard, Melissa A and Smith, Ryan G and Gribble, Matthew O},
  journal={Journal of Open Source Software},
  year={2025},
  note={Under review},
  url={https://github.com/montimaj/gwArsenicR}
}
```

**DOI:** *Coming soon upon publication*

<img src="Readme_Figures/UCSF_Logo_21_Navy_300dpi_RGB.png" height="35"/> &nbsp; <img src="Readme_Figures/DRITaglineLogoTransparentBackground.png" height="35"/> &nbsp; <img src="Readme_Figures/CSU-Signature-C-357.png" height="45"/> &nbsp;  <img src="Readme_Figures/USGS_logo.png" height="35"/> &nbsp; <img src="Readme_Figures/brand-uci-edu-primarylogo-UC-Irvine.svg" height="35"/> &nbsp; <img src="Readme_Figures/NIH_Master_Logo_Vertical_2Color.png" height="35"/>

## License

This project is licensed under the Apache 2.0 License - see the [LICENSE.md](LICENSE.md) file for details.

**Version:** 0.1.0 | **Status:** Under active development

## References
Bulka, C. M., Bryan, M. S., Lombard, M. A., Bartell, S. M., Jones, D. K., Bradley, P. M., ... & Argos, M. (2022). Arsenic in private well water and birth outcomes in the United States. _Environment International, 163_, 107176. https://doi.org/10.1016/j.envint.2022.107176

Lombard, M. A., Bryan, M. S., Jones, D. K., Bulka, C., Bradley, P. M., Backer, L. C., ... & Ayotte, J. D. (2021). Machine learning models of arsenic in private wells throughout the conterminous United States as a tool for exposure assessment in human health studies. _Environmental science & technology, 55_(8), 5012-5023. https://doi.org/10.1021/acs.est.0c05239

Wickham, H., & Bryan, J. (2023). R Packages (2nd ed.). O'Reilly Media. https://r-pkgs.org/
