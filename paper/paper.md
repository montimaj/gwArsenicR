---
title: 'gwArsenicR: An R package for modeling health effects of groundwater arsenic exposure'
tags:
  - R
  - arsenic
  - groundwater
  - epidemiology
  - environmental health
  - multiple imputation
authors:
  - name: Sayantan Majumdar
    orcid: 0000-0002-3539-0147
    affiliation: 1
    corresponding: true
  - name: Scott M. Bartell
    orcid: 0000-0001-7797-2906
    affiliation: 2
  - name: Melissa A. Lombard
    orcid: 0000-0001-5924-6556
    affiliation: 3
  - name: Ryan G. Smith
    orcid: 0000-0002-3747-6868
    affiliation: 4
  - name: Matthew O. Gribble
    orcid: 0000-0002-1614-2981
    affiliation: 5  
affiliations:
  - name: Division of Hydrologic Sciences, Desert Research Institute, Reno, NV, USA
    index: 1
  - name: Joe C. Wen School of Population & Public Health, University of California, Irvine, CA, USA
    index: 2 
  - name: New England Water Science Center, U.S. Geological Survey, Pembroke, NH, USA
    index: 3
  - name: Department of Civil and Environmental Engineering, Colorado State University, Fort Collins, CO, USA
    index: 4
  - name: Division of Occupational, Environmental, and Climate Medicine, Department of Medicine, University of California, San Francisco, CA, USA
    index: 5
date: 16 June 2025
bibliography: paper.bib
---


# Summary

gwArsenicR is an R package \[@R-base\] that provides a streamlined workflow for epidemiologists and public health researchers to estimate arsenic exposure from private and public well water and to assess its association with various health outcomes. The package implements a sophisticated statistical approach that combines geospatial arsenic prediction models with a multiple imputation framework to account for exposure uncertainty. The core functionality integrates predicted arsenic concentration probabilities from U.S. Geological Survey (USGS) models for private wells with U.S. Environmental Protection Agency (EPA) data for public water systems. It then uses a weighting scheme based on the proportion of the population served by each water source to create a unified, county-level exposure probability distribution. By encapsulating these complex methodologies into a single, user-friendly function, gwArsenicR makes this type of analysis more accessible, reproducible, and standardized.

# Statement of Need

Chronic exposure to arsenic in drinking water is a significant public health problem, linked to a range of adverse health outcomes. Assessing this risk is challenging, particularly for the estimated 40 million Americans who rely on unregulated private wells \[@DeSimone-2015\]. Foundational studies by @Bulka-2022 and @Lombard-2021 have established robust methodologies for this purpose, but their implementation requires specialized expertise in data integration, statistical modeling, and computational methods. This presents a significant barrier for many environmental health researchers who may lack the necessary programming skills to replicate or adapt these complex analyses.

gwArsenicR directly addresses this need by packaging the entire analytical pipeline into an easy-to-use R function. It automates the process of:

1. Converting lognormal EPA arsenic data to a multinomial distribution.  
2. Combining USGS and EPA models based on population-use weights.  
3. Performing multiple imputation to manage exposure uncertainty.  
4. Fitting mixed-effects regression models to health data.  
5. Pooling results according to Rubin's Rules \[@Rubin-1987\].

By providing this accessible tool, gwArsenicR lowers the barrier to entry for conducting high-quality research on the health impacts of groundwater contaminants, promoting reproducibility and wider application of these state-of-the-art methods in environmental epidemiology.

# Design and Features

The design of gwArsenicR prioritizes ease of use, flexibility, and statistical rigor.

* **Modular Architecture**: The package is centered around a primary function, perform\_sensitivity\_analysis(), which orchestrates the entire workflow. This function calls internal helpers for specific tasks, such as the regression\_analysis() function, which handles model fitting and the pooling of results. This modularity makes the code base easier to maintain and extend.  
* **Flexible Parameterization**: Users have control over key aspects of the analysis. They can specify the number of imputation draws (ndraws), define the health outcomes (targets) and covariates through a standard R formula (regression\_formula), and adjust the standard deviation of the lognormal distribution (epa\_lognormal\_sdlog) for the EPA data, allowing for tailored sensitivity analyses.  
* **Efficient Data Handling**: The package leverages the data.table package for fast and memory-efficient loading and processing of large datasets, which is crucial when working with national-scale health and environmental data.  
* **Statistically Robust Modeling**: gwArsenicR uses the lme4 package to fit linear mixed-effects models, allowing researchers to properly account for hierarchical data structures (e.g., individuals within counties, counties within states). The use of Amelia \[@Amelia-2011\] ensures that results from the multiple imputations are pooled correctly according to Rubin's Rules.

# **Research Enabled by gwArsenicR**

By simplifying a complex analytical workflow, gwArsenicR can accelerate and broaden the scope of research in environmental health. The package enables researchers to:

* **Rapidly Test Hypotheses**: Easily investigate the health effects of arsenic exposure on a wide range of outcomes, such as birth weight, gestational age, cancer incidence, or cardiovascular disease, using large, nationally representative datasets.  
* **Conduct Subgroup Analyses**: Modify the regression formula to explore whether associations differ across specific subpopulations (e.g., by race/ethnicity, socioeconomic status, or geographic region), which can help identify vulnerable groups.  
* **Inform Public Health Policy**: Generate robust evidence that can inform policy decisions, such as establishing state-level guidelines for private well testing or targeting public health interventions in high-risk areas.  
* **Serve as a Methodological Template**: The package's framework can be adapted to study other environmental contaminants where exposure data is uncertain and requires integration from multiple sources.

Ultimately, gwArsenicR empowers a wider research community to contribute to our understanding of the health risks posed by groundwater contaminants, fostering more timely and impactful science.

# Acknowledgments

This work was supported by the National Heart, Lung, and Blood Institute (R21HL159574) and funding from the United States Geological Survey’s John Wesley Powell Center for Analysis and Synthesis.

# References