# charls_memory_equating
This repo contains all the code necessary to replicate data construction and analyses in the paper: 

*How to assess cognitive decline when cognitive test administration procedures change across study waves? Harmonizing word recall scores across study waves in the China Health and Retirement Longitudinal Study*

## Project Descriptions
We aimed to document the changes made to the word recall tests in the China Health and Retirement Longitudinal Study (CHARLS) and apply a test-equating approach to adjust for the test item nonequivalence that stems from different test administrations to generate comparable scores that facilitate longitudinal analysis.

* Dataset: the China Health and Retirement Longitudinal Study (CHARLS waves 2015, 2018, 2020)
* Measures of interest: Immediate word recall test scores and delayed word recall test scores
* Methods: We created a calibration sample balancing age, gender, and education to ensure consistent underlying test ability across waves. Within this sample, we used weighted equipercentile equating to crosswalk percentile ranks between the scores in the 2015 and 2018 waves and 2015 and 2020 waves, then extended to the full study sample. We used R package `equate`(Robitzsch, 2019) for the equipercentile method. Analyses were run using `R 4.3.1`.

## Scripts Descriptions
*R scripts are named in the order they should be run. Functions created by the authors were sourced where needed.*
* **1.dataset_export_equating_addw5.R**: Cleans the data and creates the necessary variable for the analysis.
* **2.equating_cog_addw5.R**: Creates the calibration group, generates weights for covariate balance and performs the equating to generate a comparable score.
* **3.tables_figures.R**: Contains code to create tables and figures in the results
* **functions_weight_dev.R**: Contains the function for generating weights


