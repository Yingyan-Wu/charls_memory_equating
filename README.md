# charls_memory_equating
This repo contains all the code necessary to replicate data construction and analyses in the paper: 

*How to assess cognitive decline when cognitive test administration procedures change across study waves? Harmonizing word recall scores across study waves in the China Health and Retirement Longitudinal Study*

## Project Descriptions
We aimed to document the changes made to the word recall tests in the China Health and Retirement Longitudinal Study (CHARLS) and apply a test-equating approach to adjust for the test item nonequivalence that stems from different test administrations to generate comparable scores that facilitate longitudinal analysis.

* Dataset: the China Health and Retirement Longitudinal Study (CHARLS waves 2015, 2018, 2020)
* Measures of interest: Immediate word recall test scores and delayed word recall test scores
* Methods: We created a calibration sample balancing age, gender, and education to ensure consistent underlying test ability across waves. Within this sample, we used weighted equipercentile equating to crosswalk percentile ranks between the scores in the 2015 and 2018 waves and 2015 and 2020 waves, then extended to the full study sample. We used R package `equate`(Robitzsch, 2019) for the equipercentile method. Analyses were run using `R 4.3.1`.

## The crosswalk between original immediate and delayed word recall scores and equated scores in waves 2018 and 2020
### Immediate word recall
|     Original score    |     Wave 2018 Equated score    |     Wave 2020 Equated score    |
|-----------------------|--------------------------------|--------------------------------|
|     0                 |     0.39                       |     0.03                       |
|     1                 |     1.79                       |     1.17                       |
|     2                 |     2.67                       |     2.17                       |
|     3                 |     3.62                       |     3.19                       |
|     4                 |     4.55                       |     4.24                       |
|     5                 |     5.43                       |     5.31                       |
|     6                 |     6.36                       |     6.38                       |
|     7                 |     7.28                       |     7.42                       |
|     8                 |     8.21                       |     8.47                       |
|     9                 |     9.16                       |     9.42                       |
|     10                |     10.27                      |     10.22                      |
### Delayed word recall
|     Original score    |     Wave 2018 Equated score    |     Wave 2020 Equated score    |
|-----------------------|--------------------------------|--------------------------------|
|     0                 |     -0.25                      |     -0.34                      |
|     1                 |     0.06                       |     -0.15                      |
|     2                 |     0.25                       |     -0.03                      |
|     3                 |     0.88                       |     0.27                       |
|     4                 |     2.03                       |     1.53                       |
|     5                 |     2.96                       |     2.61                       |
|     6                 |     3.83                       |     3.55                       |
|     7                 |     4.65                       |     4.45                       |
|     8                 |     5.46                       |     5.36                       |
|     9                 |     6.44                       |     6.39                       |
|     10                |     7.78                       |     7.83                       |


## Scripts Descriptions
*R scripts are named in the order they should be run. Functions created by the authors were sourced where needed.*
* **1.dataset_export_equating_addw5.R**: Cleans the data and creates the necessary variable for the analysis.
* **2.equating_cog_addw5.R**: Creates the calibration group, generates weights for covariate balance and performs the equating to generate a comparable score.
* **3.tables_figures.R**: Contains code to create tables and figures in the results
* **functions_weight_dev.R**: Contains the function for generating weights

