# charls_memory_equating
This repo contains all the code necessary to replicate data construction and analyses in the paper: 

Wu Y, Zhang YS, Kobayashi LC, Mayeda ER, Gross AL. How to assess cognitive decline when test administration changes across study waves? Harmonizing cognitive scores across waves in the China Health and Retirement Longitudinal Study. *Journal of Alzheimer’s Disease Reports*. 2024;8(1):1661-1669. [doi:10.1177/25424823241302759](https://journals.sagepub.com/doi/10.1177/25424823241302759)

## Project Descriptions
We aimed to document the changes made to the word recall tests in the China Health and Retirement Longitudinal Study (CHARLS) and apply a test-equating approach to adjust for the test item nonequivalence that stems from different test administrations to generate comparable scores that facilitate longitudinal analysis.

* Dataset: the China Health and Retirement Longitudinal Study (CHARLS waves 2015, 2018, 2020)
* Measures of interest: Immediate word recall test scores and delayed word recall test scores
* Methods: We created a calibration sample balancing age, gender, and education to ensure consistent underlying test ability across waves. Within this sample, we used weighted equipercentile equating to crosswalk percentile ranks between the scores in the 2015 and 2018/2020 participants, then extended to the full study sample. We used the R package `equate`(Robitzsch, 2019) for the equipercentile method. Analyses were run using `R 4.3.1`.

## The crosswalk between original immediate and delayed word recall scores and equated scores in waves 2018 and 2020
**Note**: The equated scores should be applied to the **first trial** of the immediate word recall tests in waves 2018 and 2020. In these waves, the immediate recall tests were administered in three trials using the same word list with different word orders, whereas only one trial was conducted in waves 2011-2015. To ensure consistency, we performed the equating method on the first trial of the immediate word recall for waves 2018 and 2020.

|     Score        |     Original score    |     Wave 2018/2020 Equated score    |
|------------------|-----------------------|-------------------------------------|
|     Immediate    |     0                 |     0.2                             |
|                  |     1                 |     1.58                            |
|                  |     2                 |     2.51                            |
|                  |     3                 |     3.43                            |
|                  |     4                 |     4.39                            |
|                  |     5                 |     5.36                            |
|                  |     6                 |     6.37                            |
|                  |     7                 |     7.34                            |
|                  |     8                 |     8.32                            |
|                  |     9                 |     9.27                            |
|                  |     10                |     10.22                           |
|     Delayed      |     0                 |     -0.3                            |
|                  |     1                 |     -0.05                           |
|                  |     2                 |     0.1                             |
|                  |     3                 |     0.44                            |
|                  |     4                 |     1.76                            |
|                  |     5                 |     2.78                            |
|                  |     6                 |     3.68                            |
|                  |     7                 |     4.54                            |
|                  |     8                 |     5.41                            |
|                  |     9                 |     6.42                            |
|                  |     10                |     7.8                             |


## Scripts Descriptions
*R scripts are named in the order they should be run. Functions created by the authors were sourced where needed.*
* **1.dataset_export_equating_addw5.R**: Cleans the data and creates the necessary variable for the analysis.
* **2.equating_cog.R**: Creates the calibration group, generates weights for covariate balance and performs the equating to generate a comparable score.
* **3.tables_figures.R**: Contains code to create tables and figures in the results
* **functions_weight_dev.R**: Contains the functions for generating weights
* **functions.R**: Contains the functions for data construction

