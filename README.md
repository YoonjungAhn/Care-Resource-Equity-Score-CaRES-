# Care-Resource-Equity-Score-CaRES-

This repository contains code used to compute and visualize measures of inequality and clustering in the distribution of formal care labor resources across U.S. counties. It includes scripts to calculate the **Care Resource Equity Score (CaRES)** using the Gini coefficient and Location Quotient (LQ), and to conduct Local Indicators of Spatial Association (LISA) analysis.

## Files and Descriptions

### `gini_coefficient.Rmd`
This script:
- Loads county-level care labor data.
- Computes the **Gini coefficient** to measure inequality in the distribution of formal care labor relative to population.
- Produces plots and maps showing geographic disparities in care resource concentration.

### `gini_LQ_score.Rmd`
This script:
- Computes the **Location Quotient (LQ)** for each county, indicating how concentrated care employment is relative to the national average.
- Combines the LQ and Gini scores to generate the **CaRES Score**, which jointly reflects spatial concentration and relative employment intensity in the care economy.
- Outputs a typology of counties based on high/low Gini and high/low LQ values.

### `UnivariateLISA.Rmd`
This script:
- Performs **Univariate Local Moran’s I (LISA)** analysis on the CaRES Score or related variables to detect spatial clusters (e.g., high-high, low-low).
- Maps areas with statistically significant local clustering of care inequality or concentration.
- Helps identify regions that may require policy attention or intervention.

## Data

Please ensure you have the following files in your working directory:
- `ruralurbancodes2013.csv`: USDA rural-urban classification codes.
- `your_data_file.csv`: Replace with your specific care labor or population dataset as needed.

## Dependencies

The scripts require the following R packages:
- `tidyverse`
- `sf`
- `tmap`
- `spdep`
- `ineq`
- `readr`
- `dplyr`
- `ggplot2`

Install missing packages using:

```r
install.packages(c("tidyverse", "sf", "tmap", "spdep", "ineq"))
