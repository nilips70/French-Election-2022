# Exploring the Effects of Socioeconomic Factors on Voter Preferences: A Case-Study of France 2022
This repository contains the resources and code associated with our paper titled **"Exploring the Effects of Socioeconomic Factors on Voter Preferences: A Case-Study of France 2022"** authored by Niloufar Pourshir Sefidi, Amin Shoari Nejad, and Peter Mooney. The paper was presented at [GISRUK2023](https://gisruk.org/gisruk-2023/) and can be accessed through the following link: [Paper](https://mural.maynoothuniversity.ie/17135/). Additionally, a ShinyApp for exploring election data, the socioeconomic situation in each department, and the association between these variables and a candidate's vote has been developed and can be accessed at: [ShinyApp](https://nilips.shinyapps.io/app2/)

## Repository Structure
This repository is organized into two main folders:
1. **Data:** This folder contains the raw datasets used in the paper. These datasets are the foundation for your analysis and can be accessed for reference.
2. **Codes:** This folder contains the code used for data preparation and modelling. It includes the following files:

   - `data_preparation.R`: This R script is used to prepare the raw datasets for modelling. It performs data cleaning, transformation, and aggregation as necessary to create the final dataset for analysis.

   - `french_election_modeling.R`: This R script is where the final dataset is used for modelling. It contains the code for running statistical analyses, generating visualizations, and obtaining the results presented in the paper.
3. **ShinyApp:** The `app2.R` file is an R script that creates a Shiny app for visualizing the variables used in the paper, as well as the winner and each candidate's vote share across the French departments. It also shows the association between each candidate's votes and the mentioned variables.
   
## Getting Started

To replicate the findings presented in the paper or use the data for further analysis, follow these steps:

1. Clone this repository to your local machine using Git:

2. Navigate to the `Data` folder to access the raw datasets.

3. Open the `Codes` folder to find the R scripts:
   - Start with `data_preparation.R` to prepare the data for modelling.
   - Proceed to `french_election_modeling.R` for statistical modelling and result generation.


## Slides
**Presentation Slides**: The slides presented at the GISRUK2023 conference for this paper are available in this repository. You can access them in the file named `GISRUK2023-Pourshir-Sefidi-Mooney.pdf`.
