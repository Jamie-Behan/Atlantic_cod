# Atlantic cod

### To help identify ecosystem and climate influences on Atlantic cod recruitment, distribution, and growth, generalized additive models (GAMs) were used to examine associations between Atlantic cod population dynamics and environmental variables in the Northwest Atlantic region.
 
  <img align="right" src="C:/Users/jbehan/Stock_Assessments/Atlantic_cod/Figures/mediamodifier_cropped_image.png" width="250" alt="My Image">

### **Included in this Repository is the following:**

###   **1. "Code" folder containing R code used:**
  
- "**DisMAP_vs_adultjuv_raw.R**" - .R used to explre possible differences between DisMAP data and NEFSC distribution data disaggregated by adults and juveniles to explore potential environmental effects by maturity stage.
        
- "**Env_data_by_stock.R**" - .R file used to pull relevant environemntaldata from ecodata package in R, and wrangle data into consistent format to be read in other .r files.

- "**Gam_data_exploration.R**" - .R file containing functions used to check data assumptions and explore data distributions

- "**SSB_estimates.R**" - .R file estimating SSB using WAA and NAA data form NEFSC trawl survey.

- "**SSB_estimates_other.R**" - exploratory .R file estimating SSB using WAA from NEFSC and NAA data form MENH and DFO surveys *lacks overlap

- "**cod_distribution.R**" - .R file used for to achieve distribution GAM results

- "**cod_growth.R**" - .R file used for to achieve growth GAM results

- "**cod_recruitment.R**" - old .R file used for recruitment. updated results are in "cod_recrusitment_NEFSC.R"

- "**cod_recruitment_NEFSC.R**" - .R file used for to achieve recruitment GAM results using NEFSC data.

- "**get_WAA_anomalies.R**" - .R file used for to get weight at age (WAA) anomaly data used in growth analyses.

- "**get_depth_lat.R**" - .R file used get mean depth and latitude data, disaggregated by adults and juvenile groups.
      - data generated were used in the "DisMAP_vs_adultjuv_raw.R" exploratory analyses.

#### Any further questions can be directed to Jamie Behan: jbehan@gmri.org
