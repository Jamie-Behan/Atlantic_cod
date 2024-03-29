# Atlantic cod

  <img align="right" src="https://user-images.githubusercontent.com/62613926/204640321-e351b50a-f6ee-4bdc-ac44-9687318fb916.png" width="400" alt="My Image">
  
### To help identify ecosystem and climate influences on Atlantic cod recruitment, distribution, and growth, generalized additive models (GAMs) were used to examine associations between Atlantic cod population dynamics and environmental variables in the Northwest Atlantic region.

  <br>
  
### **Included in this Repository is the following:**
###   **1. "Figures" folder containing .png images of supplemental figures not included in technical report:**

###   **2. "Code" folder containing R code used:**

| File | Description |
| ----------- | ----------- |
|**changepoint.R**|Chanepoint analysis run on recruitment and growth data using EnvCpt package|
|**cod_distribution.R**|.R file used for to achieve distribution GAM results|
|**cod_growth.R**|.R file used for to achieve growth GAM results|
|**cod_recruitment_NEFSC.R**|.R file used for to achieve recruitment GAM results using NEFSC data|
|**DisMAP_vs_adultjuv_raw.R**| .R used to explore possible differences between [DisMAP data](https://apps-st.fisheries.noaa.gov/dismap/DisMAP.html) and NEFSC distribution data disaggregated by adults and juveniles to explore potential environmental effects by maturity stage|        
|**Env_data_by_stock.R**| .R file used to pull relevant environmental data from [ecodata package](https://github.com/NOAA-EDAB/ecodata) in R, and wrangle data into consistent format to be read in other .r files. Originally used but later replaced by "Environmental_data2.R" file|
|**Environmental_data2.R**| New environmental data with updated lags and bottom temperature data. This was used to wrangle final environmental datasets used in models|
|**Envdata2_MADMF.R**| Environmental data wrangling used in MADMF models in the "WGOM_R_MADMF.R" script |
|**GAM_forloop.R**|.R file containing functions used to to fun multiple GAM ieterations in effort to streamline best model selection.|
|**Gam_data_exploration.R**|.R file containing functions used to check data assumptions and explore data distributions|
|**get_WAA_anomalies.R**|.R file used for to get weight at age (WAA) anomaly data used in growth analyses|
|**get_depth_lat.R**|.R file used get mean depth and latitude data, disaggregated by adults and juvenile groups<ul><li>Data generated were used in the "DisMAP_vs_adultjuv_raw.R" exploratory analyses</li>|
|**Recruitment_other_data.R**|exploratory .R file testing GAM recruitment relationships using MENH and DFO data indstead of NEFSC <ul><li>This was done due to speculation that NEFSC data may not represent age 1 data as well as other surveys</li>|  
|**SSB_estimates.R**|.R file estimating SSB using WAA and NAA data form NEFSC trawl survey|
|**SSB_estimates_other.R**|exploratory .R file estimating SSB using WAA from NEFSC and NAA data form MENH and DFO surveys. *lacks overlap|
|**WGOM_R_MADMF.R**|.R file used to run models using age 0 MADMF recruitment data in the WGOM to compare with results from NEFSC age 1 recruitment data.|

###   **3. "Final_Data_for_Modelers.zip" zip folder containing data that went into GAM analyses:**
 * ##### The environmental data can also be used by modelers working within the assessment. However, PLEASE note the **environmental data have ALREADY BEEN LAGGED**

#### Any further questions can be directed to Jamie Behan: jbehan@gmri.org
