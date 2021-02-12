# Time Series Stochastic Causal Estimands: Assessing Heat Alert Effectiveness in Reducing Morbidity and Mortality
This is the data repository for public available code and data to reproduce analyses in Wu, X., Weinberger, K. R., Wellenius, G. A., Braun, D. and Dominici, F., 2020. Time Series Stochastic Causal Estimands: Assessing Heat Alert Effectiveness in Reducing Morbidity and Mortality

<b>Simulation Code: </b><br>
[`ts_ips_fun2.R`](https://github.com/wxwx1993/TS_Incremental/blob/main/Simulation/ts_ips_fun2.R) contains the functions to generate simulated data sets under each scenario and estimate the proposed estimator.

[`FAS folder`](https://github.com/wxwx1993/TS_Incremental/tree/main/Simulation/FAS) contains all the necessary implementation code to run all simulation replicates at the Odyssey cluster, supported by the FAS Division of Science, Research Computing Group at Harvard University.

[`Table1.R`](https://github.com/wxwx1993/TS_Incremental/blob/main/Simulation/Table1.R) contains the code to reproduce Table 1 in the manuscript.

[`Figure2.R`](https://github.com/wxwx1993/TS_Incremental/blob/main/Simulation/Figure2.R) contains the code to reproduce Figure 2 in the manuscript.

<b>Application Code: </b><br>
[`ts_ipw_function.R`](https://github.com/wxwx1993/TS_Incremental/blob/main/Application/ts_ipw_function.R) contains the function to estimate the proposed estimator.

[`RCE folder`](https://github.com/wxwx1993/TS_Incremental/blob/main/Application/RCE) contains the function to estimate the proposed estimator. contains all the necessary implementation code to analyze the real data to estimate the causal effect curves among Medicare population in each of 2817 US counties at the Level-3 secured data platform on Research Computing Environment, supported by the Institute for Quantitative Social Science in the Faculty of Arts and Sciences at Harvard University.

[`meta.R`](https://github.com/wxwx1993/TS_Incremental/blob/main/Application/meta.R) contains the code to run random-effect meta-analysis that pooled causal effect cruve from 2817 US counties.

[`Table2.R`](https://github.com/wxwx1993/TS_Incremental/blob/main/Application/Table2.R) contains the code to reproduce Table 2 in the manuscript.

[`Figure3.R`](https://github.com/wxwx1993/TS_Incremental/blob/main/Application/Figure3.R) contains the code to reproduce Figure 3 in the manuscript.

[`Figure4.R`](https://github.com/wxwx1993/TS_Incremental/blob/main/Application/Figure4.R) contains the code to reproduce Figure 4 in the manuscript.

[`Figure5.R`](https://github.com/wxwx1993/TS_Incremental/blob/main/Application/Figure5.R) contains the code to reproduce Figure 5 in the manuscript.


<b>Data: </b><br>
All data needed to evaluate the conclusions in the paper are present in the paper and/or the Supplementary Materials. Medicare patient individual-level data are stored at a Level-3 secured data platform on Research Computing Environment, supported by the Institute for Quantitative Social Science in the Faculty of Arts and Sciences at Harvard University. Those interested in the original data can contact the corresponding author.

Heat Alert: We gathered text files containing records of all non-precipitation alerts issued by NWS between 2006 and 2016 from the National Oceanic and Atmospheric Administration (NOAA). We used the information in the file header, which contains information on the type, location, and timing of each alert in a standard format, to identify the date and location of each heat alert issued between April 1st and October 31st for the years 2006 to 2016 in the contiguous US. We then created a daily time series containing a binary variable for the issuance of heat alerts for each of the 2817 US counties. We defined “heat alerts” to include both heat advisories (a type of heat alert issued when less severe heat is forecast) and excessive heat warnings (a type of heat alert issued when more severe heat is forecast).

Heat Index: We obtained 4-km gridded estimates of daily maximum temperature and vapor-pressure deficit using the Parameter-elevation Regressions on Independent Slopes Model (PRISM). From these variables, the time series of population-weighted daily maximum heat index is calculated for each county as previously described by Spangler et al, 2019. 

Medicare Outcome: We obtained daily all-cause deaths among the entire Medicare enrollees and cause-specific hospitalizations for five heat-related diseases (heat stroke, urinary tract infections, septicemia, renal failure, fluid and electrolyte disorders) using Clinical Classifications Software (CCS) groupings of principal discharge diagnosis codes among the Medicare Fee-for-Service (FFS) enrollees.
