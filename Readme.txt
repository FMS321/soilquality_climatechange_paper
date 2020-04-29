Interactive effect of soil and climate on production

R Scripts to analysis interactive effects of soil and climate on China's crop production.

We used a data-driven approach based on Gradient Boosted Regression Tree (GBRT) algorithm to quantify the benefits of enhanced soil quality on crop yield and its variability for both current and future climates.

Three parts of data and scripts are present in this repository

1. Yield variations and biophysical causes

(1) Data availability

"Database.csv": Database comprised with total on-farm trials conducted in nine cropping systems of China
"W-NCP.csv": sub-database comprised with on-farm trials conducted in wheat system of North China Plain (NCP)
"W-YZB.csv": sub-database comprised with on-farm trials conducted in wheat system of Yangtze River Basin (YZB)
"W-NWC.csv": sub-database comprised with on-farm trials conducted in wheat system of Northwest China (NWC)
"M-NEC.csv": sub-database comprised with on-farm trials conducted in maize system of Northeast China (NEC)
"M-NCP.csv": sub-database comprised with on-farm trials conducted in maize systems of North China Plain (NCP)
"M-SWC.csv": sub-database comprised with on-farm trials conducted in maize systems of Southwest China (NCP)
"SR-YZB.csv": sub-database comprised with on-farm trials conducted in single rice system of Yangtze River Basin (YZB)
"ER-SC.csv": sub-database comprised with on-farm trials conducted in early rice system of South China (SC)
"LR-SC.csv": sub-database comprised with on-farm trials conducted in late rice system of South China (SC)

(2) R script

"GBRT model.R": R script to set up nine GBRT models of cropping systems, and explore bilphysical causes of yield variations using "gbm" and "caret" packages

2. Mean yield and yield variablity in high and low quality soils

(1) Data availability

"high and low soils.csv":  sub-database composed of locally paired on-farm trials with high- and low-quality soils in the same climatic conditions and with the same BMPs.

(2) R script

"mean yield and yield variablity.R": R script to compare mean yield and yield variablit between high- and low-quality soils, and the and the degree to which yield variability is explained by climate variability.

3. Predicted yield under climate change

3.1 Predicted yield for whole on-farm trials

(1) Data availability

"base_all.csv": Climate conditions in baseline period (1986-2005) for whole on-farm trials.
"RCP2.6_2050_all.csv": The projected climate conditions under RCP2.6 in 2050s period (2040-2059) for whole on-farm trials.
"RCP2.6_2100_all.csv": The projected climate conditions under RCP2.6 in 2090s period (2080-2099) for whole on-farm trials.
"RCP8.5_2050_all.csv": The projected climate conditions under RCP8.5 in 2050s period (2040-2059) for whole on-farm trials.
"RCP8.5_2100_all.csv": The projected climate conditions under RCP8.5 in 2090s period (2080-2099) for whole on-farm trials.

(2) R script

"GBRT model.R": R script to predict yield change under future climate change scenarios by GBRT models for whole on-farm trials.

3.2 Predicted yield for high and low soils

(1) Data availability

"base_new.csv": Climate conditions in baseline period (1986-2005) for paired on-farm trials with high- and low-quality soils.
"RCP2.6_2050.csv": The projected climate conditions under RCP2.6 in 2050s period (2040-2059) for paired on-farm trials with high- and low-quality soils.
"RCP2.6_2100.csv": The projected climate conditions under RCP2.6 in 2090s period (2080-2099) for paired on-farm trials with high- and low-quality soils.
"RCP8.5_2050.csv": The projected climate conditions under RCP8.5 in 2050s period (2040-2059) for paired on-farm trials with high- and low-quality soils.
"RCP8.5_2100.csv": The projected climate conditions under RCP8.5 in 2090s period (2080-2099) for paired on-farm trials with high- and low-quality soils.

(2) R script

"GBRT model.R": R script to predict yield change under future climate change scenarios by GBRT models for paired on-farm trials with high- and low-quality soils.

4 Production fluctuations derived from climate-soil interactions

(1) "RCP_production change_summary.xlsx": Summary of predicted absolut yield change (kg/ha) under different climate scenarios for nine cropping systems, and assessment of total production fluctuations derived from climate-soil interactions.

(2) "RCP_relative yield_change_summary.xlsx": Summary of predicted relative yield change (%) under different climate scenarios for nine cropping systems.

