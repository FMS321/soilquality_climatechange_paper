Code and data for "Soil quality both increases crop production and improves resilience to climate change"

R Scripts to analysis interactive effects of soil and climate on China's crop production.

Contributors: Lei Qiao, Yaojun Wang, Yuqing Ma, Mingsheng Fan* 
Contact details: qiaolei@caas.cn; Fanms@cau.edu.cn

We used a data-driven approach based on Gradient Boosted Regression Tree (GBRT) algorithm to quantify the benefits of enhanced soil quality on crop yield and its variability for both current and future climates.

Four parts of data and scripts are present in this repository

1. Yield variations and biophysical explanations

(1) Data availability

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

"GBRT model-random sample.R": R script to set up nine GBRT models of cropping systems, and explore bilphysical causes of yield variations using "gbm" and "caret" packages

2. Mean yield and yield variablity in high and low quality soils

(1) Data availability

"high and low soils.csv":  sub-database composed of locally paired on-farm trials with high- and low-quality soils in the same climatic conditions and with the same BMPs.

(2) R script

"mean yield and yield variablity.R": R script to compare mean yield and yield variablit between high- and low-quality soils, and the and the degree to which yield variability is explained by climate variability.

3. Predicted yield under climate change for high and low soils

(1) Data availability

"base_new_1.csv": Climate conditions in baseline period (1986-2005) for paired on-farm trials with high- and low-quality soils.
"RCP2.6_2050_1.csv": The projected climate conditions under RCP2.6 in 2050s period (2040-2059) for paired on-farm trials with high- and low-quality soils.
"RCP2.6_2100_1.csv": The projected climate conditions under RCP2.6 in 2090s period (2080-2099) for paired on-farm trials with high- and low-quality soils.
"RCP8.5_2050_1.csv": The projected climate conditions under RCP8.5 in 2050s period (2040-2059) for paired on-farm trials with high- and low-quality soils.
"RCP8.5_2100_1.csv": The projected climate conditions under RCP8.5 in 2090s period (2080-2099) for paired on-farm trials with high- and low-quality soils.

(2) R script

"Predicted yield under high and low quality soils.R": R script to predict yield change under future climate change scenarios by GBRT models for paired on-farm trials with high- and low-quality soils.

4. Production fluctuations derived from climate-soil interactions

(1) "RCP_production change_summary.xlsx": Summary of predicted absolut yield change (kg/ha) under different climate scenarios for nine cropping systems, and assessment of total production fluctuations derived from climate-soil interactions. The source data for Figure 3.

(2) "RCP_relative yield_change_summary.xlsx": Summary of predicted relative yield change (%) under different climate scenarios for nine cropping systems.

