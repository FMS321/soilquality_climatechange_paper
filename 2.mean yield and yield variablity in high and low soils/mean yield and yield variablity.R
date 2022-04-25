# =========================================================
# Title: Soil quality both impact crop production and climate resilience
#
# Author details: Author: Lei Qiao 
# Contact details: qiaolei@caas.cn
#
#Objective: Comparison of Mean yield and yield variability (CV) between high and low quality soils 
#           and the degree to which yield variability is explained by climate variability
#           
#========================================================== 
# Removing objects from the envrionment
rm(list = ls())
#==========================================================
# Loading library
library(dplyr)
library(plyr)
library(caret)
library(gbm)
#==========================================================
# loading functions
# coefficient of variation calculated by dividing mean yield by standard deviation
cv <- function(x) sd(x)/mean(x)

#==========================================================
# loading files
yield_data<-read.csv("high and low soils.csv")
W_NCP_high<-subset(yield_data,Crop=="north wheat"& Soil.quality=="high")
W_NCP_low<-subset(yield_data,Crop=="north wheat"& Soil.quality=="low")
W_YZB_high<-subset(yield_data,Crop=="south wheat"& Soil.quality=="high")
W_YZB_low<-subset(yield_data,Crop=="south wheat"& Soil.quality=="low")
W_NWC_high<-subset(yield_data,Crop=="northwest wheat"& Soil.quality=="high")
W_NWC_low<-subset(yield_data,Crop=="northwest wheat"& Soil.quality=="low")
M_NEC_high<-subset(yield_data,Crop=="northeast maize"& Soil.quality=="high")
M_NEC_low<-subset(yield_data,Crop=="northeast maize"& Soil.quality=="low")
M_NCP_high<-subset(yield_data,Crop=="north maize"& Soil.quality=="high")
M_NCP_low<-subset(yield_data,Crop=="north maize"& Soil.quality=="low")
M_SWC_high<-subset(yield_data,Crop=="south maize"& Soil.quality=="high")
M_SWC_low<-subset(yield_data,Crop=="south maize"& Soil.quality=="low")
SR_YZB_high<-subset(yield_data,Crop=="single rice"& Soil.quality=="high")
SR_YZB_low<-subset(yield_data,Crop=="single rice"& Soil.quality=="low")
ER_SC_high<-subset(yield_data,Crop=="early rice"& Soil.quality=="high")
ER_SC_low<-subset(yield_data,Crop=="early rice"& Soil.quality=="low")
LR_SC_high<-subset(yield_data,Crop=="late rice"& Soil.quality=="high")
LR_SC_low<-subset(yield_data,Crop=="late rice"& Soil.quality=="low")

#==========================================================
# mean, sd and cv of yield in high and low quality soils
yield_data$Crop<-factor(yield_data$Crop,
                          levels = c("north wheat","south wheat",
                                     "northwest wheat","northeast maize","north maize",
                                     "south maize","single rice","early rice","late rice"))
df.table1<-data.frame(ddply(yield_data,.(Crop,Soil.quality),summarize,
                 N = round(sum(n.site,na.rm = T)),
                 Yield_mean = round(mean(Ynpk,na.rm = T)/1000,2),
                 Yield_SD = round(sd(Ynpk,na.rm = T)/1000,2),
                 Yield_cv = round(cv(Ynpk)*100,2)))
print(df.table1)

# mean, sd of soil properties in high and low quality soils
df.table2<-data.frame(ddply(yield_data,.(Crop,Soil.quality),summarize,
                            N = round(sum(n.site,na.rm = T)),
                            SOM_mean = round(mean(SOM,na.rm = T),2),
                            SOM_SD = round(sd(SOM,na.rm = T),2),
                            OP_mean = round(mean(OP,na.rm = T),2),
                            OP_SD = round(sd(OP,na.rm = T),2),
                            AK_mean = round(mean(AK,na.rm = T),2),
                            AK_SD = round(sd(AK,na.rm = T),2),
                            pH_mean = round(mean(pH,na.rm = T),2),
                            pH_SD = round(sd(pH,na.rm = T),2)))
print(df.table2)
write.csv(df.table2,"df.table2.csv")
#==========================================================
#T test for mean yield in high and low quality soils
#W-NCP
t.test(Ynpk~Soil.quality,subset(yield_data,Crop=="north wheat"))
#W-YZB
t.test(Ynpk~Soil.quality,subset(yield_data,Crop=="south wheat"))
#W-NWC
t.test(Ynpk~Soil.quality,subset(yield_data,Crop=="northwest wheat"))
#M-NEC
t.test(Ynpk~Soil.quality,subset(yield_data,Crop=="northeast maize"))
#M-NCP
t.test(Ynpk~Soil.quality,subset(yield_data,Crop=="north maize"))
#M-SWC
t.test(Ynpk~Soil.quality,subset(yield_data,Crop=="south maize"))
#SR-YZB
t.test(Ynpk~Soil.quality,subset(yield_data,Crop=="single rice"))
#ER-SC
t.test(Ynpk~Soil.quality,subset(yield_data,Crop=="early rice"))
#LR-SC
t.test(Ynpk~Soil.quality,subset(yield_data,Crop=="late rice"))

#==========================================================
#T test for soil properties in high and low quality soils
#SOM
#W-NCP
t.test(SOM~Soil.quality,subset(yield_data,Crop=="north wheat"))
#W-YZB
t.test(SOM~Soil.quality,subset(yield_data,Crop=="south wheat"))
#W-NWC
t.test(SOM~Soil.quality,subset(yield_data,Crop=="northwest wheat"))
#M-NEC
t.test(SOM~Soil.quality,subset(yield_data,Crop=="northeast maize"))
#M-NCP
t.test(SOM~Soil.quality,subset(yield_data,Crop=="north maize"))
#M-SWC
t.test(SOM~Soil.quality,subset(yield_data,Crop=="south maize"))
#SR-YZB
t.test(SOM~Soil.quality,subset(yield_data,Crop=="single rice"))
#ER-SC
t.test(SOM~Soil.quality,subset(yield_data,Crop=="early rice"))
#LR-SC
t.test(SOM~Soil.quality,subset(yield_data,Crop=="late rice"))

#OP
#W-NCP
t.test(OP~Soil.quality,subset(yield_data,Crop=="north wheat"))
#W-YZB
t.test(OP~Soil.quality,subset(yield_data,Crop=="south wheat"))
#W-NWC
t.test(OP~Soil.quality,subset(yield_data,Crop=="northwest wheat"))
#M-NEC
t.test(OP~Soil.quality,subset(yield_data,Crop=="northeast maize"))
#M-NCP
t.test(OP~Soil.quality,subset(yield_data,Crop=="north maize"))
#M-SWC
t.test(OP~Soil.quality,subset(yield_data,Crop=="south maize"))
#SR-YZB
t.test(OP~Soil.quality,subset(yield_data,Crop=="single rice"))
#ER-SC
t.test(OP~Soil.quality,subset(yield_data,Crop=="early rice"))
#LR-SC
t.test(OP~Soil.quality,subset(yield_data,Crop=="late rice"))

#AK
#W-NCP
t.test(AK~Soil.quality,subset(yield_data,Crop=="north wheat"))
#W-YZB
t.test(AK~Soil.quality,subset(yield_data,Crop=="south wheat"))
#W-NWC
t.test(AK~Soil.quality,subset(yield_data,Crop=="northwest wheat"))
#M-NEC
t.test(AK~Soil.quality,subset(yield_data,Crop=="northeast maize"))
#M-NCP
t.test(AK~Soil.quality,subset(yield_data,Crop=="north maize"))
#M-SWC
t.test(AK~Soil.quality,subset(yield_data,Crop=="south maize"))
#SR-YZB
t.test(AK~Soil.quality,subset(yield_data,Crop=="single rice"))
#ER-SC
t.test(AK~Soil.quality,subset(yield_data,Crop=="early rice"))
#LR-SC
t.test(AK~Soil.quality,subset(yield_data,Crop=="late rice"))

#pH
#W-NCP
t.test(pH~Soil.quality,subset(yield_data,Crop=="north wheat"))
#W-YZB
t.test(pH~Soil.quality,subset(yield_data,Crop=="south wheat"))
#W-NWC
t.test(pH~Soil.quality,subset(yield_data,Crop=="northwest wheat"))
#M-NEC
t.test(pH~Soil.quality,subset(yield_data,Crop=="northeast maize"))
#M-NCP
t.test(pH~Soil.quality,subset(yield_data,Crop=="north maize"))
#M-SWC
t.test(pH~Soil.quality,subset(yield_data,Crop=="south maize"))
#SR-YZB
t.test(pH~Soil.quality,subset(yield_data,Crop=="single rice"))
#ER-SC
t.test(pH~Soil.quality,subset(yield_data,Crop=="early rice"))
#LR-SC
t.test(pH~Soil.quality,subset(yield_data,Crop=="late rice"))

#==========================================================
###T test for sd and cv in high and low quality soils conbined with bootstrap
#######W-NCP
###bootstrap
###high soil
W_NCP_high_vector<-as.vector(as.matrix(W_NCP_high$Ynpk))
set.seed(100)
sample(W_NCP_high_vector,330,replace=T)
sd_W_NCP_high=sd(sample(W_NCP_high_vector,330,replace=T))
set.seed(100)
for (i in 2:1000) {a=sd(sample(W_NCP_high_vector,330,replace=T))
sd_W_NCP_high=c(sd_W_NCP_high,a)}
sort(sd_W_NCP_high)

set.seed(100)
cv_W_NCP_high=cv(sample(W_NCP_high_vector,330,replace=T))
set.seed(100)
for (i in 2:1000) {b=cv(sample(W_NCP_high_vector,330,replace=T))
cv_W_NCP_high=c(cv_W_NCP_high,b)}
sort(cv_W_NCP_high)

SQ_WNCP_high<-rep("high",1000)
sd_W_NCP_high_fram<-data.frame(SQ_WNCP_high,sd_W_NCP_high,cv_W_NCP_high)
names(sd_W_NCP_high_fram)<-c("SQ","SD","CV")
###low soil
W_NCP_low_vector<-as.vector(as.matrix(W_NCP_low$Ynpk))
set.seed(100)
sample(W_NCP_low_vector,332,replace=T)
sd_W_NCP_low=sd(sample(W_NCP_low_vector,332,replace=T))
set.seed(100)
for (i in 2:1000) {a=sd(sample(W_NCP_low_vector,332,replace=T))
sd_W_NCP_low=c(sd_W_NCP_low,a)}
sort(sd_W_NCP_low)

set.seed(100)
cv_W_NCP_low=cv(sample(W_NCP_low_vector,332,replace=T))
set.seed(100)
for (i in 2:1000) {b=cv(sample(W_NCP_low_vector,332,replace=T))
cv_W_NCP_low=c(cv_W_NCP_low,b)}
sort(cv_W_NCP_low)

SQ_WNCP_low<-rep("low",1000)
sd_W_NCP_low_fram<-data.frame(SQ_WNCP_low,sd_W_NCP_low,cv_W_NCP_low)
names(sd_W_NCP_low_fram)<-c("SQ","SD","CV")
###T.Test
sd_W_NCP<-rbind(sd_W_NCP_high_fram,sd_W_NCP_low_fram)
t.test(SD~SQ,sd_W_NCP)
t.test(CV~SQ,sd_W_NCP)

#######W_YZB
###bootstrap
###high soil
W_YZB_high_vector<-as.vector(as.matrix(W_YZB_high$Ynpk))
set.seed(100)
sample(W_YZB_high_vector,158,replace=T)
sd_W_YZB_high=sd(sample(W_YZB_high_vector,158,replace=T))
set.seed(100)
for (i in 2:1000) {a=sd(sample(W_YZB_high_vector,158,replace=T))
sd_W_YZB_high=c(sd_W_YZB_high,a)}
sort(sd_W_YZB_high)

set.seed(100)
cv_W_YZB_high=cv(sample(W_YZB_high_vector,158,replace=T))
set.seed(100)
for (i in 2:1000) {b=cv(sample(W_YZB_high_vector,158,replace=T))
cv_W_YZB_high=c(cv_W_YZB_high,b)}
sort(cv_W_YZB_high)

SQ_WYZB_high<-rep("high",1000)
sd_W_YZB_high_fram<-data.frame(SQ_WYZB_high,sd_W_YZB_high,cv_W_YZB_high)
names(sd_W_YZB_high_fram)<-c("SQ","SD","CV")
###low soil
W_YZB_low_vector<-as.vector(as.matrix(W_YZB_low$Ynpk))
set.seed(100)
sample(W_YZB_low_vector,160,replace=T)
sd_W_YZB_low=sd(sample(W_YZB_low_vector,160,replace=T))
set.seed(100)
for (i in 2:1000) {a=sd(sample(W_YZB_low_vector,160,replace=T))
sd_W_YZB_low=c(sd_W_YZB_low,a)}
sort(sd_W_YZB_low)

set.seed(100)
cv_W_YZB_low=cv(sample(W_YZB_low_vector,160,replace=T))
set.seed(100)
for (i in 2:1000) {b=cv(sample(W_YZB_low_vector,160,replace=T))
cv_W_YZB_low=c(cv_W_YZB_low,b)}
sort(cv_W_YZB_low)

SQ_WYZB_low<-rep("low",1000)
sd_W_YZB_low_fram<-data.frame(SQ_WYZB_low,sd_W_YZB_low,cv_W_YZB_low)
names(sd_W_YZB_low_fram)<-c("SQ","SD","CV")
###T.Test
sd_W_YZB<-rbind(sd_W_YZB_high_fram,sd_W_YZB_low_fram)
t.test(SD~SQ,sd_W_YZB)
t.test(CV~SQ,sd_W_YZB)

#######W-NWC
###high soil
W_NWC_high_vector<-as.vector(as.matrix(W_NWC_high$Ynpk))
set.seed(100)
sample(W_NWC_high_vector,106,replace=T)
sd_W_NWC_high=sd(sample(W_NWC_high_vector,106,replace=T))
set.seed(100)
for (i in 2:1000) {a=sd(sample(W_NWC_high_vector,106,replace=T))
sd_W_NWC_high=c(sd_W_NWC_high,a)}
sort(sd_W_NWC_high)

set.seed(100)
cv_W_NWC_high=cv(sample(W_NWC_high_vector,106,replace=T))
set.seed(100)
for (i in 2:1000) {b=cv(sample(W_NWC_high_vector,106,replace=T))
cv_W_NWC_high=c(cv_W_NWC_high,b)}
sort(cv_W_NWC_high)

SQ_WNWC_high<-rep("high",1000)
sd_W_NWC_high_fram<-data.frame(SQ_WNWC_high,sd_W_NWC_high,cv_W_NWC_high)
names(sd_W_NWC_high_fram)<-c("SQ","SD","CV")
###low soil
W_NWC_low_vector<-as.vector(as.matrix(W_NWC_low$Ynpk))
set.seed(100)
sample(W_NWC_low_vector,71,replace=T)
sd_W_NWC_low=sd(sample(W_NWC_low_vector,71,replace=T))
set.seed(100)
for (i in 2:1000) {a=sd(sample(W_NWC_low_vector,71,replace=T))
sd_W_NWC_low=c(sd_W_NWC_low,a)}
sort(sd_W_NWC_low)

set.seed(100)
cv_W_NWC_low=cv(sample(W_NWC_low_vector,71,replace=T))
set.seed(100)
for (i in 2:1000) {b=cv(sample(W_NWC_low_vector,71,replace=T))
cv_W_NWC_low=c(cv_W_NWC_low,b)}
sort(cv_W_NWC_low)

SQ_WNWC_low<-rep("low",1000)
sd_W_NWC_low_fram<-data.frame(SQ_WNWC_low,sd_W_NWC_low,cv_W_NWC_low)
names(sd_W_NWC_low_fram)<-c("SQ","SD","CV")
###T.Test
sd_W_NWC<-rbind(sd_W_NWC_high_fram,sd_W_NWC_low_fram)
t.test(SD~SQ,sd_W_NWC)
t.test(CV~SQ,sd_W_NWC)

#######M-NEC
###high soil
M_NEC_high_vector<-as.vector(as.matrix(M_NEC_high$Ynpk))
set.seed(100)
sample(M_NEC_high_vector,104,replace=T)
sd_M_NEC_high=sd(sample(M_NEC_high_vector,104,replace=T))
set.seed(100)
for (i in 2:1000) {a=sd(sample(M_NEC_high_vector,104,replace=T))
sd_M_NEC_high=c(sd_M_NEC_high,a)}
sort(sd_M_NEC_high)

set.seed(100)
cv_M_NEC_high=cv(sample(M_NEC_high_vector,104,replace=T))
set.seed(100)
for (i in 2:1000) {b=cv(sample(M_NEC_high_vector,104,replace=T))
cv_M_NEC_high=c(cv_M_NEC_high,b)}
sort(cv_M_NEC_high)

SQ_MNEC_high<-rep("high",1000)
sd_M_NEC_high_fram<-data.frame(SQ_MNEC_high,sd_M_NEC_high,cv_M_NEC_high)
names(sd_M_NEC_high_fram)<-c("SQ","SD","CV")
###low soil
M_NEC_low_vector<-as.vector(as.matrix(M_NEC_low$Ynpk))
set.seed(100)
sample(M_NEC_low_vector,97,replace=T)
sd_M_NEC_low=sd(sample(M_NEC_low_vector,97,replace=T))
set.seed(100)
for (i in 2:1000) {a=sd(sample(M_NEC_low_vector,97,replace=T))
sd_M_NEC_low=c(sd_M_NEC_low,a)}
sort(sd_M_NEC_low)

set.seed(100)
cv_M_NEC_low=cv(sample(M_NEC_low_vector,97,replace=T))
set.seed(100)
for (i in 2:1000) {b=cv(sample(M_NEC_low_vector,97,replace=T))
cv_M_NEC_low=c(cv_M_NEC_low,b)}
sort(cv_M_NEC_low)

SQ_MNEC_low<-rep("low",1000)
sd_M_NEC_low_fram<-data.frame(SQ_MNEC_low,sd_M_NEC_low,cv_M_NEC_low)
names(sd_M_NEC_low_fram)<-c("SQ","SD","CV")
###T.Test
sd_M_NEC<-rbind(sd_M_NEC_high_fram,sd_M_NEC_low_fram)
t.test(SD~SQ,sd_M_NEC)
t.test(CV~SQ,sd_M_NEC)

#######M-NCP
###high soil
M_NCP_high_vector<-as.vector(as.matrix(M_NCP_high$Ynpk))
set.seed(100)
sample(M_NCP_high_vector,183,replace=T)
sd_M_NCP_high=sd(sample(M_NCP_high_vector,183,replace=T))
set.seed(100)
for (i in 2:1000) {a=sd(sample(M_NCP_high_vector,183,replace=T))
sd_M_NCP_high=c(sd_M_NCP_high,a)}
sort(sd_M_NCP_high)

set.seed(100)
cv_M_NCP_high=cv(sample(M_NCP_high_vector,183,replace=T))
set.seed(100)
for (i in 2:1000) {b=cv(sample(M_NCP_high_vector,183,replace=T))
cv_M_NCP_high=c(cv_M_NCP_high,b)}
sort(cv_M_NCP_high)

SQ_MNCP_high<-rep("high",1000)
sd_M_NCP_high_fram<-data.frame(SQ_MNCP_high,sd_M_NCP_high,cv_M_NCP_high)
names(sd_M_NCP_high_fram)<-c("SQ","SD","CV")
###low soil
M_NCP_low_vector<-as.vector(as.matrix(M_NCP_low$Ynpk))
set.seed(100)
sample(M_NCP_low_vector,180,replace=T)
sd_M_NCP_low=sd(sample(M_NCP_low_vector,180,replace=T))
set.seed(100)
for (i in 2:1000) {a=sd(sample(M_NCP_low_vector,180,replace=T))
sd_M_NCP_low=c(sd_M_NCP_low,a)}
sort(sd_M_NCP_low)

set.seed(100)
cv_M_NCP_low=cv(sample(M_NCP_low_vector,180,replace=T))
set.seed(100)
for (i in 2:1000) {b=cv(sample(M_NCP_low_vector,180,replace=T))
cv_M_NCP_low=c(cv_M_NCP_low,b)}
sort(cv_M_NCP_low)

SQ_MNCP_low<-rep("low",1000)
sd_M_NCP_low_fram<-data.frame(SQ_MNCP_low,sd_M_NCP_low,cv_M_NCP_low)
names(sd_M_NCP_low_fram)<-c("SQ","SD","CV")
###T.Test
sd_M_NCP<-rbind(sd_M_NCP_high_fram,sd_M_NCP_low_fram)
t.test(SD~SQ,sd_M_NCP)
t.test(CV~SQ,sd_M_NCP)

#######M-SWC
###high soil
M_SWC_high_vector<-as.vector(as.matrix(M_SWC_high$Ynpk))
set.seed(100)
sample(M_SWC_high_vector,136,replace=T)
sd_M_SWC_high=sd(sample(M_SWC_high_vector,136,replace=T))
set.seed(100)
for (i in 2:1000) {a=sd(sample(M_SWC_high_vector,136,replace=T))
sd_M_SWC_high=c(sd_M_SWC_high,a)}
sort(sd_M_SWC_high)

set.seed(100)
cv_M_SWC_high=cv(sample(M_SWC_high_vector,136,replace=T))
set.seed(100)
for (i in 2:1000) {b=cv(sample(M_SWC_high_vector,136,replace=T))
cv_M_SWC_high=c(cv_M_SWC_high,b)}
sort(cv_M_SWC_high)

SQ_MSWC_high<-rep("high",1000)
sd_M_SWC_high_fram<-data.frame(SQ_MSWC_high,sd_M_SWC_high,cv_M_SWC_high)
names(sd_M_SWC_high_fram)<-c("SQ","SD","CV")
###low soil
M_SWC_low_vector<-as.vector(as.matrix(M_SWC_low$Ynpk))
set.seed(100)
sample(M_SWC_low_vector,134,replace=T)
sd_M_SWC_low=sd(sample(M_SWC_low_vector,134,replace=T))
set.seed(100)
for (i in 2:1000) {a=sd(sample(M_SWC_low_vector,134,replace=T))
sd_M_SWC_low=c(sd_M_SWC_low,a)}
sort(sd_M_SWC_low)

set.seed(100)
cv_M_SWC_low=cv(sample(M_SWC_low_vector,134,replace=T))
set.seed(100)
for (i in 2:1000) {b=cv(sample(M_SWC_low_vector,134,replace=T))
cv_M_SWC_low=c(cv_M_SWC_low,b)}
sort(cv_M_SWC_low)

SQ_MSWC_low<-rep("low",1000)
sd_M_SWC_low_fram<-data.frame(SQ_MSWC_low,sd_M_SWC_low,cv_M_SWC_low)
names(sd_M_SWC_low_fram)<-c("SQ","SD","CV")
###T.Test
sd_M_SWC<-rbind(sd_M_SWC_high_fram,sd_M_SWC_low_fram)
t.test(SD~SQ,sd_M_SWC)
t.test(CV~SQ,sd_M_SWC)

#######SR-YZB
###high soil
SR_YZB_high_vector<-as.vector(as.matrix(SR_YZB_high$Ynpk))
set.seed(100)
sample(SR_YZB_high_vector,249,replace=T)
sd_SR_YZB_high=sd(sample(SR_YZB_high_vector,249,replace=T))
set.seed(100)
for (i in 2:1000) {a=sd(sample(SR_YZB_high_vector,249,replace=T))
sd_SR_YZB_high=c(sd_SR_YZB_high,a)}
sort(sd_SR_YZB_high)

set.seed(100)
cv_SR_YZB_high=cv(sample(SR_YZB_high_vector,249,replace=T))
set.seed(100)
for (i in 2:1000) {b=cv(sample(SR_YZB_high_vector,249,replace=T))
cv_SR_YZB_high=c(cv_SR_YZB_high,b)}
sort(cv_SR_YZB_high)

SQ_SR_YZB_high<-rep("high",1000)
sd_SR_YZB_high_fram<-data.frame(SQ_MSWC_high,sd_SR_YZB_high,cv_SR_YZB_high)
names(sd_SR_YZB_high_fram)<-c("SQ","SD","CV")
###low soil
SR_YZB_low_vector<-as.vector(as.matrix(SR_YZB_low$Ynpk))
set.seed(100)
sample(SR_YZB_low_vector,250,replace=T)
sd_SR_YZB_low=sd(sample(SR_YZB_low_vector,250,replace=T))
set.seed(100)
for (i in 2:1000) {a=sd(sample(SR_YZB_low_vector,250,replace=T))
sd_SR_YZB_low=c(sd_SR_YZB_low,a)}
sort(sd_SR_YZB_low)

set.seed(100)
cv_SR_YZB_low=cv(sample(SR_YZB_low_vector,250,replace=T))
set.seed(100)
for (i in 2:1000) {b=cv(sample(SR_YZB_low_vector,250,replace=T))
cv_SR_YZB_low=c(cv_SR_YZB_low,b)}
sort(cv_SR_YZB_low)

SQ_SR_YZB_low<-rep("low",1000)
sd_SR_YZB_low_fram<-data.frame(SQ_MSWC_low,sd_SR_YZB_low,cv_SR_YZB_low)
names(sd_SR_YZB_low_fram)<-c("SQ","SD","CV")
###T.Test
sd_SR_YZB<-rbind(sd_SR_YZB_high_fram,sd_SR_YZB_low_fram)
t.test(SD~SQ,sd_SR_YZB)
t.test(CV~SQ,sd_SR_YZB)

#######ER-SC
###high soil
ER_SC_high_vector<-as.vector(as.matrix(ER_SC_high$Ynpk))
set.seed(100)
sample(ER_SC_high_vector,194,replace=T)
sd_ER_SC_high=sd(sample(ER_SC_high_vector,194,replace=T))
set.seed(100)
for (i in 2:1000) {a=sd(sample(ER_SC_high_vector,194,replace=T))
sd_ER_SC_high=c(sd_ER_SC_high,a)}
sort(sd_ER_SC_high)

set.seed(100)
cv_ER_SC_high=cv(sample(ER_SC_high_vector,194,replace=T))
set.seed(100)
for (i in 2:1000) {b=cv(sample(ER_SC_high_vector,194,replace=T))
cv_ER_SC_high=c(cv_ER_SC_high,b)}
sort(cv_ER_SC_high)

SQ_ER_SC_high<-rep("high",1000)
sd_ER_SC_high_fram<-data.frame(SQ_MSWC_high,sd_ER_SC_high,cv_ER_SC_high)
names(sd_ER_SC_high_fram)<-c("SQ","SD","CV")
###low soil
ER_SC_low_vector<-as.vector(as.matrix(ER_SC_low$Ynpk))
set.seed(100)
sample(ER_SC_low_vector,195,replace=T)
sd_ER_SC_low=sd(sample(ER_SC_low_vector,195,replace=T))
set.seed(100)
for (i in 2:1000) {a=sd(sample(ER_SC_low_vector,195,replace=T))
sd_ER_SC_low=c(sd_ER_SC_low,a)}
sort(sd_ER_SC_low)

set.seed(100)
cv_ER_SC_low=cv(sample(ER_SC_low_vector,195,replace=T))
set.seed(100)
for (i in 2:1000) {b=cv(sample(ER_SC_low_vector,195,replace=T))
cv_ER_SC_low=c(cv_ER_SC_low,b)}
sort(cv_ER_SC_low)

SQ_ER_SC_low<-rep("low",1000)
sd_ER_SC_low_fram<-data.frame(SQ_MSWC_low,sd_ER_SC_low,cv_ER_SC_low)
names(sd_ER_SC_low_fram)<-c("SQ","SD","CV")
###T.Test
sd_ER_SC<-rbind(sd_ER_SC_high_fram,sd_ER_SC_low_fram)
t.test(SD~SQ,sd_ER_SC)
t.test(CV~SQ,sd_ER_SC)

#######LR-SC
###high soil
LR_SC_high_vector<-as.vector(as.matrix(LR_SC_high$Ynpk))
set.seed(100)
sample(LR_SC_high_vector,205,replace=T)
sd_LR_SC_high=sd(sample(LR_SC_high_vector,205,replace=T))
set.seed(100)
for (i in 2:1000) {a=sd(sample(LR_SC_high_vector,205,replace=T))
sd_LR_SC_high=c(sd_LR_SC_high,a)}
sort(sd_LR_SC_high)

set.seed(100)
cv_LR_SC_high=cv(sample(LR_SC_high_vector,205,replace=T))
set.seed(1000)
for (i in 2:1000) {b=cv(sample(LR_SC_high_vector,205,replace=T))
cv_LR_SC_high=c(cv_LR_SC_high,b)}
sort(cv_LR_SC_high)

SQ_LR_SC_high<-rep("high",1000)
sd_LR_SC_high_fram<-data.frame(SQ_MSWC_high,sd_LR_SC_high,cv_LR_SC_high)
names(sd_LR_SC_high_fram)<-c("SQ","SD","CV")
###low soil
LR_SC_low_vector<-as.vector(as.matrix(LR_SC_low$Ynpk))
set.seed(100)
sample(LR_SC_low_vector,257,replace=T)
sd_LR_SC_low=sd(sample(LR_SC_low_vector,257,replace=T))
set.seed(100)
for (i in 2:1000) {a=sd(sample(LR_SC_low_vector,257,replace=T))
sd_LR_SC_low=c(sd_LR_SC_low,a)}
sort(sd_LR_SC_low)

set.seed(100)
cv_LR_SC_low=cv(sample(LR_SC_low_vector,257,replace=T))
set.seed(100)
for (i in 2:1000) {b=cv(sample(LR_SC_low_vector,257,replace=T))
cv_LR_SC_low=c(cv_LR_SC_low,b)}
sort(cv_LR_SC_low)

SQ_LR_SC_low<-rep("low",1000)
sd_LR_SC_low_fram<-data.frame(SQ_MSWC_low,sd_LR_SC_low,cv_LR_SC_low)
names(sd_LR_SC_low_fram)<-c("SQ","SD","CV")
###T.Test
sd_LR_SC<-rbind(sd_LR_SC_high_fram,sd_LR_SC_low_fram)
t.test(SD~SQ,sd_LR_SC)
t.test(CV~SQ,sd_LR_SC)

#==========================================================
#Yield variation explained by climate variability
#estimate R2 for GBRT models through 10-fold cross valiadtin

#W-NCP
#high soil
set.seed(123)
gbmGrid<- expand.grid(.interaction.depth = seq(1, 9, by = 2),
                      .n.trees = seq(100, 2000, by = 100),
                      .shrinkage = c(0.001,0.005,0.01, 0.05, 0.1),
                      .n.minobsinnode = 10)
brt_WNCP_high <- train(Ynpk ~ Tmax + Tmin + GDD.0 + PRE +  RAD,
                      data = W_NCP_high,method = "gbm",
                      tuneGrid = gbmGrid, 
                      trControl=trainControl(method = "repeatedcv", number = 10,repeats = 5),
                      verbose = FALSE)
plot(brt_WNCP_high)
results_WNCP_high<-data.frame(brt_WNCP_high$results)
#bestTurn
#R2=0.128

#low soil
set.seed(123)
gbmGrid<- expand.grid(.interaction.depth = seq(1, 9, by = 2),
                      .n.trees = seq(100, 2000, by = 100),
                      .shrinkage = c(0.001,0.005,0.01, 0.05, 0.1),
                      .n.minobsinnode = 10)
brt_WNCP_low <- train(Ynpk ~ Tmax + Tmin + GDD.0 + PRE +  RAD,
                       data = W_NCP_low,method = "gbm",
                       tuneGrid = gbmGrid, 
                       trControl=trainControl(method = "repeatedcv", number = 10,repeats = 5),
                       verbose = FALSE)
plot(brt_WNCP_low)
results_WNCP_low<-data.frame(brt_WNCP_low$results)
#bestTurn
#R2=0.194

#W-YZB
#high soil
set.seed(123)
gbmGrid<- expand.grid(.interaction.depth = seq(1, 9, by = 2),
                      .n.trees = seq(100, 2000, by = 100),
                      .shrinkage = c(0.001,0.005,0.01, 0.05, 0.1),
                      .n.minobsinnode = 10)
brt_WYZB_high <- train(Ynpk ~ Tmax + Tmin + GDD.0 + PRE +  RAD,
                       data = W_YZB_high,method = "gbm",
                       tuneGrid = gbmGrid, 
                       trControl=trainControl(method = "repeatedcv", number = 10,repeats = 5),
                       verbose = FALSE)
plot(brt_WYZB_high)
results_WYZB_high<-data.frame(brt_WYZB_high$results)
#bestTurn
#R2=0.201

#low soil
set.seed(123)
gbmGrid<- expand.grid(.interaction.depth = seq(1, 9, by = 2),
                      .n.trees = seq(100, 2000, by = 100),
                      .shrinkage = c(0.001,0.005,0.01, 0.05, 0.1),
                      .n.minobsinnode = 10)
brt_WYZB_low <- train(Ynpk ~ Tmax + Tmin + GDD.0 + PRE +  RAD,
                      data = W_YZB_low,method = "gbm",
                      tuneGrid = gbmGrid, 
                      trControl=trainControl(method = "repeatedcv", number = 10,repeats = 5),
                      verbose = FALSE)
plot(brt_WYZB_low)
results_WYZB_low<-data.frame(brt_WYZB_low$results)
#bestTurn
#R2=0.316

#W-NWC
#high soil
set.seed(123)
gbmGrid<- expand.grid(.interaction.depth = seq(1, 9, by = 2),
                      .n.trees = seq(100, 2000, by = 100),
                      .shrinkage = c(0.001,0.005,0.01, 0.05, 0.1),
                      .n.minobsinnode = 10)
brt_WNWC_high <- train(Ynpk ~ Tmax + Tmin + GDD.0 + PRE +  RAD,
                       data = W_NWC_high,method = "gbm",
                       tuneGrid = gbmGrid, 
                       trControl=trainControl(method = "repeatedcv", number = 10,repeats = 5),
                       verbose = FALSE)
plot(brt_WNWC_high)
results_WNWC_high<-data.frame(brt_WNWC_high$results)
#bestTurn
#R2=0.231

#low soil
set.seed(123)
gbmGrid<- expand.grid(.interaction.depth = seq(1, 9, by = 2),
                      .n.trees = seq(100, 2000, by = 100),
                      .shrinkage = c(0.001,0.005,0.01, 0.05, 0.1),
                      .n.minobsinnode = 10)
brt_WNWC_low <- train(Ynpk ~ Tmax + Tmin + GDD.0 + PRE +  RAD,
                      data = W_NWC_low,method = "gbm",
                      tuneGrid = gbmGrid, 
                      trControl=trainControl(method = "repeatedcv", number = 10,repeats = 5),
                      verbose = FALSE)
plot(brt_WNWC_low)
results_WNWC_low<-data.frame(brt_WNWC_low$results)
#bestTurn
#R2=0.426

#M-NEC
#high soil
set.seed(123)
gbmGrid<- expand.grid(.interaction.depth = seq(1, 9, by = 2),
                      .n.trees = seq(100, 2000, by = 100),
                      .shrinkage = c(0.001,0.005,0.01, 0.05, 0.1),
                      .n.minobsinnode = 10)
brt_MNEC_high <- train(Ynpk ~ Tmax + Tmin + GDD.10 + PRE +  RAD,
                       data = M_NEC_high,method = "gbm",
                       tuneGrid = gbmGrid, 
                       trControl=trainControl(method = "repeatedcv", number = 10,repeats = 5),
                       verbose = FALSE)
plot(brt_MNEC_high)
results_MNEC_high<-data.frame(brt_MNEC_high$results)
#bestTurn
#R2=0.203

#low soil
set.seed(123)
gbmGrid<- expand.grid(.interaction.depth = seq(1, 9, by = 2),
                      .n.trees = seq(100, 2000, by = 100),
                      .shrinkage = c(0.001,0.005,0.01, 0.05, 0.1),
                      .n.minobsinnode = 10)
brt_MNEC_low <- train(Ynpk ~ Tmax + Tmin + GDD.10 + PRE +  RAD,
                      data = M_NEC_low,method = "gbm",
                      tuneGrid = gbmGrid, 
                      trControl=trainControl(method = "repeatedcv", number = 10,repeats = 5),
                      verbose = FALSE)
plot(brt_MNEC_low)
results_MNEC_low<-data.frame(brt_MNEC_low$results)
#bestTurn
#R2=0.367

#M-NCP
#high soil
set.seed(123)
gbmGrid<- expand.grid(.interaction.depth = seq(1, 9, by = 2),
                      .n.trees = seq(100, 2000, by = 100),
                      .shrinkage = c(0.001,0.005,0.01, 0.05, 0.1),
                      .n.minobsinnode = 10)
brt_MNCP_high <- train(Ynpk ~ Tmax + Tmin + GDD.10 + PRE +  RAD,
                       data = M_NCP_high,method = "gbm",
                       tuneGrid = gbmGrid, 
                       trControl=trainControl(method = "repeatedcv", number = 10,repeats = 5),
                       verbose = FALSE)
plot(brt_MNCP_high)
results_MNCP_high<-data.frame(brt_MNCP_high$results)
#bestTurn
#R2=0.163

#low soil
set.seed(123)
gbmGrid<- expand.grid(.interaction.depth = seq(1, 9, by = 2),
                      .n.trees = seq(100, 2000, by = 100),
                      .shrinkage = c(0.001,0.005,0.01, 0.05, 0.1),
                      .n.minobsinnode = 10)
brt_MNCP_low <- train(Ynpk ~ Tmax + Tmin + GDD.10 + PRE +  RAD,
                      data = M_NCP_low,method = "gbm",
                      tuneGrid = gbmGrid, 
                      trControl=trainControl(method = "repeatedcv", number = 10,repeats = 5),
                      verbose = FALSE)
plot(brt_MNCP_low)
results_MNCP_low<-data.frame(brt_MNCP_low$results)
#bestTurn
#R2=0.20

#M-SWC
#high soil
set.seed(123)
gbmGrid<- expand.grid(.interaction.depth = seq(1, 9, by = 2),
                      .n.trees = seq(100, 2000, by = 100),
                      .shrinkage = c(0.001,0.005,0.01, 0.05, 0.1),
                      .n.minobsinnode = 10)
brt_MSWC_high <- train(Ynpk ~ Tmax + Tmin + GDD.10 + PRE +  RAD,
                       data = M_SWC_high,method = "gbm",
                       tuneGrid = gbmGrid, 
                       trControl=trainControl(method = "repeatedcv", number = 10,repeats = 5),
                       verbose = FALSE)
plot(brt_MSWC_high)
results_MSWC_high<-data.frame(brt_MSWC_high$results)
#bestTurn
#R2=0.15

#low soil
set.seed(123)
gbmGrid<- expand.grid(.interaction.depth = seq(1, 9, by = 2),
                      .n.trees = seq(100, 2000, by = 100),
                      .shrinkage = c(0.001,0.005,0.01, 0.05, 0.1),
                      .n.minobsinnode = 10)
brt_MSWC_low <- train(Ynpk ~ Tmax + Tmin + GDD.10 + PRE +  RAD,
                      data = M_SWC_low,method = "gbm",
                      tuneGrid = gbmGrid, 
                      trControl=trainControl(method = "repeatedcv", number = 10,repeats = 5),
                      verbose = FALSE)
plot(brt_MSWC_low)
results_MSWC_low<-data.frame(brt_MSWC_low$results)
#bestTurn
#R2=0.15

#SR-YZB
#high soil
set.seed(123)
gbmGrid<- expand.grid(.interaction.depth = seq(1, 9, by = 2),
                      .n.trees = seq(100, 2000, by = 100),
                      .shrinkage = c(0.001,0.005,0.01, 0.05, 0.1),
                      .n.minobsinnode = 10)
brt_SRYZB_high <- train(Ynpk ~ Tmax + Tmin + GDD.10 + PRE +  RAD,
                       data = SR_YZB_high,method = "gbm",
                       tuneGrid = gbmGrid, 
                       trControl=trainControl(method = "repeatedcv", number = 10,repeats = 5),
                       verbose = FALSE)
plot(brt_SRYZB_high)
results_SRYZB_high<-data.frame(brt_SRYZB_high$results)
#bestTurn
#R2=0.167

#low soil
set.seed(123)
gbmGrid<- expand.grid(.interaction.depth = seq(1, 9, by = 2),
                      .n.trees = seq(100, 2000, by = 100),
                      .shrinkage = c(0.001,0.005,0.01, 0.05, 0.1),
                      .n.minobsinnode = 10)
brt_SRYZB_low <- train(Ynpk ~ Tmax + Tmin + GDD.10 + PRE +  RAD,
                      data = SR_YZB_low,method = "gbm",
                      tuneGrid = gbmGrid, 
                      trControl=trainControl(method = "repeatedcv", number = 10,repeats = 5),
                      verbose = FALSE)
plot(brt_SRYZB_low)
results_SRYZB_low<-data.frame(brt_SRYZB_low$results)
#R2=0.182

#ER-SC
#high soil
set.seed(123)
gbmGrid<- expand.grid(.interaction.depth = seq(1, 9, by = 2),
                      .n.trees = seq(100, 2000, by = 100),
                      .shrinkage = c(0.001,0.005,0.01, 0.05, 0.1),
                      .n.minobsinnode = 10)
brt_ERSC_high <- train(Ynpk ~ Tmax + Tmin + GDD.10 + PRE +  RAD,
                        data = ER_SC_high,method = "gbm",
                        tuneGrid = gbmGrid, 
                        trControl=trainControl(method = "repeatedcv", number = 10,repeats = 5),
                        verbose = FALSE)
plot(brt_ERSC_high)
results_ERSC_high<-data.frame(brt_ERSC_high$results)
#bestTurn
#R2=0.112

#low soil
set.seed(123)
gbmGrid<- expand.grid(.interaction.depth = seq(1, 9, by = 2),
                      .n.trees = seq(100, 2000, by = 100),
                      .shrinkage = c(0.001,0.005,0.01, 0.05, 0.1),
                      .n.minobsinnode = 10)
brt_ERSC_low <- train(Ynpk ~ Tmax + Tmin + GDD.10 + PRE +  RAD,
                       data = ER_SC_low,method = "gbm",
                       tuneGrid = gbmGrid, 
                       trControl=trainControl(method = "repeatedcv", number = 10,repeats = 5),
                       verbose = FALSE)
plot(brt_ERSC_low)
results_ERSC_low<-data.frame(brt_ERSC_low$results)
#bestTurn
#R2=0.161

#LR-SC
#high soil
set.seed(123)
gbmGrid<- expand.grid(.interaction.depth = seq(1, 9, by = 2),
                      .n.trees = seq(100, 2000, by = 100),
                      .shrinkage = c(0.001,0.005,0.01, 0.05, 0.1),
                      .n.minobsinnode = 10)
brt_LRSC_high <- train(Ynpk ~ Tmax + Tmin + GDD.10 + PRE +  RAD,
                       data = LR_SC_high,method = "gbm",
                       tuneGrid = gbmGrid, 
                       trControl=trainControl(method = "repeatedcv", number = 10,repeats = 5),
                       verbose = FALSE)
plot(brt_LRSC_high)
results_LRSC_high<-data.frame(brt_LRSC_high$results)
#bestTurn
#R2=0.177

#low soil
set.seed(123)
gbmGrid<- expand.grid(.interaction.depth = seq(1, 9, by = 2),
                      .n.trees = seq(100, 2000, by = 100),
                      .shrinkage = c(0.001,0.005,0.01, 0.05, 0.1),
                      .n.minobsinnode = 10)
brt_LRSC_low <- train(Ynpk ~ Tmax + Tmin + GDD.10 + PRE +  RAD,
                      data = LR_SC_low,method = "gbm",
                      tuneGrid = gbmGrid, 
                      trControl=trainControl(method = "repeatedcv", number = 10,repeats = 5),
                      verbose = FALSE)
plot(brt_LRSC_low)
results_LRSC_low<-data.frame(brt_LRSC_low$results)
#bestTurn
#R2=0.074