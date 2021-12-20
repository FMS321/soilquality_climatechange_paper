# =========================================================
# Title: Soil quality both impact crop production and climate resilience
#
# Author details: Author: Lei Qiao 
# Contact details: qiaolei1991@foxmail.com
#
# Objective: set GBRT models for nine cropping systems
#========================================================== 
# Removing objects from the envrionment
rm(list = ls())
#==========================================================
# Loading library
library(gbm)
library(plyr) 
library(dplyr)
library(ggplot2)
library(lattice)
library(caret)
library(hydroGOF)
library(gvlma)
library(car)
#==========================================================
#==========================================================
# set model for winter wheat in NCP
W_NCP<-read.csv("W-NCP.csv")
W_NCP$Year<-as.factor(W_NCP$Year)
W_NCP$Cultivar<-as.factor(W_NCP$Cultivar)
W_NCP$Soil.type<-as.factor(W_NCP$Soil.type)
W_NCP$Soil.texture<-as.factor(W_NCP$Soil.texture)

# set model for winter wheat in NCP
# set train and test data(random 10% for 50 times)
brt_WNCP_recyle<- NULL
best_inter_WNCP_recyle<-NULL
summary_recyle <- NULL
cor_1_recyle <- NULL
R2_1_recyle <- NULL
RMSE_1_recyle <- NULL
nRMSE_1_recyle<- NULL
ME_1_recyle <- NULL
P_value_1_recyle<-NULL

cor_2_recyle <- NULL
R2_2_recyle <- NULL
RMSE_2_recyle <- NULL
nRMSE_2_recyle<- NULL
ME_2_recyle <- NULL
P_value_2_recyle<-NULL

#randomly sampled test datasets and run GBRT models for 50 times 
for (i in 1:50) {
  
  set.seed(i)
  train_WNCP <- sample(nrow(W_NCP), 0.9*nrow(W_NCP))
  W_NCP_train <- W_NCP[train_WNCP,] 
  W_NCP_test <- W_NCP[-train_WNCP,]
  
  
  # select required variables
  sub_W_NCP_train <- W_NCP_train %>%
    select(NO, Province1,Tave, Tmax, Tmin, GDD.0, PRE, SSD, RAD, Cultivar, Soil.type, 
           Soil.texture, SOM, OP , AK, pH, N, P2O5, K2O, Yck, Ynpk)
  sub_W_NCP_test <- W_NCP_test %>%
    select(NO, Province1,Tave, Tmax, Tmin, GDD.0, PRE, SSD, RAD, Cultivar, Soil.type, 
           Soil.texture, SOM, OP , AK, pH, N, P2O5, K2O, Yck, Ynpk)
  
  
  #building brt model
  set.seed(i)
  brt_WNCP<-gbm(Ynpk ~ Tmax + Tmin + GDD.0 + PRE +  RAD + Cultivar+ Soil.type
                + Soil.texture + SOM + OP + AK + pH + N + P2O5 + K2O,
                data = sub_W_NCP_train, distribution = "gaussian", n.trees = 3000,
                interaction.depth = 9, shrinkage = 0.01, 
                bag.fraction = 0.5,cv.folds = 10)
  brt_WNCP_recyle[[i]]<-brt_WNCP
  print(brt_WNCP)
  best_inter_WNCP<-gbm.perf(brt_WNCP,method = "cv")
  best_inter_WNCP_recyle[[i]]<-best_inter_WNCP
  summary_record<-summary(brt_WNCP,n.trees=best_inter_WNCP)
  summary_recyle[[i]] <-summary_record
  
  ###predict for test data
  #by GBRT
  sub_W_NCP_test$Ynpk_pred_1<-predict(brt_WNCP,sub_W_NCP_test)
 
  #plot of actual and predict values
  plot(sub_W_NCP_test$Ynpk_pred_1,sub_W_NCP_test$Ynpk, xlim=c(3500,9500),ylim=c(3500,9500),
       xlab="Predicted yield (kg/ha)", ylab="Observed yield (kg/ha)")
  abline(a=0,b=1)
  fitline<-lm(Ynpk~Ynpk_pred_1,sub_W_NCP_test)
  abline(fitline,lty=2)
  summary(fitline)
  
  global_test_WNCP<-display.gvlmatests(gvlma(fitline))$`p-value`[1]
  normality_test_WNCP<-shapiro.test(resid(fitline))$p.value
  homogeneity_test_WNCP<-ncvTest(fitline)$p
  independence_test_WNCP<-durbinWatsonTest(fitline)$p
  
  global_test_WNCP_recyle[[i]]<-global_test_WNCP
  normality_test_WNCP_recyle[[i]]<-normality_test_WNCP
  homogeneity_test_WNCP_recyle[[i]]<-homogeneity_test_WNCP
  independence_test_WNCP_recyle[[i]]<-independence_test_WNCP
  
  #caculate R, R2 and RMSE
  cor_1 <- cor(sub_W_NCP_test$Ynpk_pred_1,sub_W_NCP_test$Ynpk)
  R2_1 <- R2(sub_W_NCP_test$Ynpk_pred_1,sub_W_NCP_test$Ynpk)
  RMSE_1 <- RMSE(sub_W_NCP_test$Ynpk_pred_1,sub_W_NCP_test$Ynpk)
  nRMSE_1<-RMSE_1/mean(sub_W_NCP_test$Ynpk)*100
  ME_1<-me(sub_W_NCP_test$Ynpk_pred_1,sub_W_NCP_test$Ynpk)
  P_value_1<-t.test(sub_W_NCP_test$Ynpk_pred_1-sub_W_NCP_test$Ynpk)$p.value
  
  cor_1_recyle[[i]] <- cor_1
  R2_1_recyle[[i]] <- R2_1
  RMSE_1_recyle[[i]] <- RMSE_1
  nRMSE_1_recyle[[i]] <- nRMSE_1
  ME_1_recyle[[i]] <- ME_1
  P_value_1_recyle[[i]]<-P_value_1
  
  
  #by lm
  lm_fit1_WNCP<-lm(Ynpk ~ Tmax + Tmin + GDD.0 + PRE +  RAD + Cultivar+ Soil.type +Soil.texture
                   + SOM + OP + AK + pH + N + P2O5 + K2O, data = sub_W_NCP_train)
  summary(lm_fit1_WNCP)
  
  sub_W_NCP_test$Ynpk_pred_2<-predict(lm_fit1_WNCP,sub_W_NCP_test)
  
  #plot of actual and predict values
  plot(sub_W_NCP_test$Ynpk_pred_2,sub_W_NCP_test$Ynpk, xlim=c(3500,9500),ylim=c(3500,9500),
       xlab="Predicted yield (kg/ha)", ylab="Observed yield (kg/ha)")
  abline(a=0,b=1)
  fitline2<-lm(Ynpk~Ynpk_pred_2,sub_W_NCP_test)
  abline(fitline2,lty=2)
  summary(fitline2)
  
  #caculate E, RMSE and nRMSE
  cor_2 <- cor(sub_W_NCP_test$Ynpk_pred_2,sub_W_NCP_test$Ynpk)
  R2_2 <- R2(sub_W_NCP_test$Ynpk_pred_2,sub_W_NCP_test$Ynpk)
  RMSE_2 <- RMSE(sub_W_NCP_test$Ynpk_pred_2,sub_W_NCP_test$Ynpk)
  nRMSE_2<-RMSE_2/mean(sub_W_NCP_test$Ynpk)*100
  nRMSE_sd_2<-nrmse(sub_W_NCP_test$Ynpk_pred_2,sub_W_NCP_test$Ynpk,norm="sd")
  ME_2<-me(sub_W_NCP_test$Ynpk_pred_2,sub_W_NCP_test$Ynpk)
  P_value_2<-t.test(sub_W_NCP_test$Ynpk_pred_2-sub_W_NCP_test$Ynpk)$p.value
  MAE_2<-mae(sub_W_NCP_test$Ynpk_pred_2,sub_W_NCP_test$Ynpk)
  NSE_2<-NSE(sub_W_NCP_test$Ynpk_pred_2,sub_W_NCP_test$Ynpk)
  mNSE_2<-mNSE(sub_W_NCP_test$Ynpk_pred_2,sub_W_NCP_test$Ynpk)
  rNSE_2<-rNSE(sub_W_NCP_test$Ynpk_pred_2,sub_W_NCP_test$Ynpk)
  d_2<-d(sub_W_NCP_test$Ynpk_pred_2,sub_W_NCP_test$Ynpk)
  md_2<-md(sub_W_NCP_test$Ynpk_pred_2,sub_W_NCP_test$Ynpk)
  rd_2<-rd(sub_W_NCP_test$Ynpk_pred_2,sub_W_NCP_test$Ynpk)
  
  cor_2_recyle[[i]] <- cor_2
  R2_2_recyle[[i]] <- R2_2
  RMSE_2_recyle[[i]] <- RMSE_2
  nRMSE_2_recyle[[i]] <- nRMSE_2
  nRMSE_sd_2_recyle[[i]]<-nRMSE_sd_2
  ME_2_recyle[[i]] <- ME_2
  P_value_2_recyle[[i]]<-P_value_2
  MAE_2_recyle[[i]] <- MAE_2
  NSE_2_recyle[[i]] <- NSE_2
  mNSE_2_recyle[[i]] <- mNSE_2
  rNSE_2_recyle[[i]] <- rNSE_2
  d_2_recyle[[i]] <- d_2
  md_2_recyle[[i]] <- md_2
  rd_2_recyle[[i]] <- rd_2
}

#summary of E, RMSE and nRMSE
R2_1_recyle <- as.data.frame(R2_1_recyle)
RMSE_1_recyle <- as.data.frame(RMSE_1_recyle)
nRMSE_1_recyle<- as.data.frame(nRMSE_1_recyle)
ME_1_recyle <- as.data.frame(ME_1_recyle)
P_value_1_recyle<-as.data.frame(P_value_1_recyle)

summary(R2_1_recyle$R2_1_recyle)
sd(R2_1_recyle$R2_1_recyle)
summary(RMSE_1_recyle)
sd(RMSE_1_recyle$RMSE_1_recyle)
summary(nRMSE_1_recyle$nRMSE_1_recyle)
sd(nRMSE_1_recyle$nRMSE_1_recyle)
summary(nRMSE_sd_1_recyle$nRMSE_sd_1_recyle)
sd(ME_1_recyle$ME_1_recyle)
summary(P_value_1_recyle$P_value_1_recyle)
sd(P_value_1_recyle$P_value_1_recyle)
summary(MAE_1_recyle$MAE_1_recyle)

R2_2_recyle <- as.data.frame(R2_2_recyle)
RMSE_2_recyle <- as.data.frame(RMSE_2_recyle)
nRMSE_2_recyle<- as.data.frame(nRMSE_2_recyle)
ME_2_recyle <- as.data.frame(ME_2_recyle)
P_value_2_recyle<-as.data.frame(P_value_2_recyle)

summary(R2_2_recyle)
sd(R2_2_recyle$R2_2_recyle)
summary(RMSE_2_recyle)
sd(RMSE_2_recyle$RMSE_2_recyle)
summary(nRMSE_2_recyle$nRMSE_2_recyle)
sd(nRMSE_2_recyle$nRMSE_2_recyle)
summary(ME_2_recyle$ME_2_recyle)
sd(ME_2_recyle$ME_2_recyle)
summary(P_value_2_recyle$P_value_2_recyle)
sd(P_value_2_recyle$P_value_2_recyle)
summary(MAE_2_recyle$MAE_2_recyle)

#==========================================================
# set model for winter wheat in YZB

W_YZB<-read.csv("W-YZB.csv")
W_YZB$Year<-as.factor(W_YZB$Year)
W_YZB$Cultivar<-as.factor(W_YZB$Cultivar)
W_YZB$Soil.type<-as.factor(W_YZB$Soil.type)
W_YZB$Soil.texture<-as.factor(W_YZB$Soil.texture)

# set train and test data(random 10% for 50 times)
brt_WYZB_recyle<- NULL
best_inter_WYZB_recyle<-NULL
summary_WYZB_recyle <- NULL
cor_WYZB_1_recyle <- NULL
R2_WYZB_1_recyle <- NULL
RMSE_WYZB_1_recyle <- NULL
nRMSE_WYZB_1_recyle<- NULL
ME_WYZB_1_recyle <- NULL
P_value_WYZB_1_recyle<-NULL

cor_WYZB_2_recyle <- NULL
R2_WYZB_2_recyle <- NULL
RMSE_WYZB_2_recyle <- NULL
nRMSE_WYZB_2_recyle<- NULL
ME_WYZB_2_recyle <- NULL
P_value_WYZB_2_recyle<-NULL

for (i in 1:50) {
  
  set.seed(i)
  train_WYZB <- sample(nrow(W_YZB), 0.9*nrow(W_YZB))
  W_YZB_train <- W_YZB[train_WYZB,] 
  W_YZB_test <- W_YZB[-train_WYZB,]
  
  # select required variables
  sub_W_YZB_train <- W_YZB_train %>%
    select(NO, Province1,Tave, Tmax, Tmin, GDD.0, PRE, SSD, RAD, Cultivar, Soil.type, 
           Soil.texture, SOM, OP , AK, pH, N, P2O5, K2O, Yck, Ynpk)
  sub_W_YZB_test <- W_YZB_test %>%
    select(NO, Province1,Tave, Tmax, Tmin, GDD.0, PRE, SSD, RAD, Cultivar,  Soil.type, 
           Soil.texture, SOM, OP , AK, pH, N, P2O5, K2O, Yck, Ynpk)
  
  #building brt model
  set.seed(123)
  brt_WYZB<-gbm(Ynpk ~ Tmax + Tmin + GDD.0 + PRE +  RAD + Cultivar+Soil.type 
                + Soil.texture + SOM + OP + AK + pH + N + P2O5 + K2O,
                data = sub_W_YZB_train, distribution = "gaussian", n.trees = 3000,
                interaction.depth = 9, shrinkage = 0.01, 
                bag.fraction = 0.5,cv.folds = 10)
  brt_WYZB_recyle[[i]]<-brt_WYZB
  print(brt_WYZB)
  best_inter_WYZB<-gbm.perf(brt_WYZB,method = "cv")
  best_inter_WYZB_recyle[[i]]<-best_inter_WYZB
  summary_WYZB_record<-summary(brt_WYZB,n.trees=best_inter_WYZB)
  summary_WYZB_recyle[[i]] <-summary_WYZB_record
  
  ###comparison of model performance with lm
  ###predict for test data
  #by GBRT
  sub_W_YZB_test$Ynpk_pred_1<-predict(brt_WYZB,sub_W_YZB_test)
  
  #plot of actual and predict values
  plot(sub_W_YZB_test$Ynpk_pred_1,sub_W_YZB_test$Ynpk, xlim=c(3500,9500),ylim=c(3500,9500),
       xlab="Predicted yield (kg/ha)", ylab="Observed yield (kg/ha)")
  abline(a=0,b=1)
  fitline_WYZB<-lm(Ynpk~Ynpk_pred_1,sub_W_YZB_test)
  abline(fitline_WYZB,lty=2)
  summary(fitline_WYZB)
  
  #caculate R, R2 and RMSE
  cor_WYZB_1 <- cor(sub_W_YZB_test$Ynpk_pred_1,sub_W_YZB_test$Ynpk)
  R2_WYZB_1 <- R2(sub_W_YZB_test$Ynpk_pred_1,sub_W_YZB_test$Ynpk)
  RMSE_WYZB_1 <- RMSE(sub_W_YZB_test$Ynpk_pred_1,sub_W_YZB_test$Ynpk)
  nRMSE_WYZB_1<-RMSE_WYZB_1/mean(sub_W_YZB_test$Ynpk)*100
  ME_WYZB_1<-me(sub_W_YZB_test$Ynpk_pred_1,sub_W_YZB_test$Ynpk)
  P_value_WYZB_1<-t.test(sub_W_YZB_test$Ynpk_pred_1,sub_W_YZB_test$Ynpk)$p.value
 
  cor_WYZB_1_recyle[[i]] <- cor_WYZB_1
  R2_WYZB_1_recyle[[i]] <- R2_WYZB_1
  RMSE_WYZB_1_recyle[[i]] <- RMSE_WYZB_1
  nRMSE_WYZB_1_recyle[[i]] <- nRMSE_WYZB_1
  ME_WYZB_1_recyle[[i]] <- ME_WYZB_1
  P_value_WYZB_1_recyle[[i]]<-P_value_WYZB_1
 
  #by lm
  lm_fit1_WYZB<-lm(Ynpk ~ Tmax + Tmin + GDD.0 + PRE +  RAD + Cultivar + Soil.type +Soil.texture
                   + SOM + OP + AK + pH + N + P2O5 + K2O, data = sub_W_YZB_train)
  summary(lm_fit1_WYZB)
  
  sub_W_YZB_test$Ynpk_pred_2<-predict(lm_fit1_WYZB,sub_W_YZB_test)
  
  #plot of actual and predict values
  plot(sub_W_YZB_test$Ynpk_pred_2,sub_W_YZB_test$Ynpk, xlim=c(3500,9500),ylim=c(3500,9500),
       xlab="Predicted yield (kg/ha)", ylab="Observed yield (kg/ha)")
  abline(a=0,b=1)
  fitline2<-lm(Ynpk~Ynpk_pred_2,sub_W_YZB_test)
  abline(fitline2,lty=2)
  summary(fitline2)
  
  #caculate R, R2 and RMSE
  cor_WYZB_2 <- cor(sub_W_YZB_test$Ynpk_pred_2,sub_W_YZB_test$Ynpk)
  R2_WYZB_2 <- R2(sub_W_YZB_test$Ynpk_pred_2,sub_W_YZB_test$Ynpk)
  RMSE_WYZB_2 <- RMSE(sub_W_YZB_test$Ynpk_pred_2,sub_W_YZB_test$Ynpk)
  nRMSE_WYZB_2<-RMSE_WYZB_2/mean(sub_W_YZB_test$Ynpk)*100
  ME_WYZB_2<-me(sub_W_YZB_test$Ynpk_pred_2,sub_W_YZB_test$Ynpk)
  P_value_WYZB_2<-t.test(sub_W_YZB_test$Ynpk_pred_2,sub_W_YZB_test$Ynpk)$p.value

  cor_WYZB_2_recyle[[i]] <- cor_WYZB_2
  R2_WYZB_2_recyle[[i]] <- R2_WYZB_2
  RMSE_WYZB_2_recyle[[i]] <- RMSE_WYZB_2
  nRMSE_WYZB_2_recyle[[i]] <- nRMSE_WYZB_2
  ME_WYZB_2_recyle[[i]] <- ME_WYZB_2
  P_value_WYZB_2_recyle[[i]]<-P_value_WYZB_2

}

R2_WYZB_1_recyle <- as.data.frame(R2_WYZB_1_recyle)
RMSE_WYZB_1_recyle <- as.data.frame(RMSE_WYZB_1_recyle)
nRMSE_WYZB_1_recyle<- as.data.frame(nRMSE_WYZB_1_recyle)
ME_WYZB_1_recyle <- as.data.frame(ME_WYZB_1_recyle)
P_value_WYZB_1_recyle<-as.data.frame(P_value_WYZB_1_recyle)

summary(R2_WYZB_1_recyle$R2_WYZB_1_recyle)
sd(R2_WYZB_1_recyle$R2_WYZB_1_recyle)
summary(RMSE_WYZB_1_recyle$RMSE_WYZB_1_recyle)
sd(RMSE_WYZB_1_recyle$RMSE_WYZB_1_recyle)
summary(nRMSE_WYZB_1_recyle$nRMSE_WYZB_1_recyle)
sd(nRMSE_WYZB_1_recyle$nRMSE_WYZB_1_recyle)
summary(ME_WYZB_1_recyle$ME_WYZB_1_recyle)
sd(ME_WYZB_1_recyle$ME_WYZB_1_recyle)
summary(P_value_WYZB_1_recyle$P_value_WYZB_1_recyle)
sd(P_value_WYZB_1_recyle$P_value_WYZB_1_recyle)

R2_WYZB_2_recyle <- as.data.frame(R2_WYZB_2_recyle)
RMSE_WYZB_2_recyle <- as.data.frame(RMSE_WYZB_2_recyle)
nRMSE_WYZB_2_recyle<- as.data.frame(nRMSE_WYZB_2_recyle)
ME_WYZB_2_recyle <- as.data.frame(ME_WYZB_2_recyle)
P_value_WYZB_2_recyle<-as.data.frame(P_value_WYZB_2_recyle)

summary(R2_WYZB_2_recyle$R2_WYZB_2_recyle)
sd(R2_WYZB_2_recyle$R2_WYZB_2_recyle)
summary(RMSE_WYZB_2_recyle$RMSE_WYZB_2_recyle)
sd(RMSE_WYZB_2_recyle$RMSE_WYZB_2_recyle)
summary(nRMSE_2_recyle$nRMSE_2_recyle)
sd(nRMSE_WYZB_2_recyle$nRMSE_WYZB_2_recyle)
summary(ME_WYZB_2_recyle$ME_WYZB_2_recyle)
sd(ME_WYZB_2_recyle$ME_WYZB_2_recyle)
summary(P_value_WYZB_2_recyle$P_value_WYZB_2_recyle)
sd(P_value_WYZB_2_recyle$P_value_WYZB_2_recyle)

#==========================================================
# set model for winter wheat in NWC
W_NWC<-read.csv("W-NWC-1.csv")
W_NWC$Year<-as.factor(W_NWC$Year)
W_NWC$Cultivar<-as.factor(W_NWC$Cultivar)
W_NWC$Soil.type<-as.factor(W_NWC$Soil.type)
W_NWC$Soil.texture<-as.factor(W_NWC$Soil.texture)

# set train and test data(random 10% for 50 times)
brt_WNWC_recyle<- NULL
best_inter_WNWC_recyle<-NULL
summary_WNWC_recyle <- NULL
cor_WNWC_1_recyle <- NULL
R2_WNWC_1_recyle <- NULL
RMSE_WNWC_1_recyle <- NULL
nRMSE_WNWC_1_recyle<- NULL
ME_WNWC_1_recyle <- NULL
P_value_WNWC_1_recyle<-NULL

cor_WNWC_2_recyle <- NULL
R2_WNWC_2_recyle <- NULL
RMSE_WNWC_2_recyle <- NULL
nRMSE_WNWC_2_recyle<- NULL
ME_WNWC_2_recyle <- NULL
P_value_WNWC_2_recyle<-NULL

for (i in 1:50) {
  
  set.seed(i)
  train_WNWC <- sample(nrow(W_NWC), 0.9*nrow(W_NWC))
  W_NWC_train <- W_NWC[train_WNWC,] 
  W_NWC_test <- W_NWC[-train_WNWC,]
  
  # select required variables
  sub_W_NWC_train <- W_NWC_train %>%
    select(NO, Province1,Tave, Tmax, Tmin, GDD.0, PRE, SSD, RAD, Cultivar, Soil.type, 
           Soil.texture, SOM, OP , AK, pH, N, P2O5, K2O, Yck, Ynpk)
  sub_W_NWC_test <- W_NWC_test %>%
    select(NO, Province1,Tave, Tmax, Tmin, GDD.0, PRE, SSD, RAD, Cultivar, Soil.type, 
           Soil.texture, SOM, OP , AK, pH, N, P2O5, K2O, Yck, Ynpk)
  
  #building brt model
  set.seed(123)
  brt_WNWC<-gbm(Ynpk ~ Tmax + Tmin + GDD.0 + PRE +  RAD + Cultivar+ Soil.type
                + Soil.texture + SOM + OP + AK + pH + N + P2O5 + K2O,
                data = sub_W_NWC_train, distribution = "gaussian", n.trees = 2000,
                interaction.depth = 7, shrinkage = 0.05, 
                bag.fraction = 0.5,cv.folds = 10)
  brt_WNWC_recyle[[i]]<-brt_WNWC
  print(brt_WNWC)
  best_inter_WNWC<-gbm.perf(brt_WNWC,method = "cv")
  best_inter_WNWC_recyle[[i]]<-best_inter_WNWC
  summary_WNWC_record<-summary(brt_WNWC,n.trees=best_inter_WNWC)
  summary_WNWC_recyle[[i]] <-summary_WNWC_record
  
  ###comparison of model performance with lm
  ###predict for test data
  #by GBRT
  sub_W_NWC_test$Ynpk_pred_1<-predict(brt_WNWC,sub_W_NWC_test)
  
  #plot of actual and predict values
  plot(sub_W_NWC_test$Ynpk_pred_1,sub_W_NWC_test$Ynpk, xlim=c(3500,9500),ylim=c(3500,9500),
       xlab="Predicted yield (kg/ha)", ylab="Observed yield (kg/ha)")
  abline(a=0,b=1)
  fitline_WNWC<-lm(Ynpk~Ynpk_pred_1,sub_W_NWC_test)
  abline(fitline_WNWC,lty=2)
  summary(fitline_WNWC)
  
  #caculate R, R2 and RMSE
  cor_WNWC_1 <- cor(sub_W_NWC_test$Ynpk_pred_1,sub_W_NWC_test$Ynpk)
  R2_WNWC_1 <- R2(sub_W_NWC_test$Ynpk_pred_1,sub_W_NWC_test$Ynpk)
  RMSE_WNWC_1 <- RMSE(sub_W_NWC_test$Ynpk_pred_1,sub_W_NWC_test$Ynpk)
  nRMSE_WNWC_1<-RMSE_WNWC_1/mean(sub_W_NWC_test$Ynpk)*100
  ME_WNWC_1<-me(sub_W_NWC_test$Ynpk_pred_1,sub_W_NWC_test$Ynpk)
  P_value_WNWC_1<-t.test(sub_W_NWC_test$Ynpk_pred_1,sub_W_NWC_test$Ynpk)$p.value
  
  cor_WNWC_1_recyle[[i]] <- cor_WNWC_1
  R2_WNWC_1_recyle[[i]] <- R2_WNWC_1
  RMSE_WNWC_1_recyle[[i]] <- RMSE_WNWC_1
  nRMSE_WNWC_1_recyle[[i]] <- nRMSE_WNWC_1
  ME_WNWC_1_recyle[[i]] <- ME_WNWC_1
  P_value_WNWC_1_recyle[[i]]<-P_value_WNWC_1

  #by lm
  lm_fit1_WNWC<-lm(Ynpk ~ Tmax + Tmin + GDD.0 + PRE +  RAD + Cultivar + Soil.type +Soil.texture
                   + SOM + OP + AK + pH + N + P2O5 + K2O, data = sub_W_NWC_train)
  
  sub_W_NWC_test$Ynpk_pred_2<-predict(lm_fit1_WNWC,sub_W_NWC_test)
  
  #plot of actual and predict values
  plot(sub_W_NWC_test$Ynpk_pred_2,sub_W_NWC_test$Ynpk, xlim=c(3500,9500),ylim=c(3500,9500),
       xlab="Predicted yield (kg/ha)", ylab="Observed yield (kg/ha)")
  abline(a=0,b=1)
  fitline2<-lm(Ynpk~Ynpk_pred_2,sub_W_NWC_test)
  abline(fitline2,lty=2)
  summary(fitline2)
  
  #caculate R, R2 and RMSE
  cor_WNWC_2 <- cor(sub_W_NWC_test$Ynpk_pred_2,sub_W_NWC_test$Ynpk)
  R2_WNWC_2 <- R2(sub_W_NWC_test$Ynpk_pred_2,sub_W_NWC_test$Ynpk)
  RMSE_WNWC_2 <- RMSE(sub_W_NWC_test$Ynpk_pred_2,sub_W_NWC_test$Ynpk)
  nRMSE_WNWC_2<-RMSE_WNWC_2/mean(sub_W_NWC_test$Ynpk)*100
  ME_WNWC_2<-me(sub_W_NWC_test$Ynpk_pred_2,sub_W_NWC_test$Ynpk)
  P_value_WNWC_2<-t.test(sub_W_NWC_test$Ynpk_pred_2,sub_W_NWC_test$Ynpk)$p.value

  cor_WNWC_2_recyle[[i]] <- cor_WNWC_2
  R2_WNWC_2_recyle[[i]] <- R2_WNWC_2
  RMSE_WNWC_2_recyle[[i]] <- RMSE_WNWC_2
  nRMSE_WNWC_2_recyle[[i]] <- nRMSE_WNWC_2
  ME_WNWC_2_recyle[[i]] <- ME_WNWC_2
  P_value_WNWC_2_recyle[[i]]<-P_value_WNWC_2

}
  
RMSE_WNWC_1_recyle <- as.data.frame(RMSE_WNWC_1_recyle)
nRMSE_WNWC_1_recyle<- as.data.frame(nRMSE_WNWC_1_recyle)
ME_WNWC_1_recyle <- as.data.frame(ME_WNWC_1_recyle)
P_value_WNWC_1_recyle<-as.data.frame(P_value_WNWC_1_recyle)

summary(RMSE_WNWC_1_recyle$RMSE_WNWC_1_recyle)
sd(RMSE_WNWC_1_recyle$RMSE_WNWC_1_recyle)
summary(nRMSE_WNWC_1_recyle$nRMSE_WNWC_1_recyle)
sd(nRMSE_WNWC_1_recyle$nRMSE_WNWC_1_recyle)
summary(ME_WNWC_1_recyle$ME_WNWC_1_recyle)
sd(ME_WNWC_1_recyle$ME_WNWC_1_recyle)
summary(P_value_WNWC_1_recyle$P_value_WNWC_1_recyle)
sd(P_value_WNWC_1_recyle$P_value_WNWC_1_recyle)

RMSE_WNWC_2_recyle <- as.data.frame(RMSE_WNWC_2_recyle)
nRMSE_WNWC_2_recyle<- as.data.frame(nRMSE_WNWC_2_recyle)
ME_WNWC_2_recyle <- as.data.frame(ME_WNWC_2_recyle)
P_value_WNWC_2_recyle<-as.data.frame(P_value_WNWC_2_recyle)

summary(RMSE_WNWC_2_recyle$RMSE_WNWC_2_recyle)
sd(RMSE_WNWC_2_recyle$RMSE_WNWC_2_recyle)
summary(nRMSE_2_recyle$nRMSE_2_recyle)
sd(nRMSE_WNWC_2_recyle$nRMSE_WNWC_2_recyle)
summary(ME_WNWC_2_recyle$ME_WNWC_2_recyle)
sd(ME_WNWC_2_recyle$ME_WNWC_2_recyle)
summary(P_value_WNWC_2_recyle$P_value_WNWC_2_recyle)
sd(P_value_WNWC_2_recyle$P_value_WNWC_2_recyle)

#==========================================================
# set model for maize in NEC

M_NEC<-read.csv("M-NEC.csv")
M_NEC$Year<-as.factor(M_NEC$Year)
M_NEC$Cultivar<-as.factor(M_NEC$Cultivar)
M_NEC$Soil.type<-as.factor(M_NEC$Soil.type)
M_NEC$Soil.texture<-as.factor(M_NEC$Soil.texture)

# set train and test data(random 10% for 50 times)
brt_MNEC_recyle<- NULL
best_inter_MNEC_recyle<-NULL
summary_MNEC_recyle <- NULL
cor_MNEC_1_recyle <- NULL
R2_MNEC_1_recyle <- NULL
RMSE_MNEC_1_recyle <- NULL
nRMSE_MNEC_1_recyle<- NULL
ME_MNEC_1_recyle <- NULL
P_value_MNEC_1_recyle<-NULL

cor_MNEC_2_recyle <- NULL
R2_MNEC_2_recyle <- NULL
RMSE_MNEC_2_recyle <- NULL
nRMSE_MNEC_2_recyle<- NULL
ME_MNEC_2_recyle <- NULL
P_value_MNEC_2_recyle<-NULL

for (i in 1:50) {
  

  set.seed(i)
  train_MNEC <- sample(nrow(M_NEC), 0.9*nrow(M_NEC))
  M_NEC_train <- M_NEC[train_MNEC,] 
  M_NEC_test <- M_NEC[-train_MNEC,]
  
  # select required variables
  sub_M_NEC_train <- M_NEC_train %>%
    select(NO, Province1,Tave, Tmax, Tmin, GDD.10, PRE, SSD, RAD, Cultivar, Soil.type, 
           Soil.texture, SOM, OP , AK, pH, N, P2O5, K2O, Yck, Ynpk)
  sub_M_NEC_test <- M_NEC_test %>%
    select(NO, Province1,Tave, Tmax, Tmin, GDD.10, PRE, SSD, RAD, Cultivar, Soil.type, 
           Soil.texture, SOM, OP , AK, pH, N, P2O5, K2O, Yck, Ynpk)
  
  #building brt model
  set.seed(123)
  brt_MNEC<-gbm(Ynpk ~ Tmax + Tmin + GDD.10 + PRE +  RAD + Cultivar+ Soil.type
                + Soil.texture + SOM + OP + AK + pH + N + P2O5 + K2O,
                data = sub_M_NEC_train, distribution = "gaussian", n.trees = 2000,
                interaction.depth = 9, shrinkage = 0.01, 
                bag.fraction = 0.5,cv.folds = 10)
  brt_MNEC_recyle[[i]]<-brt_MNEC
  print(brt_MNEC)
  best_inter_MNEC<-gbm.perf(brt_MNEC,method = "cv")
  best_inter_MNEC_recyle[[i]]<-best_inter_MNEC
  summary_MNEC_record<-summary(brt_MNEC,n.trees=best_inter_MNEC)
  summary_MNEC_recyle[[i]] <-summary_MNEC_record
  
  ###comparison of model performance with lm
  ###predict for test data
  #by GBRT
  sub_M_NEC_test$Ynpk_pred_1<-predict(brt_MNEC,sub_M_NEC_test)
  
  #plot of actual and predict values
  plot(sub_M_NEC_test$Ynpk_pred_1,sub_M_NEC_test$Ynpk, xlim=c(6000,12000),ylim=c(6000,12000),
       xlab="Predicted yield (kg/ha)", ylab="Observed yield (kg/ha)")
  abline(a=0,b=1)
  fitline_MNEC<-lm(Ynpk~Ynpk_pred_1,sub_M_NEC_test)
  abline(fitline_MNEC,lty=2)
  summary(fitline_MNEC)
  
  #caculate R, R2 and RMSE
  cor_MNEC_1 <- cor(sub_M_NEC_test$Ynpk_pred_1,sub_M_NEC_test$Ynpk)
  R2_MNEC_1 <- R2(sub_M_NEC_test$Ynpk_pred_1,sub_M_NEC_test$Ynpk)
  RMSE_MNEC_1 <- RMSE(sub_M_NEC_test$Ynpk_pred_1,sub_M_NEC_test$Ynpk)
  nRMSE_MNEC_1<-RMSE_MNEC_1/mean(sub_M_NEC_test$Ynpk)*100
  ME_MNEC_1<-me(sub_M_NEC_test$Ynpk_pred_1,sub_M_NEC_test$Ynpk)
  P_value_MNEC_1<-t.test(sub_M_NEC_test$Ynpk_pred_1,sub_M_NEC_test$Ynpk)$p.value

  cor_MNEC_1_recyle[[i]] <- cor_MNEC_1
  R2_MNEC_1_recyle[[i]] <- R2_MNEC_1
  RMSE_MNEC_1_recyle[[i]] <- RMSE_MNEC_1
  nRMSE_MNEC_1_recyle[[i]] <- nRMSE_MNEC_1
  ME_MNEC_1_recyle[[i]] <- ME_MNEC_1
  P_value_MNEC_1_recyle[[i]]<-P_value_MNEC_1

  #by lm
  lm_fit1_MNEC<-lm(Ynpk ~ Tmax + Tmin + GDD.10 + PRE +  RAD + Soil.type +Soil.texture
                   + SOM + OP + AK + pH + N + P2O5 + K2O, data = sub_M_NEC_train)
  summary(lm_fit1_MNEC)
  
  sub_M_NEC_test$Ynpk_pred_2<-predict(lm_fit1_MNEC,sub_M_NEC_test)
  
  #plot of actual and predict values
  plot(sub_M_NEC_test$Ynpk_pred_2,sub_M_NEC_test$Ynpk, xlim=c(3500,9500),ylim=c(3500,9500),
       xlab="Predicted yield (kg/ha)", ylab="Observed yield (kg/ha)")
  abline(a=0,b=1)
  fitline2<-lm(Ynpk~Ynpk_pred_2,sub_M_NEC_test)
  abline(fitline2,lty=2)
  summary(fitline2)
  
  #caculate R, R2 and RMSE
  cor_MNEC_2 <- cor(sub_M_NEC_test$Ynpk_pred_2,sub_M_NEC_test$Ynpk)
  R2_MNEC_2 <- R2(sub_M_NEC_test$Ynpk_pred_2,sub_M_NEC_test$Ynpk)
  RMSE_MNEC_2 <- RMSE(sub_M_NEC_test$Ynpk_pred_2,sub_M_NEC_test$Ynpk)
  nRMSE_MNEC_2<-RMSE_MNEC_2/mean(sub_M_NEC_test$Ynpk)*100
  ME_MNEC_2<-me(sub_M_NEC_test$Ynpk_pred_2,sub_M_NEC_test$Ynpk)
  P_value_MNEC_2<-t.test(sub_M_NEC_test$Ynpk_pred_2,sub_M_NEC_test$Ynpk)$p.value

  cor_MNEC_2_recyle[[i]] <- cor_MNEC_2
  R2_MNEC_2_recyle[[i]] <- R2_MNEC_2
  RMSE_MNEC_2_recyle[[i]] <- RMSE_MNEC_2
  nRMSE_MNEC_2_recyle[[i]] <- nRMSE_MNEC_2
  ME_MNEC_2_recyle[[i]] <- ME_MNEC_2
  P_value_MNEC_2_recyle[[i]]<-P_value_MNEC_2
}  
 
RMSE_MNEC_1_recyle <- as.data.frame(RMSE_MNEC_1_recyle)
nRMSE_MNEC_1_recyle<- as.data.frame(nRMSE_MNEC_1_recyle)
ME_MNEC_1_recyle <- as.data.frame(ME_MNEC_1_recyle)
P_value_MNEC_1_recyle<-as.data.frame(P_value_MNEC_1_recyle)

summary(RMSE_MNEC_1_recyle$RMSE_MNEC_1_recyle)
sd(RMSE_MNEC_1_recyle$RMSE_MNEC_1_recyle)
summary(nRMSE_MNEC_1_recyle$nRMSE_MNEC_1_recyle)
sd(nRMSE_MNEC_1_recyle$nRMSE_MNEC_1_recyle)
summary(ME_MNEC_1_recyle$ME_MNEC_1_recyle)
sd(ME_MNEC_1_recyle$ME_MNEC_1_recyle)
summary(P_value_MNEC_1_recyle$P_value_MNEC_1_recyle)
sd(P_value_MNEC_1_recyle$P_value_MNEC_1_recyle)
summary(MAE_MNEC_1_recyle$MAE_MNEC_1_recyle)

RMSE_MNEC_2_recyle <- as.data.frame(RMSE_MNEC_2_recyle)
nRMSE_MNEC_2_recyle<- as.data.frame(nRMSE_MNEC_2_recyle)
nRMSE_sd_MNEC_2_recyle<-as.data.frame(nRMSE_sd_MNEC_2_recyle)
ME_MNEC_2_recyle <- as.data.frame(ME_MNEC_2_recyle)
P_value_MNEC_2_recyle<-as.data.frame(P_value_MNEC_2_recyle)

summary(RMSE_MNEC_2_recyle$RMSE_MNEC_2_recyle)
sd(RMSE_MNEC_2_recyle$RMSE_MNEC_2_recyle)
summary(nRMSE_2_recyle$nRMSE_2_recyle)
sd(nRMSE_MNEC_2_recyle$nRMSE_MNEC_2_recyle)
summary(nRMSE_sd_MNEC_2_recyle$nRMSE_sd_MNEC_2_recyle)
sd(ME_MNEC_2_recyle$ME_MNEC_2_recyle)
summary(P_value_MNEC_2_recyle$P_value_MNEC_2_recyle)
sd(P_value_MNEC_2_recyle$P_value_MNEC_2_recyle)

#==========================================================
# set model for maize in NCP

M_NCP<-read.csv("M-NCP.csv")
M_NCP$Year<-as.factor(M_NCP$Year)
M_NCP$Cultivar<-as.factor(M_NCP$Cultivar)
M_NCP$Soil.type<-as.factor(M_NCP$Soil.type)
M_NCP$Soil.texture<-as.factor(M_NCP$Soil.texture)

# set train and test data(random 10% for 50 times)
brt_MNCP_recyle<- NULL
best_inter_MNCP_recyle<-NULL
summary_MNCP_recyle <- NULL
cor_MNCP_1_recyle <- NULL
R2_MNCP_1_recyle <- NULL
RMSE_MNCP_1_recyle <- NULL
nRMSE_MNCP_1_recyle<- NULL
ME_MNCP_1_recyle <- NULL
P_value_MNCP_1_recyle<-NULL

cor_MNCP_2_recyle <- NULL
R2_MNCP_2_recyle <- NULL
RMSE_MNCP_2_recyle <- NULL
nRMSE_MNCP_2_recyle<- NULL
ME_MNCP_2_recyle <- NULL
P_value_MNCP_2_recyle<-NULL

for (i in 1:50) {
  
  set.seed(i)
  trainingRow_MNCP <- createDataPartition(M_NCP$Province1,p=0.9, list = F)
  M_NCP_train <- M_NCP[trainingRow_MNCP,] 
  M_NCP_test <- M_NCP[-trainingRow_MNCP,]
  
  # select required variables
  sub_M_NCP_train <- M_NCP_train %>%
    select(NO, Province,Tave, Tmax, Tmin, GDD.10, PRE, SSD, RAD, Cultivar,Soil.type, 
           Soil.texture, SOM, OP , AK, pH, N, P2O5, K2O, Yck, Ynpk)
  sub_M_NCP_test <- M_NCP_test %>%
    select(NO, Province,Tave, Tmax, Tmin, GDD.10, PRE, SSD, RAD,Cultivar, Soil.type, 
           Soil.texture, SOM, OP , AK, pH, N, P2O5, K2O, Yck, Ynpk)
  
  #building brt model
  set.seed(123)
  brt_MNCP<-gbm(Ynpk ~ Tmax + Tmin + GDD.10 + PRE +  RAD +Cultivar+  Soil.type
                + Soil.texture + SOM + OP + AK + pH + N + P2O5 + K2O,
                data = sub_M_NCP_train, distribution = "gaussian", n.trees = 3000,
                interaction.depth = 9, shrinkage = 0.01, 
                bag.fraction = 0.5,cv.folds = 10)
  
  brt_MNCP_recyle[[i]]<-brt_MNCP
  print(brt_MNCP)
  best_inter_MNCP<-gbm.perf(brt_MNCP,method = "cv")
  best_inter_MNCP_recyle[[i]]<-best_inter_MNCP
  summary_MNCP_record<-summary(brt_MNCP,n.trees=best_inter_MNCP)
  summary_MNCP_recyle[[i]] <-summary_MNCP_record
  
  ###comparison of model performance with lm
  ###predict for test data
  #by GBRT
  sub_M_NCP_test$Ynpk_pred_1<-predict(brt_MNCP,sub_M_NCP_test)
  
  #plot of actual and predict values
  plot(sub_M_NCP_test$Ynpk_pred_1,sub_M_NCP_test$Ynpk, xlim=c(6000,12000),ylim=c(6000,12000),
       xlab="Predicted yield (kg/ha)", ylab="Observed yield (kg/ha)")
  abline(a=0,b=1)
  fitline_MNCP<-lm(Ynpk~Ynpk_pred_1,sub_M_NCP_test)
  abline(fitline_MNCP,lty=2)
  summary(fitline_MNCP)

  #caculate R, R2 and RMSE
  cor_MNCP_1 <- cor(sub_M_NCP_test$Ynpk_pred_1,sub_M_NCP_test$Ynpk)
  R2_MNCP_1 <- R2(sub_M_NCP_test$Ynpk_pred_1,sub_M_NCP_test$Ynpk)
  RMSE_MNCP_1 <- RMSE(sub_M_NCP_test$Ynpk_pred_1,sub_M_NCP_test$Ynpk)
  nRMSE_MNCP_1<-RMSE_MNCP_1/mean(sub_M_NCP_test$Ynpk)*100
  ME_MNCP_1<-me(sub_M_NCP_test$Ynpk_pred_1,sub_M_NCP_test$Ynpk)
  P_value_MNCP_1<-t.test(sub_M_NCP_test$Ynpk_pred_1,sub_M_NCP_test$Ynpk)$p.value

  cor_MNCP_1_recyle[[i]] <- cor_MNCP_1
  R2_MNCP_1_recyle[[i]] <- R2_MNCP_1
  RMSE_MNCP_1_recyle[[i]] <- RMSE_MNCP_1
  nRMSE_MNCP_1_recyle[[i]] <- nRMSE_MNCP_1
  ME_MNCP_1_recyle[[i]] <- ME_MNCP_1
  P_value_MNCP_1_recyle[[i]]<-P_value_MNCP_1

  #by lm
  lm_fit1_MNCP<-lm(Ynpk ~ Tmax + Tmin + GDD.10 + PRE +  RAD +Cultivar+ Soil.type +Soil.texture
                   + SOM + OP + AK + pH + N + P2O5 + K2O, data = sub_M_NCP_train)
  summary(lm_fit1_MNCP)
  
  sub_M_NCP_test$Ynpk_pred_2<-predict(lm_fit1_MNCP,sub_M_NCP_test)
  
  #plot of actual and predict values
  plot(sub_M_NCP_test$Ynpk_pred_2,sub_M_NCP_test$Ynpk, xlim=c(6000,12000),ylim=c(6000,12000),
       xlab="Predicted yield (kg/ha)", ylab="Observed yield (kg/ha)")
  abline(a=0,b=1)
  fitline2<-lm(Ynpk~Ynpk_pred_2,sub_M_NCP_test)
  abline(fitline2,lty=2)
  summary(fitline2)
  
  #caculate R, R2 and RMSE
  cor_MNCP_2 <- cor(sub_M_NCP_test$Ynpk_pred_2,sub_M_NCP_test$Ynpk)
  R2_MNCP_2 <- R2(sub_M_NCP_test$Ynpk_pred_2,sub_M_NCP_test$Ynpk)
  RMSE_MNCP_2 <- RMSE(sub_M_NCP_test$Ynpk_pred_2,sub_M_NCP_test$Ynpk)
  nRMSE_MNCP_2<-RMSE_MNCP_2/mean(sub_M_NCP_test$Ynpk)*100
  ME_MNCP_2<-me(sub_M_NCP_test$Ynpk_pred_2,sub_M_NCP_test$Ynpk)
  P_value_MNCP_2<-t.test(sub_M_NCP_test$Ynpk_pred_2,sub_M_NCP_test$Ynpk)$p.value

  cor_MNCP_2_recyle[[i]] <- cor_MNCP_2
  R2_MNCP_2_recyle[[i]] <- R2_MNCP_2
  RMSE_MNCP_2_recyle[[i]] <- RMSE_MNCP_2
  nRMSE_MNCP_2_recyle[[i]] <- nRMSE_MNCP_2
  ME_MNCP_2_recyle[[i]] <- ME_MNCP_2
  P_value_MNCP_2_recyle[[i]]<-P_value_MNCP_2
}

RMSE_MNCP_1_recyle <- as.data.frame(RMSE_MNCP_1_recyle)
nRMSE_MNCP_1_recyle<- as.data.frame(nRMSE_MNCP_1_recyle)
ME_MNCP_1_recyle <- as.data.frame(ME_MNCP_1_recyle)
P_value_MNCP_1_recyle<-as.data.frame(P_value_MNCP_1_recyle)

summary(RMSE_MNCP_1_recyle$RMSE_MNCP_1_recyle)
sd(RMSE_MNCP_1_recyle$RMSE_MNCP_1_recyle)
summary(nRMSE_MNCP_1_recyle$nRMSE_MNCP_1_recyle)
sd(nRMSE_MNCP_1_recyle$nRMSE_MNCP_1_recyle)
summary(ME_MNCP_1_recyle$ME_MNCP_1_recyle)
sd(ME_MNCP_1_recyle$ME_MNCP_1_recyle)
summary(P_value_MNCP_1_recyle$P_value_MNCP_1_recyle)
sd(P_value_MNCP_1_recyle$P_value_MNCP_1_recyle)

RMSE_MNCP_2_recyle <- as.data.frame(RMSE_MNCP_2_recyle)
nRMSE_MNCP_2_recyle<- as.data.frame(nRMSE_MNCP_2_recyle)
ME_MNCP_2_recyle <- as.data.frame(ME_MNCP_2_recyle)
P_value_MNCP_2_recyle<-as.data.frame(P_value_MNCP_2_recyle)

summary(RMSE_MNCP_2_recyle$RMSE_MNCP_2_recyle)
sd(RMSE_MNCP_2_recyle$RMSE_MNCP_2_recyle)
summary(nRMSE_2_recyle$nRMSE_2_recyle)
sd(nRMSE_MNCP_2_recyle$nRMSE_MNCP_2_recyle)
summary(ME_MNCP_2_recyle$ME_MNCP_2_recyle)
sd(ME_MNCP_2_recyle$ME_MNCP_2_recyle)
summary(P_value_MNCP_2_recyle$P_value_MNCP_2_recyle)
sd(P_value_MNCP_2_recyle$P_value_MNCP_2_recyle)


#==========================================================
# set model for maize in SW

M_SWC<-read.csv("M-SWC.csv")
M_SWC$Year<-as.factor(M_SWC$Year)
M_SWC$Cultivar<-as.factor(M_SWC$Cultivar)
M_SWC$Soil.type<-as.factor(M_SWC$Soil.type)
M_SWC$Soil.texture<-as.factor(M_SWC$Soil.texture)

# set train and test data(random 10% for 50 times)
brt_MSWC_recyle<- NULL
best_inter_MSWC_recyle<-NULL
summary_MSWC_recyle <- NULL
cor_MSWC_1_recyle <- NULL
R2_MSWC_1_recyle <- NULL
RMSE_MSWC_1_recyle <- NULL
nRMSE_MSWC_1_recyle<- NULL
ME_MSWC_1_recyle <- NULL
P_value_MSWC_1_recyle<-NULL

cor_MSWC_2_recyle <- NULL
R2_MSWC_2_recyle <- NULL
RMSE_MSWC_2_recyle <- NULL
nRMSE_MSWC_2_recyle<- NULL
ME_MSWC_2_recyle <- NULL
P_value_MSWC_2_recyle<-NULL

for (i in 1:50) {
 
  set.seed(i)
  train_MSWC <- sample(nrow(M_SWC), 0.9*nrow(M_SWC))
  M_SWC_train <- M_SWC[train_MSWC,] 
  M_SWC_test <- M_SWC[-train_MSWC,]
  
  # select required variables
  sub_M_SWC_train <- M_SWC_train %>%
    select(NO, Province1,Tave, Tmax, Tmin, GDD.10, PRE, SSD, RAD, Cultivar, Soil.type, 
           SOM, OP , AK, pH, N, P2O5, K2O, Yck, Ynpk)
  sub_M_SWC_test <- M_SWC_test %>%
    select(NO, Province1,Tave, Tmax, Tmin, GDD.10, PRE, SSD, RAD, Cultivar, Soil.type, 
           SOM, OP , AK, pH, N, P2O5, K2O, Yck, Ynpk)
  
  #building brt model
  set.seed(i)
  brt_MSWC<-gbm(Ynpk ~ Tmax + Tmin + GDD.10 + PRE +  RAD + Cultivar+ Soil.type
                + SOM + OP + AK + pH + N + P2O5 + K2O,
                data = sub_M_SWC_train, distribution = "gaussian", n.trees = 3000,
                interaction.depth = 9, shrinkage = 0.005, 
                bag.fraction = 0.5,cv.folds = 10)
  
  brt_MSWC_recyle[[i]]<-brt_MSWC
  print(brt_MSWC)
  best_inter_MSWC<-gbm.perf(brt_MSWC,method = "cv")
  best_inter_MSWC_recyle[[i]]<-best_inter_MSWC
  summary_MSWC_record<-summary(brt_MSWC,n.trees=best_inter_MSWC)
  summary_MSWC_recyle[[i]] <-summary_MSWC_record
  
  ###comparison of model performance with lm
  ###predict for test data
  #by GBRT
  sub_M_SWC_test$Ynpk_pred_1<-predict(brt_MSWC,sub_M_SWC_test)
  
  #plot of actual and predict values
  plot(sub_M_SWC_test$Ynpk_pred_1,sub_M_SWC_test$Ynpk, xlim=c(3500,9500),ylim=c(3500,9500),
       xlab="Predicted yield (kg/ha)", ylab="Observed yield (kg/ha)")
  abline(a=0,b=1)
  fitline_MSWC<-lm(Ynpk~Ynpk_pred_1,sub_M_SWC_test)
  abline(fitline_MSWC,lty=2)
  summary(fitline_MSWC)
  
  #caculate R, R2 and RMSE
  cor_MSWC_1 <- cor(sub_M_SWC_test$Ynpk_pred_1,sub_M_SWC_test$Ynpk)
  R2_MSWC_1 <- R2(sub_M_SWC_test$Ynpk_pred_1,sub_M_SWC_test$Ynpk)
  RMSE_MSWC_1 <- RMSE(sub_M_SWC_test$Ynpk_pred_1,sub_M_SWC_test$Ynpk)
  nRMSE_MSWC_1<-RMSE_MSWC_1/mean(sub_M_SWC_test$Ynpk)*100
  ME_MSWC_1<-me(sub_M_SWC_test$Ynpk_pred_1,sub_M_SWC_test$Ynpk)
  P_value_MSWC_1<-t.test(sub_M_SWC_test$Ynpk_pred_1,sub_M_SWC_test$Ynpk)$p.value

  cor_MSWC_1_recyle[[i]] <- cor_MSWC_1
  R2_MSWC_1_recyle[[i]] <- R2_MSWC_1
  RMSE_MSWC_1_recyle[[i]] <- RMSE_MSWC_1
  nRMSE_MSWC_1_recyle[[i]] <- nRMSE_MSWC_1
  ME_MSWC_1_recyle[[i]] <- ME_MSWC_1
  P_value_MSWC_1_recyle[[i]]<-P_value_MSWC_1
  
  #by lm
  lm_fit1_MSWC<-lm(Ynpk ~ Tmax + Tmin + GDD.10 + PRE +  RAD +  Cultivar + Soil.type 
                   + SOM + OP + AK + pH + N + P2O5 + K2O, data = sub_M_SWC_train)
  summary(lm_fit1_MSWC)
  
  sub_M_SWC_test$Ynpk_pred_2<-predict(lm_fit1_MSWC,sub_M_SWC_test)
  
  #plot of actual and predict values
  plot(sub_M_SWC_test$Ynpk_pred_2,sub_M_SWC_test$Ynpk, xlim=c(3500,9500),ylim=c(3500,9500),
       xlab="Predicted yield (kg/ha)", ylab="Observed yield (kg/ha)")
  abline(a=0,b=1)
  fitline2<-lm(Ynpk~Ynpk_pred_2,sub_M_SWC_test)
  abline(fitline2,lty=2)
  summary(fitline2)
  
  #caculate R, R2 and RMSE
  cor_MSWC_2 <- cor(sub_M_SWC_test$Ynpk_pred_2,sub_M_SWC_test$Ynpk)
  R2_MSWC_2 <- R2(sub_M_SWC_test$Ynpk_pred_2,sub_M_SWC_test$Ynpk)
  RMSE_MSWC_2 <- RMSE(sub_M_SWC_test$Ynpk_pred_2,sub_M_SWC_test$Ynpk)
  nRMSE_MSWC_2<-RMSE_MSWC_2/mean(sub_M_SWC_test$Ynpk)*100
  ME_MSWC_2<-me(sub_M_SWC_test$Ynpk_pred_2,sub_M_SWC_test$Ynpk)
  P_value_MSWC_2<-t.test(sub_M_SWC_test$Ynpk_pred_2,sub_M_SWC_test$Ynpk)$p.value

  cor_MSWC_2_recyle[[i]] <- cor_MSWC_2
  R2_MSWC_2_recyle[[i]] <- R2_MSWC_2
  RMSE_MSWC_2_recyle[[i]] <- RMSE_MSWC_2
  nRMSE_MSWC_2_recyle[[i]] <- nRMSE_MSWC_2
  ME_MSWC_2_recyle[[i]] <- ME_MSWC_2
  P_value_MSWC_2_recyle[[i]]<-P_value_MSWC_2
}

RMSE_MSWC_1_recyle <- as.data.frame(RMSE_MSWC_1_recyle)
nRMSE_MSWC_1_recyle<- as.data.frame(nRMSE_MSWC_1_recyle)
ME_MSWC_1_recyle <- as.data.frame(ME_MSWC_1_recyle)
P_value_MSWC_1_recyle<-as.data.frame(P_value_MSWC_1_recyle)

summary(RMSE_MSWC_1_recyle$RMSE_MSWC_1_recyle)
sd(RMSE_MSWC_1_recyle$RMSE_MSWC_1_recyle)
summary(nRMSE_MSWC_1_recyle$nRMSE_MSWC_1_recyle)
sd(nRMSE_MSWC_1_recyle$nRMSE_MSWC_1_recyle)
summary(ME_MSWC_1_recyle$ME_MSWC_1_recyle)
sd(ME_MSWC_1_recyle$ME_MSWC_1_recyle)
summary(P_value_MSWC_1_recyle$P_value_MSWC_1_recyle)
sd(P_value_MSWC_1_recyle$P_value_MSWC_1_recyle)

RMSE_MSWC_2_recyle <- as.data.frame(RMSE_MSWC_2_recyle)
nRMSE_MSWC_2_recyle<- as.data.frame(nRMSE_MSWC_2_recyle)
ME_MSWC_2_recyle <- as.data.frame(ME_MSWC_2_recyle)
P_value_MSWC_2_recyle<-as.data.frame(P_value_MSWC_2_recyle)

summary(RMSE_MSWC_2_recyle$RMSE_MSWC_2_recyle)
sd(RMSE_MSWC_2_recyle$RMSE_MSWC_2_recyle)
summary(nRMSE_2_recyle$nRMSE_2_recyle)
sd(nRMSE_MSWC_2_recyle$nRMSE_MSWC_2_recyle)
summary(ME_MSWC_2_recyle$ME_MSWC_2_recyle)
sd(ME_MSWC_2_recyle$ME_MSWC_2_recyle)
summary(P_value_MSWC_2_recyle$P_value_MSWC_2_recyle)
sd(P_value_MSWC_2_recyle$P_value_MSWC_2_recyle)

#==========================================================
# set model for single rice in YZB

SR_YZB<-read.csv("SR-YZB.csv")
SR_YZB$Year<-as.factor(SR_YZB$Year)
SR_YZB$Cultivar<-as.factor(SR_YZB$Cultivar)
SR_YZB$Soil.type<-as.factor(SR_YZB$Soil.type)
SR_YZB$Soil.texture<-as.factor(SR_YZB$Soil.texture)

# set train and test data(random 10% for 50 times)
brt_SRYZB_recyle<- NULL
best_inter_SRYZB_recyle<-NULL
summary_SRYZB_recyle <- NULL
cor_SRYZB_1_recyle <- NULL
R2_SRYZB_1_recyle <- NULL
RMSE_SRYZB_1_recyle <- NULL
nRMSE_SRYZB_1_recyle<- NULL
ME_SRYZB_1_recyle <- NULL
P_value_SRYZB_1_recyle<-NULL

cor_SRYZB_2_recyle <- NULL
R2_SRYZB_2_recyle <- NULL
RMSE_SRYZB_2_recyle <- NULL
nRMSE_SRYZB_2_recyle<- NULL
ME_SRYZB_2_recyle <- NULL
P_value_SRYZB_2_recyle<-NULL

for (i in 1:50) {
  
  set.seed(i)
  train_SRYZB <- sample(nrow(SR_YZB), 0.9*nrow(SR_YZB))
  SR_YZB_train <- SR_YZB[train_SRYZB,] 
  SR_YZB_test <- SR_YZB[-train_SRYZB,]
  
  # select required variables
  sub_SR_YZB_train <- SR_YZB_train %>%
    select(NO, Province1,Tave, Tmax, Tmin, GDD.10, PRE, SSD, RAD, Cultivar, Soil.type, Soil.type1,
           Soil.texture, SOM, OP , AK, pH, N, P2O5, K2O, Yck, Ynpk)
  sub_SR_YZB_test <- SR_YZB_test %>%
    select(NO, Province1,Tave, Tmax, Tmin, GDD.10, PRE, SSD, RAD, Cultivar, Soil.type, Soil.type1,
           Soil.texture, SOM, OP , AK, pH, N, P2O5, K2O, Yck, Ynpk)
  
  #building brt model
  set.seed(123)
  brt_SRYZB<-gbm(Ynpk ~ Tmax + Tmin + GDD.10 + PRE +  RAD + Cultivar + Soil.type
                 + Soil.texture + SOM + OP + AK + pH + N + P2O5 + K2O,
                 data = sub_SR_YZB_train, distribution = "gaussian", n.trees = 2000,
                 interaction.depth = 9, shrinkage = 0.01, 
                 bag.fraction = 0.5,cv.folds = 10)
  
  brt_SRYZB_recyle[[i]]<-brt_SRYZB
  print(brt_SRYZB)
  best_inter_SRYZB<-gbm.perf(brt_SRYZB,method = "cv")
  best_inter_SRYZB_recyle[[i]]<-best_inter_SRYZB
  summary_SRYZB_record<-summary(brt_SRYZB,n.trees=best_inter_SRYZB)
  summary_SRYZB_recyle[[i]] <-summary_SRYZB_record
  
  ###comparison of model performance with lm
  ###predict for test data
  #by GBRT
  sub_SR_YZB_test$Ynpk_pred_1<-predict(brt_SRYZB,sub_SR_YZB_test)
  
  #plot of actual and predict values
  plot(sub_SR_YZB_test$Ynpk_pred_1,sub_SR_YZB_test$Ynpk, xlim=c(3500,9500),ylim=c(3500,9500),
       xlab="Predicted yield (kg/ha)", ylab="Observed yield (kg/ha)")
  abline(a=0,b=1)
  fitline_SRYZB<-lm(Ynpk~Ynpk_pred_1,sub_SR_YZB_test)
  abline(fitline_SRYZB,lty=2)
  summary(fitline_SRYZB)
  
  #caculate R, R2 and RMSE
  cor_SRYZB_1 <- cor(sub_SR_YZB_test$Ynpk_pred_1,sub_SR_YZB_test$Ynpk)
  R2_SRYZB_1 <- R2(sub_SR_YZB_test$Ynpk_pred_1,sub_SR_YZB_test$Ynpk)
  RMSE_SRYZB_1 <- RMSE(sub_SR_YZB_test$Ynpk_pred_1,sub_SR_YZB_test$Ynpk)
  nRMSE_SRYZB_1<-RMSE_SRYZB_1/mean(sub_SR_YZB_test$Ynpk)*100
  ME_SRYZB_1<-me(sub_SR_YZB_test$Ynpk_pred_1,sub_SR_YZB_test$Ynpk)
  P_value_SRYZB_1<-t.test(sub_SR_YZB_test$Ynpk_pred_1,sub_SR_YZB_test$Ynpk)$p.value

  cor_SRYZB_1_recyle[[i]] <- cor_SRYZB_1
  R2_SRYZB_1_recyle[[i]] <- R2_SRYZB_1
  RMSE_SRYZB_1_recyle[[i]] <- RMSE_SRYZB_1
  nRMSE_SRYZB_1_recyle[[i]] <- nRMSE_SRYZB_1
  ME_SRYZB_1_recyle[[i]] <- ME_SRYZB_1
  P_value_SRYZB_1_recyle[[i]]<-P_value_SRYZB_1

  #by lm
  lm_fit1_SRYZB<-lm(Ynpk ~ Tmax + Tmin + GDD.10 + PRE +  RAD + Cultivar +Soil.type +Soil.texture
                    + SOM + OP + AK + pH + N + P2O5 + K2O, data = sub_SR_YZB_train)
  summary(lm_fit1_SRYZB)
  
  sub_SR_YZB_test$Ynpk_pred_2<-predict(lm_fit1_SRYZB,sub_SR_YZB_test)
  
  #plot of actual and predict values
  plot(sub_SR_YZB_test$Ynpk_pred_2,sub_SR_YZB_test$Ynpk, xlim=c(3500,9500),ylim=c(3500,9500),
       xlab="Predicted yield (kg/ha)", ylab="Observed yield (kg/ha)")
  abline(a=0,b=1)
  fitline2<-lm(Ynpk~Ynpk_pred_2,sub_SR_YZB_test)
  abline(fitline2,lty=2)
  summary(fitline2)
  
  #caculate R, R2 and RMSE
  cor_SRYZB_2 <- cor(sub_SR_YZB_test$Ynpk_pred_2,sub_SR_YZB_test$Ynpk)
  R2_SRYZB_2 <- R2(sub_SR_YZB_test$Ynpk_pred_2,sub_SR_YZB_test$Ynpk)
  RMSE_SRYZB_2 <- RMSE(sub_SR_YZB_test$Ynpk_pred_2,sub_SR_YZB_test$Ynpk)
  nRMSE_SRYZB_2<-RMSE_SRYZB_2/mean(sub_SR_YZB_test$Ynpk)*100
  ME_SRYZB_2<-me(sub_SR_YZB_test$Ynpk_pred_2,sub_SR_YZB_test$Ynpk)
  P_value_SRYZB_2<-t.test(sub_SR_YZB_test$Ynpk_pred_2,sub_SR_YZB_test$Ynpk)$p.value

  cor_SRYZB_2_recyle[[i]] <- cor_SRYZB_2
  R2_SRYZB_2_recyle[[i]] <- R2_SRYZB_2
  RMSE_SRYZB_2_recyle[[i]] <- RMSE_SRYZB_2
  nRMSE_SRYZB_2_recyle[[i]] <- nRMSE_SRYZB_2
  ME_SRYZB_2_recyle[[i]] <- ME_SRYZB_2
  P_value_SRYZB_2_recyle[[i]]<-P_value_SRYZB_2
}

RMSE_SRYZB_1_recyle <- as.data.frame(RMSE_SRYZB_1_recyle)
nRMSE_SRYZB_1_recyle<- as.data.frame(nRMSE_SRYZB_1_recyle)
ME_SRYZB_1_recyle <- as.data.frame(ME_SRYZB_1_recyle)
P_value_SRYZB_1_recyle<-as.data.frame(P_value_SRYZB_1_recyle)

summary(RMSE_SRYZB_1_recyle$RMSE_SRYZB_1_recyle)
sd(RMSE_SRYZB_1_recyle$RMSE_SRYZB_1_recyle)
summary(nRMSE_SRYZB_1_recyle$nRMSE_SRYZB_1_recyle)
sd(nRMSE_SRYZB_1_recyle$nRMSE_SRYZB_1_recyle)
summary(ME_SRYZB_1_recyle$ME_SRYZB_1_recyle)
sd(ME_SRYZB_1_recyle$ME_SRYZB_1_recyle)
summary(P_value_SRYZB_1_recyle$P_value_SRYZB_1_recyle)
sd(P_value_SRYZB_1_recyle$P_value_SRYZB_1_recyle)
summary(MAE_SRYZB_1_recyle$MAE_SRYZB_1_recyle)

RMSE_SRYZB_2_recyle <- as.data.frame(RMSE_SRYZB_2_recyle)
nRMSE_SRYZB_2_recyle<- as.data.frame(nRMSE_SRYZB_2_recyle)
ME_SRYZB_2_recyle <- as.data.frame(ME_SRYZB_2_recyle)
P_value_SRYZB_2_recyle<-as.data.frame(P_value_SRYZB_2_recyle)

summary(RMSE_SRYZB_2_recyle$RMSE_SRYZB_2_recyle)
sd(RMSE_SRYZB_2_recyle$RMSE_SRYZB_2_recyle)
summary(nRMSE_2_recyle$nRMSE_2_recyle)
sd(nRMSE_SRYZB_2_recyle$nRMSE_SRYZB_2_recyle)
summary(nRMSE_sd_SRYZB_2_recyle$nRMSE_sd_SRYZB_2_recyle)
sd(ME_SRYZB_2_recyle$ME_SRYZB_2_recyle)
summary(P_value_SRYZB_2_recyle$P_value_SRYZB_2_recyle)
sd(P_value_SRYZB_2_recyle$P_value_SRYZB_2_recyle)
summary(MAE_SRYZB_2_recyle$MAE_SRYZB_2_recyle)

#==========================================================
# set model for early rice in SC

ER_SC<-read.csv("ER-SC.csv")
ER_SC$Year<-as.factor(ER_SC$Year)
ER_SC$Cultivar<-as.factor(ER_SC$Cultivar)
ER_SC$Soil.type<-as.factor(ER_SC$Soil.type)
ER_SC$Soil.texture<-as.factor(ER_SC$Soil.texture)

# set train and test data(random 10% for 50 times)
brt_ERSC_recyle<- NULL
best_inter_ERSC_recyle<-NULL
summary_ERSC_recyle <- NULL
cor_ERSC_1_recyle <- NULL
R2_ERSC_1_recyle <- NULL
RMSE_ERSC_1_recyle <- NULL
nRMSE_ERSC_1_recyle<- NULL
ME_ERSC_1_recyle <- NULL
P_value_ERSC_1_recyle<-NULL

cor_ERSC_2_recyle <- NULL
R2_ERSC_2_recyle <- NULL
RMSE_ERSC_2_recyle <- NULL
nRMSE_ERSC_2_recyle<- NULL
ME_ERSC_2_recyle <- NULL
P_value_ERSC_2_recyle<-NULL

for (i in 1:50) {
  # set train and test data(random 10%)

  set.seed(i)
  train_ERSC <- sample(nrow(ER_SC), 0.9*nrow(ER_SC))
  ER_SC_train <- ER_SC[train_ERSC,] 
  ER_SC_test <- ER_SC[-train_ERSC,]
  
  # select required variables
  sub_ER_SC_train <- ER_SC_train %>%
    select(NO, Province,Tave, Tmax, Tmin, GDD.10, PRE, SSD, RAD, Cultivar, Soil.type, 
           Soil.texture, SOM, OP , AK, pH, N, P2O5, K2O, Yck, Ynpk)
  sub_ER_SC_test <- ER_SC_test %>%
    select(NO, Province,Tave, Tmax, Tmin, GDD.10, PRE, SSD, RAD, Cultivar, Soil.type, 
           Soil.texture, SOM, OP , AK, pH, N, P2O5, K2O, Yck, Ynpk)
  
  #building brt model
  set.seed(123)
  brt_ERSC<-gbm(Ynpk ~ Tmax + Tmin + GDD.10 + PRE +  RAD +Cultivar
                + SOM + OP + AK + pH + N + P2O5 + K2O,
                data = sub_ER_SC_train, distribution = "gaussian", n.trees = 2000,
                interaction.depth = 9, shrinkage = 0.005, 
                bag.fraction = 0.5,cv.folds = 10)
  
  brt_ERSC_recyle[[i]]<-brt_ERSC
  print(brt_ERSC)
  best_inter_ERSC<-gbm.perf(brt_ERSC,method = "cv")
  best_inter_ERSC_recyle[[i]]<-best_inter_ERSC
  summary_ERSC_record<-summary(brt_ERSC,n.trees=best_inter_ERSC)
  summary_ERSC_recyle[[i]] <-summary_ERSC_record
  
  ###comparison of model performance with lm
  ###predict for test data
  #by GBRT
  sub_ER_SC_test$Ynpk_pred_1<-predict(brt_ERSC,sub_ER_SC_test)
  
  #plot of actual and predict values
  plot(sub_ER_SC_test$Ynpk_pred_1,sub_ER_SC_test$Ynpk, xlim=c(3500,9500),ylim=c(3500,9500),
       xlab="Predicted yield (kg/ha)", ylab="Observed yield (kg/ha)")
  abline(a=0,b=1)
  fitline_ERSC<-lm(Ynpk~Ynpk_pred_1,sub_ER_SC_test)
  abline(fitline_ERSC,lty=2)
  summary(fitline_ERSC)

  #caculate R, R2 and RMSE
  cor_ERSC_1 <- cor(sub_ER_SC_test$Ynpk_pred_1,sub_ER_SC_test$Ynpk)
  R2_ERSC_1 <- R2(sub_ER_SC_test$Ynpk_pred_1,sub_ER_SC_test$Ynpk)
  RMSE_ERSC_1 <- RMSE(sub_ER_SC_test$Ynpk_pred_1,sub_ER_SC_test$Ynpk)
  nRMSE_ERSC_1<-RMSE_ERSC_1/mean(sub_ER_SC_test$Ynpk)*100
  ME_ERSC_1<-me(sub_ER_SC_test$Ynpk_pred_1,sub_ER_SC_test$Ynpk)
  P_value_ERSC_1<-t.test(sub_ER_SC_test$Ynpk_pred_1,sub_ER_SC_test$Ynpk)$p.value

  cor_ERSC_1_recyle[[i]] <- cor_ERSC_1
  R2_ERSC_1_recyle[[i]] <- R2_ERSC_1
  RMSE_ERSC_1_recyle[[i]] <- RMSE_ERSC_1
  nRMSE_ERSC_1_recyle[[i]] <- nRMSE_ERSC_1
  ME_ERSC_1_recyle[[i]] <- ME_ERSC_1
  P_value_ERSC_1_recyle[[i]]<-P_value_ERSC_1

  #by lm
  lm_fit1_ERSC<-lm(Ynpk ~ Tmax + Tmin + GDD.10 + PRE +  RAD +Cultivar
                   + SOM + OP + AK + pH + N + P2O5 + K2O, data = sub_ER_SC_train)
  summary(lm_fit1_ERSC)
  
  sub_ER_SC_test$Ynpk_pred_2<-predict(lm_fit1_ERSC,sub_ER_SC_test)
  
  #plot of actual and predict values
  plot(sub_ER_SC_test$Ynpk_pred_2,sub_ER_SC_test$Ynpk, xlim=c(3500,9500),ylim=c(3500,9500),
       xlab="Predicted yield (kg/ha)", ylab="Observed yield (kg/ha)")
  abline(a=0,b=1)
  fitline2<-lm(Ynpk~Ynpk_pred_2,sub_ER_SC_test)
  abline(fitline2,lty=2)
  summary(fitline2)
  
  #caculate R, R2 and RMSE
  cor_ERSC_2 <- cor(sub_ER_SC_test$Ynpk_pred_2,sub_ER_SC_test$Ynpk)
  R2_ERSC_2 <- R2(sub_ER_SC_test$Ynpk_pred_2,sub_ER_SC_test$Ynpk)
  RMSE_ERSC_2 <- RMSE(sub_ER_SC_test$Ynpk_pred_2,sub_ER_SC_test$Ynpk)
  nRMSE_ERSC_2<-RMSE_ERSC_2/mean(sub_ER_SC_test$Ynpk)*100
  ME_ERSC_2<-me(sub_ER_SC_test$Ynpk_pred_2,sub_ER_SC_test$Ynpk)
  P_value_ERSC_2<-t.test(sub_ER_SC_test$Ynpk_pred_2,sub_ER_SC_test$Ynpk)$p.value

  cor_ERSC_2_recyle[[i]] <- cor_ERSC_2
  R2_ERSC_2_recyle[[i]] <- R2_ERSC_2
  RMSE_ERSC_2_recyle[[i]] <- RMSE_ERSC_2
  nRMSE_ERSC_2_recyle[[i]] <- nRMSE_ERSC_2
  ME_ERSC_2_recyle[[i]] <- ME_ERSC_2
  P_value_ERSC_2_recyle[[i]]<-P_value_ERSC_2
}

RMSE_ERSC_1_recyle <- as.data.frame(RMSE_ERSC_1_recyle)
nRMSE_ERSC_1_recyle<- as.data.frame(nRMSE_ERSC_1_recyle)
ME_ERSC_1_recyle <- as.data.frame(ME_ERSC_1_recyle)
P_value_ERSC_1_recyle<-as.data.frame(P_value_ERSC_1_recyle)

summary(RMSE_ERSC_1_recyle$RMSE_ERSC_1_recyle)
sd(RMSE_ERSC_1_recyle$RMSE_ERSC_1_recyle)
summary(nRMSE_ERSC_1_recyle$nRMSE_ERSC_1_recyle)
sd(nRMSE_ERSC_1_recyle$nRMSE_ERSC_1_recyle)
summary(ME_ERSC_1_recyle$ME_ERSC_1_recyle)
sd(ME_ERSC_1_recyle$ME_ERSC_1_recyle)
summary(P_value_ERSC_1_recyle$P_value_ERSC_1_recyle)
sd(P_value_ERSC_1_recyle$P_value_ERSC_1_recyle)

RMSE_ERSC_2_recyle <- as.data.frame(RMSE_ERSC_2_recyle)
nRMSE_ERSC_2_recyle<- as.data.frame(nRMSE_ERSC_2_recyle)
ME_ERSC_2_recyle <- as.data.frame(ME_ERSC_2_recyle)
P_value_ERSC_2_recyle<-as.data.frame(P_value_ERSC_2_recyle)

summary(RMSE_ERSC_2_recyle$RMSE_ERSC_2_recyle)
sd(RMSE_ERSC_2_recyle$RMSE_ERSC_2_recyle)
summary(nRMSE_2_recyle$nRMSE_2_recyle)
sd(nRMSE_ERSC_2_recyle$nRMSE_ERSC_2_recyle)
summary(ME_ERSC_2_recyle$ME_ERSC_2_recyle)
sd(ME_ERSC_2_recyle$ME_ERSC_2_recyle)
summary(P_value_ERSC_2_recyle$P_value_ERSC_2_recyle)
sd(P_value_ERSC_2_recyle$P_value_ERSC_2_recyle)

#==========================================================
# set model for late rice in SC

LR_SC<-read.csv("LR-SC.csv")
LR_SC$Year<-as.factor(LR_SC$Year)
LR_SC$Cultivar<-as.factor(LR_SC$Cultivar)
LR_SC$Soil.type<-as.factor(LR_SC$Soil.type)
LR_SC$Soil.texture<-as.factor(LR_SC$Soil.texture)

# set train and test data(random 10% for 50 times)
brt_LRSC_recyle<- NULL
best_inter_LRSC_recyle<-NULL
summary_LRSC_recyle <- NULL
cor_LRSC_1_recyle <- NULL
R2_LRSC_1_recyle <- NULL
RMSE_LRSC_1_recyle <- NULL
nRMSE_LRSC_1_recyle<- NULL
ME_LRSC_1_recyle <- NULL
P_value_LRSC_1_recyle<-NULL

cor_LRSC_2_recyle <- NULL
R2_LRSC_2_recyle <- NULL
RMSE_LRSC_2_recyle <- NULL
nRMSE_LRSC_2_recyle<- NULL
ME_LRSC_2_recyle <- NULL
P_value_LRSC_2_recyle<-NULL

for (i in 1:50) {
  # set train and test data(random 10%)
  set.seed(i)
  trainingRow_LRSC <- createDataPartition(LR_SC$Province1,p=0.9, list = F)
  LR_SC_train <- LR_SC[trainingRow_LRSC,] 
  LR_SC_test <- LR_SC[-trainingRow_LRSC,] 
  
  # select required variables
  sub_LR_SC_train <- LR_SC_train %>%
    select(NO, Province1,Tave, Tmax, Tmin, GDD.10, PRE, SSD, RAD, Cultivar, Soil.type, 
           Soil.texture, SOM, OP , AK, pH, N, P2O5, K2O, Yck, Ynpk)
  sub_LR_SC_test <- LR_SC_test %>%
    select(NO, Province1,Tave, Tmax, Tmin, GDD.10, PRE, SSD, RAD, Cultivar, Soil.type, 
           Soil.texture, SOM, OP , AK, pH, N, P2O5, K2O, Yck, Ynpk)
  
  #building brt model
  set.seed(123)
  brt_LRSC<-gbm(Ynpk ~ Tmax + Tmin + GDD.10 + PRE +  RAD +Cultivar
                + Soil.texture + SOM + OP + AK + pH + N + P2O5 + K2O,
                data = sub_LR_SC_train, distribution = "gaussian", n.trees = 2000,
                interaction.depth = 9, shrinkage = 0.005, 
                bag.fraction = 0.5,cv.folds = 10)
  
  brt_LRSC_recyle[[i]]<-brt_LRSC
  print(brt_LRSC)
  best_inter_LRSC<-gbm.perf(brt_LRSC,method = "cv")
  best_inter_LRSC_recyle[[i]]<-best_inter_LRSC
  summary_LRSC_record<-summary(brt_LRSC,n.trees=best_inter_LRSC)
  summary_LRSC_recyle[[i]] <-summary_LRSC_record
  
  ###comparison of model performance with lm
  ###predict for test data
  #by GBRT
  sub_LR_SC_test$Ynpk_pred_1<-predict(brt_LRSC,sub_LR_SC_test)
  
  #plot of actual and predict values
  plot(sub_LR_SC_test$Ynpk_pred_1,sub_LR_SC_test$Ynpk, xlim=c(3500,9500),ylim=c(3500,9500),
       xlab="Predicted yield (kg/ha)", ylab="Observed yield (kg/ha)")
  abline(a=0,b=1)
  fitline_LRSC<-lm(Ynpk~Ynpk_pred_1,sub_LR_SC_test)
  abline(fitline_LRSC,lty=2)
  summary(fitline_LRSC)
  
  #caculate R, R2 and RMSE
  cor_LRSC_1 <- cor(sub_LR_SC_test$Ynpk_pred_1,sub_LR_SC_test$Ynpk)
  R2_LRSC_1 <- R2(sub_LR_SC_test$Ynpk_pred_1,sub_LR_SC_test$Ynpk)
  RMSE_LRSC_1 <- RMSE(sub_LR_SC_test$Ynpk_pred_1,sub_LR_SC_test$Ynpk)
  nRMSE_LRSC_1<-RMSE_LRSC_1/mean(sub_LR_SC_test$Ynpk)*100
  ME_LRSC_1<-me(sub_LR_SC_test$Ynpk_pred_1,sub_LR_SC_test$Ynpk)
  P_value_LRSC_1<-t.test(sub_LR_SC_test$Ynpk_pred_1,sub_LR_SC_test$Ynpk)$p.value
  
  cor_LRSC_1_recyle[[i]] <- cor_LRSC_1
  R2_LRSC_1_recyle[[i]] <- R2_LRSC_1
  RMSE_LRSC_1_recyle[[i]] <- RMSE_LRSC_1
  nRMSE_LRSC_1_recyle[[i]] <- nRMSE_LRSC_1
  ME_LRSC_1_recyle[[i]] <- ME_LRSC_1
  P_value_LRSC_1_recyle[[i]]<-P_value_LRSC_1

  #by lm
  lm_fit1_LRSC<-lm(Ynpk ~ Tmax + Tmin + GDD.10 + PRE +  RAD +Cultivar+ Soil.texture
                   + SOM + OP + AK + pH + N + P2O5 + K2O, data = sub_LR_SC_train)
  summary(lm_fit1_LRSC)
  
  sub_LR_SC_test$Ynpk_pred_2<-predict(lm_fit1_LRSC,sub_LR_SC_test)
  
  #plot of actual and predict values
  plot(sub_LR_SC_test$Ynpk_pred_2,sub_LR_SC_test$Ynpk, xlim=c(3500,9500),ylim=c(3500,9500),
       xlab="Predicted yield (kg/ha)", ylab="Observed yield (kg/ha)")
  abline(a=0,b=1)
  fitline2<-lm(Ynpk~Ynpk_pred_2,sub_LR_SC_test)
  abline(fitline2,lty=2)
  summary(fitline2)
  
  #caculate R, R2 and RMSE
  cor_LRSC_2 <- cor(sub_LR_SC_test$Ynpk_pred_2,sub_LR_SC_test$Ynpk)
  R2_LRSC_2 <- R2(sub_LR_SC_test$Ynpk_pred_2,sub_LR_SC_test$Ynpk)
  RMSE_LRSC_2 <- RMSE(sub_LR_SC_test$Ynpk_pred_2,sub_LR_SC_test$Ynpk)
  nRMSE_LRSC_2<-RMSE_LRSC_2/mean(sub_LR_SC_test$Ynpk)*100
  ME_LRSC_2<-me(sub_LR_SC_test$Ynpk_pred_2,sub_LR_SC_test$Ynpk)
  P_value_LRSC_2<-t.test(sub_LR_SC_test$Ynpk_pred_2,sub_LR_SC_test$Ynpk)$p.value
 
  cor_LRSC_2_recyle[[i]] <- cor_LRSC_2
  R2_LRSC_2_recyle[[i]] <- R2_LRSC_2
  RMSE_LRSC_2_recyle[[i]] <- RMSE_LRSC_2
  nRMSE_LRSC_2_recyle[[i]] <- nRMSE_LRSC_2
  ME_LRSC_2_recyle[[i]] <- ME_LRSC_2
  P_value_LRSC_2_recyle[[i]]<-P_value_LRSC_2
}

RMSE_LRSC_1_recyle <- as.data.frame(RMSE_LRSC_1_recyle)
nRMSE_LRSC_1_recyle<- as.data.frame(nRMSE_LRSC_1_recyle)
ME_LRSC_1_recyle <- as.data.frame(ME_LRSC_1_recyle)
P_value_LRSC_1_recyle<-as.data.frame(P_value_LRSC_1_recyle)

summary(RMSE_LRSC_1_recyle$RMSE_LRSC_1_recyle)
sd(RMSE_LRSC_1_recyle$RMSE_LRSC_1_recyle)
summary(nRMSE_LRSC_1_recyle$nRMSE_LRSC_1_recyle)
sd(nRMSE_LRSC_1_recyle$nRMSE_LRSC_1_recyle)
summary(ME_LRSC_1_recyle$ME_LRSC_1_recyle)
sd(ME_LRSC_1_recyle$ME_LRSC_1_recyle)
summary(P_value_LRSC_1_recyle$P_value_LRSC_1_recyle)
sd(P_value_LRSC_1_recyle$P_value_LRSC_1_recyle)

RMSE_LRSC_2_recyle <- as.data.frame(RMSE_LRSC_2_recyle)
nRMSE_LRSC_2_recyle<- as.data.frame(nRMSE_LRSC_2_recyle)
ME_LRSC_2_recyle <- as.data.frame(ME_LRSC_2_recyle)
P_value_LRSC_2_recyle<-as.data.frame(P_value_LRSC_2_recyle)

summary(RMSE_LRSC_2_recyle$RMSE_LRSC_2_recyle)
sd(RMSE_LRSC_2_recyle$RMSE_LRSC_2_recyle)
summary(nRMSE_2_recyle$nRMSE_2_recyle)
sd(nRMSE_LRSC_2_recyle$nRMSE_LRSC_2_recyle)
summary(ME_LRSC_2_recyle$ME_LRSC_2_recyle)
sd(ME_LRSC_2_recyle$ME_LRSC_2_recyle)
summary(P_value_LRSC_2_recyle$P_value_LRSC_2_recyle)
sd(P_value_LRSC_2_recyle$P_value_LRSC_2_recyle)
