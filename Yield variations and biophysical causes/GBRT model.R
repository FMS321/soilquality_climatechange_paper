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
#==========================================================
# Loading files
All_crop<-read.csv("database_soil and climate.csv")
All_crop$Soil.type<-as.factor(All_crop$Soil.type)
All_crop$Soil.texture<-as.factor(All_crop$Soil.texture)
#==========================================================
# set model for winter wheat in NCP
W_NCP<-read.csv("W-NCP.csv")

# set train and test data(random 10%)
set.seed(123)
trainingRow_WNCP <- createDataPartition(W_NCP$Province,p=0.9, list = F)
W_NCP_train <- W_NCP[trainingRow_WNCP,] 
W_NCP_test <- W_NCP[-trainingRow_WNCP,] 
# select required variables
sub_W_NCP_train <- W_NCP_train %>%
  select(NO, Province,Tave, Tmax, Tmin, GDD.0, PRE, SSD, RAD,  Soil.type, 
         Soil.texture, SOM, OP , AK, pH, N, P2O5, K2O, Yck, Ynpk)
sub_W_NCP_test <- W_NCP_test %>%
  select(NO, Province,Tave, Tmax, Tmin, GDD.0, PRE, SSD, RAD,  Soil.type, 
         Soil.texture, SOM, OP , AK, pH, N, P2O5, K2O, Yck, Ynpk)
#Determination of Tuning Parameters OF GBRT
set.seed(123)
gbmGrid<- expand.grid(.interaction.depth = seq(1, 9, by = 2),
                      .n.trees = seq(100, 1000, by = 50),
                      .shrinkage = c(0.001,0.005,0.01, 0.05, 0.1),
                      .n.minobsinnode = 10)
gbmTune_WNCP <- train(Ynpk ~ Tmax + Tmin + GDD.0 + PRE +  RAD +  Soil.type
                      + Soil.texture + SOM + OP + AK + pH + N + P2O5 + K2O,
                      data = W_NCP_train,method = "gbm",
                      tuneGrid = gbmGrid, 
                      trControl=trainControl(method = "repeatedcv", number = 10,repeats = 5),
                      verbose = FALSE)
plot(gbmTune_WNCP)
# best fitted 
# interaction.depth = 7, shrinkage = 0.05, n.trees = 1000 

#building brt model
set.seed(123)
brt_WNCP<-gbm(Ynpk ~ Tmax + Tmin + GDD.0 + PRE +  RAD +  Soil.type
                     + Soil.texture + SOM + OP + AK + pH + N + P2O5 + K2O,
                     data = sub_W_NCP_train, distribution = "gaussian", n.trees = 1000,
                     interaction.depth = 7, shrinkage = 0.05, 
                     bag.fraction = 0.5,cv.folds = 10)

print(brt_WNCP)
best_inter_WNCP<-gbm.perf(brt_WNCP,method = "cv")
summary(brt_WNCP,n.trees=best_inter_WNCP)

# partial response plot
varnam_WNCP<-best_inter_WNCP$var.names

op <- par(no.readonly = TRUE) 
par(mfrow = c(3, 5), mar = c(2.5, 4.5, 4, 1.5))
for (i in 1:14) {
  plot.gbm(brt_WNCP, i, best_inter_WNCP, 
           main = paste("Partial Dependence on", brt_WNCP$var.names[i]),
           ylim = c(5500, 7500))
}
par(op)
dev.off()

###comparison of model performance with lm
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

#caculate R, R2 and RMSE
cor(sub_W_NCP_test$Ynpk_pred_1,sub_W_NCP_test$Ynpk)
R2(sub_W_NCP_test$Ynpk_pred_1,sub_W_NCP_test$Ynpk)
RMSE(sub_W_NCP_test$Ynpk_pred_1,sub_W_NCP_test$Ynpk)

#by lm
lm_fit1_WNCP<-lm(Ynpk ~ Tmax + Tmin + GDD.0 + PRE +  RAD + Soil.type +Soil.texture
            + SOM + OP + AK + pH + N + P2O5 + K2O, data = sub_W_NCP_train)
summary(lm_fit1_WNCP)

step(lm_fit1_WNCP)

lm_fit2_WNCP<-lm(Ynpk ~ Tmax + GDD.0 + PRE + RAD + Soil.type + SOM + 
                   OP + pH + N + K2O, data = sub_W_NCP_train)
summary(lm_fit2_WNCP)
sub_W_NCP_test$Ynpk_pred_2<-predict(lm_fit2_WNCP,sub_W_NCP_test)

#plot of actual and predict values
plot(sub_W_NCP_test$Ynpk_pred_2,sub_W_NCP_test$Ynpk, xlim=c(3500,9500),ylim=c(3500,9500),
     xlab="Predicted yield (kg/ha)", ylab="Observed yield (kg/ha)")
abline(a=0,b=1)
fitline2<-lm(Ynpk~Ynpk_pred_2,sub_W_NCP_test)
abline(fitline2,lty=2)
summary(fitline2)

#caculate R, R2 and RMSE
cor(sub_W_NCP_test$Ynpk_pred_2,sub_W_NCP_test$Ynpk)
R2(sub_W_NCP_test$Ynpk_pred_2,sub_W_NCP_test$Ynpk)
RMSE(sub_W_NCP_test$Ynpk_pred_2,sub_W_NCP_test$Ynpk)

#==========================================================
# set model for winter wheat in YZB

W_YZB<-read.csv("W-YZB.csv")

# set train and test data(random 10%)
set.seed(123)
trainingRow_WYZB <- createDataPartition(W_YZB$Province,p=0.9, list = F)
W_YZB_train <- W_YZB[trainingRow_WYZB,] 
W_YZB_test <- W_YZB[-trainingRow_WYZB,] 
# select required variables
sub_W_YZB_train <- W_YZB_train %>%
  select(NO, Province1,Tave, Tmax, Tmin, GDD.0, PRE, SSD, RAD,  Soil.type, 
         Soil.texture, SOM, OP , AK, pH, N, P2O5, K2O, Yck, Ynpk)
sub_W_YZB_test <- W_YZB_test %>%
  select(NO, Province1,Tave, Tmax, Tmin, GDD.0, PRE, SSD, RAD,  Soil.type, 
         Soil.texture, SOM, OP , AK, pH, N, P2O5, K2O, Yck, Ynpk)

#Determination of Tuning Parameters OF GBRT
set.seed(123)
gbmGrid<- expand.grid(.interaction.depth = seq(1, 9, by = 2),
                      .n.trees = seq(100, 1000, by = 50),
                      .shrinkage = c(0.001,0.005,0.01, 0.05, 0.1),
                      .n.minobsinnode = 10)
gbmTune_WYZB <- train(Ynpk ~ Tmax + Tmin + GDD.0 + PRE +  RAD +  Soil.type
                      + Soil.texture + SOM + OP + AK + pH + N + P2O5 + K2O,
                      data = W_YZB_train,method = "gbm",
                      tuneGrid = gbmGrid, 
                      trControl=trainControl(method = "repeatedcv", number = 10,repeats = 5),
                      verbose = FALSE)
plot(gbmTune_WYZB)
# best fitted 
# interaction.depth = 7, shrinkage = 0.05, n.trees = 1000 

#building brt model
set.seed(123)
brt_WYZB<-gbm(Ynpk ~ Tmax + Tmin + GDD.0 + PRE +  RAD +  Soil.type
              + Soil.texture + SOM + OP + AK + pH + N + P2O5 + K2O,
              data = sub_W_YZB_train, distribution = "gaussian", n.trees = 1200,
              interaction.depth = 7, shrinkage = 0.05, 
              bag.fraction = 0.5,cv.folds = 10)

print(brt_WYZB)
best_inter_WYZB<-gbm.perf(brt_WYZB,method = "cv")
summary(brt_WYZB,n.trees=best_inter_WYZB)

# partial response plot
varnam_WYZB<-best_inter_WYZB$var.names

par(mfrow = c(3, 5), mar = c(2.5, 4.5, 4, 1.5))
for (i in 1:14) {
  plot.gbm(brt_WYZB, i, best_inter_WYZB, 
           main = paste("Partial Dependence on", brt_WYZB$var.names[i]),
           ylim = c(5000, 7000))
}
par(op)
dev.off()

###comparison of model performance with lm
###predict for test data
#by GBRT
sub_W_YZB_test$Ynpk_pred_1<-predict(brt_WYZB,sub_W_YZB_test)

#plot of actual and predict values
plot(sub_W_YZB_test$Ynpk_pred_1,sub_W_YZB_test$Ynpk, xlim=c(3500,9500),ylim=c(3500,9500),
     xlab="Predicted yield (kg/ha)", ylab="Observed yield (kg/ha)")
abline(a=0,b=1)
fitline<-lm(Ynpk~Ynpk_pred_1,sub_W_YZB_test)
abline(fitline,lty=2)
summary(fitline)

#caculate R, R2 and RMSE
cor(sub_W_YZB_test$Ynpk_pred_1,sub_W_YZB_test$Ynpk)
R2(sub_W_YZB_test$Ynpk_pred_1,sub_W_YZB_test$Ynpk)
RMSE(sub_W_YZB_test$Ynpk_pred_1,sub_W_YZB_test$Ynpk)

#by lm
lm_fit1_WYZB<-lm(Ynpk ~ Tmax + Tmin + GDD.0 + PRE +  RAD + Soil.type +Soil.texture
            + SOM + OP + AK + pH + N + P2O5 + K2O, data = sub_W_YZB_train)
summary(lm_fit1_WYZB)

step(lm_fit1_WYZB)

lm_fit2_WYZB<-lm(Ynpk ~ Tmax + Tmin + GDD.0 + PRE + RAD + Soil.type + 
                   SOM + OP + AK + N + P2O5 + K2O, data = sub_W_YZB_train)
summary(lm_fit2_WYZB)
sub_W_YZB_test$Ynpk_pred_2<-predict(lm_fit2_WYZB,sub_W_YZB_test)

#plot of actual and predict values
plot(sub_W_YZB_test$Ynpk_pred_2,sub_W_YZB_test$Ynpk, xlim=c(3500,9500),ylim=c(3500,9500),
     xlab="Predicted yield (kg/ha)", ylab="Observed yield (kg/ha)")
abline(a=0,b=1)
fitline2<-lm(Ynpk~Ynpk_pred_2,sub_W_YZB_test)
abline(fitline2,lty=2)
summary(fitline2)

#caculate R, R2 and RMSE
cor(sub_W_YZB_test$Ynpk_pred_2,sub_W_YZB_test$Ynpk)
R2(sub_W_YZB_test$Ynpk_pred_2,sub_W_YZB_test$Ynpk)
RMSE(sub_W_YZB_test$Ynpk_pred_2,sub_W_YZB_test$Ynpk)

#==========================================================
# set model for winter wheat in NWC
W_NWC<-read.csv("W-NWC.csv")

# set train and test data(random 10%)
set.seed(100)
trainingRow_WNWC <- createDataPartition(W_NWC$Province1,p=0.9, list = F)
W_NWC_train <- W_NWC[trainingRow_WNWC,] 
W_NWC_test <- W_NWC[-trainingRow_WNWC,] 
# select required variables
sub_W_NWC_train <- W_NWC_train %>%
  select(NO, Province1,Tave, Tmax, Tmin, GDD.0, PRE, SSD, RAD,  Soil.type, 
         Soil.texture, SOM, OP , AK, pH, N, P2O5, K2O, Yck, Ynpk)
sub_W_NWC_test <- W_NWC_test %>%
  select(NO, Province1,Tave, Tmax, Tmin, GDD.0, PRE, SSD, RAD,  Soil.type, 
         Soil.texture, SOM, OP , AK, pH, N, P2O5, K2O, Yck, Ynpk)
#Determination of Tuning Parameters OF GBRT
set.seed(123)
gbmGrid<- expand.grid(.interaction.depth = seq(1, 9, by = 2),
                      .n.trees = seq(100, 1000, by = 50),
                      .shrinkage = c(0.001,0.005,0.01, 0.05, 0.1),
                      .n.minobsinnode = 10)
gbmTune_WNWC <- train(Ynpk ~ Tmax + Tmin + GDD.0 + PRE +  RAD +  Soil.type
                      + Soil.texture + SOM + OP + AK + pH + N + P2O5 + K2O,
                      data = W_NWC_train,method = "gbm",
                      tuneGrid = gbmGrid, 
                      trControl=trainControl(method = "repeatedcv", number = 10,repeats = 5),
                      verbose = FALSE)
plot(gbmTune_WNWC)

#building brt model
set.seed(123)
brt_WNWC<-gbm(Ynpk ~ Tmax + Tmin + GDD.0 + PRE +  RAD +  Soil.type
              + Soil.texture + SOM + OP + AK + pH + N + P2O5 + K2O,
              data = sub_W_NWC_train, distribution = "gaussian", n.trees = 2000,
              interaction.depth = 7, shrinkage = 0.05, 
              bag.fraction = 0.5,cv.folds = 10)

print(brt_WNWC)
best_inter_WNWC<-gbm.perf(brt_WNWC,method = "cv")
summary(brt_WNWC,n.trees=best_inter_WNWC)

# partial response plot
varnam1<-best_inter_2$var.names
pdf("./results/brt_partial_plot_Ynpk.pdf", width = 12.5, height = 7.5)
op <- par(no.readonly = TRUE) 
par(mfrow = c(3, 5), mar = c(2.5, 4.5, 4, 1.5))
for (i in 1:14) {
  plot.gbm(brt_fit2, i, best_inter_WNWC, 
           main = paste("Partial Dependence on", brt_fit2$var.names[i]),
           ylim = c(5500, 7500))
}
par(op)
dev.off()

###comparison of model performance with lm
###predict for test data
#by GBRT
sub_W_NWC_test$Ynpk_pred_1<-predict(brt_WNWC,sub_W_NWC_test)

#plot of actual and predict values
plot(sub_W_NWC_test$Ynpk_pred_1,sub_W_NWC_test$Ynpk, xlim=c(3500,9500),ylim=c(3500,9500),
     xlab="Predicted yield (kg/ha)", ylab="Observed yield (kg/ha)")
abline(a=0,b=1)
fitline<-lm(Ynpk~Ynpk_pred_1,sub_W_NWC_test)
abline(fitline,lty=2)
summary(fitline)

#caculate R, R2 and RMSE
cor(sub_W_NWC_test$Ynpk_pred_1,sub_W_NWC_test$Ynpk)
R2(sub_W_NWC_test$Ynpk_pred_1,sub_W_NWC_test$Ynpk)
RMSE(sub_W_NWC_test$Ynpk_pred_1,sub_W_NWC_test$Ynpk)

#by lm
lm_fit1_WNWC<-lm(Ynpk ~ Tmax + Tmin + GDD.0 + PRE +  RAD + Soil.type +Soil.texture
            + SOM + OP + AK + pH + N + P2O5 + K2O, data = sub_W_NWC_train)
summary(lm_fit1_WNWC)

step(lm_fit1_WNWC)

lm_fit2_WNWC<-lm(Ynpk ~ Tmax + GDD.0 + PRE + RAD + Soil.type + Soil.texture + 
                   OP + N, data = sub_W_NWC_train)
summary(lm_fit2_WNWC)
sub_W_NWC_test$Ynpk_pred_2<-predict(lm_fit2_WNWC,sub_W_NWC_test)

#plot of actual and predict values
plot(sub_W_NWC_test$Ynpk_pred_2,sub_W_NWC_test$Ynpk, xlim=c(3500,9500),ylim=c(3500,9500),
     xlab="Predicted yield (kg/ha)", ylab="Observed yield (kg/ha)")
abline(a=0,b=1)
fitline2<-lm(Ynpk~Ynpk_pred_2,sub_W_NWC_test)
abline(fitline2,lty=2)
summary(fitline2)

#caculate R, R2 and RMSE
cor(sub_W_NWC_test$Ynpk_pred_2,sub_W_NWC_test$Ynpk)
R2(sub_W_NWC_test$Ynpk_pred_2,sub_W_NWC_test$Ynpk)
RMSE(sub_W_NWC_test$Ynpk_pred_2,sub_W_NWC_test$Ynpk)

#==========================================================
# set model for maize in NEC

M_NEC<-read.csv("M-NEC.csv")

# set train and test data(random 10%)
set.seed(1000)
trainingRow_MNEC <- createDataPartition(M_NEC$Province,p=0.9, list = F)
M_NEC_train <- M_NEC[trainingRow_MNEC,] 
M_NEC_test <- M_NEC[-trainingRow_MNEC,] 
# select required variables
sub_M_NEC_train <- M_NEC_train %>%
  select(NO, Province1,Tave, Tmax, Tmin, GDD.10, PRE, SSD, RAD,  Soil.type, 
         Soil.texture, SOM, OP , AK, pH, N, P2O5, K2O, Yck, Ynpk)
sub_M_NEC_test <- M_NEC_test %>%
  select(NO, Province1,Tave, Tmax, Tmin, GDD.10, PRE, SSD, RAD,  Soil.type, 
         Soil.texture, SOM, OP , AK, pH, N, P2O5, K2O, Yck, Ynpk)
#Determination of Tuning Parameters OF GBRT
set.seed(123)
gbmGrid<- expand.grid(.interaction.depth = seq(1, 9, by = 2),
                      .n.trees = seq(100, 1000, by = 50),
                      .shrinkage = c(0.001,0.005,0.01, 0.05, 0.1),
                      .n.minobsinnode = 10)
gbmTune_MNEC <- train(Ynpk ~ Tmax + Tmin + GDD.10 + PRE +  RAD +  Soil.type
                      + Soil.texture + SOM + OP + AK + pH + N + P2O5 + K2O,
                      data = M_NEC_train,method = "gbm",
                      tuneGrid = gbmGrid, 
                      trControl=trainControl(method = "repeatedcv", number = 10,repeats = 5),
                      verbose = FALSE)
plot(gbmTune_MNEC)

#building brt model
set.seed(123)
brt_MNEC<-gbm(Ynpk ~ Tmax + Tmin + GDD.10 + PRE +  RAD +  Soil.type
              + Soil.texture + SOM + OP + AK + pH + N + P2O5 + K2O,
              data = sub_M_NEC_train, distribution = "gaussian", n.trees = 2000,
              interaction.depth = 9, shrinkage = 0.05, 
              bag.fraction = 0.5,cv.folds = 10)

print(brt_MNEC)
best_inter_MNEC<-gbm.perf(brt_MNEC,method = "cv")
summary(brt_MNEC,n.trees=best_inter_MNEC)

# partial response plot
varnam1<-best_inter_2$var.names
pdf("./results/brt_partial_plot_Ynpk.pdf", width = 12.5, height = 7.5)
op <- par(no.readonly = TRUE) 
par(mfrow = c(3, 5), mar = c(2.5, 4.5, 4, 1.5))
for (i in 1:14) {
  plot.gbm(brt_fit2, i, best_inter_MNEC, 
           main = paste("Partial Dependence on", brt_fit2$var.names[i]),
           ylim = c(8500, 11000))
}
par(op)
dev.off()

###comparison of model performance with lm
###predict for test data
#by GBRT
sub_M_NEC_test$Ynpk_pred_1<-predict(brt_MNEC,sub_M_NEC_test)

#plot of actual and predict values
plot(sub_M_NEC_test$Ynpk_pred_1,sub_M_NEC_test$Ynpk, xlim=c(3500,9500),ylim=c(3500,9500),
     xlab="Predicted yield (kg/ha)", ylab="Observed yield (kg/ha)")
abline(a=0,b=1)
fitline<-lm(Ynpk~Ynpk_pred_1,sub_M_NEC_test)
abline(fitline,lty=2)
summary(fitline)

#caculate R, R2 and RMSE
cor(sub_M_NEC_test$Ynpk_pred_1,sub_M_NEC_test$Ynpk)
R2(sub_M_NEC_test$Ynpk_pred_1,sub_M_NEC_test$Ynpk)
RMSE(sub_M_NEC_test$Ynpk_pred_1,sub_M_NEC_test$Ynpk)

#by lm
lm_fit1_MNEC<-lm(Ynpk ~ Tmax + Tmin + GDD.10 + PRE +  RAD + Soil.type +Soil.texture
            + SOM + OP + AK + pH + N + P2O5 + K2O, data = sub_M_NEC_train)
summary(lm_fit1_MNEC)

step(lm_fit1_MNEC)

lm_fit2_MNEC<-lm(Ynpk ~ Tmax + Tmin + GDD.10 + Soil.type + SOM + 
                   OP + N + K2O, data = sub_M_NEC_train)
summary(lm_fit2_MNEC)
sub_M_NEC_test$Ynpk_pred_2<-predict(lm_fit2_MNEC,sub_M_NEC_test)

#plot of actual and predict values
plot(sub_M_NEC_test$Ynpk_pred_2,sub_M_NEC_test$Ynpk, xlim=c(3500,9500),ylim=c(3500,9500),
     xlab="Predicted yield (kg/ha)", ylab="Observed yield (kg/ha)")
abline(a=0,b=1)
fitline2<-lm(Ynpk~Ynpk_pred_2,sub_M_NEC_test)
abline(fitline2,lty=2)
summary(fitline2)

#caculate R, R2 and RMSE
cor(sub_M_NEC_test$Ynpk_pred_2,sub_M_NEC_test$Ynpk)
R2(sub_M_NEC_test$Ynpk_pred_2,sub_M_NEC_test$Ynpk)
RMSE(sub_M_NEC_test$Ynpk_pred_2,sub_M_NEC_test$Ynpk)


#==========================================================
# set model for maize in NCP

M_NCP<-read.csv("M-NCP.csv")

# set train and test data(random 10%)
set.seed(100)
trainingRow_MNCP <- createDataPartition(M_NCP$Province,p=0.9, list = F)
M_NCP_train <- M_NCP[trainingRow_MNCP,] 
M_NCP_test <- M_NCP[-trainingRow_MNCP,] 
# select required variables
sub_M_NCP_train <- M_NCP_train %>%
  select(NO, Province,Tave, Tmax, Tmin, GDD.10, PRE, SSD, RAD,  Soil.type, 
         Soil.texture, SOM, OP , AK, pH, N, P2O5, K2O, Yck, Ynpk)
sub_M_NCP_test <- M_NCP_test %>%
  select(NO, Province,Tave, Tmax, Tmin, GDD.10, PRE, SSD, RAD,  Soil.type, 
         Soil.texture, SOM, OP , AK, pH, N, P2O5, K2O, Yck, Ynpk)
#Determination of Tuning Parameters OF GBRT
set.seed(123)
gbmGrid<- expand.grid(.interaction.depth = seq(1, 9, by = 2),
                      .n.trees = seq(100, 1000, by = 50),
                      .shrinkage = c(0.001,0.005,0.01, 0.05, 0.1),
                      .n.minobsinnode = 10)
gbmTune_MNCP <- train(Ynpk ~ Tmax + Tmin + GDD.10 + PRE +  RAD +  Soil.type
                      + Soil.texture + SOM + OP + AK + pH + N + P2O5 + K2O,
                      data = M_NCP_train,method = "gbm",
                      tuneGrid = gbmGrid, 
                      trControl=trainControl(method = "repeatedcv", number = 10,repeats = 5),
                      verbose = FALSE)
plot(gbmTune_MNCP)

#building brt model
set.seed(123)
brt_MNCP<-gbm(Ynpk ~ Tmax + Tmin + GDD.10 + PRE +  RAD +  Soil.type
              + Soil.texture + SOM + OP + AK + pH + N + P2O5 + K2O,
              data = sub_M_NCP_train, distribution = "gaussian", n.trees = 1000,
              interaction.depth = 9, shrinkage = 0.05, 
              bag.fraction = 0.5,cv.folds = 10)

print(brt_MNCP)
best_inter_MNCP<-gbm.perf(brt_MNCP,method = "cv")
summary(brt_MNCP,n.trees=best_inter_MNCP)

# partial response plot
varnam1<-best_inter_2$var.names
pdf("./results/brt_partial_plot_Ynpk.pdf", width = 12.5, height = 7.5)
op <- par(no.readonly = TRUE) 
par(mfrow = c(3, 5), mar = c(2.5, 4.5, 4, 1.5))
for (i in 1:14) {
  plot.gbm(brt_fit2, i, best_inter_MNCP, 
           main = paste("Partial Dependence on", brt_fit2$var.names[i]),
           ylim = c(7500, 9000))
}
par(op)
dev.off()

###comparison of model performance with lm
###predict for test data
#by GBRT
sub_M_NCP_test$Ynpk_pred_1<-predict(brt_MNCP,sub_M_NCP_test)

#plot of actual and predict values
plot(sub_M_NCP_test$Ynpk_pred_1,sub_M_NCP_test$Ynpk, xlim=c(3500,9500),ylim=c(3500,9500),
     xlab="Predicted yield (kg/ha)", ylab="Observed yield (kg/ha)")
abline(a=0,b=1)
fitline<-lm(Ynpk~Ynpk_pred_1,sub_M_NCP_test)
abline(fitline,lty=2)
summary(fitline)

#caculate R, R2 and RMSE
cor(sub_M_NCP_test$Ynpk_pred_1,sub_M_NCP_test$Ynpk)
R2(sub_M_NCP_test$Ynpk_pred_1,sub_M_NCP_test$Ynpk)
RMSE(sub_M_NCP_test$Ynpk_pred_1,sub_M_NCP_test$Ynpk)

#by lm
lm_fit1_MNCP<-lm(Ynpk ~ Tmax + Tmin + GDD.10 + PRE +  RAD + Soil.type +Soil.texture
            + SOM + OP + AK + pH + N + P2O5 + K2O, data = sub_M_NCP_train)
summary(lm_fit1_MNCP)

step(lm_fit1_MNCP)

lm_fit2_MNCP<-lm(Ynpk ~ Tmin + SOM + pH + N + K2O, data = sub_M_NCP_train)
summary(lm_fit2_MNCP)
sub_M_NCP_test$Ynpk_pred_2<-predict(lm_fit2_MNCP,sub_M_NCP_test)

#plot of actual and predict values
plot(sub_M_NCP_test$Ynpk_pred_2,sub_M_NCP_test$Ynpk, xlim=c(3500,9500),ylim=c(3500,9500),
     xlab="Predicted yield (kg/ha)", ylab="Observed yield (kg/ha)")
abline(a=0,b=1)
fitline2<-lm(Ynpk~Ynpk_pred_2,sub_M_NCP_test)
abline(fitline2,lty=2)
summary(fitline2)

#caculate R, R2 and RMSE
cor(sub_M_NCP_test$Ynpk_pred_2,sub_M_NCP_test$Ynpk)
R2(sub_M_NCP_test$Ynpk_pred_2,sub_M_NCP_test$Ynpk)
RMSE(sub_M_NCP_test$Ynpk_pred_2,sub_M_NCP_test$Ynpk)

#==========================================================
# set model for maize in SW

M_SWC<-read.csv("M-SWC.csv")

# set train and test data(random 10%)
set.seed(123)
trainingRow_MSWC <- createDataPartition(M_SWC$Province,p=0.9, list = F)
M_SWC_train <- M_SWC[trainingRow_MSWC,] 
M_SWC_test <- M_SWC[-trainingRow_MSWC,] 
# select required variables
sub_M_SWC_train <- M_SWC_train %>%
  select(NO, Province1,Tave, Tmax, Tmin, GDD.10, PRE, SSD, RAD,  Soil.type, 
         SOM, OP , AK, pH, N, P2O5, K2O, Yck, Ynpk)
sub_M_SWC_test <- M_SWC_test %>%
  select(NO, Province1,Tave, Tmax, Tmin, GDD.10, PRE, SSD, RAD,  Soil.type, 
          SOM, OP , AK, pH, N, P2O5, K2O, Yck, Ynpk)
#Determination of Tuning Parameters OF GBRT
set.seed(123)
gbmGrid<- expand.grid(.interaction.depth = seq(1, 9, by = 2),
                      .n.trees = seq(100, 1000, by = 50),
                      .shrinkage = c(0.001,0.005,0.01, 0.05, 0.1),
                      .n.minobsinnode = 10)
gbmTune_MSWC <- train(Ynpk ~ Tmax + Tmin + GDD.10 + PRE +  RAD +  Soil.type
                      + SOM + OP + AK + pH + N + P2O5 + K2O,
                      data = M_SWC_train,method = "gbm",
                      tuneGrid = gbmGrid, 
                      trControl=trainControl(method = "repeatedcv", number = 10,repeats = 5),
                      verbose = FALSE)
plot(gbmTune_MSWC)

#building brt model
set.seed(123)
brt_MSWC<-gbm(Ynpk ~ Tmax + Tmin + GDD.10 + PRE +  RAD +  Soil.type
               + SOM + OP + AK + pH + N + P2O5 + K2O,
              data = sub_M_SWC_train, distribution = "gaussian", n.trees = 2000,
              interaction.depth = 9, shrinkage = 0.005, 
              bag.fraction = 0.5,cv.folds = 10)

print(brt_MSWC)
best_inter_MSWC<-gbm.perf(brt_MSWC,method = "cv")
summary(brt_MSWC,n.trees=best_inter_MSWC)

# partial response plot
varnam1<-best_inter_2$var.names
pdf("./results/brt_partial_plot_Ynpk.pdf", width = 12.5, height = 7.5)
op <- par(no.readonly = TRUE) 
par(mfrow = c(3, 5), mar = c(2.5, 4.5, 4, 1.5))
for (i in 1:13) {
  plot.gbm(brt_fit2, i, best_inter_MSWC, 
           main = paste("Partial Dependence on", brt_fit2$var.names[i]),
           ylim = c(7000, 8500))
}
par(op)
dev.off()

###comparison of model performance with lm
###predict for test data
#by GBRT
sub_M_SWC_test$Ynpk_pred_1<-predict(brt_MSWC,sub_M_SWC_test)

#plot of actual and predict values
plot(sub_M_SWC_test$Ynpk_pred_1,sub_M_SWC_test$Ynpk, xlim=c(3500,9500),ylim=c(3500,9500),
     xlab="Predicted yield (kg/ha)", ylab="Observed yield (kg/ha)")
abline(a=0,b=1)
fitline<-lm(Ynpk~Ynpk_pred_1,sub_M_SWC_test)
abline(fitline,lty=2)
summary(fitline)

#caculate R, R2 and RMSE
cor(sub_M_SWC_test$Ynpk_pred_1,sub_M_SWC_test$Ynpk)
R2(sub_M_SWC_test$Ynpk_pred_1,sub_M_SWC_test$Ynpk)
RMSE(sub_M_SWC_test$Ynpk_pred_1,sub_M_SWC_test$Ynpk)

#by lm
lm_fit1_MSWC<-lm(Ynpk ~ Tmax + Tmin + GDD.10 + PRE +  RAD + Soil.type 
            + SOM + OP + AK + pH + N + P2O5 + K2O, data = sub_M_SWC_train)
summary(lm_fit1_MSWC)

step(lm_fit1_MSWC)

lm_fit2_MSWC<-lm(Ynpk ~ Tmax + Soil.type + SOM + AK + pH + N + K2O, data = sub_M_SWC_train)
summary(lm_fit2_MSWC)
sub_M_SWC_test$Ynpk_pred_2<-predict(lm_fit2_MSWC,sub_M_SWC_test)

#plot of actual and predict values
plot(sub_M_SWC_test$Ynpk_pred_2,sub_M_SWC_test$Ynpk, xlim=c(3500,9500),ylim=c(3500,9500),
     xlab="Predicted yield (kg/ha)", ylab="Observed yield (kg/ha)")
abline(a=0,b=1)
fitline2<-lm(Ynpk~Ynpk_pred_2,sub_M_SWC_test)
abline(fitline2,lty=2)
summary(fitline2)

#caculate R, R2 and RMSE
cor(sub_M_SWC_test$Ynpk_pred_2,sub_M_SWC_test$Ynpk)
R2(sub_M_SWC_test$Ynpk_pred_2,sub_M_SWC_test$Ynpk)
RMSE(sub_M_SWC_test$Ynpk_pred_2,sub_M_SWC_test$Ynpk)

#==========================================================
# set model for single rice in YZB

SR_YZB<-read.csv("SR-YZB.csv")

# set train and test data(random 10%)
set.seed(1000)
trainingRow_SRYZB <- createDataPartition(SR_YZB$Province,p=0.9, list = F)
SR_YZB_train <- SR_YZB[trainingRow_SRYZB,] 
SR_YZB_test <- SR_YZB[-trainingRow_SRYZB,] 
# select required variables
sub_SR_YZB_train <- SR_YZB_train %>%
  select(NO, Province1,Tave, Tmax, Tmin, GDD.10, PRE, SSD, RAD,  Soil.type, Soil.type1,
         Soil.texture, SOM, OP , AK, pH, N, P2O5, K2O, Yck, Ynpk)
sub_SR_YZB_test <- SR_YZB_test %>%
  select(NO, Province1,Tave, Tmax, Tmin, GDD.10, PRE, SSD, RAD,  Soil.type, Soil.type1,
         Soil.texture, SOM, OP , AK, pH, N, P2O5, K2O, Yck, Ynpk)
#Determination of Tuning Parameters OF GBRT
set.seed(123)
gbmGrid<- expand.grid(.interaction.depth = seq(1, 9, by = 2),
                      .n.trees = seq(100, 1000, by = 50),
                      .shrinkage = c(0.001,0.005,0.01, 0.05, 0.1),
                      .n.minobsinnode = 10)
gbmTune_SRYZB <- train(Ynpk ~ Tmax + Tmin + GDD.10 + PRE +  RAD +  Soil.type
                       + Soil.texture + SOM + OP + AK + pH + N + P2O5 + K2O,
                       data = SR_YZB_train,method = "gbm",
                       tuneGrid = gbmGrid, 
                       trControl=trainControl(method = "repeatedcv", number = 10,repeats = 5),
                       verbose = FALSE)
plot(gbmTune_SRYZB)

#building brt model
set.seed(123)
brt_SRYZB<-gbm(Ynpk ~ Tmax + Tmin + GDD.10 + PRE +  RAD +  Soil.type
               + Soil.texture + SOM + OP + AK + pH + N + P2O5 + K2O,
               data = sub_SR_YZB_train, distribution = "gaussian", n.trees = 2000,
               interaction.depth = 9, shrinkage = 0.01, 
               bag.fraction = 0.5,cv.folds = 10)

print(brt_SRYZB)
best_inter_SRYZB<-gbm.perf(brt_SRYZB,method = "cv")
summary(brt_SRYZB,n.trees=best_inter_SRYZB)

# partial response plot
varnam1<-best_inter_2$var.names
pdf("./results/brt_partial_plot_Ynpk.pdf", width = 12.5, height = 7.5)
op <- par(no.readonly = TRUE) 
par(mfrow = c(3, 5), mar = c(2.5, 4.5, 4, 1.5))
for (i in 1:14) {
  plot.gbm(brt_fit2, i, best_inter_SRYZB, 
           main = paste("Partial Dependence on", brt_fit2$var.names[i]),
           ylim = c(7500, 9000))
}
par(op)
dev.off()

###comparison of model performance with lm
###predict for test data
#by GBRT
sub_SR_YZB_test$Ynpk_pred_1<-predict(brt_SRYZB,sub_SR_YZB_test)

#plot of actual and predict values
plot(sub_SR_YZB_test$Ynpk_pred_1,sub_SR_YZB_test$Ynpk, xlim=c(3500,9500),ylim=c(3500,9500),
     xlab="Predicted yield (kg/ha)", ylab="Observed yield (kg/ha)")
abline(a=0,b=1)
fitline<-lm(Ynpk~Ynpk_pred_1,sub_SR_YZB_test)
abline(fitline,lty=2)
summary(fitline)

#caculate R, R2 and RMSE
cor(sub_SR_YZB_test$Ynpk_pred_1,sub_SR_YZB_test$Ynpk)
R2(sub_SR_YZB_test$Ynpk_pred_1,sub_SR_YZB_test$Ynpk)
RMSE(sub_SR_YZB_test$Ynpk_pred_1,sub_SR_YZB_test$Ynpk)

#by lm
lm_fit1_SRYZB<-lm(Ynpk ~ Tmax + Tmin + GDD.10 + PRE +  RAD + Soil.type +Soil.texture
            + SOM + OP + AK + pH + N + P2O5 + K2O, data = sub_SR_YZB_train)
summary(lm_fit1_SRYZB)

step(lm_fit1_SRYZB)

lm_fit2_SRYZB<-lm(Ynpk ~ Tmin + GDD.10 + PRE + RAD + Soil.type + Soil.texture + 
                    OP + AK + pH + N + K2O, data = sub_SR_YZB_train)
summary(lm_fit2_SRYZB)
sub_SR_YZB_test$Ynpk_pred_2<-predict(lm_fit2_SRYZB,sub_SR_YZB_test)

#plot of actual and predict values
plot(sub_SR_YZB_test$Ynpk_pred_2,sub_SR_YZB_test$Ynpk, xlim=c(3500,9500),ylim=c(3500,9500),
     xlab="Predicted yield (kg/ha)", ylab="Observed yield (kg/ha)")
abline(a=0,b=1)
fitline2<-lm(Ynpk~Ynpk_pred_2,sub_SR_YZB_test)
abline(fitline2,lty=2)
summary(fitline2)

#caculate R, R2 and RMSE
cor(sub_SR_YZB_test$Ynpk_pred_2,sub_SR_YZB_test$Ynpk)
R2(sub_SR_YZB_test$Ynpk_pred_2,sub_SR_YZB_test$Ynpk)
RMSE(sub_SR_YZB_test$Ynpk_pred_2,sub_SR_YZB_test$Ynpk)

#==========================================================
# set model for early rice in SC

ER_SC<-subset(All_crop,Crop=="ER-SC")
ER_SC<-read.csv("ER-SC.csv")
# set train and test data(random 10%)
set.seed(123)
trainingRow_ERSC <- createDataPartition(ER_SC$Province,p=0.9, list = F)
ER_SC_train <- ER_SC[trainingRow_ERSC,] 
ER_SC_test <- ER_SC[-trainingRow_ERSC,] 
# select required variables
sub_ER_SC_train <- ER_SC_train %>%
  select(NO, Province,Tave, Tmax, Tmin, GDD.10, PRE, SSD, RAD,  Soil.type, 
         Soil.texture, SOM, OP , AK, pH, N, P2O5, K2O, Yck, Ynpk)
sub_ER_SC_test <- ER_SC_test %>%
  select(NO, Province,Tave, Tmax, Tmin, GDD.10, PRE, SSD, RAD,  Soil.type, 
         Soil.texture, SOM, OP , AK, pH, N, P2O5, K2O, Yck, Ynpk)
#Determination of Tuning Parameters OF GBRT
set.seed(123)
gbmGrid<- expand.grid(.interaction.depth = seq(1, 9, by = 2),
                      .n.trees = seq(100, 1000, by = 50),
                      .shrinkage = c(0.001,0.005,0.01, 0.05, 0.1),
                      .n.minobsinnode = 10)
gbmTune_ERSC <- train(Ynpk ~ Tmax + Tmin + GDD.10 + PRE + RAD 
                      + SOM + OP + AK + pH + N + P2O5 + K2O,
                      data = ER_SC_train,method = "gbm",
                      tuneGrid = gbmGrid, 
                      trControl=trainControl(method = "repeatedcv", number = 10,repeats = 5),
                      verbose = FALSE)
plot(gbmTune_ERSC)

#building brt model
set.seed(123)
brt_ERSC<-gbm(Ynpk ~ Tmax + Tmin + GDD.10 + PRE +  RAD 
              + SOM + OP + AK + pH + N + P2O5 + K2O,
              data = sub_ER_SC_train, distribution = "gaussian", n.trees = 2000,
              interaction.depth = 9, shrinkage = 0.005, 
              bag.fraction = 0.5,cv.folds = 10)

print(brt_ERSC)
best_inter_ERSC<-gbm.perf(brt_ERSC,method = "cv")
summary(brt_ERSC,n.trees=best_inter_ERSC)

# partial response plot
varnam1<-best_inter_2$var.names
pdf("./results/brt_partial_plot_Ynpk.pdf", width = 12.5, height = 7.5)
op <- par(no.readonly = TRUE) 
par(mfrow = c(3, 5), mar = c(2.5, 4.5, 4, 1.5))
for (i in 1:12) {
  plot.gbm(brt_fit2, i, best_inter_ERSC, 
           main = paste("Partial Dependence on", brt_fit2$var.names[i]),
           ylim = c(5500, 7500))
}
par(op)
dev.off()

###comparison of model performance with lm
###predict for test data
#by GBRT
sub_ER_SC_test$Ynpk_pred_1<-predict(brt_ERSC,sub_ER_SC_test)

#plot of actual and predict values
plot(sub_ER_SC_test$Ynpk_pred_1,sub_ER_SC_test$Ynpk, xlim=c(3500,9500),ylim=c(3500,9500),
     xlab="Predicted yield (kg/ha)", ylab="Observed yield (kg/ha)")
abline(a=0,b=1)
fitline<-lm(Ynpk~Ynpk_pred_1,sub_ER_SC_test)
abline(fitline,lty=2)
summary(fitline)

#caculate R, R2 and RMSE
cor(sub_ER_SC_test$Ynpk_pred_1,sub_ER_SC_test$Ynpk)
R2(sub_ER_SC_test$Ynpk_pred_1,sub_ER_SC_test$Ynpk)
RMSE(sub_ER_SC_test$Ynpk_pred_1,sub_ER_SC_test$Ynpk)

#by lm
lm_fit1_ERSC<-lm(Ynpk ~ Tmax + Tmin + GDD.10 + PRE +  RAD 
            + SOM + OP + AK + pH + N + P2O5 + K2O, data = sub_ER_SC_train)
summary(lm_fit1_ERSC)

step(lm_fit1_ERSC)

lm_fit2_ERSC<-lm(Ynpk ~ Tmin + GDD.10 + PRE + SOM + AK + N, data = sub_ER_SC_train)
summary(lm_fit2_ERSC)
sub_ER_SC_test$Ynpk_pred_2<-predict(lm_fit2_ERSC,sub_ER_SC_test)

#plot of actual and predict values
plot(sub_ER_SC_test$Ynpk_pred_2,sub_ER_SC_test$Ynpk, xlim=c(3500,9500),ylim=c(3500,9500),
     xlab="Predicted yield (kg/ha)", ylab="Observed yield (kg/ha)")
abline(a=0,b=1)
fitline2<-lm(Ynpk~Ynpk_pred_2,sub_ER_SC_test)
abline(fitline2,lty=2)
summary(fitline2)

#caculate R, R2 and RMSE
cor(sub_ER_SC_test$Ynpk_pred_2,sub_ER_SC_test$Ynpk)
R2(sub_ER_SC_test$Ynpk_pred_2,sub_ER_SC_test$Ynpk)
RMSE(sub_ER_SC_test$Ynpk_pred_2,sub_ER_SC_test$Ynpk)

#==========================================================
# set model for late rice in SC

LR_SC<-subset(All_crop,Crop=="LR-SC")
LR_SC<-read.csv("LR-SC.csv")
# set train and test data(random 10%)
set.seed(1000)
trainingRow_LRSC <- createDataPartition(LR_SC$Province1,p=0.9, list = F)
LR_SC_train <- LR_SC[trainingRow_LRSC,] 
LR_SC_test <- LR_SC[-trainingRow_LRSC,] 
# select required variables
sub_LR_SC_train <- LR_SC_train %>%
  select(NO, Province1,Tave, Tmax, Tmin, GDD.10, PRE, SSD, RAD,  Soil.type, 
         Soil.texture, SOM, OP , AK, pH, N, P2O5, K2O, Yck, Ynpk)
sub_LR_SC_test <- LR_SC_test %>%
  select(NO, Province1,Tave, Tmax, Tmin, GDD.10, PRE, SSD, RAD,  Soil.type, 
         Soil.texture, SOM, OP , AK, pH, N, P2O5, K2O, Yck, Ynpk)
#Determination of Tuning Parameters OF GBRT
set.seed(123)
gbmGrid<- expand.grid(.interaction.depth = seq(1, 9, by = 2),
                      .n.trees = seq(100, 1000, by = 50),
                      .shrinkage = c(0.001,0.005,0.01, 0.05, 0.1),
                      .n.minobsinnode = 10)
gbmTune_LRSC <- train(Ynpk ~ Tmax + Tmin + GDD.10 + PRE +  RAD +  Soil.type
                      + Soil.texture + SOM + OP + AK + pH + N + P2O5 + K2O,
                      data = LR_SC_train,method = "gbm",
                      tuneGrid = gbmGrid, 
                      trControl=trainControl(method = "repeatedcv", number = 10,repeats = 5),
                      verbose = FALSE)
plot(gbmTune_LRSC)

#building brt model
set.seed(123)
brt_LRSC<-gbm(Ynpk ~ Tmax + Tmin + GDD.10 + PRE +  RAD 
              + Soil.texture + SOM + OP + AK + pH + N + P2O5 + K2O,
              data = sub_LR_SC_train, distribution = "gaussian", n.trees = 2000,
              interaction.depth = 9, shrinkage = 0.005, 
              bag.fraction = 0.5,cv.folds = 10)

print(brt_LRSC)
best_inter_LRSC<-gbm.perf(brt_LRSC,method = "cv")
summary(brt_LRSC,n.trees=best_inter_LRSC)

# partial response plot
varnam1<-best_inter_2$var.names
pdf("./results/brt_partial_plot_Ynpk.pdf", width = 12.5, height = 7.5)
op <- par(no.readonly = TRUE) 
par(mfrow = c(3, 5), mar = c(2.5, 4.5, 4, 1.5))
for (i in 1:13) {
  plot.gbm(brt_fit2, i, best_inter_LRSC, 
           main = paste("Partial Dependence on", brt_fit2$var.names[i]),
           ylim = c(5500, 7500))
}
par(op)
dev.off()

###comparison of model performance with lm
###predict for test data
#by GBRT
sub_LR_SC_test$Ynpk_pred_1<-predict(brt_LRSC,sub_LR_SC_test)

#plot of actual and predict values
plot(sub_LR_SC_test$Ynpk_pred_1,sub_LR_SC_test$Ynpk, xlim=c(3500,9500),ylim=c(3500,9500),
     xlab="Predicted yield (kg/ha)", ylab="Observed yield (kg/ha)")
abline(a=0,b=1)
fitline<-lm(Ynpk~Ynpk_pred_1,sub_LR_SC_test)
abline(fitline,lty=2)
summary(fitline)

#caculate R, R2 and RMSE
cor(sub_LR_SC_test$Ynpk_pred_1,sub_LR_SC_test$Ynpk)
R2(sub_LR_SC_test$Ynpk_pred_1,sub_LR_SC_test$Ynpk)
RMSE(sub_LR_SC_test$Ynpk_pred_1,sub_LR_SC_test$Ynpk)

#by lm
lm_fit1_LRSC<-lm(Ynpk ~ Tmax + Tmin + GDD.10 + PRE +  RAD + Soil.texture
            + SOM + OP + AK + pH + N + P2O5 + K2O, data = sub_LR_SC_train)
summary(lm_fit1_LRSC)

step(lm_fit1_LRSC)

lm_fit2_LRSC<-lm(Ynpk ~ Tmax + PRE + RAD + Soil.texture + SOM + AK + 
              N + P2O5 + K2O, data = sub_LR_SC_train)
summary(lm_fit2_LRSC)
sub_LR_SC_test$Ynpk_pred_2<-predict(lm_fit2,sub_LR_SC_test)

#plot of actual and predict values
plot(sub_LR_SC_test$Ynpk_pred_2,sub_LR_SC_test$Ynpk, xlim=c(3500,9500),ylim=c(3500,9500),
     xlab="Predicted yield (kg/ha)", ylab="Observed yield (kg/ha)")
abline(a=0,b=1)
fitline2<-lm(Ynpk~Ynpk_pred_2,sub_LR_SC_test)
abline(fitline2,lty=2)
summary(fitline2)

#caculate R, R2 and RMSE
cor(sub_LR_SC_test$Ynpk_pred_2,sub_LR_SC_test$Ynpk)
R2(sub_LR_SC_test$Ynpk_pred_2,sub_LR_SC_test$Ynpk)
RMSE(sub_LR_SC_test$Ynpk_pred_2,sub_LR_SC_test$Ynpk)
