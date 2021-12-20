# =========================================================
# Title: Soil quality both impact crop production and climate resilience
#
# Author details: Author: Lei Qiao 
# Contact details: qiaolei1991@foxmail.com
#
#Objective: Projected yield change in high- and low- quality soils 
#           in RCP2.6 and RCP8.5 pathways up to 2040-2059 and 2080-2099
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
base<-read.csv("base_new.csv")  #climate scenario under baseline period (1986-2005)
RCP2.6_2050s<-read.csv("RCP2.6_2050.csv") #climate scenario of RCP2.6 during 2040-2059
RCP2.6_2100s<-read.csv("RCP2.6_2100.csv") #climate scenario of RCP2.6 during 2080-2099
RCP8.5_2050s<-read.csv("RCP8.5_2050.csv") #climate scenario of RCP8.5 during 2040-2059
RCP8.5_2100s<-read.csv("RCP8.5_2100.csv") #climate scenario of RCP8.5 during 2080-2099

#==========================================================
#Predicted yield in W-NCP

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
}

#####Predicted yield under different climate scenarios
###baseline period 1986-2005
base_NW<-subset(base, Crop == "north wheat")
base_NW_1986 <- base_NW %>%
  select(Soil.quality, Tave_1986, Tmax_1986, Tmin_1986, GDD.0_1986, 
         PRE_1986, RAD_1986, Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NW_1986)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NW_1987 <- base_NW %>%
  select(Soil.quality, Tave_1987, Tmax_1987, Tmin_1987, GDD.0_1987, 
         PRE_1987, RAD_1987, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NW_1987)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NW_1988 <- base_NW %>%
  select(Soil.quality, Tave_1988, Tmax_1988, Tmin_1988, GDD.0_1988, 
         PRE_1988, RAD_1988, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NW_1988)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NW_1989 <- base_NW %>%
  select(Soil.quality, Tave_1989, Tmax_1989, Tmin_1989, GDD.0_1989, 
         PRE_1989, RAD_1989, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NW_1989)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NW_1990 <- base_NW %>%
  select(Soil.quality, Tave_1990, Tmax_1990, Tmin_1990, GDD.0_1990, 
         PRE_1990, RAD_1990, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NW_1990)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NW_1991 <- base_NW %>%
  select(Soil.quality, Tave_1991, Tmax_1991, Tmin_1991, GDD.0_1991, 
         PRE_1991, RAD_1991, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NW_1991)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NW_1992 <- base_NW %>%
  select(Soil.quality, Tave_1992, Tmax_1992, Tmin_1992, GDD.0_1992, 
         PRE_1992, RAD_1992,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NW_1992)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NW_1993 <- base_NW %>%
  select(Soil.quality, Tave_1993, Tmax_1993, Tmin_1993, GDD.0_1993, 
         PRE_1993, RAD_1993, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NW_1993)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NW_1994 <- base_NW %>%
  select(Soil.quality, Tave_1994, Tmax_1994, Tmin_1994, GDD.0_1994, 
         PRE_1994, RAD_1994, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NW_1994)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NW_1995 <- base_NW %>%
  select(Soil.quality, Tave_1995, Tmax_1995, Tmin_1995, GDD.0_1995, 
         PRE_1995, RAD_1995, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NW_1995)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NW_1996 <- base_NW %>%
  select(Soil.quality, Tave_1996, Tmax_1996, Tmin_1996, GDD.0_1996, 
         PRE_1996, RAD_1996, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NW_1996)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NW_1997 <- base_NW %>%
  select(Soil.quality, Tave_1997, Tmax_1997, Tmin_1997, GDD.0_1997, 
         PRE_1997, RAD_1997, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NW_1997)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NW_1998 <- base_NW %>%
  select(Soil.quality, Tave_1998, Tmax_1998, Tmin_1998, GDD.0_1998, 
         PRE_1998, RAD_1998, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NW_1998)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NW_1999 <- base_NW %>%
  select(Soil.quality, Tave_1999, Tmax_1999, Tmin_1999, GDD.0_1999, 
         PRE_1999, RAD_1999, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NW_1999)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NW_2000 <- base_NW %>%
  select(Soil.quality, Tave_2000, Tmax_2000, Tmin_2000, GDD.0_2000, 
         PRE_2000, RAD_2000, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NW_2000)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NW_2001 <- base_NW %>%
  select(Soil.quality, Tave_2001, Tmax_2001, Tmin_2001, GDD.0_2001, 
         PRE_2001, RAD_2001, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NW_2001)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NW_2002 <- base_NW %>%
  select(Soil.quality, Tave_2002, Tmax_2002, Tmin_2002, GDD.0_2002, 
         PRE_2002, RAD_2002, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NW_2002)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NW_2003 <- base_NW %>%
  select(Soil.quality, Tave_2003, Tmax_2003, Tmin_2003, GDD.0_2003, 
         PRE_2003, RAD_2003, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NW_2003)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NW_2004 <- base_NW %>%
  select(Soil.quality, Tave_2004, Tmax_2004, Tmin_2004, GDD.0_2004, 
         PRE_2004, RAD_2004, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NW_2004)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NW_2005 <- base_NW %>%
  select(Soil.quality, Tave_2005, Tmax_2005, Tmin_2005, GDD.0_2005, 
         PRE_2005, RAD_2005, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NW_2005)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NW_86.05 <- base_NW %>%
  select(Soil.quality, Tave_1986.2005, Tmax_1986.2005, Tmin_1986.2005, GDD.0_1986.2005, 
         PRE_1986.2005, RAD_1986.2005, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NW_86.05)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                         "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                         "N", "P2O5", "K2O", "Yck", "Ynpk")

###predict yield by GBRT model

base_NW_pred_all_recyle<-NULL

base_NW_pred_all <- base_NW %>%
  select(Soil.quality, Crop, NO, Year,Province, 
         NO.Station)

for (i in 1:50) {
  base_NW_pred_all$Ynpk_pred_1986<-predict(brt_WNCP_recyle[[i]],base_NW_1986, n.trees=best_inter_WNCP_recyle[[i]])
  base_NW_pred_all$Ynpk_pred_1987<-predict(brt_WNCP_recyle[[i]],base_NW_1987, n.trees=best_inter_WNCP_recyle[[i]])
  base_NW_pred_all$Ynpk_pred_1988<-predict(brt_WNCP_recyle[[i]],base_NW_1988, n.trees=best_inter_WNCP_recyle[[i]])
  base_NW_pred_all$Ynpk_pred_1989<-predict(brt_WNCP_recyle[[i]],base_NW_1989, n.trees=best_inter_WNCP_recyle[[i]])
  base_NW_pred_all$Ynpk_pred_1990<-predict(brt_WNCP_recyle[[i]],base_NW_1990, n.trees=best_inter_WNCP_recyle[[i]])
  base_NW_pred_all$Ynpk_pred_1991<-predict(brt_WNCP_recyle[[i]],base_NW_1991, n.trees=best_inter_WNCP_recyle[[i]])
  base_NW_pred_all$Ynpk_pred_1992<-predict(brt_WNCP_recyle[[i]],base_NW_1992, n.trees=best_inter_WNCP_recyle[[i]])
  base_NW_pred_all$Ynpk_pred_1993<-predict(brt_WNCP_recyle[[i]],base_NW_1993, n.trees=best_inter_WNCP_recyle[[i]])
  base_NW_pred_all$Ynpk_pred_1994<-predict(brt_WNCP_recyle[[i]],base_NW_1994, n.trees=best_inter_WNCP_recyle[[i]])
  base_NW_pred_all$Ynpk_pred_1995<-predict(brt_WNCP_recyle[[i]],base_NW_1995, n.trees=best_inter_WNCP_recyle[[i]])
  base_NW_pred_all$Ynpk_pred_1996<-predict(brt_WNCP_recyle[[i]],base_NW_1996, n.trees=best_inter_WNCP_recyle[[i]])
  base_NW_pred_all$Ynpk_pred_1997<-predict(brt_WNCP_recyle[[i]],base_NW_1997, n.trees=best_inter_WNCP_recyle[[i]])
  base_NW_pred_all$Ynpk_pred_1998<-predict(brt_WNCP_recyle[[i]],base_NW_1998, n.trees=best_inter_WNCP_recyle[[i]])
  base_NW_pred_all$Ynpk_pred_1999<-predict(brt_WNCP_recyle[[i]],base_NW_1999, n.trees=best_inter_WNCP_recyle[[i]])
  base_NW_pred_all$Ynpk_pred_2000<-predict(brt_WNCP_recyle[[i]],base_NW_2000, n.trees=best_inter_WNCP_recyle[[i]])
  base_NW_pred_all$Ynpk_pred_2001<-predict(brt_WNCP_recyle[[i]],base_NW_2001, n.trees=best_inter_WNCP_recyle[[i]])
  base_NW_pred_all$Ynpk_pred_2002<-predict(brt_WNCP_recyle[[i]],base_NW_2002, n.trees=best_inter_WNCP_recyle[[i]])
  base_NW_pred_all$Ynpk_pred_2003<-predict(brt_WNCP_recyle[[i]],base_NW_2003, n.trees=best_inter_WNCP_recyle[[i]])
  base_NW_pred_all$Ynpk_pred_2004<-predict(brt_WNCP_recyle[[i]],base_NW_2004, n.trees=best_inter_WNCP_recyle[[i]])
  base_NW_pred_all$Ynpk_pred_2005<-predict(brt_WNCP_recyle[[i]],base_NW_2005, n.trees=best_inter_WNCP_recyle[[i]])
  base_NW_pred_all$Ynpk_pred_86.05<-predict(brt_WNCP_recyle[[i]],base_NW_86.05, n.trees=best_inter_WNCP_recyle[[i]])
  
  base_NW_pred_all$average<- apply(base_NW_pred_all[,7:26], 1, mean)
  base_NW_pred_all_recyle[[i]] <- base_NW_pred_all
}
  
base_NW_pred_all_recyle_mean<-base_NW_pred_all_recyle[[1]]
base_NW_pred_all_recyle_sd<-base_NW_pred_all_recyle[[1]]
for(i in 7:28){
  da=as.matrix(as.data.frame(sapply(1:50,function(x){base_NW_pred_all_recyle[[x]][i]})))
  base_NW_pred_all_recyle_mean[,i]=rowMeans(da)
  base_NW_pred_all_recyle_sd[,i]=apply(da,1,sd)
}

###RCP2.6 during 2040-2059

RCP2.6_NW_2050s<-subset(RCP2.6_2050s, Crop == "north wheat")
RCP2.6_NW_2040 <- RCP2.6_NW_2050s %>%
  select(Soil.quality, Tave_2040, Tmax_2040, Tmin_2040, GDD.0_2040, 
         PRE_2040, RAD_2040, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NW_2040)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NW_2041 <- RCP2.6_NW_2050s %>%
  select(Soil.quality, Tave_2041, Tmax_2041, Tmin_2041, GDD.0_2041, 
         PRE_2041, RAD_2041, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NW_2041)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NW_2042 <- RCP2.6_NW_2050s %>%
  select(Soil.quality, Tave_2042, Tmax_2042, Tmin_2042, GDD.0_2042, 
         PRE_2042, RAD_2042, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NW_2042)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NW_2043 <- RCP2.6_NW_2050s %>%
  select(Soil.quality, Tave_2043, Tmax_2043, Tmin_2043, GDD.0_2043, 
         PRE_2043, RAD_2043, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NW_2043)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NW_2044 <- RCP2.6_NW_2050s %>%
  select(Soil.quality, Tave_2044, Tmax_2044, Tmin_2044, GDD.0_2044, 
         PRE_2044, RAD_2044, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NW_2044)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NW_2045 <- RCP2.6_NW_2050s %>%
  select(Soil.quality, Tave_2045, Tmax_2045, Tmin_2045, GDD.0_2045, 
         PRE_2045, RAD_2045, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NW_2045)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NW_2046 <- RCP2.6_NW_2050s %>%
  select(Soil.quality, Tave_2046, Tmax_2046, Tmin_2046, GDD.0_2046, 
         PRE_2046, RAD_2046, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NW_2046)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NW_2047 <- RCP2.6_NW_2050s %>%
  select(Soil.quality, Tave_2047, Tmax_2047, Tmin_2047, GDD.0_2047, 
         PRE_2047, RAD_2047, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NW_2047)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NW_2048 <- RCP2.6_NW_2050s %>%
  select(Soil.quality, Tave_2048, Tmax_2048, Tmin_2048, GDD.0_2048, 
         PRE_2048, RAD_2048, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NW_2048)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NW_2049 <- RCP2.6_NW_2050s %>%
  select(Soil.quality, Tave_2049, Tmax_2049, Tmin_2049, GDD.0_2049, 
         PRE_2049, RAD_2049, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NW_2049)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NW_2050 <- RCP2.6_NW_2050s %>%
  select(Soil.quality, Tave_2050, Tmax_2050, Tmin_2050, GDD.0_2050, 
         PRE_2050, RAD_2050, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NW_2050)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NW_2051 <- RCP2.6_NW_2050s %>%
  select(Soil.quality, Tave_2051, Tmax_2051, Tmin_2051, GDD.0_2051, 
         PRE_2051, RAD_2051, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NW_2051)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NW_2052 <- RCP2.6_NW_2050s %>%
  select(Soil.quality, Tave_2052, Tmax_2052, Tmin_2052, GDD.0_2052, 
         PRE_2052, RAD_2052, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NW_2052)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NW_2053 <- RCP2.6_NW_2050s %>%
  select(Soil.quality, Tave_2053, Tmax_2053, Tmin_2053, GDD.0_2053, 
         PRE_2053, RAD_2053, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NW_2053)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NW_2054 <- RCP2.6_NW_2050s %>%
  select(Soil.quality, Tave_2054, Tmax_2054, Tmin_2054, GDD.0_2054, 
         PRE_2054, RAD_2054, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NW_2054)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NW_2055 <- RCP2.6_NW_2050s %>%
  select(Soil.quality, Tave_2055, Tmax_2055, Tmin_2055, GDD.0_2055, 
         PRE_2055, RAD_2055, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NW_2055)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NW_2056 <- RCP2.6_NW_2050s %>%
  select(Soil.quality, Tave_2056, Tmax_2056, Tmin_2056, GDD.0_2056, 
         PRE_2056, RAD_2056, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NW_2056)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NW_2057 <- RCP2.6_NW_2050s %>%
  select(Soil.quality, Tave_2057, Tmax_2057, Tmin_2057, GDD.0_2057, 
         PRE_2057, RAD_2057, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NW_2057)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NW_2058 <- RCP2.6_NW_2050s %>%
  select(Soil.quality, Tave_2058, Tmax_2058, Tmin_2058, GDD.0_2058, 
         PRE_2058, RAD_2058, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NW_2058)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NW_2059 <- RCP2.6_NW_2050s %>%
  select(Soil.quality, Tave_2059, Tmax_2059, Tmin_2059, GDD.0_2059, 
         PRE_2059, RAD_2059, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NW_2059)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NW_40.59 <- RCP2.6_NW_2050s %>%
  select(Soil.quality, Tave_2040.2059, Tmax_2040.2059, Tmin_2040.2059, GDD.0_2040.2059, 
         PRE_2040.2059, RAD_2040.2059, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NW_40.59)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

###predict yield by GBRT model

RCP2.6_NW_pred_2050s_recyle<-NULL

RCP2.6_NW_pred_2050s <- RCP2.6_NW_2050s %>%
  select(Soil.quality, Crop, NO, Year, Province, 
         NO.Station)

for (i in 1:50) {
  RCP2.6_NW_pred_2050s$Ynpk_pred_2040<-predict(brt_WNCP_recyle[[i]],RCP2.6_NW_2040, n.trees=best_inter_WNCP_recyle[[i]])
  RCP2.6_NW_pred_2050s$Ynpk_pred_2041<-predict(brt_WNCP_recyle[[i]],RCP2.6_NW_2041, n.trees=best_inter_WNCP_recyle[[i]])
  RCP2.6_NW_pred_2050s$Ynpk_pred_2042<-predict(brt_WNCP_recyle[[i]],RCP2.6_NW_2042, n.trees=best_inter_WNCP_recyle[[i]])
  RCP2.6_NW_pred_2050s$Ynpk_pred_2043<-predict(brt_WNCP_recyle[[i]],RCP2.6_NW_2043, n.trees=best_inter_WNCP_recyle[[i]])
  RCP2.6_NW_pred_2050s$Ynpk_pred_2044<-predict(brt_WNCP_recyle[[i]],RCP2.6_NW_2044, n.trees=best_inter_WNCP_recyle[[i]])
  RCP2.6_NW_pred_2050s$Ynpk_pred_2045<-predict(brt_WNCP_recyle[[i]],RCP2.6_NW_2045, n.trees=best_inter_WNCP_recyle[[i]])
  RCP2.6_NW_pred_2050s$Ynpk_pred_2046<-predict(brt_WNCP_recyle[[i]],RCP2.6_NW_2046, n.trees=best_inter_WNCP_recyle[[i]])
  RCP2.6_NW_pred_2050s$Ynpk_pred_2047<-predict(brt_WNCP_recyle[[i]],RCP2.6_NW_2047, n.trees=best_inter_WNCP_recyle[[i]])
  RCP2.6_NW_pred_2050s$Ynpk_pred_2048<-predict(brt_WNCP_recyle[[i]],RCP2.6_NW_2048, n.trees=best_inter_WNCP_recyle[[i]])
  RCP2.6_NW_pred_2050s$Ynpk_pred_2049<-predict(brt_WNCP_recyle[[i]],RCP2.6_NW_2049, n.trees=best_inter_WNCP_recyle[[i]])
  RCP2.6_NW_pred_2050s$Ynpk_pred_2050<-predict(brt_WNCP_recyle[[i]],RCP2.6_NW_2050, n.trees=best_inter_WNCP_recyle[[i]])
  RCP2.6_NW_pred_2050s$Ynpk_pred_2051<-predict(brt_WNCP_recyle[[i]],RCP2.6_NW_2051, n.trees=best_inter_WNCP_recyle[[i]])
  RCP2.6_NW_pred_2050s$Ynpk_pred_2052<-predict(brt_WNCP_recyle[[i]],RCP2.6_NW_2052, n.trees=best_inter_WNCP_recyle[[i]])
  RCP2.6_NW_pred_2050s$Ynpk_pred_2053<-predict(brt_WNCP_recyle[[i]],RCP2.6_NW_2053, n.trees=best_inter_WNCP_recyle[[i]])
  RCP2.6_NW_pred_2050s$Ynpk_pred_2054<-predict(brt_WNCP_recyle[[i]],RCP2.6_NW_2054, n.trees=best_inter_WNCP_recyle[[i]])
  RCP2.6_NW_pred_2050s$Ynpk_pred_2055<-predict(brt_WNCP_recyle[[i]],RCP2.6_NW_2055, n.trees=best_inter_WNCP_recyle[[i]])
  RCP2.6_NW_pred_2050s$Ynpk_pred_2056<-predict(brt_WNCP_recyle[[i]],RCP2.6_NW_2056, n.trees=best_inter_WNCP_recyle[[i]])
  RCP2.6_NW_pred_2050s$Ynpk_pred_2057<-predict(brt_WNCP_recyle[[i]],RCP2.6_NW_2057, n.trees=best_inter_WNCP_recyle[[i]])
  RCP2.6_NW_pred_2050s$Ynpk_pred_2058<-predict(brt_WNCP_recyle[[i]],RCP2.6_NW_2058, n.trees=best_inter_WNCP_recyle[[i]])
  RCP2.6_NW_pred_2050s$Ynpk_pred_2059<-predict(brt_WNCP_recyle[[i]],RCP2.6_NW_2059, n.trees=best_inter_WNCP_recyle[[i]])
  RCP2.6_NW_pred_2050s$Ynpk_pred_40.59<-predict(brt_WNCP_recyle[[i]],RCP2.6_NW_40.59, n.trees=best_inter_WNCP_recyle[[i]])
  
  RCP2.6_NW_pred_2050s$average<- apply(RCP2.6_NW_pred_2050s[,7:26], 1, mean)
  RCP2.6_NW_pred_2050s_recyle[[i]] <- RCP2.6_NW_pred_2050s
}
  
RCP2.6_NW_pred_2050s_recyle_mean<-RCP2.6_NW_pred_2050s_recyle[[1]]
RCP2.6_NW_pred_2050s_recyle_sd<-RCP2.6_NW_pred_2050s_recyle[[1]]
for(i in 7:28){
  da=as.matrix(as.data.frame(sapply(1:50,function(x){RCP2.6_NW_pred_2050s_recyle[[x]][i]})))
  RCP2.6_NW_pred_2050s_recyle_mean[,i]=rowMeans(da)
  RCP2.6_NW_pred_2050s_recyle_sd[,i]=apply(da,1,sd)
}

###RCP2.6 during 2080-2099

RCP2.6_NW_2100s<-subset(RCP2.6_2100s, Crop == "north wheat")
RCP2.6_NW_2080 <- RCP2.6_NW_2100s %>%
  select(Soil.quality, Tave_2080, Tmax_2080, Tmin_2080, GDD.0_2080, 
         PRE_2080, RAD_2080,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NW_2080)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NW_2081 <- RCP2.6_NW_2100s %>%
  select(Soil.quality, Tave_2081, Tmax_2081, Tmin_2081, GDD.0_2081, 
         PRE_2081, RAD_2081, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NW_2081)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NW_2082 <- RCP2.6_NW_2100s %>%
  select(Soil.quality, Tave_2082, Tmax_2082, Tmin_2082, GDD.0_2082, 
         PRE_2082, RAD_2082, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NW_2082)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NW_2083 <- RCP2.6_NW_2100s %>%
  select(Soil.quality, Tave_2083, Tmax_2083, Tmin_2083, GDD.0_2083, 
         PRE_2083, RAD_2083, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NW_2083)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NW_2084 <- RCP2.6_NW_2100s %>%
  select(Soil.quality, Tave_2084, Tmax_2084, Tmin_2084, GDD.0_2084, 
         PRE_2084, RAD_2084, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NW_2084)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NW_2085 <- RCP2.6_NW_2100s %>%
  select(Soil.quality, Tave_2085, Tmax_2085, Tmin_2085, GDD.0_2085, 
         PRE_2085, RAD_2085,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NW_2085)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NW_2086 <- RCP2.6_NW_2100s %>%
  select(Soil.quality, Tave_2086, Tmax_2086, Tmin_2086, GDD.0_2086, 
         PRE_2086, RAD_2086, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NW_2086)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NW_2087 <- RCP2.6_NW_2100s %>%
  select(Soil.quality, Tave_2087, Tmax_2087, Tmin_2087, GDD.0_2087, 
         PRE_2087, RAD_2087, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NW_2087)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NW_2088 <- RCP2.6_NW_2100s %>%
  select(Soil.quality, Tave_2088, Tmax_2088, Tmin_2088, GDD.0_2088, 
         PRE_2088, RAD_2088, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NW_2088)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NW_2089 <- RCP2.6_NW_2100s %>%
  select(Soil.quality, Tave_2089, Tmax_2089, Tmin_2089, GDD.0_2089, 
         PRE_2089, RAD_2089, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NW_2089)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NW_2090 <- RCP2.6_NW_2100s %>%
  select(Soil.quality, Tave_2090, Tmax_2090, Tmin_2090, GDD.0_2090, 
         PRE_2090, RAD_2090,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NW_2090)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NW_2091 <- RCP2.6_NW_2100s %>%
  select(Soil.quality, Tave_2091, Tmax_2091, Tmin_2091, GDD.0_2091, 
         PRE_2091, RAD_2091, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NW_2091)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NW_2092 <- RCP2.6_NW_2100s %>%
  select(Soil.quality, Tave_2092, Tmax_2092, Tmin_2092, GDD.0_2092, 
         PRE_2092, RAD_2092, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NW_2092)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NW_2093 <- RCP2.6_NW_2100s %>%
  select(Soil.quality, Tave_2093, Tmax_2093, Tmin_2093, GDD.0_2093, 
         PRE_2093, RAD_2093,Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NW_2093)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NW_2094 <- RCP2.6_NW_2100s %>%
  select(Soil.quality, Tave_2094, Tmax_2094, Tmin_2094, GDD.0_2094, 
         PRE_2094, RAD_2094,Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NW_2094)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NW_2095 <- RCP2.6_NW_2100s %>%
  select(Soil.quality, Tave_2095, Tmax_2095, Tmin_2095, GDD.0_2095, 
         PRE_2095, RAD_2095, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NW_2095)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NW_2096 <- RCP2.6_NW_2100s %>%
  select(Soil.quality, Tave_2096, Tmax_2096, Tmin_2096, GDD.0_2096, 
         PRE_2096, RAD_2096, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NW_2096)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NW_2097 <- RCP2.6_NW_2100s %>%
  select(Soil.quality, Tave_2097, Tmax_2097, Tmin_2097, GDD.0_2097, 
         PRE_2097, RAD_2097,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NW_2097)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NW_2098 <- RCP2.6_NW_2100s %>%
  select(Soil.quality, Tave_2098, Tmax_2098, Tmin_2098, GDD.0_2098, 
         PRE_2098, RAD_2098,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NW_2098)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NW_2099 <- RCP2.6_NW_2100s %>%
  select(Soil.quality, Tave_2099, Tmax_2099, Tmin_2099, GDD.0_2099, 
         PRE_2099, RAD_2099, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NW_2099)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NW_80.99 <- RCP2.6_NW_2100s %>%
  select(Soil.quality, Tave_2080.2099, Tmax_2080.2099, Tmin_2080.2099, GDD.0_2080.2099, 
         PRE_2080.2099, RAD_2080.2099, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NW_80.99)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")


###predict yield by GBRT model

RCP2.6_NW_pred_2100s_recyle<-NULL

RCP2.6_NW_pred_2100s <- RCP2.6_NW_2100s %>%
  select(Soil.quality, Crop, NO, Year, Province, 
         NO.Station)

for (i in 1:50) {
  RCP2.6_NW_pred_2100s$Ynpk_pred_2080<-predict(brt_WNCP_recyle[[i]],RCP2.6_NW_2080, n.trees=best_inter_WNCP_recyle[[i]])
  RCP2.6_NW_pred_2100s$Ynpk_pred_2081<-predict(brt_WNCP_recyle[[i]],RCP2.6_NW_2081, n.trees=best_inter_WNCP_recyle[[i]])
  RCP2.6_NW_pred_2100s$Ynpk_pred_2082<-predict(brt_WNCP_recyle[[i]],RCP2.6_NW_2082, n.trees=best_inter_WNCP_recyle[[i]])
  RCP2.6_NW_pred_2100s$Ynpk_pred_2083<-predict(brt_WNCP_recyle[[i]],RCP2.6_NW_2083, n.trees=best_inter_WNCP_recyle[[i]])
  RCP2.6_NW_pred_2100s$Ynpk_pred_2084<-predict(brt_WNCP_recyle[[i]],RCP2.6_NW_2084, n.trees=best_inter_WNCP_recyle[[i]])
  RCP2.6_NW_pred_2100s$Ynpk_pred_2085<-predict(brt_WNCP_recyle[[i]],RCP2.6_NW_2085, n.trees=best_inter_WNCP_recyle[[i]])
  RCP2.6_NW_pred_2100s$Ynpk_pred_2086<-predict(brt_WNCP_recyle[[i]],RCP2.6_NW_2086, n.trees=best_inter_WNCP_recyle[[i]])
  RCP2.6_NW_pred_2100s$Ynpk_pred_2087<-predict(brt_WNCP_recyle[[i]],RCP2.6_NW_2087, n.trees=best_inter_WNCP_recyle[[i]])
  RCP2.6_NW_pred_2100s$Ynpk_pred_2088<-predict(brt_WNCP_recyle[[i]],RCP2.6_NW_2088, n.trees=best_inter_WNCP_recyle[[i]])
  RCP2.6_NW_pred_2100s$Ynpk_pred_2089<-predict(brt_WNCP_recyle[[i]],RCP2.6_NW_2089, n.trees=best_inter_WNCP_recyle[[i]])
  RCP2.6_NW_pred_2100s$Ynpk_pred_2090<-predict(brt_WNCP_recyle[[i]],RCP2.6_NW_2090, n.trees=best_inter_WNCP_recyle[[i]])
  RCP2.6_NW_pred_2100s$Ynpk_pred_2091<-predict(brt_WNCP_recyle[[i]],RCP2.6_NW_2091, n.trees=best_inter_WNCP_recyle[[i]])
  RCP2.6_NW_pred_2100s$Ynpk_pred_2092<-predict(brt_WNCP_recyle[[i]],RCP2.6_NW_2092, n.trees=best_inter_WNCP_recyle[[i]])
  RCP2.6_NW_pred_2100s$Ynpk_pred_2093<-predict(brt_WNCP_recyle[[i]],RCP2.6_NW_2093, n.trees=best_inter_WNCP_recyle[[i]])
  RCP2.6_NW_pred_2100s$Ynpk_pred_2094<-predict(brt_WNCP_recyle[[i]],RCP2.6_NW_2094, n.trees=best_inter_WNCP_recyle[[i]])
  RCP2.6_NW_pred_2100s$Ynpk_pred_2095<-predict(brt_WNCP_recyle[[i]],RCP2.6_NW_2095, n.trees=best_inter_WNCP_recyle[[i]])
  RCP2.6_NW_pred_2100s$Ynpk_pred_2096<-predict(brt_WNCP_recyle[[i]],RCP2.6_NW_2096, n.trees=best_inter_WNCP_recyle[[i]])
  RCP2.6_NW_pred_2100s$Ynpk_pred_2097<-predict(brt_WNCP_recyle[[i]],RCP2.6_NW_2097, n.trees=best_inter_WNCP_recyle[[i]])
  RCP2.6_NW_pred_2100s$Ynpk_pred_2098<-predict(brt_WNCP_recyle[[i]],RCP2.6_NW_2098, n.trees=best_inter_WNCP_recyle[[i]])
  RCP2.6_NW_pred_2100s$Ynpk_pred_2099<-predict(brt_WNCP_recyle[[i]],RCP2.6_NW_2099, n.trees=best_inter_WNCP_recyle[[i]])
  RCP2.6_NW_pred_2100s$Ynpk_pred_80.99<-predict(brt_WNCP_recyle[[i]],RCP2.6_NW_80.99, n.trees=best_inter_WNCP_recyle[[i]])
  
  RCP2.6_NW_pred_2100s$average<- apply(RCP2.6_NW_pred_2100s[,7:26], 1, mean)
  RCP2.6_NW_pred_2100s_recyle[[i]] <- RCP2.6_NW_pred_2100s
}

RCP2.6_NW_pred_2100s_recyle_mean<-RCP2.6_NW_pred_2100s_recyle[[1]]
RCP2.6_NW_pred_2100s_recyle_sd<-RCP2.6_NW_pred_2100s_recyle[[1]]
for(i in 7:28){
  da=as.matrix(as.data.frame(sapply(1:50,function(x){RCP2.6_NW_pred_2100s_recyle[[x]][i]})))
  RCP2.6_NW_pred_2100s_recyle_mean[,i]=rowMeans(da)
  RCP2.6_NW_pred_2100s_recyle_sd[,i]=apply(da,1,sd)
}


###RCP8.5 during 2040-2099

RCP8.5_NW_2050s<-subset(RCP8.5_2050s, Crop == "north wheat")
RCP8.5_NW_2040 <- RCP8.5_NW_2050s %>%
  select(Soil.quality, Tave_2040, Tmax_2040, Tmin_2040, GDD.0_2040, 
         PRE_2040, RAD_2040, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NW_2040)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NW_2041 <- RCP8.5_NW_2050s %>%
  select(Soil.quality, Tave_2041, Tmax_2041, Tmin_2041, GDD.0_2041, 
         PRE_2041, RAD_2041, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NW_2041)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NW_2042 <- RCP8.5_NW_2050s %>%
  select(Soil.quality, Tave_2042, Tmax_2042, Tmin_2042, GDD.0_2042, 
         PRE_2042, RAD_2042, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NW_2042)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NW_2043 <- RCP8.5_NW_2050s %>%
  select(Soil.quality, Tave_2043, Tmax_2043, Tmin_2043, GDD.0_2043, 
         PRE_2043, RAD_2043, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NW_2043)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NW_2044 <- RCP8.5_NW_2050s %>%
  select(Soil.quality, Tave_2044, Tmax_2044, Tmin_2044, GDD.0_2044, 
         PRE_2044, RAD_2044, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NW_2044)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NW_2045 <- RCP8.5_NW_2050s %>%
  select(Soil.quality, Tave_2045, Tmax_2045, Tmin_2045, GDD.0_2045, 
         PRE_2045, RAD_2045, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NW_2045)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NW_2046 <- RCP8.5_NW_2050s %>%
  select(Soil.quality, Tave_2046, Tmax_2046, Tmin_2046, GDD.0_2046, 
         PRE_2046, RAD_2046,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NW_2046)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NW_2047 <- RCP8.5_NW_2050s %>%
  select(Soil.quality, Tave_2047, Tmax_2047, Tmin_2047, GDD.0_2047, 
         PRE_2047, RAD_2047, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NW_2047)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NW_2048 <- RCP8.5_NW_2050s %>%
  select(Soil.quality, Tave_2048, Tmax_2048, Tmin_2048, GDD.0_2048, 
         PRE_2048, RAD_2048, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NW_2048)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NW_2049 <- RCP8.5_NW_2050s %>%
  select(Soil.quality, Tave_2049, Tmax_2049, Tmin_2049, GDD.0_2049, 
         PRE_2049, RAD_2049, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NW_2049)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NW_2050 <- RCP8.5_NW_2050s %>%
  select(Soil.quality, Tave_2050, Tmax_2050, Tmin_2050, GDD.0_2050, 
         PRE_2050, RAD_2050, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NW_2050)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NW_2051 <- RCP8.5_NW_2050s %>%
  select(Soil.quality, Tave_2051, Tmax_2051, Tmin_2051, GDD.0_2051, 
         PRE_2051, RAD_2051,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NW_2051)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NW_2052 <- RCP8.5_NW_2050s %>%
  select(Soil.quality, Tave_2052, Tmax_2052, Tmin_2052, GDD.0_2052, 
         PRE_2052, RAD_2052, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NW_2052)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NW_2053 <- RCP8.5_NW_2050s %>%
  select(Soil.quality, Tave_2053, Tmax_2053, Tmin_2053, GDD.0_2053, 
         PRE_2053, RAD_2053, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NW_2053)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NW_2054 <- RCP8.5_NW_2050s %>%
  select(Soil.quality, Tave_2054, Tmax_2054, Tmin_2054, GDD.0_2054, 
         PRE_2054, RAD_2054,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NW_2054)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NW_2055 <- RCP8.5_NW_2050s %>%
  select(Soil.quality, Tave_2055, Tmax_2055, Tmin_2055, GDD.0_2055, 
         PRE_2055, RAD_2055, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NW_2055)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NW_2056 <- RCP8.5_NW_2050s %>%
  select(Soil.quality, Tave_2056, Tmax_2056, Tmin_2056, GDD.0_2056, 
         PRE_2056, RAD_2056, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NW_2056)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NW_2057 <- RCP8.5_NW_2050s %>%
  select(Soil.quality, Tave_2057, Tmax_2057, Tmin_2057, GDD.0_2057, 
         PRE_2057, RAD_2057, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NW_2057)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NW_2058 <- RCP8.5_NW_2050s %>%
  select(Soil.quality, Tave_2058, Tmax_2058, Tmin_2058, GDD.0_2058, 
         PRE_2058, RAD_2058, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NW_2058)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NW_2059 <- RCP8.5_NW_2050s %>%
  select(Soil.quality, Tave_2059, Tmax_2059, Tmin_2059, GDD.0_2059, 
         PRE_2059, RAD_2059, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NW_2059)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NW_40.59 <- RCP8.5_NW_2050s %>%
  select(Soil.quality, Tave_2040.2059, Tmax_2040.2059, Tmin_2040.2059, GDD.0_2040.2059, 
         PRE_2040.2059, RAD_2040.2059,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NW_40.59)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

###predict yield by GBRT model

RCP8.5_NW_pred_2050s_recyle<-NULL

RCP8.5_NW_pred_2050s <- RCP8.5_NW_2050s %>%
  select(Soil.quality, Crop, NO, Year, Province, 
         NO.Station)

for (i in 1:50) {
  RCP8.5_NW_pred_2050s$Ynpk_pred_2040<-predict(brt_WNCP_recyle[[i]],RCP8.5_NW_2040, n.trees=best_inter_WNCP_recyle[[i]])
  RCP8.5_NW_pred_2050s$Ynpk_pred_2041<-predict(brt_WNCP_recyle[[i]],RCP8.5_NW_2041, n.trees=best_inter_WNCP_recyle[[i]])
  RCP8.5_NW_pred_2050s$Ynpk_pred_2042<-predict(brt_WNCP_recyle[[i]],RCP8.5_NW_2042, n.trees=best_inter_WNCP_recyle[[i]])
  RCP8.5_NW_pred_2050s$Ynpk_pred_2043<-predict(brt_WNCP_recyle[[i]],RCP8.5_NW_2043, n.trees=best_inter_WNCP_recyle[[i]])
  RCP8.5_NW_pred_2050s$Ynpk_pred_2044<-predict(brt_WNCP_recyle[[i]],RCP8.5_NW_2044, n.trees=best_inter_WNCP_recyle[[i]])
  RCP8.5_NW_pred_2050s$Ynpk_pred_2045<-predict(brt_WNCP_recyle[[i]],RCP8.5_NW_2045, n.trees=best_inter_WNCP_recyle[[i]])
  RCP8.5_NW_pred_2050s$Ynpk_pred_2046<-predict(brt_WNCP_recyle[[i]],RCP8.5_NW_2046, n.trees=best_inter_WNCP_recyle[[i]])
  RCP8.5_NW_pred_2050s$Ynpk_pred_2047<-predict(brt_WNCP_recyle[[i]],RCP8.5_NW_2047, n.trees=best_inter_WNCP_recyle[[i]])
  RCP8.5_NW_pred_2050s$Ynpk_pred_2048<-predict(brt_WNCP_recyle[[i]],RCP8.5_NW_2048, n.trees=best_inter_WNCP_recyle[[i]])
  RCP8.5_NW_pred_2050s$Ynpk_pred_2049<-predict(brt_WNCP_recyle[[i]],RCP8.5_NW_2049, n.trees=best_inter_WNCP_recyle[[i]])
  RCP8.5_NW_pred_2050s$Ynpk_pred_2050<-predict(brt_WNCP_recyle[[i]],RCP8.5_NW_2050, n.trees=best_inter_WNCP_recyle[[i]])
  RCP8.5_NW_pred_2050s$Ynpk_pred_2051<-predict(brt_WNCP_recyle[[i]],RCP8.5_NW_2051, n.trees=best_inter_WNCP_recyle[[i]])
  RCP8.5_NW_pred_2050s$Ynpk_pred_2052<-predict(brt_WNCP_recyle[[i]],RCP8.5_NW_2052, n.trees=best_inter_WNCP_recyle[[i]])
  RCP8.5_NW_pred_2050s$Ynpk_pred_2053<-predict(brt_WNCP_recyle[[i]],RCP8.5_NW_2053, n.trees=best_inter_WNCP_recyle[[i]])
  RCP8.5_NW_pred_2050s$Ynpk_pred_2054<-predict(brt_WNCP_recyle[[i]],RCP8.5_NW_2054, n.trees=best_inter_WNCP_recyle[[i]])
  RCP8.5_NW_pred_2050s$Ynpk_pred_2055<-predict(brt_WNCP_recyle[[i]],RCP8.5_NW_2055, n.trees=best_inter_WNCP_recyle[[i]])
  RCP8.5_NW_pred_2050s$Ynpk_pred_2056<-predict(brt_WNCP_recyle[[i]],RCP8.5_NW_2056, n.trees=best_inter_WNCP_recyle[[i]])
  RCP8.5_NW_pred_2050s$Ynpk_pred_2057<-predict(brt_WNCP_recyle[[i]],RCP8.5_NW_2057, n.trees=best_inter_WNCP_recyle[[i]])
  RCP8.5_NW_pred_2050s$Ynpk_pred_2058<-predict(brt_WNCP_recyle[[i]],RCP8.5_NW_2058, n.trees=best_inter_WNCP_recyle[[i]])
  RCP8.5_NW_pred_2050s$Ynpk_pred_2059<-predict(brt_WNCP_recyle[[i]],RCP8.5_NW_2059, n.trees=best_inter_WNCP_recyle[[i]])
  RCP8.5_NW_pred_2050s$Ynpk_pred_40.59<-predict(brt_WNCP_recyle[[i]],RCP8.5_NW_40.59, n.trees=best_inter_WNCP_recyle[[i]])
  
  RCP8.5_NW_pred_2050s$average<- apply(RCP8.5_NW_pred_2050s[,7:26], 1, mean)
  RCP8.5_NW_pred_2050s_recyle[[i]] <- RCP8.5_NW_pred_2050s
}
  
RCP8.5_NW_pred_2050s_recyle_mean<-RCP8.5_NW_pred_2050s_recyle[[1]]
RCP8.5_NW_pred_2050s_recyle_sd<-RCP8.5_NW_pred_2050s_recyle[[1]]
for(i in 7:28){
  da=as.matrix(as.data.frame(sapply(1:50,function(x){RCP8.5_NW_pred_2050s_recyle[[x]][i]})))
  RCP8.5_NW_pred_2050s_recyle_mean[,i]=rowMeans(da)
  RCP8.5_NW_pred_2050s_recyle_sd[,i]=apply(da,1,sd)
}

###RCP8.5 during 2080-2099

RCP8.5_NW_2100s<-subset(RCP8.5_2100s, Crop == "north wheat")
RCP8.5_NW_2080 <- RCP8.5_NW_2100s %>%
  select(Soil.quality, Tave_2080, Tmax_2080, Tmin_2080, GDD.0_2080, 
         PRE_2080, RAD_2080,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NW_2080)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NW_2081 <- RCP8.5_NW_2100s %>%
  select(Soil.quality, Tave_2081, Tmax_2081, Tmin_2081, GDD.0_2081, 
         PRE_2081, RAD_2081, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NW_2081)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NW_2082 <- RCP8.5_NW_2100s %>%
  select(Soil.quality, Tave_2082, Tmax_2082, Tmin_2082, GDD.0_2082, 
         PRE_2082, RAD_2082, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NW_2082)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NW_2083 <- RCP8.5_NW_2100s %>%
  select(Soil.quality, Tave_2083, Tmax_2083, Tmin_2083, GDD.0_2083, 
         PRE_2083, RAD_2083, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NW_2083)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NW_2084 <- RCP8.5_NW_2100s %>%
  select(Soil.quality, Tave_2084, Tmax_2084, Tmin_2084, GDD.0_2084, 
         PRE_2084, RAD_2084, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NW_2084)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NW_2085 <- RCP8.5_NW_2100s %>%
  select(Soil.quality, Tave_2085, Tmax_2085, Tmin_2085, GDD.0_2085, 
         PRE_2085, RAD_2085, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NW_2085)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NW_2086 <- RCP8.5_NW_2100s %>%
  select(Soil.quality, Tave_2086, Tmax_2086, Tmin_2086, GDD.0_2086, 
         PRE_2086, RAD_2086,Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NW_2086)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NW_2087 <- RCP8.5_NW_2100s %>%
  select(Soil.quality, Tave_2087, Tmax_2087, Tmin_2087, GDD.0_2087, 
         PRE_2087, RAD_2087, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NW_2087)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NW_2088 <- RCP8.5_NW_2100s %>%
  select(Soil.quality, Tave_2088, Tmax_2088, Tmin_2088, GDD.0_2088, 
         PRE_2088, RAD_2088, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NW_2088)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NW_2089 <- RCP8.5_NW_2100s %>%
  select(Soil.quality, Tave_2089, Tmax_2089, Tmin_2089, GDD.0_2089, 
         PRE_2089, RAD_2089, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NW_2089)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NW_2090 <- RCP8.5_NW_2100s %>%
  select(Soil.quality, Tave_2090, Tmax_2090, Tmin_2090, GDD.0_2090, 
         PRE_2090, RAD_2090, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NW_2090)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NW_2091 <- RCP8.5_NW_2100s %>%
  select(Soil.quality, Tave_2091, Tmax_2091, Tmin_2091, GDD.0_2091, 
         PRE_2091, RAD_2091,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NW_2091)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NW_2092 <- RCP8.5_NW_2100s %>%
  select(Soil.quality, Tave_2092, Tmax_2092, Tmin_2092, GDD.0_2092, 
         PRE_2092, RAD_2092, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NW_2092)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NW_2093 <- RCP8.5_NW_2100s %>%
  select(Soil.quality, Tave_2093, Tmax_2093, Tmin_2093, GDD.0_2093, 
         PRE_2093, RAD_2093, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NW_2093)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NW_2094 <- RCP8.5_NW_2100s %>%
  select(Soil.quality, Tave_2094, Tmax_2094, Tmin_2094, GDD.0_2094, 
         PRE_2094, RAD_2094, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NW_2094)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NW_2095 <- RCP8.5_NW_2100s %>%
  select(Soil.quality, Tave_2095, Tmax_2095, Tmin_2095, GDD.0_2095, 
         PRE_2095, RAD_2095, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NW_2095)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NW_2096 <- RCP8.5_NW_2100s %>%
  select(Soil.quality, Tave_2096, Tmax_2096, Tmin_2096, GDD.0_2096, 
         PRE_2096, RAD_2096,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NW_2096)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NW_2097 <- RCP8.5_NW_2100s %>%
  select(Soil.quality, Tave_2097, Tmax_2097, Tmin_2097, GDD.0_2097, 
         PRE_2097, RAD_2097, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NW_2097)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NW_2098 <- RCP8.5_NW_2100s %>%
  select(Soil.quality, Tave_2098, Tmax_2098, Tmin_2098, GDD.0_2098, 
         PRE_2098, RAD_2098, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NW_2098)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NW_2099 <- RCP8.5_NW_2100s %>%
  select(Soil.quality, Tave_2099, Tmax_2099, Tmin_2099, GDD.0_2099, 
         PRE_2099, RAD_2099, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NW_2099)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NW_80.99 <- RCP8.5_NW_2100s %>%
  select(Soil.quality, Tave_2080.2099, Tmax_2080.2099, Tmin_2080.2099, GDD.0_2080.2099, 
         PRE_2080.2099, RAD_2080.2099, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NW_80.99)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")


###predict yield by GBRT model
RCP8.5_NW_pred_2100s_recyle<-NULL

RCP8.5_NW_pred_2100s <- RCP8.5_NW_2100s %>%
  select(Soil.quality, Crop, NO, Year, Province, 
         NO.Station)

for (i in 1:50) {
  RCP8.5_NW_pred_2100s$Ynpk_pred_2080<-predict(brt_WNCP_recyle[[i]],RCP8.5_NW_2080, n.trees=best_inter_WNCP_recyle[[i]])
  RCP8.5_NW_pred_2100s$Ynpk_pred_2081<-predict(brt_WNCP_recyle[[i]],RCP8.5_NW_2081, n.trees=best_inter_WNCP_recyle[[i]])
  RCP8.5_NW_pred_2100s$Ynpk_pred_2082<-predict(brt_WNCP_recyle[[i]],RCP8.5_NW_2082, n.trees=best_inter_WNCP_recyle[[i]])
  RCP8.5_NW_pred_2100s$Ynpk_pred_2083<-predict(brt_WNCP_recyle[[i]],RCP8.5_NW_2083, n.trees=best_inter_WNCP_recyle[[i]])
  RCP8.5_NW_pred_2100s$Ynpk_pred_2084<-predict(brt_WNCP_recyle[[i]],RCP8.5_NW_2084, n.trees=best_inter_WNCP_recyle[[i]])
  RCP8.5_NW_pred_2100s$Ynpk_pred_2085<-predict(brt_WNCP_recyle[[i]],RCP8.5_NW_2085, n.trees=best_inter_WNCP_recyle[[i]])
  RCP8.5_NW_pred_2100s$Ynpk_pred_2086<-predict(brt_WNCP_recyle[[i]],RCP8.5_NW_2086, n.trees=best_inter_WNCP_recyle[[i]])
  RCP8.5_NW_pred_2100s$Ynpk_pred_2087<-predict(brt_WNCP_recyle[[i]],RCP8.5_NW_2087, n.trees=best_inter_WNCP_recyle[[i]])
  RCP8.5_NW_pred_2100s$Ynpk_pred_2088<-predict(brt_WNCP_recyle[[i]],RCP8.5_NW_2088, n.trees=best_inter_WNCP_recyle[[i]])
  RCP8.5_NW_pred_2100s$Ynpk_pred_2089<-predict(brt_WNCP_recyle[[i]],RCP8.5_NW_2089, n.trees=best_inter_WNCP_recyle[[i]])
  RCP8.5_NW_pred_2100s$Ynpk_pred_2090<-predict(brt_WNCP_recyle[[i]],RCP8.5_NW_2090, n.trees=best_inter_WNCP_recyle[[i]])
  RCP8.5_NW_pred_2100s$Ynpk_pred_2091<-predict(brt_WNCP_recyle[[i]],RCP8.5_NW_2091, n.trees=best_inter_WNCP_recyle[[i]])
  RCP8.5_NW_pred_2100s$Ynpk_pred_2092<-predict(brt_WNCP_recyle[[i]],RCP8.5_NW_2092, n.trees=best_inter_WNCP_recyle[[i]])
  RCP8.5_NW_pred_2100s$Ynpk_pred_2093<-predict(brt_WNCP_recyle[[i]],RCP8.5_NW_2093, n.trees=best_inter_WNCP_recyle[[i]])
  RCP8.5_NW_pred_2100s$Ynpk_pred_2094<-predict(brt_WNCP_recyle[[i]],RCP8.5_NW_2094, n.trees=best_inter_WNCP_recyle[[i]])
  RCP8.5_NW_pred_2100s$Ynpk_pred_2095<-predict(brt_WNCP_recyle[[i]],RCP8.5_NW_2095, n.trees=best_inter_WNCP_recyle[[i]])
  RCP8.5_NW_pred_2100s$Ynpk_pred_2096<-predict(brt_WNCP_recyle[[i]],RCP8.5_NW_2096, n.trees=best_inter_WNCP_recyle[[i]])
  RCP8.5_NW_pred_2100s$Ynpk_pred_2097<-predict(brt_WNCP_recyle[[i]],RCP8.5_NW_2097, n.trees=best_inter_WNCP_recyle[[i]])
  RCP8.5_NW_pred_2100s$Ynpk_pred_2098<-predict(brt_WNCP_recyle[[i]],RCP8.5_NW_2098, n.trees=best_inter_WNCP_recyle[[i]])
  RCP8.5_NW_pred_2100s$Ynpk_pred_2099<-predict(brt_WNCP_recyle[[i]],RCP8.5_NW_2099, n.trees=best_inter_WNCP_recyle[[i]])
  RCP8.5_NW_pred_2100s$Ynpk_pred_80.99<-predict(brt_WNCP_recyle[[i]],RCP8.5_NW_80.99, n.trees=best_inter_WNCP_recyle[[i]])
  
  RCP8.5_NW_pred_2100s$average<- apply(RCP8.5_NW_pred_2100s[,7:26], 1, mean)
  RCP8.5_NW_pred_2100s_recyle[[i]] <- RCP8.5_NW_pred_2100s
}

RCP8.5_NW_pred_2100s_recyle_mean<-RCP8.5_NW_pred_2100s_recyle[[1]]
RCP8.5_NW_pred_2100s_recyle_sd<-RCP8.5_NW_pred_2100s_recyle[[1]]
for(i in 7:28){
  da=as.matrix(as.data.frame(sapply(1:50,function(x){RCP8.5_NW_pred_2100s_recyle[[x]][i]})))
  RCP8.5_NW_pred_2100s_recyle_mean[,i]=rowMeans(da)
  RCP8.5_NW_pred_2100s_recyle_sd[,i]=apply(da,1,sd)
}

###summarise predicted yield

sum_RCP2.6_NW_2050s_year<- cbind(base_NW_pred_all[,c("Soil.quality","Crop","NO","average")],RCP2.6_NW_pred_2050s[,7:28])
names(sum_RCP2.6_NW_2050s_year)[1:4]<- c("Soil quality", "Crop", "No", "Yield_base")
names(sum_RCP2.6_NW_2050s_year)[26]<- "Yield_RCP"
sum_RCP2.6_NW_2050s_year$Yield_change <- sum_RCP2.6_NW_2050s_year$`Yield_RCP` - sum_RCP2.6_NW_2050s_year$`Yield_base`
sum_RCP2.6_NW_2050s_year$Yield_change_per <- (sum_RCP2.6_NW_2050s_year$Yield_change / sum_RCP2.6_NW_2050s_year$Yield_base)*100
sum_RCP2.6_NW_2050s_year$RCP <- "RCP2.6"
sum_RCP2.6_NW_2050s_year$Time <- "2050"

sum_RCP8.5_NW_2050s_year<- cbind(base_NW_pred_all[,c("Soil.quality","Crop","NO","average")],RCP8.5_NW_pred_2050s[,7:28])
names(sum_RCP8.5_NW_2050s_year)[1:4]<- c("Soil quality", "Crop", "No", "Yield_base")
names(sum_RCP8.5_NW_2050s_year)[26]<- "Yield_RCP"
sum_RCP8.5_NW_2050s_year$Yield_change <- sum_RCP8.5_NW_2050s_year$`Yield_RCP` - sum_RCP8.5_NW_2050s_year$`Yield_base`
sum_RCP8.5_NW_2050s_year$Yield_change_per <- (sum_RCP8.5_NW_2050s_year$Yield_change / sum_RCP8.5_NW_2050s_year$Yield_base)*100
sum_RCP8.5_NW_2050s_year$RCP <- "RCP8.5"
sum_RCP8.5_NW_2050s_year$Time <- "2050"

sum_RCP2.6_NW_2100s_year<- cbind(base_NW_pred_all[,c("Soil.quality","Crop","NO","average")],RCP2.6_NW_pred_2100s[,7:28])
names(sum_RCP2.6_NW_2100s_year)[1:4]<- c("Soil quality", "Crop", "No", "Yield_base")
names(sum_RCP2.6_NW_2100s_year)[26]<- "Yield_RCP"
sum_RCP2.6_NW_2100s_year$Yield_change <- sum_RCP2.6_NW_2100s_year$`Yield_RCP` - sum_RCP2.6_NW_2100s_year$`Yield_base`
sum_RCP2.6_NW_2100s_year$Yield_change_per <- (sum_RCP2.6_NW_2100s_year$Yield_change / sum_RCP2.6_NW_2100s_year$Yield_base)*100
sum_RCP2.6_NW_2100s_year$RCP <- "RCP2.6"
sum_RCP2.6_NW_2100s_year$Time <- "2100"

sum_RCP8.5_NW_2100s_year<- cbind(base_NW_pred_all[,c("Soil.quality","Crop","NO","average")],RCP8.5_NW_pred_2100s[,7:28])
names(sum_RCP8.5_NW_2100s_year)[1:4]<- c("Soil quality", "Crop", "No", "Yield_base")
names(sum_RCP8.5_NW_2100s_year)[26]<- "Yield_RCP"
sum_RCP8.5_NW_2100s_year$Yield_change <- sum_RCP8.5_NW_2100s_year$`Yield_RCP` - sum_RCP8.5_NW_2100s_year$`Yield_base`
sum_RCP8.5_NW_2100s_year$Yield_change_per <- (sum_RCP8.5_NW_2100s_year$Yield_change / sum_RCP8.5_NW_2100s_year$Yield_base)*100
sum_RCP8.5_NW_2100s_year$RCP <- "RCP8.5"
sum_RCP8.5_NW_2100s_year$Time <- "2100"

sum_RCP_NW_2050s_year <- rbind(sum_RCP2.6_NW_2050s_year,sum_RCP8.5_NW_2050s_year)
sum_RCP_NW_2100s_year<-rbind(sum_RCP2.6_NW_2100s_year,sum_RCP8.5_NW_2100s_year)

write.csv(sum_RCP_NW_2050s_year,"./results/sum_RCP_NW_2050s_year.csv")
write.csv(sum_RCP_NW_2100s_year,"./results/sum_RCP_NW_2100s_year.csv")

###comparison of yield change between high and low quality soils 
###T test

t.test(Yield_change~`Soil quality` ,sum_RCP2.6_NW_2050s_year) #RCP2.6-2050s
t.test(Yield_change~`Soil quality` ,sum_RCP8.5_NW_2050s_year) #RCP8.5-2050s
t.test(Yield_change~`Soil quality` ,sum_RCP2.6_NW_2100s_year) #RCP2.6-2100s
t.test(Yield_change~`Soil quality` ,sum_RCP8.5_NW_2100s_year) #RCP8.5-2100s

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
}

#####Predicted yield under different climate scenarios
###baseline period 1986-2005
base_SW<-subset(base, Crop == "south wheat")
base_SW_1986 <- base_SW %>%
  select(Soil.quality, Tave_1986, Tmax_1986, Tmin_1986, GDD.0_1986, 
         PRE_1986, RAD_1986, Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_SW_1986)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_SW_1987 <- base_SW %>%
  select(Soil.quality, Tave_1987, Tmax_1987, Tmin_1987, GDD.0_1987, 
         PRE_1987, RAD_1987, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_SW_1987)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_SW_1988 <- base_SW %>%
  select(Soil.quality, Tave_1988, Tmax_1988, Tmin_1988, GDD.0_1988, 
         PRE_1988, RAD_1988, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_SW_1988)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_SW_1989 <- base_SW %>%
  select(Soil.quality, Tave_1989, Tmax_1989, Tmin_1989, GDD.0_1989, 
         PRE_1989, RAD_1989, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_SW_1989)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_SW_1990 <- base_SW %>%
  select(Soil.quality, Tave_1990, Tmax_1990, Tmin_1990, GDD.0_1990, 
         PRE_1990, RAD_1990, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_SW_1990)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_SW_1991 <- base_SW %>%
  select(Soil.quality, Tave_1991, Tmax_1991, Tmin_1991, GDD.0_1991, 
         PRE_1991, RAD_1991, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_SW_1991)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_SW_1992 <- base_SW %>%
  select(Soil.quality, Tave_1992, Tmax_1992, Tmin_1992, GDD.0_1992, 
         PRE_1992, RAD_1992,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_SW_1992)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_SW_1993 <- base_SW %>%
  select(Soil.quality, Tave_1993, Tmax_1993, Tmin_1993, GDD.0_1993, 
         PRE_1993, RAD_1993, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_SW_1993)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_SW_1994 <- base_SW %>%
  select(Soil.quality, Tave_1994, Tmax_1994, Tmin_1994, GDD.0_1994, 
         PRE_1994, RAD_1994, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_SW_1994)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_SW_1995 <- base_SW %>%
  select(Soil.quality, Tave_1995, Tmax_1995, Tmin_1995, GDD.0_1995, 
         PRE_1995, RAD_1995, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_SW_1995)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_SW_1996 <- base_SW %>%
  select(Soil.quality, Tave_1996, Tmax_1996, Tmin_1996, GDD.0_1996, 
         PRE_1996, RAD_1996, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_SW_1996)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_SW_1997 <- base_SW %>%
  select(Soil.quality, Tave_1997, Tmax_1997, Tmin_1997, GDD.0_1997, 
         PRE_1997, RAD_1997, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_SW_1997)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_SW_1998 <- base_SW %>%
  select(Soil.quality, Tave_1998, Tmax_1998, Tmin_1998, GDD.0_1998, 
         PRE_1998, RAD_1998, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_SW_1998)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_SW_1999 <- base_SW %>%
  select(Soil.quality, Tave_1999, Tmax_1999, Tmin_1999, GDD.0_1999, 
         PRE_1999, RAD_1999, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_SW_1999)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_SW_2000 <- base_SW %>%
  select(Soil.quality, Tave_2000, Tmax_2000, Tmin_2000, GDD.0_2000, 
         PRE_2000, RAD_2000, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_SW_2000)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_SW_2001 <- base_SW %>%
  select(Soil.quality, Tave_2001, Tmax_2001, Tmin_2001, GDD.0_2001, 
         PRE_2001, RAD_2001, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_SW_2001)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_SW_2002 <- base_SW %>%
  select(Soil.quality, Tave_2002, Tmax_2002, Tmin_2002, GDD.0_2002, 
         PRE_2002, RAD_2002, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_SW_2002)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_SW_2003 <- base_SW %>%
  select(Soil.quality, Tave_2003, Tmax_2003, Tmin_2003, GDD.0_2003, 
         PRE_2003, RAD_2003, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_SW_2003)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_SW_2004 <- base_SW %>%
  select(Soil.quality, Tave_2004, Tmax_2004, Tmin_2004, GDD.0_2004, 
         PRE_2004, RAD_2004, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_SW_2004)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_SW_2005 <- base_SW %>%
  select(Soil.quality, Tave_2005, Tmax_2005, Tmin_2005, GDD.0_2005, 
         PRE_2005, RAD_2005, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_SW_2005)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_SW_86.05 <- base_SW %>%
  select(Soil.quality, Tave_1986.2005, Tmax_1986.2005, Tmin_1986.2005, GDD.0_1986.2005, 
         PRE_1986.2005, RAD_1986.2005, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_SW_86.05)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                         "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                         "N", "P2O5", "K2O", "Yck", "Ynpk")

###predict yield by GBRT model

base_SW_pred_all_recyle<-NULL

base_SW_pred_all <- base_SW %>%
  select(Soil.quality, Crop, NO, Year,Province, 
         NO.Station)

for (i in 1:50) {
  base_SW_pred_all$Ynpk_pred_1986<-predict(brt_WYZB_recyle[[i]],base_SW_1986, n.trees=best_inter_WYZB_recyle[[i]])
  base_SW_pred_all$Ynpk_pred_1987<-predict(brt_WYZB_recyle[[i]],base_SW_1987, n.trees=best_inter_WYZB_recyle[[i]])
  base_SW_pred_all$Ynpk_pred_1988<-predict(brt_WYZB_recyle[[i]],base_SW_1988, n.trees=best_inter_WYZB_recyle[[i]])
  base_SW_pred_all$Ynpk_pred_1989<-predict(brt_WYZB_recyle[[i]],base_SW_1989, n.trees=best_inter_WYZB_recyle[[i]])
  base_SW_pred_all$Ynpk_pred_1990<-predict(brt_WYZB_recyle[[i]],base_SW_1990, n.trees=best_inter_WYZB_recyle[[i]])
  base_SW_pred_all$Ynpk_pred_1991<-predict(brt_WYZB_recyle[[i]],base_SW_1991, n.trees=best_inter_WYZB_recyle[[i]])
  base_SW_pred_all$Ynpk_pred_1992<-predict(brt_WYZB_recyle[[i]],base_SW_1992, n.trees=best_inter_WYZB_recyle[[i]])
  base_SW_pred_all$Ynpk_pred_1993<-predict(brt_WYZB_recyle[[i]],base_SW_1993, n.trees=best_inter_WYZB_recyle[[i]])
  base_SW_pred_all$Ynpk_pred_1994<-predict(brt_WYZB_recyle[[i]],base_SW_1994, n.trees=best_inter_WYZB_recyle[[i]])
  base_SW_pred_all$Ynpk_pred_1995<-predict(brt_WYZB_recyle[[i]],base_SW_1995, n.trees=best_inter_WYZB_recyle[[i]])
  base_SW_pred_all$Ynpk_pred_1996<-predict(brt_WYZB_recyle[[i]],base_SW_1996, n.trees=best_inter_WYZB_recyle[[i]])
  base_SW_pred_all$Ynpk_pred_1997<-predict(brt_WYZB_recyle[[i]],base_SW_1997, n.trees=best_inter_WYZB_recyle[[i]])
  base_SW_pred_all$Ynpk_pred_1998<-predict(brt_WYZB_recyle[[i]],base_SW_1998, n.trees=best_inter_WYZB_recyle[[i]])
  base_SW_pred_all$Ynpk_pred_1999<-predict(brt_WYZB_recyle[[i]],base_SW_1999, n.trees=best_inter_WYZB_recyle[[i]])
  base_SW_pred_all$Ynpk_pred_2000<-predict(brt_WYZB_recyle[[i]],base_SW_2000, n.trees=best_inter_WYZB_recyle[[i]])
  base_SW_pred_all$Ynpk_pred_2001<-predict(brt_WYZB_recyle[[i]],base_SW_2001, n.trees=best_inter_WYZB_recyle[[i]])
  base_SW_pred_all$Ynpk_pred_2002<-predict(brt_WYZB_recyle[[i]],base_SW_2002, n.trees=best_inter_WYZB_recyle[[i]])
  base_SW_pred_all$Ynpk_pred_2003<-predict(brt_WYZB_recyle[[i]],base_SW_2003, n.trees=best_inter_WYZB_recyle[[i]])
  base_SW_pred_all$Ynpk_pred_2004<-predict(brt_WYZB_recyle[[i]],base_SW_2004, n.trees=best_inter_WYZB_recyle[[i]])
  base_SW_pred_all$Ynpk_pred_2005<-predict(brt_WYZB_recyle[[i]],base_SW_2005, n.trees=best_inter_WYZB_recyle[[i]])
  base_SW_pred_all$Ynpk_pred_86.05<-predict(brt_WYZB_recyle[[i]],base_SW_86.05, n.trees=best_inter_WYZB_recyle[[i]])
  
  base_SW_pred_all$average<- apply(base_SW_pred_all[,7:26], 1, mean)
  base_SW_pred_all_recyle[[i]] <- base_SW_pred_all
}

base_SW_pred_all_recyle_mean<-base_SW_pred_all_recyle[[1]]
base_SW_pred_all_recyle_sd<-base_SW_pred_all_recyle[[1]]
for(i in 7:28){
  da=as.matrix(as.data.frame(sapply(1:50,function(x){base_SW_pred_all_recyle[[x]][i]})))
  base_SW_pred_all_recyle_mean[,i]=rowMeans(da)
  base_SW_pred_all_recyle_sd[,i]=apply(da,1,sd)
}

###RCP2.6 during 2040-2059

RCP2.6_SW_2050s<-subset(RCP2.6_2050s, Crop == "south wheat")
RCP2.6_SW_2040 <- RCP2.6_SW_2050s %>%
  select(Soil.quality, Tave_2040, Tmax_2040, Tmin_2040, GDD.0_2040, 
         PRE_2040, RAD_2040, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SW_2040)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SW_2041 <- RCP2.6_SW_2050s %>%
  select(Soil.quality, Tave_2041, Tmax_2041, Tmin_2041, GDD.0_2041, 
         PRE_2041, RAD_2041, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SW_2041)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SW_2042 <- RCP2.6_SW_2050s %>%
  select(Soil.quality, Tave_2042, Tmax_2042, Tmin_2042, GDD.0_2042, 
         PRE_2042, RAD_2042, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SW_2042)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SW_2043 <- RCP2.6_SW_2050s %>%
  select(Soil.quality, Tave_2043, Tmax_2043, Tmin_2043, GDD.0_2043, 
         PRE_2043, RAD_2043, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SW_2043)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SW_2044 <- RCP2.6_SW_2050s %>%
  select(Soil.quality, Tave_2044, Tmax_2044, Tmin_2044, GDD.0_2044, 
         PRE_2044, RAD_2044, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SW_2044)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SW_2045 <- RCP2.6_SW_2050s %>%
  select(Soil.quality, Tave_2045, Tmax_2045, Tmin_2045, GDD.0_2045, 
         PRE_2045, RAD_2045, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SW_2045)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SW_2046 <- RCP2.6_SW_2050s %>%
  select(Soil.quality, Tave_2046, Tmax_2046, Tmin_2046, GDD.0_2046, 
         PRE_2046, RAD_2046, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SW_2046)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SW_2047 <- RCP2.6_SW_2050s %>%
  select(Soil.quality, Tave_2047, Tmax_2047, Tmin_2047, GDD.0_2047, 
         PRE_2047, RAD_2047, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SW_2047)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SW_2048 <- RCP2.6_SW_2050s %>%
  select(Soil.quality, Tave_2048, Tmax_2048, Tmin_2048, GDD.0_2048, 
         PRE_2048, RAD_2048, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SW_2048)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SW_2049 <- RCP2.6_SW_2050s %>%
  select(Soil.quality, Tave_2049, Tmax_2049, Tmin_2049, GDD.0_2049, 
         PRE_2049, RAD_2049, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SW_2049)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SW_2050 <- RCP2.6_SW_2050s %>%
  select(Soil.quality, Tave_2050, Tmax_2050, Tmin_2050, GDD.0_2050, 
         PRE_2050, RAD_2050, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SW_2050)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SW_2051 <- RCP2.6_SW_2050s %>%
  select(Soil.quality, Tave_2051, Tmax_2051, Tmin_2051, GDD.0_2051, 
         PRE_2051, RAD_2051, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SW_2051)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SW_2052 <- RCP2.6_SW_2050s %>%
  select(Soil.quality, Tave_2052, Tmax_2052, Tmin_2052, GDD.0_2052, 
         PRE_2052, RAD_2052, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SW_2052)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SW_2053 <- RCP2.6_SW_2050s %>%
  select(Soil.quality, Tave_2053, Tmax_2053, Tmin_2053, GDD.0_2053, 
         PRE_2053, RAD_2053, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SW_2053)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SW_2054 <- RCP2.6_SW_2050s %>%
  select(Soil.quality, Tave_2054, Tmax_2054, Tmin_2054, GDD.0_2054, 
         PRE_2054, RAD_2054, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SW_2054)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SW_2055 <- RCP2.6_SW_2050s %>%
  select(Soil.quality, Tave_2055, Tmax_2055, Tmin_2055, GDD.0_2055, 
         PRE_2055, RAD_2055, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SW_2055)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SW_2056 <- RCP2.6_SW_2050s %>%
  select(Soil.quality, Tave_2056, Tmax_2056, Tmin_2056, GDD.0_2056, 
         PRE_2056, RAD_2056, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SW_2056)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SW_2057 <- RCP2.6_SW_2050s %>%
  select(Soil.quality, Tave_2057, Tmax_2057, Tmin_2057, GDD.0_2057, 
         PRE_2057, RAD_2057, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SW_2057)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SW_2058 <- RCP2.6_SW_2050s %>%
  select(Soil.quality, Tave_2058, Tmax_2058, Tmin_2058, GDD.0_2058, 
         PRE_2058, RAD_2058, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SW_2058)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SW_2059 <- RCP2.6_SW_2050s %>%
  select(Soil.quality, Tave_2059, Tmax_2059, Tmin_2059, GDD.0_2059, 
         PRE_2059, RAD_2059, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SW_2059)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SW_40.59 <- RCP2.6_SW_2050s %>%
  select(Soil.quality, Tave_2040.2059, Tmax_2040.2059, Tmin_2040.2059, GDD.0_2040.2059, 
         PRE_2040.2059, RAD_2040.2059, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SW_40.59)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

###predict yield by GBRT model

RCP2.6_SW_pred_2050s_recyle<-NULL

RCP2.6_SW_pred_2050s <- RCP2.6_SW_2050s %>%
  select(Soil.quality, Crop, NO, Year, Province, 
         NO.Station)

for (i in 1:50) {
  RCP2.6_SW_pred_2050s$Ynpk_pred_2040<-predict(brt_WYZB_recyle[[i]],RCP2.6_SW_2040, n.trees=best_inter_WYZB_recyle[[i]])
  RCP2.6_SW_pred_2050s$Ynpk_pred_2041<-predict(brt_WYZB_recyle[[i]],RCP2.6_SW_2041, n.trees=best_inter_WYZB_recyle[[i]])
  RCP2.6_SW_pred_2050s$Ynpk_pred_2042<-predict(brt_WYZB_recyle[[i]],RCP2.6_SW_2042, n.trees=best_inter_WYZB_recyle[[i]])
  RCP2.6_SW_pred_2050s$Ynpk_pred_2043<-predict(brt_WYZB_recyle[[i]],RCP2.6_SW_2043, n.trees=best_inter_WYZB_recyle[[i]])
  RCP2.6_SW_pred_2050s$Ynpk_pred_2044<-predict(brt_WYZB_recyle[[i]],RCP2.6_SW_2044, n.trees=best_inter_WYZB_recyle[[i]])
  RCP2.6_SW_pred_2050s$Ynpk_pred_2045<-predict(brt_WYZB_recyle[[i]],RCP2.6_SW_2045, n.trees=best_inter_WYZB_recyle[[i]])
  RCP2.6_SW_pred_2050s$Ynpk_pred_2046<-predict(brt_WYZB_recyle[[i]],RCP2.6_SW_2046, n.trees=best_inter_WYZB_recyle[[i]])
  RCP2.6_SW_pred_2050s$Ynpk_pred_2047<-predict(brt_WYZB_recyle[[i]],RCP2.6_SW_2047, n.trees=best_inter_WYZB_recyle[[i]])
  RCP2.6_SW_pred_2050s$Ynpk_pred_2048<-predict(brt_WYZB_recyle[[i]],RCP2.6_SW_2048, n.trees=best_inter_WYZB_recyle[[i]])
  RCP2.6_SW_pred_2050s$Ynpk_pred_2049<-predict(brt_WYZB_recyle[[i]],RCP2.6_SW_2049, n.trees=best_inter_WYZB_recyle[[i]])
  RCP2.6_SW_pred_2050s$Ynpk_pred_2050<-predict(brt_WYZB_recyle[[i]],RCP2.6_SW_2050, n.trees=best_inter_WYZB_recyle[[i]])
  RCP2.6_SW_pred_2050s$Ynpk_pred_2051<-predict(brt_WYZB_recyle[[i]],RCP2.6_SW_2051, n.trees=best_inter_WYZB_recyle[[i]])
  RCP2.6_SW_pred_2050s$Ynpk_pred_2052<-predict(brt_WYZB_recyle[[i]],RCP2.6_SW_2052, n.trees=best_inter_WYZB_recyle[[i]])
  RCP2.6_SW_pred_2050s$Ynpk_pred_2053<-predict(brt_WYZB_recyle[[i]],RCP2.6_SW_2053, n.trees=best_inter_WYZB_recyle[[i]])
  RCP2.6_SW_pred_2050s$Ynpk_pred_2054<-predict(brt_WYZB_recyle[[i]],RCP2.6_SW_2054, n.trees=best_inter_WYZB_recyle[[i]])
  RCP2.6_SW_pred_2050s$Ynpk_pred_2055<-predict(brt_WYZB_recyle[[i]],RCP2.6_SW_2055, n.trees=best_inter_WYZB_recyle[[i]])
  RCP2.6_SW_pred_2050s$Ynpk_pred_2056<-predict(brt_WYZB_recyle[[i]],RCP2.6_SW_2056, n.trees=best_inter_WYZB_recyle[[i]])
  RCP2.6_SW_pred_2050s$Ynpk_pred_2057<-predict(brt_WYZB_recyle[[i]],RCP2.6_SW_2057, n.trees=best_inter_WYZB_recyle[[i]])
  RCP2.6_SW_pred_2050s$Ynpk_pred_2058<-predict(brt_WYZB_recyle[[i]],RCP2.6_SW_2058, n.trees=best_inter_WYZB_recyle[[i]])
  RCP2.6_SW_pred_2050s$Ynpk_pred_2059<-predict(brt_WYZB_recyle[[i]],RCP2.6_SW_2059, n.trees=best_inter_WYZB_recyle[[i]])
  RCP2.6_SW_pred_2050s$Ynpk_pred_40.59<-predict(brt_WYZB_recyle[[i]],RCP2.6_SW_40.59, n.trees=best_inter_WYZB_recyle[[i]])
  
  RCP2.6_SW_pred_2050s$average<- apply(RCP2.6_SW_pred_2050s[,7:26], 1, mean)
  RCP2.6_SW_pred_2050s_recyle[[i]] <- RCP2.6_SW_pred_2050s
}

RCP2.6_SW_pred_2050s_recyle_mean<-RCP2.6_SW_pred_2050s_recyle[[1]]
RCP2.6_SW_pred_2050s_recyle_sd<-RCP2.6_SW_pred_2050s_recyle[[1]]
for(i in 7:28){
  da=as.matrix(as.data.frame(sapply(1:50,function(x){RCP2.6_SW_pred_2050s_recyle[[x]][i]})))
  RCP2.6_SW_pred_2050s_recyle_mean[,i]=rowMeans(da)
  RCP2.6_SW_pred_2050s_recyle_sd[,i]=apply(da,1,sd)
}

###RCP2.6 during 2080-2099

RCP2.6_SW_2100s<-subset(RCP2.6_2100s, Crop == "south wheat")
RCP2.6_SW_2080 <- RCP2.6_SW_2100s %>%
  select(Soil.quality, Tave_2080, Tmax_2080, Tmin_2080, GDD.0_2080, 
         PRE_2080, RAD_2080,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SW_2080)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SW_2081 <- RCP2.6_SW_2100s %>%
  select(Soil.quality, Tave_2081, Tmax_2081, Tmin_2081, GDD.0_2081, 
         PRE_2081, RAD_2081, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SW_2081)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SW_2082 <- RCP2.6_SW_2100s %>%
  select(Soil.quality, Tave_2082, Tmax_2082, Tmin_2082, GDD.0_2082, 
         PRE_2082, RAD_2082, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SW_2082)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SW_2083 <- RCP2.6_SW_2100s %>%
  select(Soil.quality, Tave_2083, Tmax_2083, Tmin_2083, GDD.0_2083, 
         PRE_2083, RAD_2083, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SW_2083)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SW_2084 <- RCP2.6_SW_2100s %>%
  select(Soil.quality, Tave_2084, Tmax_2084, Tmin_2084, GDD.0_2084, 
         PRE_2084, RAD_2084, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SW_2084)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SW_2085 <- RCP2.6_SW_2100s %>%
  select(Soil.quality, Tave_2085, Tmax_2085, Tmin_2085, GDD.0_2085, 
         PRE_2085, RAD_2085,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SW_2085)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SW_2086 <- RCP2.6_SW_2100s %>%
  select(Soil.quality, Tave_2086, Tmax_2086, Tmin_2086, GDD.0_2086, 
         PRE_2086, RAD_2086, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SW_2086)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SW_2087 <- RCP2.6_SW_2100s %>%
  select(Soil.quality, Tave_2087, Tmax_2087, Tmin_2087, GDD.0_2087, 
         PRE_2087, RAD_2087, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SW_2087)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SW_2088 <- RCP2.6_SW_2100s %>%
  select(Soil.quality, Tave_2088, Tmax_2088, Tmin_2088, GDD.0_2088, 
         PRE_2088, RAD_2088, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SW_2088)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SW_2089 <- RCP2.6_SW_2100s %>%
  select(Soil.quality, Tave_2089, Tmax_2089, Tmin_2089, GDD.0_2089, 
         PRE_2089, RAD_2089, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SW_2089)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SW_2090 <- RCP2.6_SW_2100s %>%
  select(Soil.quality, Tave_2090, Tmax_2090, Tmin_2090, GDD.0_2090, 
         PRE_2090, RAD_2090,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SW_2090)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SW_2091 <- RCP2.6_SW_2100s %>%
  select(Soil.quality, Tave_2091, Tmax_2091, Tmin_2091, GDD.0_2091, 
         PRE_2091, RAD_2091, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SW_2091)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SW_2092 <- RCP2.6_SW_2100s %>%
  select(Soil.quality, Tave_2092, Tmax_2092, Tmin_2092, GDD.0_2092, 
         PRE_2092, RAD_2092, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SW_2092)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SW_2093 <- RCP2.6_SW_2100s %>%
  select(Soil.quality, Tave_2093, Tmax_2093, Tmin_2093, GDD.0_2093, 
         PRE_2093, RAD_2093,Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SW_2093)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SW_2094 <- RCP2.6_SW_2100s %>%
  select(Soil.quality, Tave_2094, Tmax_2094, Tmin_2094, GDD.0_2094, 
         PRE_2094, RAD_2094,Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SW_2094)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SW_2095 <- RCP2.6_SW_2100s %>%
  select(Soil.quality, Tave_2095, Tmax_2095, Tmin_2095, GDD.0_2095, 
         PRE_2095, RAD_2095, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SW_2095)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SW_2096 <- RCP2.6_SW_2100s %>%
  select(Soil.quality, Tave_2096, Tmax_2096, Tmin_2096, GDD.0_2096, 
         PRE_2096, RAD_2096, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SW_2096)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SW_2097 <- RCP2.6_SW_2100s %>%
  select(Soil.quality, Tave_2097, Tmax_2097, Tmin_2097, GDD.0_2097, 
         PRE_2097, RAD_2097,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SW_2097)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SW_2098 <- RCP2.6_SW_2100s %>%
  select(Soil.quality, Tave_2098, Tmax_2098, Tmin_2098, GDD.0_2098, 
         PRE_2098, RAD_2098,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SW_2098)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SW_2099 <- RCP2.6_SW_2100s %>%
  select(Soil.quality, Tave_2099, Tmax_2099, Tmin_2099, GDD.0_2099, 
         PRE_2099, RAD_2099, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SW_2099)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SW_80.99 <- RCP2.6_SW_2100s %>%
  select(Soil.quality, Tave_2080.2099, Tmax_2080.2099, Tmin_2080.2099, GDD.0_2080.2099, 
         PRE_2080.2099, RAD_2080.2099, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SW_80.99)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")


###predict yield by GBRT model

RCP2.6_SW_pred_2100s_recyle<-NULL

RCP2.6_SW_pred_2100s <- RCP2.6_SW_2100s %>%
  select(Soil.quality, Crop, NO, Year, Province, 
         NO.Station)

for (i in 1:50) {
  RCP2.6_SW_pred_2100s$Ynpk_pred_2080<-predict(brt_WYZB_recyle[[i]],RCP2.6_SW_2080, n.trees=best_inter_WYZB_recyle[[i]])
  RCP2.6_SW_pred_2100s$Ynpk_pred_2081<-predict(brt_WYZB_recyle[[i]],RCP2.6_SW_2081, n.trees=best_inter_WYZB_recyle[[i]])
  RCP2.6_SW_pred_2100s$Ynpk_pred_2082<-predict(brt_WYZB_recyle[[i]],RCP2.6_SW_2082, n.trees=best_inter_WYZB_recyle[[i]])
  RCP2.6_SW_pred_2100s$Ynpk_pred_2083<-predict(brt_WYZB_recyle[[i]],RCP2.6_SW_2083, n.trees=best_inter_WYZB_recyle[[i]])
  RCP2.6_SW_pred_2100s$Ynpk_pred_2084<-predict(brt_WYZB_recyle[[i]],RCP2.6_SW_2084, n.trees=best_inter_WYZB_recyle[[i]])
  RCP2.6_SW_pred_2100s$Ynpk_pred_2085<-predict(brt_WYZB_recyle[[i]],RCP2.6_SW_2085, n.trees=best_inter_WYZB_recyle[[i]])
  RCP2.6_SW_pred_2100s$Ynpk_pred_2086<-predict(brt_WYZB_recyle[[i]],RCP2.6_SW_2086, n.trees=best_inter_WYZB_recyle[[i]])
  RCP2.6_SW_pred_2100s$Ynpk_pred_2087<-predict(brt_WYZB_recyle[[i]],RCP2.6_SW_2087, n.trees=best_inter_WYZB_recyle[[i]])
  RCP2.6_SW_pred_2100s$Ynpk_pred_2088<-predict(brt_WYZB_recyle[[i]],RCP2.6_SW_2088, n.trees=best_inter_WYZB_recyle[[i]])
  RCP2.6_SW_pred_2100s$Ynpk_pred_2089<-predict(brt_WYZB_recyle[[i]],RCP2.6_SW_2089, n.trees=best_inter_WYZB_recyle[[i]])
  RCP2.6_SW_pred_2100s$Ynpk_pred_2090<-predict(brt_WYZB_recyle[[i]],RCP2.6_SW_2090, n.trees=best_inter_WYZB_recyle[[i]])
  RCP2.6_SW_pred_2100s$Ynpk_pred_2091<-predict(brt_WYZB_recyle[[i]],RCP2.6_SW_2091, n.trees=best_inter_WYZB_recyle[[i]])
  RCP2.6_SW_pred_2100s$Ynpk_pred_2092<-predict(brt_WYZB_recyle[[i]],RCP2.6_SW_2092, n.trees=best_inter_WYZB_recyle[[i]])
  RCP2.6_SW_pred_2100s$Ynpk_pred_2093<-predict(brt_WYZB_recyle[[i]],RCP2.6_SW_2093, n.trees=best_inter_WYZB_recyle[[i]])
  RCP2.6_SW_pred_2100s$Ynpk_pred_2094<-predict(brt_WYZB_recyle[[i]],RCP2.6_SW_2094, n.trees=best_inter_WYZB_recyle[[i]])
  RCP2.6_SW_pred_2100s$Ynpk_pred_2095<-predict(brt_WYZB_recyle[[i]],RCP2.6_SW_2095, n.trees=best_inter_WYZB_recyle[[i]])
  RCP2.6_SW_pred_2100s$Ynpk_pred_2096<-predict(brt_WYZB_recyle[[i]],RCP2.6_SW_2096, n.trees=best_inter_WYZB_recyle[[i]])
  RCP2.6_SW_pred_2100s$Ynpk_pred_2097<-predict(brt_WYZB_recyle[[i]],RCP2.6_SW_2097, n.trees=best_inter_WYZB_recyle[[i]])
  RCP2.6_SW_pred_2100s$Ynpk_pred_2098<-predict(brt_WYZB_recyle[[i]],RCP2.6_SW_2098, n.trees=best_inter_WYZB_recyle[[i]])
  RCP2.6_SW_pred_2100s$Ynpk_pred_2099<-predict(brt_WYZB_recyle[[i]],RCP2.6_SW_2099, n.trees=best_inter_WYZB_recyle[[i]])
  RCP2.6_SW_pred_2100s$Ynpk_pred_80.99<-predict(brt_WYZB_recyle[[i]],RCP2.6_SW_80.99, n.trees=best_inter_WYZB_recyle[[i]])
  
  RCP2.6_SW_pred_2100s$average<- apply(RCP2.6_SW_pred_2100s[,7:26], 1, mean)
  RCP2.6_SW_pred_2100s_recyle[[i]] <- RCP2.6_SW_pred_2100s
}

RCP2.6_SW_pred_2100s_recyle_mean<-RCP2.6_SW_pred_2100s_recyle[[1]]
RCP2.6_SW_pred_2100s_recyle_sd<-RCP2.6_SW_pred_2100s_recyle[[1]]
for(i in 7:28){
  da=as.matrix(as.data.frame(sapply(1:50,function(x){RCP2.6_SW_pred_2100s_recyle[[x]][i]})))
  RCP2.6_SW_pred_2100s_recyle_mean[,i]=rowMeans(da)
  RCP2.6_SW_pred_2100s_recyle_sd[,i]=apply(da,1,sd)
}


###RCP8.5 during 2040-2099

RCP8.5_SW_2050s<-subset(RCP8.5_2050s, Crop == "south wheat")
RCP8.5_SW_2040 <- RCP8.5_SW_2050s %>%
  select(Soil.quality, Tave_2040, Tmax_2040, Tmin_2040, GDD.0_2040, 
         PRE_2040, RAD_2040, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SW_2040)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SW_2041 <- RCP8.5_SW_2050s %>%
  select(Soil.quality, Tave_2041, Tmax_2041, Tmin_2041, GDD.0_2041, 
         PRE_2041, RAD_2041, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SW_2041)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SW_2042 <- RCP8.5_SW_2050s %>%
  select(Soil.quality, Tave_2042, Tmax_2042, Tmin_2042, GDD.0_2042, 
         PRE_2042, RAD_2042, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SW_2042)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SW_2043 <- RCP8.5_SW_2050s %>%
  select(Soil.quality, Tave_2043, Tmax_2043, Tmin_2043, GDD.0_2043, 
         PRE_2043, RAD_2043, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SW_2043)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SW_2044 <- RCP8.5_SW_2050s %>%
  select(Soil.quality, Tave_2044, Tmax_2044, Tmin_2044, GDD.0_2044, 
         PRE_2044, RAD_2044, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SW_2044)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SW_2045 <- RCP8.5_SW_2050s %>%
  select(Soil.quality, Tave_2045, Tmax_2045, Tmin_2045, GDD.0_2045, 
         PRE_2045, RAD_2045, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SW_2045)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SW_2046 <- RCP8.5_SW_2050s %>%
  select(Soil.quality, Tave_2046, Tmax_2046, Tmin_2046, GDD.0_2046, 
         PRE_2046, RAD_2046,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SW_2046)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SW_2047 <- RCP8.5_SW_2050s %>%
  select(Soil.quality, Tave_2047, Tmax_2047, Tmin_2047, GDD.0_2047, 
         PRE_2047, RAD_2047, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SW_2047)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SW_2048 <- RCP8.5_SW_2050s %>%
  select(Soil.quality, Tave_2048, Tmax_2048, Tmin_2048, GDD.0_2048, 
         PRE_2048, RAD_2048, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SW_2048)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SW_2049 <- RCP8.5_SW_2050s %>%
  select(Soil.quality, Tave_2049, Tmax_2049, Tmin_2049, GDD.0_2049, 
         PRE_2049, RAD_2049, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SW_2049)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SW_2050 <- RCP8.5_SW_2050s %>%
  select(Soil.quality, Tave_2050, Tmax_2050, Tmin_2050, GDD.0_2050, 
         PRE_2050, RAD_2050, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SW_2050)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SW_2051 <- RCP8.5_SW_2050s %>%
  select(Soil.quality, Tave_2051, Tmax_2051, Tmin_2051, GDD.0_2051, 
         PRE_2051, RAD_2051,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SW_2051)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SW_2052 <- RCP8.5_SW_2050s %>%
  select(Soil.quality, Tave_2052, Tmax_2052, Tmin_2052, GDD.0_2052, 
         PRE_2052, RAD_2052, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SW_2052)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SW_2053 <- RCP8.5_SW_2050s %>%
  select(Soil.quality, Tave_2053, Tmax_2053, Tmin_2053, GDD.0_2053, 
         PRE_2053, RAD_2053, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SW_2053)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SW_2054 <- RCP8.5_SW_2050s %>%
  select(Soil.quality, Tave_2054, Tmax_2054, Tmin_2054, GDD.0_2054, 
         PRE_2054, RAD_2054,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SW_2054)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SW_2055 <- RCP8.5_SW_2050s %>%
  select(Soil.quality, Tave_2055, Tmax_2055, Tmin_2055, GDD.0_2055, 
         PRE_2055, RAD_2055, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SW_2055)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SW_2056 <- RCP8.5_SW_2050s %>%
  select(Soil.quality, Tave_2056, Tmax_2056, Tmin_2056, GDD.0_2056, 
         PRE_2056, RAD_2056, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SW_2056)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SW_2057 <- RCP8.5_SW_2050s %>%
  select(Soil.quality, Tave_2057, Tmax_2057, Tmin_2057, GDD.0_2057, 
         PRE_2057, RAD_2057, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SW_2057)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SW_2058 <- RCP8.5_SW_2050s %>%
  select(Soil.quality, Tave_2058, Tmax_2058, Tmin_2058, GDD.0_2058, 
         PRE_2058, RAD_2058, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SW_2058)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SW_2059 <- RCP8.5_SW_2050s %>%
  select(Soil.quality, Tave_2059, Tmax_2059, Tmin_2059, GDD.0_2059, 
         PRE_2059, RAD_2059, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SW_2059)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SW_40.59 <- RCP8.5_SW_2050s %>%
  select(Soil.quality, Tave_2040.2059, Tmax_2040.2059, Tmin_2040.2059, GDD.0_2040.2059, 
         PRE_2040.2059, RAD_2040.2059,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SW_40.59)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

###predict yield by GBRT model

RCP8.5_SW_pred_2050s_recyle<-NULL

RCP8.5_SW_pred_2050s <- RCP8.5_SW_2050s %>%
  select(Soil.quality, Crop, NO, Year, Province, 
         NO.Station)

for (i in 1:50) {
  RCP8.5_SW_pred_2050s$Ynpk_pred_2040<-predict(brt_WYZB_recyle[[i]],RCP8.5_SW_2040, n.trees=best_inter_WYZB_recyle[[i]])
  RCP8.5_SW_pred_2050s$Ynpk_pred_2041<-predict(brt_WYZB_recyle[[i]],RCP8.5_SW_2041, n.trees=best_inter_WYZB_recyle[[i]])
  RCP8.5_SW_pred_2050s$Ynpk_pred_2042<-predict(brt_WYZB_recyle[[i]],RCP8.5_SW_2042, n.trees=best_inter_WYZB_recyle[[i]])
  RCP8.5_SW_pred_2050s$Ynpk_pred_2043<-predict(brt_WYZB_recyle[[i]],RCP8.5_SW_2043, n.trees=best_inter_WYZB_recyle[[i]])
  RCP8.5_SW_pred_2050s$Ynpk_pred_2044<-predict(brt_WYZB_recyle[[i]],RCP8.5_SW_2044, n.trees=best_inter_WYZB_recyle[[i]])
  RCP8.5_SW_pred_2050s$Ynpk_pred_2045<-predict(brt_WYZB_recyle[[i]],RCP8.5_SW_2045, n.trees=best_inter_WYZB_recyle[[i]])
  RCP8.5_SW_pred_2050s$Ynpk_pred_2046<-predict(brt_WYZB_recyle[[i]],RCP8.5_SW_2046, n.trees=best_inter_WYZB_recyle[[i]])
  RCP8.5_SW_pred_2050s$Ynpk_pred_2047<-predict(brt_WYZB_recyle[[i]],RCP8.5_SW_2047, n.trees=best_inter_WYZB_recyle[[i]])
  RCP8.5_SW_pred_2050s$Ynpk_pred_2048<-predict(brt_WYZB_recyle[[i]],RCP8.5_SW_2048, n.trees=best_inter_WYZB_recyle[[i]])
  RCP8.5_SW_pred_2050s$Ynpk_pred_2049<-predict(brt_WYZB_recyle[[i]],RCP8.5_SW_2049, n.trees=best_inter_WYZB_recyle[[i]])
  RCP8.5_SW_pred_2050s$Ynpk_pred_2050<-predict(brt_WYZB_recyle[[i]],RCP8.5_SW_2050, n.trees=best_inter_WYZB_recyle[[i]])
  RCP8.5_SW_pred_2050s$Ynpk_pred_2051<-predict(brt_WYZB_recyle[[i]],RCP8.5_SW_2051, n.trees=best_inter_WYZB_recyle[[i]])
  RCP8.5_SW_pred_2050s$Ynpk_pred_2052<-predict(brt_WYZB_recyle[[i]],RCP8.5_SW_2052, n.trees=best_inter_WYZB_recyle[[i]])
  RCP8.5_SW_pred_2050s$Ynpk_pred_2053<-predict(brt_WYZB_recyle[[i]],RCP8.5_SW_2053, n.trees=best_inter_WYZB_recyle[[i]])
  RCP8.5_SW_pred_2050s$Ynpk_pred_2054<-predict(brt_WYZB_recyle[[i]],RCP8.5_SW_2054, n.trees=best_inter_WYZB_recyle[[i]])
  RCP8.5_SW_pred_2050s$Ynpk_pred_2055<-predict(brt_WYZB_recyle[[i]],RCP8.5_SW_2055, n.trees=best_inter_WYZB_recyle[[i]])
  RCP8.5_SW_pred_2050s$Ynpk_pred_2056<-predict(brt_WYZB_recyle[[i]],RCP8.5_SW_2056, n.trees=best_inter_WYZB_recyle[[i]])
  RCP8.5_SW_pred_2050s$Ynpk_pred_2057<-predict(brt_WYZB_recyle[[i]],RCP8.5_SW_2057, n.trees=best_inter_WYZB_recyle[[i]])
  RCP8.5_SW_pred_2050s$Ynpk_pred_2058<-predict(brt_WYZB_recyle[[i]],RCP8.5_SW_2058, n.trees=best_inter_WYZB_recyle[[i]])
  RCP8.5_SW_pred_2050s$Ynpk_pred_2059<-predict(brt_WYZB_recyle[[i]],RCP8.5_SW_2059, n.trees=best_inter_WYZB_recyle[[i]])
  RCP8.5_SW_pred_2050s$Ynpk_pred_40.59<-predict(brt_WYZB_recyle[[i]],RCP8.5_SW_40.59, n.trees=best_inter_WYZB_recyle[[i]])
  
  RCP8.5_SW_pred_2050s$average<- apply(RCP8.5_SW_pred_2050s[,7:26], 1, mean)
  RCP8.5_SW_pred_2050s_recyle[[i]] <- RCP8.5_SW_pred_2050s
}

RCP8.5_SW_pred_2050s_recyle_mean<-RCP8.5_SW_pred_2050s_recyle[[1]]
RCP8.5_SW_pred_2050s_recyle_sd<-RCP8.5_SW_pred_2050s_recyle[[1]]
for(i in 7:28){
  da=as.matrix(as.data.frame(sapply(1:50,function(x){RCP8.5_SW_pred_2050s_recyle[[x]][i]})))
  RCP8.5_SW_pred_2050s_recyle_mean[,i]=rowMeans(da)
  RCP8.5_SW_pred_2050s_recyle_sd[,i]=apply(da,1,sd)
}

###RCP8.5 during 2080-2099

RCP8.5_SW_2100s<-subset(RCP8.5_2100s, Crop == "south wheat")
RCP8.5_SW_2080 <- RCP8.5_SW_2100s %>%
  select(Soil.quality, Tave_2080, Tmax_2080, Tmin_2080, GDD.0_2080, 
         PRE_2080, RAD_2080,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SW_2080)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SW_2081 <- RCP8.5_SW_2100s %>%
  select(Soil.quality, Tave_2081, Tmax_2081, Tmin_2081, GDD.0_2081, 
         PRE_2081, RAD_2081, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SW_2081)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SW_2082 <- RCP8.5_SW_2100s %>%
  select(Soil.quality, Tave_2082, Tmax_2082, Tmin_2082, GDD.0_2082, 
         PRE_2082, RAD_2082, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SW_2082)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SW_2083 <- RCP8.5_SW_2100s %>%
  select(Soil.quality, Tave_2083, Tmax_2083, Tmin_2083, GDD.0_2083, 
         PRE_2083, RAD_2083, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SW_2083)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SW_2084 <- RCP8.5_SW_2100s %>%
  select(Soil.quality, Tave_2084, Tmax_2084, Tmin_2084, GDD.0_2084, 
         PRE_2084, RAD_2084, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SW_2084)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SW_2085 <- RCP8.5_SW_2100s %>%
  select(Soil.quality, Tave_2085, Tmax_2085, Tmin_2085, GDD.0_2085, 
         PRE_2085, RAD_2085, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SW_2085)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SW_2086 <- RCP8.5_SW_2100s %>%
  select(Soil.quality, Tave_2086, Tmax_2086, Tmin_2086, GDD.0_2086, 
         PRE_2086, RAD_2086,Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SW_2086)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SW_2087 <- RCP8.5_SW_2100s %>%
  select(Soil.quality, Tave_2087, Tmax_2087, Tmin_2087, GDD.0_2087, 
         PRE_2087, RAD_2087, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SW_2087)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SW_2088 <- RCP8.5_SW_2100s %>%
  select(Soil.quality, Tave_2088, Tmax_2088, Tmin_2088, GDD.0_2088, 
         PRE_2088, RAD_2088, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SW_2088)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SW_2089 <- RCP8.5_SW_2100s %>%
  select(Soil.quality, Tave_2089, Tmax_2089, Tmin_2089, GDD.0_2089, 
         PRE_2089, RAD_2089, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SW_2089)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SW_2090 <- RCP8.5_SW_2100s %>%
  select(Soil.quality, Tave_2090, Tmax_2090, Tmin_2090, GDD.0_2090, 
         PRE_2090, RAD_2090, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SW_2090)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SW_2091 <- RCP8.5_SW_2100s %>%
  select(Soil.quality, Tave_2091, Tmax_2091, Tmin_2091, GDD.0_2091, 
         PRE_2091, RAD_2091,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SW_2091)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SW_2092 <- RCP8.5_SW_2100s %>%
  select(Soil.quality, Tave_2092, Tmax_2092, Tmin_2092, GDD.0_2092, 
         PRE_2092, RAD_2092, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SW_2092)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SW_2093 <- RCP8.5_SW_2100s %>%
  select(Soil.quality, Tave_2093, Tmax_2093, Tmin_2093, GDD.0_2093, 
         PRE_2093, RAD_2093, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SW_2093)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SW_2094 <- RCP8.5_SW_2100s %>%
  select(Soil.quality, Tave_2094, Tmax_2094, Tmin_2094, GDD.0_2094, 
         PRE_2094, RAD_2094, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SW_2094)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SW_2095 <- RCP8.5_SW_2100s %>%
  select(Soil.quality, Tave_2095, Tmax_2095, Tmin_2095, GDD.0_2095, 
         PRE_2095, RAD_2095, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SW_2095)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SW_2096 <- RCP8.5_SW_2100s %>%
  select(Soil.quality, Tave_2096, Tmax_2096, Tmin_2096, GDD.0_2096, 
         PRE_2096, RAD_2096,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SW_2096)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SW_2097 <- RCP8.5_SW_2100s %>%
  select(Soil.quality, Tave_2097, Tmax_2097, Tmin_2097, GDD.0_2097, 
         PRE_2097, RAD_2097, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SW_2097)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SW_2098 <- RCP8.5_SW_2100s %>%
  select(Soil.quality, Tave_2098, Tmax_2098, Tmin_2098, GDD.0_2098, 
         PRE_2098, RAD_2098, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SW_2098)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SW_2099 <- RCP8.5_SW_2100s %>%
  select(Soil.quality, Tave_2099, Tmax_2099, Tmin_2099, GDD.0_2099, 
         PRE_2099, RAD_2099, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SW_2099)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SW_80.99 <- RCP8.5_SW_2100s %>%
  select(Soil.quality, Tave_2080.2099, Tmax_2080.2099, Tmin_2080.2099, GDD.0_2080.2099, 
         PRE_2080.2099, RAD_2080.2099, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SW_80.99)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")


###predict yield by GBRT model
RCP8.5_SW_pred_2100s_recyle<-NULL

RCP8.5_SW_pred_2100s <- RCP8.5_SW_2100s %>%
  select(Soil.quality, Crop, NO, Year, Province, 
         NO.Station)

for (i in 1:50) {
  RCP8.5_SW_pred_2100s$Ynpk_pred_2080<-predict(brt_WYZB_recyle[[i]],RCP8.5_SW_2080, n.trees=best_inter_WYZB_recyle[[i]])
  RCP8.5_SW_pred_2100s$Ynpk_pred_2081<-predict(brt_WYZB_recyle[[i]],RCP8.5_SW_2081, n.trees=best_inter_WYZB_recyle[[i]])
  RCP8.5_SW_pred_2100s$Ynpk_pred_2082<-predict(brt_WYZB_recyle[[i]],RCP8.5_SW_2082, n.trees=best_inter_WYZB_recyle[[i]])
  RCP8.5_SW_pred_2100s$Ynpk_pred_2083<-predict(brt_WYZB_recyle[[i]],RCP8.5_SW_2083, n.trees=best_inter_WYZB_recyle[[i]])
  RCP8.5_SW_pred_2100s$Ynpk_pred_2084<-predict(brt_WYZB_recyle[[i]],RCP8.5_SW_2084, n.trees=best_inter_WYZB_recyle[[i]])
  RCP8.5_SW_pred_2100s$Ynpk_pred_2085<-predict(brt_WYZB_recyle[[i]],RCP8.5_SW_2085, n.trees=best_inter_WYZB_recyle[[i]])
  RCP8.5_SW_pred_2100s$Ynpk_pred_2086<-predict(brt_WYZB_recyle[[i]],RCP8.5_SW_2086, n.trees=best_inter_WYZB_recyle[[i]])
  RCP8.5_SW_pred_2100s$Ynpk_pred_2087<-predict(brt_WYZB_recyle[[i]],RCP8.5_SW_2087, n.trees=best_inter_WYZB_recyle[[i]])
  RCP8.5_SW_pred_2100s$Ynpk_pred_2088<-predict(brt_WYZB_recyle[[i]],RCP8.5_SW_2088, n.trees=best_inter_WYZB_recyle[[i]])
  RCP8.5_SW_pred_2100s$Ynpk_pred_2089<-predict(brt_WYZB_recyle[[i]],RCP8.5_SW_2089, n.trees=best_inter_WYZB_recyle[[i]])
  RCP8.5_SW_pred_2100s$Ynpk_pred_2090<-predict(brt_WYZB_recyle[[i]],RCP8.5_SW_2090, n.trees=best_inter_WYZB_recyle[[i]])
  RCP8.5_SW_pred_2100s$Ynpk_pred_2091<-predict(brt_WYZB_recyle[[i]],RCP8.5_SW_2091, n.trees=best_inter_WYZB_recyle[[i]])
  RCP8.5_SW_pred_2100s$Ynpk_pred_2092<-predict(brt_WYZB_recyle[[i]],RCP8.5_SW_2092, n.trees=best_inter_WYZB_recyle[[i]])
  RCP8.5_SW_pred_2100s$Ynpk_pred_2093<-predict(brt_WYZB_recyle[[i]],RCP8.5_SW_2093, n.trees=best_inter_WYZB_recyle[[i]])
  RCP8.5_SW_pred_2100s$Ynpk_pred_2094<-predict(brt_WYZB_recyle[[i]],RCP8.5_SW_2094, n.trees=best_inter_WYZB_recyle[[i]])
  RCP8.5_SW_pred_2100s$Ynpk_pred_2095<-predict(brt_WYZB_recyle[[i]],RCP8.5_SW_2095, n.trees=best_inter_WYZB_recyle[[i]])
  RCP8.5_SW_pred_2100s$Ynpk_pred_2096<-predict(brt_WYZB_recyle[[i]],RCP8.5_SW_2096, n.trees=best_inter_WYZB_recyle[[i]])
  RCP8.5_SW_pred_2100s$Ynpk_pred_2097<-predict(brt_WYZB_recyle[[i]],RCP8.5_SW_2097, n.trees=best_inter_WYZB_recyle[[i]])
  RCP8.5_SW_pred_2100s$Ynpk_pred_2098<-predict(brt_WYZB_recyle[[i]],RCP8.5_SW_2098, n.trees=best_inter_WYZB_recyle[[i]])
  RCP8.5_SW_pred_2100s$Ynpk_pred_2099<-predict(brt_WYZB_recyle[[i]],RCP8.5_SW_2099, n.trees=best_inter_WYZB_recyle[[i]])
  RCP8.5_SW_pred_2100s$Ynpk_pred_80.99<-predict(brt_WYZB_recyle[[i]],RCP8.5_SW_80.99, n.trees=best_inter_WYZB_recyle[[i]])
  
  RCP8.5_SW_pred_2100s$average<- apply(RCP8.5_SW_pred_2100s[,7:26], 1, mean)
  RCP8.5_SW_pred_2100s_recyle[[i]] <- RCP8.5_SW_pred_2100s
}

RCP8.5_SW_pred_2100s_recyle_mean<-RCP8.5_SW_pred_2100s_recyle[[1]]
RCP8.5_SW_pred_2100s_recyle_sd<-RCP8.5_SW_pred_2100s_recyle[[1]]
for(i in 7:28){
  da=as.matrix(as.data.frame(sapply(1:50,function(x){RCP8.5_SW_pred_2100s_recyle[[x]][i]})))
  RCP8.5_SW_pred_2100s_recyle_mean[,i]=rowMeans(da)
  RCP8.5_SW_pred_2100s_recyle_sd[,i]=apply(da,1,sd)
}

###summarise predicted yield

sum_RCP2.6_SW_2050s_year<- cbind(base_SW_pred_all[,c("Soil.quality","Crop","NO","average")],RCP2.6_SW_pred_2050s[,7:28])
names(sum_RCP2.6_SW_2050s_year)[1:4]<- c("Soil quality", "Crop", "No", "Yield_base")
names(sum_RCP2.6_SW_2050s_year)[26]<- "Yield_RCP"
sum_RCP2.6_SW_2050s_year$Yield_change <- sum_RCP2.6_SW_2050s_year$`Yield_RCP` - sum_RCP2.6_SW_2050s_year$`Yield_base`
sum_RCP2.6_SW_2050s_year$Yield_change_per <- (sum_RCP2.6_SW_2050s_year$Yield_change / sum_RCP2.6_SW_2050s_year$Yield_base)*100
sum_RCP2.6_SW_2050s_year$RCP <- "RCP2.6"
sum_RCP2.6_SW_2050s_year$Time <- "2050"

sum_RCP8.5_SW_2050s_year<- cbind(base_SW_pred_all[,c("Soil.quality","Crop","NO","average")],RCP8.5_SW_pred_2050s[,7:28])
names(sum_RCP8.5_SW_2050s_year)[1:4]<- c("Soil quality", "Crop", "No", "Yield_base")
names(sum_RCP8.5_SW_2050s_year)[26]<- "Yield_RCP"
sum_RCP8.5_SW_2050s_year$Yield_change <- sum_RCP8.5_SW_2050s_year$`Yield_RCP` - sum_RCP8.5_SW_2050s_year$`Yield_base`
sum_RCP8.5_SW_2050s_year$Yield_change_per <- (sum_RCP8.5_SW_2050s_year$Yield_change / sum_RCP8.5_SW_2050s_year$Yield_base)*100
sum_RCP8.5_SW_2050s_year$RCP <- "RCP8.5"
sum_RCP8.5_SW_2050s_year$Time <- "2050"

sum_RCP2.6_SW_2100s_year<- cbind(base_SW_pred_all[,c("Soil.quality","Crop","NO","average")],RCP2.6_SW_pred_2100s[,7:28])
names(sum_RCP2.6_SW_2100s_year)[1:4]<- c("Soil quality", "Crop", "No", "Yield_base")
names(sum_RCP2.6_SW_2100s_year)[26]<- "Yield_RCP"
sum_RCP2.6_SW_2100s_year$Yield_change <- sum_RCP2.6_SW_2100s_year$`Yield_RCP` - sum_RCP2.6_SW_2100s_year$`Yield_base`
sum_RCP2.6_SW_2100s_year$Yield_change_per <- (sum_RCP2.6_SW_2100s_year$Yield_change / sum_RCP2.6_SW_2100s_year$Yield_base)*100
sum_RCP2.6_SW_2100s_year$RCP <- "RCP2.6"
sum_RCP2.6_SW_2100s_year$Time <- "2100"

sum_RCP8.5_SW_2100s_year<- cbind(base_SW_pred_all[,c("Soil.quality","Crop","NO","average")],RCP8.5_SW_pred_2100s[,7:28])
names(sum_RCP8.5_SW_2100s_year)[1:4]<- c("Soil quality", "Crop", "No", "Yield_base")
names(sum_RCP8.5_SW_2100s_year)[26]<- "Yield_RCP"
sum_RCP8.5_SW_2100s_year$Yield_change <- sum_RCP8.5_SW_2100s_year$`Yield_RCP` - sum_RCP8.5_SW_2100s_year$`Yield_base`
sum_RCP8.5_SW_2100s_year$Yield_change_per <- (sum_RCP8.5_SW_2100s_year$Yield_change / sum_RCP8.5_SW_2100s_year$Yield_base)*100
sum_RCP8.5_SW_2100s_year$RCP <- "RCP8.5"
sum_RCP8.5_SW_2100s_year$Time <- "2100"

sum_RCP_SW_2050s_year <- rbind(sum_RCP2.6_SW_2050s_year,sum_RCP8.5_SW_2050s_year)
sum_RCP_SW_2100s_year<-rbind(sum_RCP2.6_SW_2100s_year,sum_RCP8.5_SW_2100s_year)

write.csv(sum_RCP_SW_2050s_year,"./results/sum_RCP_SW_2050s_year.csv")
write.csv(sum_RCP_SW_2100s_year,"./results/sum_RCP_SW_2100s_year.csv")

###comparison of yield change between high and low quality soils 
###T test

t.test(Yield_change~`Soil quality` ,sum_RCP2.6_SW_2050s_year) #RCP2.6-2050s
t.test(Yield_change~`Soil quality` ,sum_RCP8.5_SW_2050s_year) #RCP8.5-2050s
t.test(Yield_change~`Soil quality` ,sum_RCP2.6_SW_2100s_year) #RCP2.6-2100s
t.test(Yield_change~`Soil quality` ,sum_RCP8.5_SW_2100s_year) #RCP8.5-2100s

#==========================================================
# set model for winter wheat in NWC
W_NWC<-read.csv("W-NWC.csv")
W_NWC$Year<-as.factor(W_NWC$Year)
W_NWC$Cultivar<-as.factor(W_NWC$Cultivar)
W_NWC$Soil.type<-as.factor(W_NWC$Soil.type)
W_NWC$Soil.texture<-as.factor(W_NWC$Soil.texture)

# set train and test data(random 10% for 50 times)
brt_WNWC_recyle<- NULL
best_inter_WNWC_recyle<-NULL
summary_WNWC_recyle <- NULL

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
                interaction.depth = 9, shrinkage = 0.05, 
                bag.fraction = 0.5,cv.folds = 10)
  brt_WNWC_recyle[[i]]<-brt_WNWC
  print(brt_WNWC)
  best_inter_WNWC<-gbm.perf(brt_WNWC,method = "cv")
  best_inter_WNWC_recyle[[i]]<-best_inter_WNWC
  summary_WNWC_record<-summary(brt_WNWC,n.trees=best_inter_WNWC)
  summary_WNWC_recyle[[i]] <-summary_WNWC_record
}

#####Predicted yield under different climate scenarios
###baseline period 1986-2005
base_NWW<-subset(base, Crop == "northwest wheat")
base_NWW_1986 <- base_NWW %>%
  select(Soil.quality, Tave_1986, Tmax_1986, Tmin_1986, GDD.0_1986, 
         PRE_1986, RAD_1986, Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NWW_1986)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                         "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                         "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NWW_1987 <- base_NWW %>%
  select(Soil.quality, Tave_1987, Tmax_1987, Tmin_1987, GDD.0_1987, 
         PRE_1987, RAD_1987, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NWW_1987)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                         "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                         "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NWW_1988 <- base_NWW %>%
  select(Soil.quality, Tave_1988, Tmax_1988, Tmin_1988, GDD.0_1988, 
         PRE_1988, RAD_1988, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NWW_1988)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                         "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                         "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NWW_1989 <- base_NWW %>%
  select(Soil.quality, Tave_1989, Tmax_1989, Tmin_1989, GDD.0_1989, 
         PRE_1989, RAD_1989, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NWW_1989)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                         "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                         "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NWW_1990 <- base_NWW %>%
  select(Soil.quality, Tave_1990, Tmax_1990, Tmin_1990, GDD.0_1990, 
         PRE_1990, RAD_1990, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NWW_1990)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                         "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                         "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NWW_1991 <- base_NWW %>%
  select(Soil.quality, Tave_1991, Tmax_1991, Tmin_1991, GDD.0_1991, 
         PRE_1991, RAD_1991, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NWW_1991)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                         "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                         "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NWW_1992 <- base_NWW %>%
  select(Soil.quality, Tave_1992, Tmax_1992, Tmin_1992, GDD.0_1992, 
         PRE_1992, RAD_1992,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NWW_1992)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                         "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                         "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NWW_1993 <- base_NWW %>%
  select(Soil.quality, Tave_1993, Tmax_1993, Tmin_1993, GDD.0_1993, 
         PRE_1993, RAD_1993, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NWW_1993)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                         "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                         "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NWW_1994 <- base_NWW %>%
  select(Soil.quality, Tave_1994, Tmax_1994, Tmin_1994, GDD.0_1994, 
         PRE_1994, RAD_1994, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NWW_1994)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                         "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                         "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NWW_1995 <- base_NWW %>%
  select(Soil.quality, Tave_1995, Tmax_1995, Tmin_1995, GDD.0_1995, 
         PRE_1995, RAD_1995, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NWW_1995)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                         "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                         "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NWW_1996 <- base_NWW %>%
  select(Soil.quality, Tave_1996, Tmax_1996, Tmin_1996, GDD.0_1996, 
         PRE_1996, RAD_1996, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NWW_1996)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                         "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                         "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NWW_1997 <- base_NWW %>%
  select(Soil.quality, Tave_1997, Tmax_1997, Tmin_1997, GDD.0_1997, 
         PRE_1997, RAD_1997, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NWW_1997)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                         "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                         "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NWW_1998 <- base_NWW %>%
  select(Soil.quality, Tave_1998, Tmax_1998, Tmin_1998, GDD.0_1998, 
         PRE_1998, RAD_1998, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NWW_1998)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                         "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                         "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NWW_1999 <- base_NWW %>%
  select(Soil.quality, Tave_1999, Tmax_1999, Tmin_1999, GDD.0_1999, 
         PRE_1999, RAD_1999, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NWW_1999)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                         "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                         "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NWW_2000 <- base_NWW %>%
  select(Soil.quality, Tave_2000, Tmax_2000, Tmin_2000, GDD.0_2000, 
         PRE_2000, RAD_2000, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NWW_2000)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                         "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                         "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NWW_2001 <- base_NWW %>%
  select(Soil.quality, Tave_2001, Tmax_2001, Tmin_2001, GDD.0_2001, 
         PRE_2001, RAD_2001, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NWW_2001)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                         "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                         "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NWW_2002 <- base_NWW %>%
  select(Soil.quality, Tave_2002, Tmax_2002, Tmin_2002, GDD.0_2002, 
         PRE_2002, RAD_2002, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NWW_2002)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                         "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                         "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NWW_2003 <- base_NWW %>%
  select(Soil.quality, Tave_2003, Tmax_2003, Tmin_2003, GDD.0_2003, 
         PRE_2003, RAD_2003, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NWW_2003)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                         "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                         "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NWW_2004 <- base_NWW %>%
  select(Soil.quality, Tave_2004, Tmax_2004, Tmin_2004, GDD.0_2004, 
         PRE_2004, RAD_2004, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NWW_2004)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                         "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                         "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NWW_2005 <- base_NWW %>%
  select(Soil.quality, Tave_2005, Tmax_2005, Tmin_2005, GDD.0_2005, 
         PRE_2005, RAD_2005, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NWW_2005)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                         "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                         "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NWW_86.05 <- base_NWW %>%
  select(Soil.quality, Tave_1986.2005, Tmax_1986.2005, Tmin_1986.2005, GDD.0_1986.2005, 
         PRE_1986.2005, RAD_1986.2005, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NWW_86.05)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

###predict yield by GBRT model

base_NWW_pred_all_recyle<-NULL

base_NWW_pred_all <- base_NWW %>%
  select(Soil.quality, Crop, NO, Year,Province, 
         NO.Station)

for (i in 1:50) {
  base_NWW_pred_all$Ynpk_pred_1986<-predict(brt_WNWC_recyle[[i]],base_NWW_1986, n.trees=best_inter_WNWC_recyle[[i]])
  base_NWW_pred_all$Ynpk_pred_1987<-predict(brt_WNWC_recyle[[i]],base_NWW_1987, n.trees=best_inter_WNWC_recyle[[i]])
  base_NWW_pred_all$Ynpk_pred_1988<-predict(brt_WNWC_recyle[[i]],base_NWW_1988, n.trees=best_inter_WNWC_recyle[[i]])
  base_NWW_pred_all$Ynpk_pred_1989<-predict(brt_WNWC_recyle[[i]],base_NWW_1989, n.trees=best_inter_WNWC_recyle[[i]])
  base_NWW_pred_all$Ynpk_pred_1990<-predict(brt_WNWC_recyle[[i]],base_NWW_1990, n.trees=best_inter_WNWC_recyle[[i]])
  base_NWW_pred_all$Ynpk_pred_1991<-predict(brt_WNWC_recyle[[i]],base_NWW_1991, n.trees=best_inter_WNWC_recyle[[i]])
  base_NWW_pred_all$Ynpk_pred_1992<-predict(brt_WNWC_recyle[[i]],base_NWW_1992, n.trees=best_inter_WNWC_recyle[[i]])
  base_NWW_pred_all$Ynpk_pred_1993<-predict(brt_WNWC_recyle[[i]],base_NWW_1993, n.trees=best_inter_WNWC_recyle[[i]])
  base_NWW_pred_all$Ynpk_pred_1994<-predict(brt_WNWC_recyle[[i]],base_NWW_1994, n.trees=best_inter_WNWC_recyle[[i]])
  base_NWW_pred_all$Ynpk_pred_1995<-predict(brt_WNWC_recyle[[i]],base_NWW_1995, n.trees=best_inter_WNWC_recyle[[i]])
  base_NWW_pred_all$Ynpk_pred_1996<-predict(brt_WNWC_recyle[[i]],base_NWW_1996, n.trees=best_inter_WNWC_recyle[[i]])
  base_NWW_pred_all$Ynpk_pred_1997<-predict(brt_WNWC_recyle[[i]],base_NWW_1997, n.trees=best_inter_WNWC_recyle[[i]])
  base_NWW_pred_all$Ynpk_pred_1998<-predict(brt_WNWC_recyle[[i]],base_NWW_1998, n.trees=best_inter_WNWC_recyle[[i]])
  base_NWW_pred_all$Ynpk_pred_1999<-predict(brt_WNWC_recyle[[i]],base_NWW_1999, n.trees=best_inter_WNWC_recyle[[i]])
  base_NWW_pred_all$Ynpk_pred_2000<-predict(brt_WNWC_recyle[[i]],base_NWW_2000, n.trees=best_inter_WNWC_recyle[[i]])
  base_NWW_pred_all$Ynpk_pred_2001<-predict(brt_WNWC_recyle[[i]],base_NWW_2001, n.trees=best_inter_WNWC_recyle[[i]])
  base_NWW_pred_all$Ynpk_pred_2002<-predict(brt_WNWC_recyle[[i]],base_NWW_2002, n.trees=best_inter_WNWC_recyle[[i]])
  base_NWW_pred_all$Ynpk_pred_2003<-predict(brt_WNWC_recyle[[i]],base_NWW_2003, n.trees=best_inter_WNWC_recyle[[i]])
  base_NWW_pred_all$Ynpk_pred_2004<-predict(brt_WNWC_recyle[[i]],base_NWW_2004, n.trees=best_inter_WNWC_recyle[[i]])
  base_NWW_pred_all$Ynpk_pred_2005<-predict(brt_WNWC_recyle[[i]],base_NWW_2005, n.trees=best_inter_WNWC_recyle[[i]])
  base_NWW_pred_all$Ynpk_pred_86.05<-predict(brt_WNWC_recyle[[i]],base_NWW_86.05, n.trees=best_inter_WNWC_recyle[[i]])
  
  base_NWW_pred_all$average<- apply(base_NWW_pred_all[,7:26], 1, mean)
  base_NWW_pred_all_recyle[[i]] <- base_NWW_pred_all
}

base_NWW_pred_all_recyle_mean<-base_NWW_pred_all_recyle[[1]]
base_NWW_pred_all_recyle_sd<-base_NWW_pred_all_recyle[[1]]
for(i in 7:28){
  da=as.matrix(as.data.frame(sapply(1:50,function(x){base_NWW_pred_all_recyle[[x]][i]})))
  base_NWW_pred_all_recyle_mean[,i]=rowMeans(da)
  base_NWW_pred_all_recyle_sd[,i]=apply(da,1,sd)
}

###RCP2.6 during 2040-2059

RCP2.6_NWW_2050s<-subset(RCP2.6_2050s, Crop == "northwest wheat")
RCP2.6_NWW_2040 <- RCP2.6_NWW_2050s %>%
  select(Soil.quality, Tave_2040, Tmax_2040, Tmin_2040, GDD.0_2040, 
         PRE_2040, RAD_2040, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NWW_2040)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NWW_2041 <- RCP2.6_NWW_2050s %>%
  select(Soil.quality, Tave_2041, Tmax_2041, Tmin_2041, GDD.0_2041, 
         PRE_2041, RAD_2041, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NWW_2041)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NWW_2042 <- RCP2.6_NWW_2050s %>%
  select(Soil.quality, Tave_2042, Tmax_2042, Tmin_2042, GDD.0_2042, 
         PRE_2042, RAD_2042, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NWW_2042)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NWW_2043 <- RCP2.6_NWW_2050s %>%
  select(Soil.quality, Tave_2043, Tmax_2043, Tmin_2043, GDD.0_2043, 
         PRE_2043, RAD_2043, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NWW_2043)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NWW_2044 <- RCP2.6_NWW_2050s %>%
  select(Soil.quality, Tave_2044, Tmax_2044, Tmin_2044, GDD.0_2044, 
         PRE_2044, RAD_2044, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NWW_2044)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NWW_2045 <- RCP2.6_NWW_2050s %>%
  select(Soil.quality, Tave_2045, Tmax_2045, Tmin_2045, GDD.0_2045, 
         PRE_2045, RAD_2045, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NWW_2045)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NWW_2046 <- RCP2.6_NWW_2050s %>%
  select(Soil.quality, Tave_2046, Tmax_2046, Tmin_2046, GDD.0_2046, 
         PRE_2046, RAD_2046, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NWW_2046)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NWW_2047 <- RCP2.6_NWW_2050s %>%
  select(Soil.quality, Tave_2047, Tmax_2047, Tmin_2047, GDD.0_2047, 
         PRE_2047, RAD_2047, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NWW_2047)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NWW_2048 <- RCP2.6_NWW_2050s %>%
  select(Soil.quality, Tave_2048, Tmax_2048, Tmin_2048, GDD.0_2048, 
         PRE_2048, RAD_2048, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NWW_2048)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NWW_2049 <- RCP2.6_NWW_2050s %>%
  select(Soil.quality, Tave_2049, Tmax_2049, Tmin_2049, GDD.0_2049, 
         PRE_2049, RAD_2049, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NWW_2049)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NWW_2050 <- RCP2.6_NWW_2050s %>%
  select(Soil.quality, Tave_2050, Tmax_2050, Tmin_2050, GDD.0_2050, 
         PRE_2050, RAD_2050, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NWW_2050)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NWW_2051 <- RCP2.6_NWW_2050s %>%
  select(Soil.quality, Tave_2051, Tmax_2051, Tmin_2051, GDD.0_2051, 
         PRE_2051, RAD_2051, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NWW_2051)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NWW_2052 <- RCP2.6_NWW_2050s %>%
  select(Soil.quality, Tave_2052, Tmax_2052, Tmin_2052, GDD.0_2052, 
         PRE_2052, RAD_2052, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NWW_2052)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NWW_2053 <- RCP2.6_NWW_2050s %>%
  select(Soil.quality, Tave_2053, Tmax_2053, Tmin_2053, GDD.0_2053, 
         PRE_2053, RAD_2053, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NWW_2053)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NWW_2054 <- RCP2.6_NWW_2050s %>%
  select(Soil.quality, Tave_2054, Tmax_2054, Tmin_2054, GDD.0_2054, 
         PRE_2054, RAD_2054, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NWW_2054)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NWW_2055 <- RCP2.6_NWW_2050s %>%
  select(Soil.quality, Tave_2055, Tmax_2055, Tmin_2055, GDD.0_2055, 
         PRE_2055, RAD_2055, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NWW_2055)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NWW_2056 <- RCP2.6_NWW_2050s %>%
  select(Soil.quality, Tave_2056, Tmax_2056, Tmin_2056, GDD.0_2056, 
         PRE_2056, RAD_2056, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NWW_2056)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NWW_2057 <- RCP2.6_NWW_2050s %>%
  select(Soil.quality, Tave_2057, Tmax_2057, Tmin_2057, GDD.0_2057, 
         PRE_2057, RAD_2057, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NWW_2057)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NWW_2058 <- RCP2.6_NWW_2050s %>%
  select(Soil.quality, Tave_2058, Tmax_2058, Tmin_2058, GDD.0_2058, 
         PRE_2058, RAD_2058, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NWW_2058)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NWW_2059 <- RCP2.6_NWW_2050s %>%
  select(Soil.quality, Tave_2059, Tmax_2059, Tmin_2059, GDD.0_2059, 
         PRE_2059, RAD_2059, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NWW_2059)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NWW_40.59 <- RCP2.6_NWW_2050s %>%
  select(Soil.quality, Tave_2040.2059, Tmax_2040.2059, Tmin_2040.2059, GDD.0_2040.2059, 
         PRE_2040.2059, RAD_2040.2059, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NWW_40.59)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                            "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                            "N", "P2O5", "K2O", "Yck", "Ynpk")

###predict yield by GBRT model

RCP2.6_NWW_pred_2050s_recyle<-NULL

RCP2.6_NWW_pred_2050s <- RCP2.6_NWW_2050s %>%
  select(Soil.quality, Crop, NO, Year, Province, 
         NO.Station)

for (i in 1:50) {
  RCP2.6_NWW_pred_2050s$Ynpk_pred_2040<-predict(brt_WNWC_recyle[[i]],RCP2.6_NWW_2040, n.trees=best_inter_WNWC_recyle[[i]])
  RCP2.6_NWW_pred_2050s$Ynpk_pred_2041<-predict(brt_WNWC_recyle[[i]],RCP2.6_NWW_2041, n.trees=best_inter_WNWC_recyle[[i]])
  RCP2.6_NWW_pred_2050s$Ynpk_pred_2042<-predict(brt_WNWC_recyle[[i]],RCP2.6_NWW_2042, n.trees=best_inter_WNWC_recyle[[i]])
  RCP2.6_NWW_pred_2050s$Ynpk_pred_2043<-predict(brt_WNWC_recyle[[i]],RCP2.6_NWW_2043, n.trees=best_inter_WNWC_recyle[[i]])
  RCP2.6_NWW_pred_2050s$Ynpk_pred_2044<-predict(brt_WNWC_recyle[[i]],RCP2.6_NWW_2044, n.trees=best_inter_WNWC_recyle[[i]])
  RCP2.6_NWW_pred_2050s$Ynpk_pred_2045<-predict(brt_WNWC_recyle[[i]],RCP2.6_NWW_2045, n.trees=best_inter_WNWC_recyle[[i]])
  RCP2.6_NWW_pred_2050s$Ynpk_pred_2046<-predict(brt_WNWC_recyle[[i]],RCP2.6_NWW_2046, n.trees=best_inter_WNWC_recyle[[i]])
  RCP2.6_NWW_pred_2050s$Ynpk_pred_2047<-predict(brt_WNWC_recyle[[i]],RCP2.6_NWW_2047, n.trees=best_inter_WNWC_recyle[[i]])
  RCP2.6_NWW_pred_2050s$Ynpk_pred_2048<-predict(brt_WNWC_recyle[[i]],RCP2.6_NWW_2048, n.trees=best_inter_WNWC_recyle[[i]])
  RCP2.6_NWW_pred_2050s$Ynpk_pred_2049<-predict(brt_WNWC_recyle[[i]],RCP2.6_NWW_2049, n.trees=best_inter_WNWC_recyle[[i]])
  RCP2.6_NWW_pred_2050s$Ynpk_pred_2050<-predict(brt_WNWC_recyle[[i]],RCP2.6_NWW_2050, n.trees=best_inter_WNWC_recyle[[i]])
  RCP2.6_NWW_pred_2050s$Ynpk_pred_2051<-predict(brt_WNWC_recyle[[i]],RCP2.6_NWW_2051, n.trees=best_inter_WNWC_recyle[[i]])
  RCP2.6_NWW_pred_2050s$Ynpk_pred_2052<-predict(brt_WNWC_recyle[[i]],RCP2.6_NWW_2052, n.trees=best_inter_WNWC_recyle[[i]])
  RCP2.6_NWW_pred_2050s$Ynpk_pred_2053<-predict(brt_WNWC_recyle[[i]],RCP2.6_NWW_2053, n.trees=best_inter_WNWC_recyle[[i]])
  RCP2.6_NWW_pred_2050s$Ynpk_pred_2054<-predict(brt_WNWC_recyle[[i]],RCP2.6_NWW_2054, n.trees=best_inter_WNWC_recyle[[i]])
  RCP2.6_NWW_pred_2050s$Ynpk_pred_2055<-predict(brt_WNWC_recyle[[i]],RCP2.6_NWW_2055, n.trees=best_inter_WNWC_recyle[[i]])
  RCP2.6_NWW_pred_2050s$Ynpk_pred_2056<-predict(brt_WNWC_recyle[[i]],RCP2.6_NWW_2056, n.trees=best_inter_WNWC_recyle[[i]])
  RCP2.6_NWW_pred_2050s$Ynpk_pred_2057<-predict(brt_WNWC_recyle[[i]],RCP2.6_NWW_2057, n.trees=best_inter_WNWC_recyle[[i]])
  RCP2.6_NWW_pred_2050s$Ynpk_pred_2058<-predict(brt_WNWC_recyle[[i]],RCP2.6_NWW_2058, n.trees=best_inter_WNWC_recyle[[i]])
  RCP2.6_NWW_pred_2050s$Ynpk_pred_2059<-predict(brt_WNWC_recyle[[i]],RCP2.6_NWW_2059, n.trees=best_inter_WNWC_recyle[[i]])
  RCP2.6_NWW_pred_2050s$Ynpk_pred_40.59<-predict(brt_WNWC_recyle[[i]],RCP2.6_NWW_40.59, n.trees=best_inter_WNWC_recyle[[i]])
  
  RCP2.6_NWW_pred_2050s$average<- apply(RCP2.6_NWW_pred_2050s[,7:26], 1, mean)
  RCP2.6_NWW_pred_2050s_recyle[[i]] <- RCP2.6_NWW_pred_2050s
}

RCP2.6_NWW_pred_2050s_recyle_mean<-RCP2.6_NWW_pred_2050s_recyle[[1]]
RCP2.6_NWW_pred_2050s_recyle_sd<-RCP2.6_NWW_pred_2050s_recyle[[1]]
for(i in 7:28){
  da=as.matrix(as.data.frame(sapply(1:50,function(x){RCP2.6_NWW_pred_2050s_recyle[[x]][i]})))
  RCP2.6_NWW_pred_2050s_recyle_mean[,i]=rowMeans(da)
  RCP2.6_NWW_pred_2050s_recyle_sd[,i]=apply(da,1,sd)
}

###RCP2.6 during 2080-2099

RCP2.6_NWW_2100s<-subset(RCP2.6_2100s, Crop == "northwest wheat")
RCP2.6_NWW_2080 <- RCP2.6_NWW_2100s %>%
  select(Soil.quality, Tave_2080, Tmax_2080, Tmin_2080, GDD.0_2080, 
         PRE_2080, RAD_2080,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NWW_2080)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NWW_2081 <- RCP2.6_NWW_2100s %>%
  select(Soil.quality, Tave_2081, Tmax_2081, Tmin_2081, GDD.0_2081, 
         PRE_2081, RAD_2081, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NWW_2081)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NWW_2082 <- RCP2.6_NWW_2100s %>%
  select(Soil.quality, Tave_2082, Tmax_2082, Tmin_2082, GDD.0_2082, 
         PRE_2082, RAD_2082, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NWW_2082)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NWW_2083 <- RCP2.6_NWW_2100s %>%
  select(Soil.quality, Tave_2083, Tmax_2083, Tmin_2083, GDD.0_2083, 
         PRE_2083, RAD_2083, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NWW_2083)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NWW_2084 <- RCP2.6_NWW_2100s %>%
  select(Soil.quality, Tave_2084, Tmax_2084, Tmin_2084, GDD.0_2084, 
         PRE_2084, RAD_2084, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NWW_2084)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NWW_2085 <- RCP2.6_NWW_2100s %>%
  select(Soil.quality, Tave_2085, Tmax_2085, Tmin_2085, GDD.0_2085, 
         PRE_2085, RAD_2085,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NWW_2085)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NWW_2086 <- RCP2.6_NWW_2100s %>%
  select(Soil.quality, Tave_2086, Tmax_2086, Tmin_2086, GDD.0_2086, 
         PRE_2086, RAD_2086, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NWW_2086)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NWW_2087 <- RCP2.6_NWW_2100s %>%
  select(Soil.quality, Tave_2087, Tmax_2087, Tmin_2087, GDD.0_2087, 
         PRE_2087, RAD_2087, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NWW_2087)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NWW_2088 <- RCP2.6_NWW_2100s %>%
  select(Soil.quality, Tave_2088, Tmax_2088, Tmin_2088, GDD.0_2088, 
         PRE_2088, RAD_2088, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NWW_2088)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NWW_2089 <- RCP2.6_NWW_2100s %>%
  select(Soil.quality, Tave_2089, Tmax_2089, Tmin_2089, GDD.0_2089, 
         PRE_2089, RAD_2089, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NWW_2089)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NWW_2090 <- RCP2.6_NWW_2100s %>%
  select(Soil.quality, Tave_2090, Tmax_2090, Tmin_2090, GDD.0_2090, 
         PRE_2090, RAD_2090,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NWW_2090)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NWW_2091 <- RCP2.6_NWW_2100s %>%
  select(Soil.quality, Tave_2091, Tmax_2091, Tmin_2091, GDD.0_2091, 
         PRE_2091, RAD_2091, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NWW_2091)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NWW_2092 <- RCP2.6_NWW_2100s %>%
  select(Soil.quality, Tave_2092, Tmax_2092, Tmin_2092, GDD.0_2092, 
         PRE_2092, RAD_2092, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NWW_2092)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NWW_2093 <- RCP2.6_NWW_2100s %>%
  select(Soil.quality, Tave_2093, Tmax_2093, Tmin_2093, GDD.0_2093, 
         PRE_2093, RAD_2093,Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NWW_2093)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NWW_2094 <- RCP2.6_NWW_2100s %>%
  select(Soil.quality, Tave_2094, Tmax_2094, Tmin_2094, GDD.0_2094, 
         PRE_2094, RAD_2094,Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NWW_2094)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NWW_2095 <- RCP2.6_NWW_2100s %>%
  select(Soil.quality, Tave_2095, Tmax_2095, Tmin_2095, GDD.0_2095, 
         PRE_2095, RAD_2095, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NWW_2095)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NWW_2096 <- RCP2.6_NWW_2100s %>%
  select(Soil.quality, Tave_2096, Tmax_2096, Tmin_2096, GDD.0_2096, 
         PRE_2096, RAD_2096, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NWW_2096)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NWW_2097 <- RCP2.6_NWW_2100s %>%
  select(Soil.quality, Tave_2097, Tmax_2097, Tmin_2097, GDD.0_2097, 
         PRE_2097, RAD_2097,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NWW_2097)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NWW_2098 <- RCP2.6_NWW_2100s %>%
  select(Soil.quality, Tave_2098, Tmax_2098, Tmin_2098, GDD.0_2098, 
         PRE_2098, RAD_2098,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NWW_2098)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NWW_2099 <- RCP2.6_NWW_2100s %>%
  select(Soil.quality, Tave_2099, Tmax_2099, Tmin_2099, GDD.0_2099, 
         PRE_2099, RAD_2099, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NWW_2099)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NWW_80.99 <- RCP2.6_NWW_2100s %>%
  select(Soil.quality, Tave_2080.2099, Tmax_2080.2099, Tmin_2080.2099, GDD.0_2080.2099, 
         PRE_2080.2099, RAD_2080.2099, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NWW_80.99)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                            "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                            "N", "P2O5", "K2O", "Yck", "Ynpk")


###predict yield by GBRT model

RCP2.6_NWW_pred_2100s_recyle<-NULL

RCP2.6_NWW_pred_2100s <- RCP2.6_NWW_2100s %>%
  select(Soil.quality, Crop, NO, Year, Province, 
         NO.Station)

for (i in 1:50) {
  RCP2.6_NWW_pred_2100s$Ynpk_pred_2080<-predict(brt_WNWC_recyle[[i]],RCP2.6_NWW_2080, n.trees=best_inter_WNWC_recyle[[i]])
  RCP2.6_NWW_pred_2100s$Ynpk_pred_2081<-predict(brt_WNWC_recyle[[i]],RCP2.6_NWW_2081, n.trees=best_inter_WNWC_recyle[[i]])
  RCP2.6_NWW_pred_2100s$Ynpk_pred_2082<-predict(brt_WNWC_recyle[[i]],RCP2.6_NWW_2082, n.trees=best_inter_WNWC_recyle[[i]])
  RCP2.6_NWW_pred_2100s$Ynpk_pred_2083<-predict(brt_WNWC_recyle[[i]],RCP2.6_NWW_2083, n.trees=best_inter_WNWC_recyle[[i]])
  RCP2.6_NWW_pred_2100s$Ynpk_pred_2084<-predict(brt_WNWC_recyle[[i]],RCP2.6_NWW_2084, n.trees=best_inter_WNWC_recyle[[i]])
  RCP2.6_NWW_pred_2100s$Ynpk_pred_2085<-predict(brt_WNWC_recyle[[i]],RCP2.6_NWW_2085, n.trees=best_inter_WNWC_recyle[[i]])
  RCP2.6_NWW_pred_2100s$Ynpk_pred_2086<-predict(brt_WNWC_recyle[[i]],RCP2.6_NWW_2086, n.trees=best_inter_WNWC_recyle[[i]])
  RCP2.6_NWW_pred_2100s$Ynpk_pred_2087<-predict(brt_WNWC_recyle[[i]],RCP2.6_NWW_2087, n.trees=best_inter_WNWC_recyle[[i]])
  RCP2.6_NWW_pred_2100s$Ynpk_pred_2088<-predict(brt_WNWC_recyle[[i]],RCP2.6_NWW_2088, n.trees=best_inter_WNWC_recyle[[i]])
  RCP2.6_NWW_pred_2100s$Ynpk_pred_2089<-predict(brt_WNWC_recyle[[i]],RCP2.6_NWW_2089, n.trees=best_inter_WNWC_recyle[[i]])
  RCP2.6_NWW_pred_2100s$Ynpk_pred_2090<-predict(brt_WNWC_recyle[[i]],RCP2.6_NWW_2090, n.trees=best_inter_WNWC_recyle[[i]])
  RCP2.6_NWW_pred_2100s$Ynpk_pred_2091<-predict(brt_WNWC_recyle[[i]],RCP2.6_NWW_2091, n.trees=best_inter_WNWC_recyle[[i]])
  RCP2.6_NWW_pred_2100s$Ynpk_pred_2092<-predict(brt_WNWC_recyle[[i]],RCP2.6_NWW_2092, n.trees=best_inter_WNWC_recyle[[i]])
  RCP2.6_NWW_pred_2100s$Ynpk_pred_2093<-predict(brt_WNWC_recyle[[i]],RCP2.6_NWW_2093, n.trees=best_inter_WNWC_recyle[[i]])
  RCP2.6_NWW_pred_2100s$Ynpk_pred_2094<-predict(brt_WNWC_recyle[[i]],RCP2.6_NWW_2094, n.trees=best_inter_WNWC_recyle[[i]])
  RCP2.6_NWW_pred_2100s$Ynpk_pred_2095<-predict(brt_WNWC_recyle[[i]],RCP2.6_NWW_2095, n.trees=best_inter_WNWC_recyle[[i]])
  RCP2.6_NWW_pred_2100s$Ynpk_pred_2096<-predict(brt_WNWC_recyle[[i]],RCP2.6_NWW_2096, n.trees=best_inter_WNWC_recyle[[i]])
  RCP2.6_NWW_pred_2100s$Ynpk_pred_2097<-predict(brt_WNWC_recyle[[i]],RCP2.6_NWW_2097, n.trees=best_inter_WNWC_recyle[[i]])
  RCP2.6_NWW_pred_2100s$Ynpk_pred_2098<-predict(brt_WNWC_recyle[[i]],RCP2.6_NWW_2098, n.trees=best_inter_WNWC_recyle[[i]])
  RCP2.6_NWW_pred_2100s$Ynpk_pred_2099<-predict(brt_WNWC_recyle[[i]],RCP2.6_NWW_2099, n.trees=best_inter_WNWC_recyle[[i]])
  RCP2.6_NWW_pred_2100s$Ynpk_pred_80.99<-predict(brt_WNWC_recyle[[i]],RCP2.6_NWW_80.99, n.trees=best_inter_WNWC_recyle[[i]])
  
  RCP2.6_NWW_pred_2100s$average<- apply(RCP2.6_NWW_pred_2100s[,7:26], 1, mean)
  RCP2.6_NWW_pred_2100s_recyle[[i]] <- RCP2.6_NWW_pred_2100s
}

RCP2.6_NWW_pred_2100s_recyle_mean<-RCP2.6_NWW_pred_2100s_recyle[[1]]
RCP2.6_NWW_pred_2100s_recyle_sd<-RCP2.6_NWW_pred_2100s_recyle[[1]]
for(i in 7:28){
  da=as.matrix(as.data.frame(sapply(1:50,function(x){RCP2.6_NWW_pred_2100s_recyle[[x]][i]})))
  RCP2.6_NWW_pred_2100s_recyle_mean[,i]=rowMeans(da)
  RCP2.6_NWW_pred_2100s_recyle_sd[,i]=apply(da,1,sd)
}


###RCP8.5 during 2040-2099

RCP8.5_NWW_2050s<-subset(RCP8.5_2050s, Crop == "northwest wheat")
RCP8.5_NWW_2040 <- RCP8.5_NWW_2050s %>%
  select(Soil.quality, Tave_2040, Tmax_2040, Tmin_2040, GDD.0_2040, 
         PRE_2040, RAD_2040, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NWW_2040)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NWW_2041 <- RCP8.5_NWW_2050s %>%
  select(Soil.quality, Tave_2041, Tmax_2041, Tmin_2041, GDD.0_2041, 
         PRE_2041, RAD_2041, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NWW_2041)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NWW_2042 <- RCP8.5_NWW_2050s %>%
  select(Soil.quality, Tave_2042, Tmax_2042, Tmin_2042, GDD.0_2042, 
         PRE_2042, RAD_2042, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NWW_2042)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NWW_2043 <- RCP8.5_NWW_2050s %>%
  select(Soil.quality, Tave_2043, Tmax_2043, Tmin_2043, GDD.0_2043, 
         PRE_2043, RAD_2043, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NWW_2043)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NWW_2044 <- RCP8.5_NWW_2050s %>%
  select(Soil.quality, Tave_2044, Tmax_2044, Tmin_2044, GDD.0_2044, 
         PRE_2044, RAD_2044, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NWW_2044)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NWW_2045 <- RCP8.5_NWW_2050s %>%
  select(Soil.quality, Tave_2045, Tmax_2045, Tmin_2045, GDD.0_2045, 
         PRE_2045, RAD_2045, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NWW_2045)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NWW_2046 <- RCP8.5_NWW_2050s %>%
  select(Soil.quality, Tave_2046, Tmax_2046, Tmin_2046, GDD.0_2046, 
         PRE_2046, RAD_2046,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NWW_2046)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NWW_2047 <- RCP8.5_NWW_2050s %>%
  select(Soil.quality, Tave_2047, Tmax_2047, Tmin_2047, GDD.0_2047, 
         PRE_2047, RAD_2047, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NWW_2047)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NWW_2048 <- RCP8.5_NWW_2050s %>%
  select(Soil.quality, Tave_2048, Tmax_2048, Tmin_2048, GDD.0_2048, 
         PRE_2048, RAD_2048, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NWW_2048)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NWW_2049 <- RCP8.5_NWW_2050s %>%
  select(Soil.quality, Tave_2049, Tmax_2049, Tmin_2049, GDD.0_2049, 
         PRE_2049, RAD_2049, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NWW_2049)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NWW_2050 <- RCP8.5_NWW_2050s %>%
  select(Soil.quality, Tave_2050, Tmax_2050, Tmin_2050, GDD.0_2050, 
         PRE_2050, RAD_2050, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NWW_2050)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NWW_2051 <- RCP8.5_NWW_2050s %>%
  select(Soil.quality, Tave_2051, Tmax_2051, Tmin_2051, GDD.0_2051, 
         PRE_2051, RAD_2051,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NWW_2051)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NWW_2052 <- RCP8.5_NWW_2050s %>%
  select(Soil.quality, Tave_2052, Tmax_2052, Tmin_2052, GDD.0_2052, 
         PRE_2052, RAD_2052, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NWW_2052)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NWW_2053 <- RCP8.5_NWW_2050s %>%
  select(Soil.quality, Tave_2053, Tmax_2053, Tmin_2053, GDD.0_2053, 
         PRE_2053, RAD_2053, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NWW_2053)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NWW_2054 <- RCP8.5_NWW_2050s %>%
  select(Soil.quality, Tave_2054, Tmax_2054, Tmin_2054, GDD.0_2054, 
         PRE_2054, RAD_2054,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NWW_2054)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NWW_2055 <- RCP8.5_NWW_2050s %>%
  select(Soil.quality, Tave_2055, Tmax_2055, Tmin_2055, GDD.0_2055, 
         PRE_2055, RAD_2055, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NWW_2055)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NWW_2056 <- RCP8.5_NWW_2050s %>%
  select(Soil.quality, Tave_2056, Tmax_2056, Tmin_2056, GDD.0_2056, 
         PRE_2056, RAD_2056, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NWW_2056)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NWW_2057 <- RCP8.5_NWW_2050s %>%
  select(Soil.quality, Tave_2057, Tmax_2057, Tmin_2057, GDD.0_2057, 
         PRE_2057, RAD_2057, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NWW_2057)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NWW_2058 <- RCP8.5_NWW_2050s %>%
  select(Soil.quality, Tave_2058, Tmax_2058, Tmin_2058, GDD.0_2058, 
         PRE_2058, RAD_2058, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NWW_2058)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NWW_2059 <- RCP8.5_NWW_2050s %>%
  select(Soil.quality, Tave_2059, Tmax_2059, Tmin_2059, GDD.0_2059, 
         PRE_2059, RAD_2059, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NWW_2059)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NWW_40.59 <- RCP8.5_NWW_2050s %>%
  select(Soil.quality, Tave_2040.2059, Tmax_2040.2059, Tmin_2040.2059, GDD.0_2040.2059, 
         PRE_2040.2059, RAD_2040.2059,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NWW_40.59)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                            "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                            "N", "P2O5", "K2O", "Yck", "Ynpk")

###predict yield by GBRT model

RCP8.5_NWW_pred_2050s_recyle<-NULL

RCP8.5_NWW_pred_2050s <- RCP8.5_NWW_2050s %>%
  select(Soil.quality, Crop, NO, Year, Province, 
         NO.Station)

for (i in 1:50) {
  RCP8.5_NWW_pred_2050s$Ynpk_pred_2040<-predict(brt_WNWC_recyle[[i]],RCP8.5_NWW_2040, n.trees=best_inter_WNWC_recyle[[i]])
  RCP8.5_NWW_pred_2050s$Ynpk_pred_2041<-predict(brt_WNWC_recyle[[i]],RCP8.5_NWW_2041, n.trees=best_inter_WNWC_recyle[[i]])
  RCP8.5_NWW_pred_2050s$Ynpk_pred_2042<-predict(brt_WNWC_recyle[[i]],RCP8.5_NWW_2042, n.trees=best_inter_WNWC_recyle[[i]])
  RCP8.5_NWW_pred_2050s$Ynpk_pred_2043<-predict(brt_WNWC_recyle[[i]],RCP8.5_NWW_2043, n.trees=best_inter_WNWC_recyle[[i]])
  RCP8.5_NWW_pred_2050s$Ynpk_pred_2044<-predict(brt_WNWC_recyle[[i]],RCP8.5_NWW_2044, n.trees=best_inter_WNWC_recyle[[i]])
  RCP8.5_NWW_pred_2050s$Ynpk_pred_2045<-predict(brt_WNWC_recyle[[i]],RCP8.5_NWW_2045, n.trees=best_inter_WNWC_recyle[[i]])
  RCP8.5_NWW_pred_2050s$Ynpk_pred_2046<-predict(brt_WNWC_recyle[[i]],RCP8.5_NWW_2046, n.trees=best_inter_WNWC_recyle[[i]])
  RCP8.5_NWW_pred_2050s$Ynpk_pred_2047<-predict(brt_WNWC_recyle[[i]],RCP8.5_NWW_2047, n.trees=best_inter_WNWC_recyle[[i]])
  RCP8.5_NWW_pred_2050s$Ynpk_pred_2048<-predict(brt_WNWC_recyle[[i]],RCP8.5_NWW_2048, n.trees=best_inter_WNWC_recyle[[i]])
  RCP8.5_NWW_pred_2050s$Ynpk_pred_2049<-predict(brt_WNWC_recyle[[i]],RCP8.5_NWW_2049, n.trees=best_inter_WNWC_recyle[[i]])
  RCP8.5_NWW_pred_2050s$Ynpk_pred_2050<-predict(brt_WNWC_recyle[[i]],RCP8.5_NWW_2050, n.trees=best_inter_WNWC_recyle[[i]])
  RCP8.5_NWW_pred_2050s$Ynpk_pred_2051<-predict(brt_WNWC_recyle[[i]],RCP8.5_NWW_2051, n.trees=best_inter_WNWC_recyle[[i]])
  RCP8.5_NWW_pred_2050s$Ynpk_pred_2052<-predict(brt_WNWC_recyle[[i]],RCP8.5_NWW_2052, n.trees=best_inter_WNWC_recyle[[i]])
  RCP8.5_NWW_pred_2050s$Ynpk_pred_2053<-predict(brt_WNWC_recyle[[i]],RCP8.5_NWW_2053, n.trees=best_inter_WNWC_recyle[[i]])
  RCP8.5_NWW_pred_2050s$Ynpk_pred_2054<-predict(brt_WNWC_recyle[[i]],RCP8.5_NWW_2054, n.trees=best_inter_WNWC_recyle[[i]])
  RCP8.5_NWW_pred_2050s$Ynpk_pred_2055<-predict(brt_WNWC_recyle[[i]],RCP8.5_NWW_2055, n.trees=best_inter_WNWC_recyle[[i]])
  RCP8.5_NWW_pred_2050s$Ynpk_pred_2056<-predict(brt_WNWC_recyle[[i]],RCP8.5_NWW_2056, n.trees=best_inter_WNWC_recyle[[i]])
  RCP8.5_NWW_pred_2050s$Ynpk_pred_2057<-predict(brt_WNWC_recyle[[i]],RCP8.5_NWW_2057, n.trees=best_inter_WNWC_recyle[[i]])
  RCP8.5_NWW_pred_2050s$Ynpk_pred_2058<-predict(brt_WNWC_recyle[[i]],RCP8.5_NWW_2058, n.trees=best_inter_WNWC_recyle[[i]])
  RCP8.5_NWW_pred_2050s$Ynpk_pred_2059<-predict(brt_WNWC_recyle[[i]],RCP8.5_NWW_2059, n.trees=best_inter_WNWC_recyle[[i]])
  RCP8.5_NWW_pred_2050s$Ynpk_pred_40.59<-predict(brt_WNWC_recyle[[i]],RCP8.5_NWW_40.59, n.trees=best_inter_WNWC_recyle[[i]])
  
  RCP8.5_NWW_pred_2050s$average<- apply(RCP8.5_NWW_pred_2050s[,7:26], 1, mean)
  RCP8.5_NWW_pred_2050s_recyle[[i]] <- RCP8.5_NWW_pred_2050s
}

RCP8.5_NWW_pred_2050s_recyle_mean<-RCP8.5_NWW_pred_2050s_recyle[[1]]
RCP8.5_NWW_pred_2050s_recyle_sd<-RCP8.5_NWW_pred_2050s_recyle[[1]]
for(i in 7:28){
  da=as.matrix(as.data.frame(sapply(1:50,function(x){RCP8.5_NWW_pred_2050s_recyle[[x]][i]})))
  RCP8.5_NWW_pred_2050s_recyle_mean[,i]=rowMeans(da)
  RCP8.5_NWW_pred_2050s_recyle_sd[,i]=apply(da,1,sd)
}

###RCP8.5 during 2080-2099

RCP8.5_NWW_2100s<-subset(RCP8.5_2100s, Crop == "northwest wheat")
RCP8.5_NWW_2080 <- RCP8.5_NWW_2100s %>%
  select(Soil.quality, Tave_2080, Tmax_2080, Tmin_2080, GDD.0_2080, 
         PRE_2080, RAD_2080,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NWW_2080)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NWW_2081 <- RCP8.5_NWW_2100s %>%
  select(Soil.quality, Tave_2081, Tmax_2081, Tmin_2081, GDD.0_2081, 
         PRE_2081, RAD_2081, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NWW_2081)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NWW_2082 <- RCP8.5_NWW_2100s %>%
  select(Soil.quality, Tave_2082, Tmax_2082, Tmin_2082, GDD.0_2082, 
         PRE_2082, RAD_2082, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NWW_2082)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NWW_2083 <- RCP8.5_NWW_2100s %>%
  select(Soil.quality, Tave_2083, Tmax_2083, Tmin_2083, GDD.0_2083, 
         PRE_2083, RAD_2083, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NWW_2083)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NWW_2084 <- RCP8.5_NWW_2100s %>%
  select(Soil.quality, Tave_2084, Tmax_2084, Tmin_2084, GDD.0_2084, 
         PRE_2084, RAD_2084, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NWW_2084)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NWW_2085 <- RCP8.5_NWW_2100s %>%
  select(Soil.quality, Tave_2085, Tmax_2085, Tmin_2085, GDD.0_2085, 
         PRE_2085, RAD_2085, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NWW_2085)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NWW_2086 <- RCP8.5_NWW_2100s %>%
  select(Soil.quality, Tave_2086, Tmax_2086, Tmin_2086, GDD.0_2086, 
         PRE_2086, RAD_2086,Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NWW_2086)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NWW_2087 <- RCP8.5_NWW_2100s %>%
  select(Soil.quality, Tave_2087, Tmax_2087, Tmin_2087, GDD.0_2087, 
         PRE_2087, RAD_2087, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NWW_2087)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NWW_2088 <- RCP8.5_NWW_2100s %>%
  select(Soil.quality, Tave_2088, Tmax_2088, Tmin_2088, GDD.0_2088, 
         PRE_2088, RAD_2088, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NWW_2088)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NWW_2089 <- RCP8.5_NWW_2100s %>%
  select(Soil.quality, Tave_2089, Tmax_2089, Tmin_2089, GDD.0_2089, 
         PRE_2089, RAD_2089, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NWW_2089)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NWW_2090 <- RCP8.5_NWW_2100s %>%
  select(Soil.quality, Tave_2090, Tmax_2090, Tmin_2090, GDD.0_2090, 
         PRE_2090, RAD_2090, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NWW_2090)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NWW_2091 <- RCP8.5_NWW_2100s %>%
  select(Soil.quality, Tave_2091, Tmax_2091, Tmin_2091, GDD.0_2091, 
         PRE_2091, RAD_2091,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NWW_2091)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NWW_2092 <- RCP8.5_NWW_2100s %>%
  select(Soil.quality, Tave_2092, Tmax_2092, Tmin_2092, GDD.0_2092, 
         PRE_2092, RAD_2092, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NWW_2092)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NWW_2093 <- RCP8.5_NWW_2100s %>%
  select(Soil.quality, Tave_2093, Tmax_2093, Tmin_2093, GDD.0_2093, 
         PRE_2093, RAD_2093, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NWW_2093)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NWW_2094 <- RCP8.5_NWW_2100s %>%
  select(Soil.quality, Tave_2094, Tmax_2094, Tmin_2094, GDD.0_2094, 
         PRE_2094, RAD_2094, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NWW_2094)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NWW_2095 <- RCP8.5_NWW_2100s %>%
  select(Soil.quality, Tave_2095, Tmax_2095, Tmin_2095, GDD.0_2095, 
         PRE_2095, RAD_2095, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NWW_2095)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NWW_2096 <- RCP8.5_NWW_2100s %>%
  select(Soil.quality, Tave_2096, Tmax_2096, Tmin_2096, GDD.0_2096, 
         PRE_2096, RAD_2096,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NWW_2096)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NWW_2097 <- RCP8.5_NWW_2100s %>%
  select(Soil.quality, Tave_2097, Tmax_2097, Tmin_2097, GDD.0_2097, 
         PRE_2097, RAD_2097, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NWW_2097)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NWW_2098 <- RCP8.5_NWW_2100s %>%
  select(Soil.quality, Tave_2098, Tmax_2098, Tmin_2098, GDD.0_2098, 
         PRE_2098, RAD_2098, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NWW_2098)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NWW_2099 <- RCP8.5_NWW_2100s %>%
  select(Soil.quality, Tave_2099, Tmax_2099, Tmin_2099, GDD.0_2099, 
         PRE_2099, RAD_2099, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NWW_2099)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NWW_80.99 <- RCP8.5_NWW_2100s %>%
  select(Soil.quality, Tave_2080.2099, Tmax_2080.2099, Tmin_2080.2099, GDD.0_2080.2099, 
         PRE_2080.2099, RAD_2080.2099, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NWW_80.99)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.0", "PRE", "RAD","Cultivar",
                            "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                            "N", "P2O5", "K2O", "Yck", "Ynpk")


###predict yield by GBRT model
RCP8.5_NWW_pred_2100s_recyle<-NULL

RCP8.5_NWW_pred_2100s <- RCP8.5_NWW_2100s %>%
  select(Soil.quality, Crop, NO, Year, Province, 
         NO.Station)

for (i in 1:50) {
  RCP8.5_NWW_pred_2100s$Ynpk_pred_2080<-predict(brt_WNWC_recyle[[i]],RCP8.5_NWW_2080, n.trees=best_inter_WNWC_recyle[[i]])
  RCP8.5_NWW_pred_2100s$Ynpk_pred_2081<-predict(brt_WNWC_recyle[[i]],RCP8.5_NWW_2081, n.trees=best_inter_WNWC_recyle[[i]])
  RCP8.5_NWW_pred_2100s$Ynpk_pred_2082<-predict(brt_WNWC_recyle[[i]],RCP8.5_NWW_2082, n.trees=best_inter_WNWC_recyle[[i]])
  RCP8.5_NWW_pred_2100s$Ynpk_pred_2083<-predict(brt_WNWC_recyle[[i]],RCP8.5_NWW_2083, n.trees=best_inter_WNWC_recyle[[i]])
  RCP8.5_NWW_pred_2100s$Ynpk_pred_2084<-predict(brt_WNWC_recyle[[i]],RCP8.5_NWW_2084, n.trees=best_inter_WNWC_recyle[[i]])
  RCP8.5_NWW_pred_2100s$Ynpk_pred_2085<-predict(brt_WNWC_recyle[[i]],RCP8.5_NWW_2085, n.trees=best_inter_WNWC_recyle[[i]])
  RCP8.5_NWW_pred_2100s$Ynpk_pred_2086<-predict(brt_WNWC_recyle[[i]],RCP8.5_NWW_2086, n.trees=best_inter_WNWC_recyle[[i]])
  RCP8.5_NWW_pred_2100s$Ynpk_pred_2087<-predict(brt_WNWC_recyle[[i]],RCP8.5_NWW_2087, n.trees=best_inter_WNWC_recyle[[i]])
  RCP8.5_NWW_pred_2100s$Ynpk_pred_2088<-predict(brt_WNWC_recyle[[i]],RCP8.5_NWW_2088, n.trees=best_inter_WNWC_recyle[[i]])
  RCP8.5_NWW_pred_2100s$Ynpk_pred_2089<-predict(brt_WNWC_recyle[[i]],RCP8.5_NWW_2089, n.trees=best_inter_WNWC_recyle[[i]])
  RCP8.5_NWW_pred_2100s$Ynpk_pred_2090<-predict(brt_WNWC_recyle[[i]],RCP8.5_NWW_2090, n.trees=best_inter_WNWC_recyle[[i]])
  RCP8.5_NWW_pred_2100s$Ynpk_pred_2091<-predict(brt_WNWC_recyle[[i]],RCP8.5_NWW_2091, n.trees=best_inter_WNWC_recyle[[i]])
  RCP8.5_NWW_pred_2100s$Ynpk_pred_2092<-predict(brt_WNWC_recyle[[i]],RCP8.5_NWW_2092, n.trees=best_inter_WNWC_recyle[[i]])
  RCP8.5_NWW_pred_2100s$Ynpk_pred_2093<-predict(brt_WNWC_recyle[[i]],RCP8.5_NWW_2093, n.trees=best_inter_WNWC_recyle[[i]])
  RCP8.5_NWW_pred_2100s$Ynpk_pred_2094<-predict(brt_WNWC_recyle[[i]],RCP8.5_NWW_2094, n.trees=best_inter_WNWC_recyle[[i]])
  RCP8.5_NWW_pred_2100s$Ynpk_pred_2095<-predict(brt_WNWC_recyle[[i]],RCP8.5_NWW_2095, n.trees=best_inter_WNWC_recyle[[i]])
  RCP8.5_NWW_pred_2100s$Ynpk_pred_2096<-predict(brt_WNWC_recyle[[i]],RCP8.5_NWW_2096, n.trees=best_inter_WNWC_recyle[[i]])
  RCP8.5_NWW_pred_2100s$Ynpk_pred_2097<-predict(brt_WNWC_recyle[[i]],RCP8.5_NWW_2097, n.trees=best_inter_WNWC_recyle[[i]])
  RCP8.5_NWW_pred_2100s$Ynpk_pred_2098<-predict(brt_WNWC_recyle[[i]],RCP8.5_NWW_2098, n.trees=best_inter_WNWC_recyle[[i]])
  RCP8.5_NWW_pred_2100s$Ynpk_pred_2099<-predict(brt_WNWC_recyle[[i]],RCP8.5_NWW_2099, n.trees=best_inter_WNWC_recyle[[i]])
  RCP8.5_NWW_pred_2100s$Ynpk_pred_80.99<-predict(brt_WNWC_recyle[[i]],RCP8.5_NWW_80.99, n.trees=best_inter_WNWC_recyle[[i]])
  
  RCP8.5_NWW_pred_2100s$average<- apply(RCP8.5_NWW_pred_2100s[,7:26], 1, mean)
  RCP8.5_NWW_pred_2100s_recyle[[i]] <- RCP8.5_NWW_pred_2100s
}

RCP8.5_NWW_pred_2100s_recyle_mean<-RCP8.5_NWW_pred_2100s_recyle[[1]]
RCP8.5_NWW_pred_2100s_recyle_sd<-RCP8.5_NWW_pred_2100s_recyle[[1]]
for(i in 7:28){
  da=as.matrix(as.data.frame(sapply(1:50,function(x){RCP8.5_NWW_pred_2100s_recyle[[x]][i]})))
  RCP8.5_NWW_pred_2100s_recyle_mean[,i]=rowMeans(da)
  RCP8.5_NWW_pred_2100s_recyle_sd[,i]=apply(da,1,sd)
}

###summarise predicted yield

sum_RCP2.6_NWW_2050s_year<- cbind(base_NWW_pred_all[,c("Soil.quality","Crop","NO","average")],RCP2.6_NWW_pred_2050s[,7:28])
names(sum_RCP2.6_NWW_2050s_year)[1:4]<- c("Soil quality", "Crop", "No", "Yield_base")
names(sum_RCP2.6_NWW_2050s_year)[26]<- "Yield_RCP"
sum_RCP2.6_NWW_2050s_year$Yield_change <- sum_RCP2.6_NWW_2050s_year$`Yield_RCP` - sum_RCP2.6_NWW_2050s_year$`Yield_base`
sum_RCP2.6_NWW_2050s_year$Yield_change_per <- (sum_RCP2.6_NWW_2050s_year$Yield_change / sum_RCP2.6_NWW_2050s_year$Yield_base)*100
sum_RCP2.6_NWW_2050s_year$RCP <- "RCP2.6"
sum_RCP2.6_NWW_2050s_year$Time <- "2050"

sum_RCP8.5_NWW_2050s_year<- cbind(base_NWW_pred_all[,c("Soil.quality","Crop","NO","average")],RCP8.5_NWW_pred_2050s[,7:28])
names(sum_RCP8.5_NWW_2050s_year)[1:4]<- c("Soil quality", "Crop", "No", "Yield_base")
names(sum_RCP8.5_NWW_2050s_year)[26]<- "Yield_RCP"
sum_RCP8.5_NWW_2050s_year$Yield_change <- sum_RCP8.5_NWW_2050s_year$`Yield_RCP` - sum_RCP8.5_NWW_2050s_year$`Yield_base`
sum_RCP8.5_NWW_2050s_year$Yield_change_per <- (sum_RCP8.5_NWW_2050s_year$Yield_change / sum_RCP8.5_NWW_2050s_year$Yield_base)*100
sum_RCP8.5_NWW_2050s_year$RCP <- "RCP8.5"
sum_RCP8.5_NWW_2050s_year$Time <- "2050"

sum_RCP2.6_NWW_2100s_year<- cbind(base_NWW_pred_all[,c("Soil.quality","Crop","NO","average")],RCP2.6_NWW_pred_2100s[,7:28])
names(sum_RCP2.6_NWW_2100s_year)[1:4]<- c("Soil quality", "Crop", "No", "Yield_base")
names(sum_RCP2.6_NWW_2100s_year)[26]<- "Yield_RCP"
sum_RCP2.6_NWW_2100s_year$Yield_change <- sum_RCP2.6_NWW_2100s_year$`Yield_RCP` - sum_RCP2.6_NWW_2100s_year$`Yield_base`
sum_RCP2.6_NWW_2100s_year$Yield_change_per <- (sum_RCP2.6_NWW_2100s_year$Yield_change / sum_RCP2.6_NWW_2100s_year$Yield_base)*100
sum_RCP2.6_NWW_2100s_year$RCP <- "RCP2.6"
sum_RCP2.6_NWW_2100s_year$Time <- "2100"

sum_RCP8.5_NWW_2100s_year<- cbind(base_NWW_pred_all[,c("Soil.quality","Crop","NO","average")],RCP8.5_NWW_pred_2100s[,7:28])
names(sum_RCP8.5_NWW_2100s_year)[1:4]<- c("Soil quality", "Crop", "No", "Yield_base")
names(sum_RCP8.5_NWW_2100s_year)[26]<- "Yield_RCP"
sum_RCP8.5_NWW_2100s_year$Yield_change <- sum_RCP8.5_NWW_2100s_year$`Yield_RCP` - sum_RCP8.5_NWW_2100s_year$`Yield_base`
sum_RCP8.5_NWW_2100s_year$Yield_change_per <- (sum_RCP8.5_NWW_2100s_year$Yield_change / sum_RCP8.5_NWW_2100s_year$Yield_base)*100
sum_RCP8.5_NWW_2100s_year$RCP <- "RCP8.5"
sum_RCP8.5_NWW_2100s_year$Time <- "2100"

sum_RCP_NWW_2050s_year <- rbind(sum_RCP2.6_NWW_2050s_year,sum_RCP8.5_NWW_2050s_year)
sum_RCP_NWW_2100s_year<-rbind(sum_RCP2.6_NWW_2100s_year,sum_RCP8.5_NWW_2100s_year)

write.csv(sum_RCP_NWW_2050s_year,"./results/sum_RCP_NWW_2050s_year.csv")
write.csv(sum_RCP_NWW_2100s_year,"./results/sum_RCP_NWW_2100s_year.csv")

###comparison of yield change between high and low quality soils 
###T test

t.test(Yield_change~`Soil quality` ,sum_RCP2.6_NWW_2050s_year) #RCP2.6-2050s
t.test(Yield_change~`Soil quality` ,sum_RCP8.5_NWW_2050s_year) #RCP8.5-2050s
t.test(Yield_change~`Soil quality` ,sum_RCP2.6_NWW_2100s_year) #RCP2.6-2100s
t.test(Yield_change~`Soil quality` ,sum_RCP8.5_NWW_2100s_year) #RCP8.5-2100s

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
}

#####Predicted yield under different climate scenarios
###baseline period 1986-2005
base_NEM<-subset(base, Crop == "northeast maize")
base_NEM_1986 <- base_NEM %>%
  select(Soil.quality, Tave_1986, Tmax_1986, Tmin_1986, GDD.10_1986, 
         PRE_1986, RAD_1986, Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NEM_1986)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                         "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                         "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NEM_1987 <- base_NEM %>%
  select(Soil.quality, Tave_1987, Tmax_1987, Tmin_1987, GDD.10_1987, 
         PRE_1987, RAD_1987, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NEM_1987)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                         "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                         "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NEM_1988 <- base_NEM %>%
  select(Soil.quality, Tave_1988, Tmax_1988, Tmin_1988, GDD.10_1988, 
         PRE_1988, RAD_1988, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NEM_1988)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                         "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                         "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NEM_1989 <- base_NEM %>%
  select(Soil.quality, Tave_1989, Tmax_1989, Tmin_1989, GDD.10_1989, 
         PRE_1989, RAD_1989, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NEM_1989)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                         "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                         "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NEM_1990 <- base_NEM %>%
  select(Soil.quality, Tave_1990, Tmax_1990, Tmin_1990, GDD.10_1990, 
         PRE_1990, RAD_1990, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NEM_1990)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                         "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                         "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NEM_1991 <- base_NEM %>%
  select(Soil.quality, Tave_1991, Tmax_1991, Tmin_1991, GDD.10_1991, 
         PRE_1991, RAD_1991, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NEM_1991)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                         "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                         "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NEM_1992 <- base_NEM %>%
  select(Soil.quality, Tave_1992, Tmax_1992, Tmin_1992, GDD.10_1992, 
         PRE_1992, RAD_1992,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NEM_1992)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                         "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                         "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NEM_1993 <- base_NEM %>%
  select(Soil.quality, Tave_1993, Tmax_1993, Tmin_1993, GDD.10_1993, 
         PRE_1993, RAD_1993, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NEM_1993)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                         "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                         "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NEM_1994 <- base_NEM %>%
  select(Soil.quality, Tave_1994, Tmax_1994, Tmin_1994, GDD.10_1994, 
         PRE_1994, RAD_1994, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NEM_1994)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                         "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                         "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NEM_1995 <- base_NEM %>%
  select(Soil.quality, Tave_1995, Tmax_1995, Tmin_1995, GDD.10_1995, 
         PRE_1995, RAD_1995, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NEM_1995)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                         "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                         "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NEM_1996 <- base_NEM %>%
  select(Soil.quality, Tave_1996, Tmax_1996, Tmin_1996, GDD.10_1996, 
         PRE_1996, RAD_1996, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NEM_1996)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                         "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                         "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NEM_1997 <- base_NEM %>%
  select(Soil.quality, Tave_1997, Tmax_1997, Tmin_1997, GDD.10_1997, 
         PRE_1997, RAD_1997, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NEM_1997)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                         "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                         "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NEM_1998 <- base_NEM %>%
  select(Soil.quality, Tave_1998, Tmax_1998, Tmin_1998, GDD.10_1998, 
         PRE_1998, RAD_1998, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NEM_1998)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                         "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                         "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NEM_1999 <- base_NEM %>%
  select(Soil.quality, Tave_1999, Tmax_1999, Tmin_1999, GDD.10_1999, 
         PRE_1999, RAD_1999, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NEM_1999)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                         "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                         "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NEM_2000 <- base_NEM %>%
  select(Soil.quality, Tave_2000, Tmax_2000, Tmin_2000, GDD.10_2000, 
         PRE_2000, RAD_2000, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NEM_2000)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                         "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                         "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NEM_2001 <- base_NEM %>%
  select(Soil.quality, Tave_2001, Tmax_2001, Tmin_2001, GDD.10_2001, 
         PRE_2001, RAD_2001, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NEM_2001)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                         "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                         "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NEM_2002 <- base_NEM %>%
  select(Soil.quality, Tave_2002, Tmax_2002, Tmin_2002, GDD.10_2002, 
         PRE_2002, RAD_2002, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NEM_2002)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                         "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                         "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NEM_2003 <- base_NEM %>%
  select(Soil.quality, Tave_2003, Tmax_2003, Tmin_2003, GDD.10_2003, 
         PRE_2003, RAD_2003, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NEM_2003)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                         "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                         "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NEM_2004 <- base_NEM %>%
  select(Soil.quality, Tave_2004, Tmax_2004, Tmin_2004, GDD.10_2004, 
         PRE_2004, RAD_2004, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NEM_2004)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                         "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                         "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NEM_2005 <- base_NEM %>%
  select(Soil.quality, Tave_2005, Tmax_2005, Tmin_2005, GDD.10_2005, 
         PRE_2005, RAD_2005, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NEM_2005)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                         "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                         "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NEM_86.05 <- base_NEM %>%
  select(Soil.quality, Tave_1986.2005, Tmax_1986.2005, Tmin_1986.2005, GDD.10_1986.2005, 
         PRE_1986.2005, RAD_1986.2005, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NEM_86.05)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

###predict yield by GBRT model

base_NEM_pred_all_recyle<-NULL

base_NEM_pred_all <- base_NEM %>%
  select(Soil.quality, Crop, NO, Year,Province, 
         NO.Station)

for (i in 1:50) {
  base_NEM_pred_all$Ynpk_pred_1986<-predict(brt_MNEC_recyle[[i]],base_NEM_1986, n.trees=best_inter_MNEC_recyle[[i]])
  base_NEM_pred_all$Ynpk_pred_1987<-predict(brt_MNEC_recyle[[i]],base_NEM_1987, n.trees=best_inter_MNEC_recyle[[i]])
  base_NEM_pred_all$Ynpk_pred_1988<-predict(brt_MNEC_recyle[[i]],base_NEM_1988, n.trees=best_inter_MNEC_recyle[[i]])
  base_NEM_pred_all$Ynpk_pred_1989<-predict(brt_MNEC_recyle[[i]],base_NEM_1989, n.trees=best_inter_MNEC_recyle[[i]])
  base_NEM_pred_all$Ynpk_pred_1990<-predict(brt_MNEC_recyle[[i]],base_NEM_1990, n.trees=best_inter_MNEC_recyle[[i]])
  base_NEM_pred_all$Ynpk_pred_1991<-predict(brt_MNEC_recyle[[i]],base_NEM_1991, n.trees=best_inter_MNEC_recyle[[i]])
  base_NEM_pred_all$Ynpk_pred_1992<-predict(brt_MNEC_recyle[[i]],base_NEM_1992, n.trees=best_inter_MNEC_recyle[[i]])
  base_NEM_pred_all$Ynpk_pred_1993<-predict(brt_MNEC_recyle[[i]],base_NEM_1993, n.trees=best_inter_MNEC_recyle[[i]])
  base_NEM_pred_all$Ynpk_pred_1994<-predict(brt_MNEC_recyle[[i]],base_NEM_1994, n.trees=best_inter_MNEC_recyle[[i]])
  base_NEM_pred_all$Ynpk_pred_1995<-predict(brt_MNEC_recyle[[i]],base_NEM_1995, n.trees=best_inter_MNEC_recyle[[i]])
  base_NEM_pred_all$Ynpk_pred_1996<-predict(brt_MNEC_recyle[[i]],base_NEM_1996, n.trees=best_inter_MNEC_recyle[[i]])
  base_NEM_pred_all$Ynpk_pred_1997<-predict(brt_MNEC_recyle[[i]],base_NEM_1997, n.trees=best_inter_MNEC_recyle[[i]])
  base_NEM_pred_all$Ynpk_pred_1998<-predict(brt_MNEC_recyle[[i]],base_NEM_1998, n.trees=best_inter_MNEC_recyle[[i]])
  base_NEM_pred_all$Ynpk_pred_1999<-predict(brt_MNEC_recyle[[i]],base_NEM_1999, n.trees=best_inter_MNEC_recyle[[i]])
  base_NEM_pred_all$Ynpk_pred_2000<-predict(brt_MNEC_recyle[[i]],base_NEM_2000, n.trees=best_inter_MNEC_recyle[[i]])
  base_NEM_pred_all$Ynpk_pred_2001<-predict(brt_MNEC_recyle[[i]],base_NEM_2001, n.trees=best_inter_MNEC_recyle[[i]])
  base_NEM_pred_all$Ynpk_pred_2002<-predict(brt_MNEC_recyle[[i]],base_NEM_2002, n.trees=best_inter_MNEC_recyle[[i]])
  base_NEM_pred_all$Ynpk_pred_2003<-predict(brt_MNEC_recyle[[i]],base_NEM_2003, n.trees=best_inter_MNEC_recyle[[i]])
  base_NEM_pred_all$Ynpk_pred_2004<-predict(brt_MNEC_recyle[[i]],base_NEM_2004, n.trees=best_inter_MNEC_recyle[[i]])
  base_NEM_pred_all$Ynpk_pred_2005<-predict(brt_MNEC_recyle[[i]],base_NEM_2005, n.trees=best_inter_MNEC_recyle[[i]])
  base_NEM_pred_all$Ynpk_pred_86.05<-predict(brt_MNEC_recyle[[i]],base_NEM_86.05, n.trees=best_inter_MNEC_recyle[[i]])
  
  base_NEM_pred_all$average<- apply(base_NEM_pred_all[,7:26], 1, mean)
  base_NEM_pred_all_recyle[[i]] <- base_NEM_pred_all
}

base_NEM_pred_all_recyle_mean<-base_NEM_pred_all_recyle[[1]]
base_NEM_pred_all_recyle_sd<-base_NEM_pred_all_recyle[[1]]
for(i in 7:28){
  da=as.matrix(as.data.frame(sapply(1:50,function(x){base_NEM_pred_all_recyle[[x]][i]})))
  base_NEM_pred_all_recyle_mean[,i]=rowMeans(da)
  base_NEM_pred_all_recyle_sd[,i]=apply(da,1,sd)
}

###RCP2.6 during 2040-2059

RCP2.6_NEM_2050s<-subset(RCP2.6_2050s, Crop == "northeast maize")
RCP2.6_NEM_2040 <- RCP2.6_NEM_2050s %>%
  select(Soil.quality, Tave_2040, Tmax_2040, Tmin_2040, GDD.10_2040, 
         PRE_2040, RAD_2040, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NEM_2040)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NEM_2041 <- RCP2.6_NEM_2050s %>%
  select(Soil.quality, Tave_2041, Tmax_2041, Tmin_2041, GDD.10_2041, 
         PRE_2041, RAD_2041, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NEM_2041)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NEM_2042 <- RCP2.6_NEM_2050s %>%
  select(Soil.quality, Tave_2042, Tmax_2042, Tmin_2042, GDD.10_2042, 
         PRE_2042, RAD_2042, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NEM_2042)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NEM_2043 <- RCP2.6_NEM_2050s %>%
  select(Soil.quality, Tave_2043, Tmax_2043, Tmin_2043, GDD.10_2043, 
         PRE_2043, RAD_2043, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NEM_2043)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NEM_2044 <- RCP2.6_NEM_2050s %>%
  select(Soil.quality, Tave_2044, Tmax_2044, Tmin_2044, GDD.10_2044, 
         PRE_2044, RAD_2044, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NEM_2044)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NEM_2045 <- RCP2.6_NEM_2050s %>%
  select(Soil.quality, Tave_2045, Tmax_2045, Tmin_2045, GDD.10_2045, 
         PRE_2045, RAD_2045, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NEM_2045)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NEM_2046 <- RCP2.6_NEM_2050s %>%
  select(Soil.quality, Tave_2046, Tmax_2046, Tmin_2046, GDD.10_2046, 
         PRE_2046, RAD_2046, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NEM_2046)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NEM_2047 <- RCP2.6_NEM_2050s %>%
  select(Soil.quality, Tave_2047, Tmax_2047, Tmin_2047, GDD.10_2047, 
         PRE_2047, RAD_2047, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NEM_2047)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NEM_2048 <- RCP2.6_NEM_2050s %>%
  select(Soil.quality, Tave_2048, Tmax_2048, Tmin_2048, GDD.10_2048, 
         PRE_2048, RAD_2048, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NEM_2048)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NEM_2049 <- RCP2.6_NEM_2050s %>%
  select(Soil.quality, Tave_2049, Tmax_2049, Tmin_2049, GDD.10_2049, 
         PRE_2049, RAD_2049, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NEM_2049)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NEM_2050 <- RCP2.6_NEM_2050s %>%
  select(Soil.quality, Tave_2050, Tmax_2050, Tmin_2050, GDD.10_2050, 
         PRE_2050, RAD_2050, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NEM_2050)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NEM_2051 <- RCP2.6_NEM_2050s %>%
  select(Soil.quality, Tave_2051, Tmax_2051, Tmin_2051, GDD.10_2051, 
         PRE_2051, RAD_2051, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NEM_2051)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NEM_2052 <- RCP2.6_NEM_2050s %>%
  select(Soil.quality, Tave_2052, Tmax_2052, Tmin_2052, GDD.10_2052, 
         PRE_2052, RAD_2052, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NEM_2052)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NEM_2053 <- RCP2.6_NEM_2050s %>%
  select(Soil.quality, Tave_2053, Tmax_2053, Tmin_2053, GDD.10_2053, 
         PRE_2053, RAD_2053, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NEM_2053)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NEM_2054 <- RCP2.6_NEM_2050s %>%
  select(Soil.quality, Tave_2054, Tmax_2054, Tmin_2054, GDD.10_2054, 
         PRE_2054, RAD_2054, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NEM_2054)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NEM_2055 <- RCP2.6_NEM_2050s %>%
  select(Soil.quality, Tave_2055, Tmax_2055, Tmin_2055, GDD.10_2055, 
         PRE_2055, RAD_2055, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NEM_2055)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NEM_2056 <- RCP2.6_NEM_2050s %>%
  select(Soil.quality, Tave_2056, Tmax_2056, Tmin_2056, GDD.10_2056, 
         PRE_2056, RAD_2056, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NEM_2056)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NEM_2057 <- RCP2.6_NEM_2050s %>%
  select(Soil.quality, Tave_2057, Tmax_2057, Tmin_2057, GDD.10_2057, 
         PRE_2057, RAD_2057, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NEM_2057)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NEM_2058 <- RCP2.6_NEM_2050s %>%
  select(Soil.quality, Tave_2058, Tmax_2058, Tmin_2058, GDD.10_2058, 
         PRE_2058, RAD_2058, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NEM_2058)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NEM_2059 <- RCP2.6_NEM_2050s %>%
  select(Soil.quality, Tave_2059, Tmax_2059, Tmin_2059, GDD.10_2059, 
         PRE_2059, RAD_2059, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NEM_2059)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NEM_40.59 <- RCP2.6_NEM_2050s %>%
  select(Soil.quality, Tave_2040.2059, Tmax_2040.2059, Tmin_2040.2059, GDD.10_2040.2059, 
         PRE_2040.2059, RAD_2040.2059, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NEM_40.59)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                            "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                            "N", "P2O5", "K2O", "Yck", "Ynpk")

###predict yield by GBRT model

RCP2.6_NEM_pred_2050s_recyle<-NULL

RCP2.6_NEM_pred_2050s <- RCP2.6_NEM_2050s %>%
  select(Soil.quality, Crop, NO, Year, Province, 
         NO.Station)

for (i in 1:50) {
  RCP2.6_NEM_pred_2050s$Ynpk_pred_2040<-predict(brt_MNEC_recyle[[i]],RCP2.6_NEM_2040, n.trees=best_inter_MNEC_recyle[[i]])
  RCP2.6_NEM_pred_2050s$Ynpk_pred_2041<-predict(brt_MNEC_recyle[[i]],RCP2.6_NEM_2041, n.trees=best_inter_MNEC_recyle[[i]])
  RCP2.6_NEM_pred_2050s$Ynpk_pred_2042<-predict(brt_MNEC_recyle[[i]],RCP2.6_NEM_2042, n.trees=best_inter_MNEC_recyle[[i]])
  RCP2.6_NEM_pred_2050s$Ynpk_pred_2043<-predict(brt_MNEC_recyle[[i]],RCP2.6_NEM_2043, n.trees=best_inter_MNEC_recyle[[i]])
  RCP2.6_NEM_pred_2050s$Ynpk_pred_2044<-predict(brt_MNEC_recyle[[i]],RCP2.6_NEM_2044, n.trees=best_inter_MNEC_recyle[[i]])
  RCP2.6_NEM_pred_2050s$Ynpk_pred_2045<-predict(brt_MNEC_recyle[[i]],RCP2.6_NEM_2045, n.trees=best_inter_MNEC_recyle[[i]])
  RCP2.6_NEM_pred_2050s$Ynpk_pred_2046<-predict(brt_MNEC_recyle[[i]],RCP2.6_NEM_2046, n.trees=best_inter_MNEC_recyle[[i]])
  RCP2.6_NEM_pred_2050s$Ynpk_pred_2047<-predict(brt_MNEC_recyle[[i]],RCP2.6_NEM_2047, n.trees=best_inter_MNEC_recyle[[i]])
  RCP2.6_NEM_pred_2050s$Ynpk_pred_2048<-predict(brt_MNEC_recyle[[i]],RCP2.6_NEM_2048, n.trees=best_inter_MNEC_recyle[[i]])
  RCP2.6_NEM_pred_2050s$Ynpk_pred_2049<-predict(brt_MNEC_recyle[[i]],RCP2.6_NEM_2049, n.trees=best_inter_MNEC_recyle[[i]])
  RCP2.6_NEM_pred_2050s$Ynpk_pred_2050<-predict(brt_MNEC_recyle[[i]],RCP2.6_NEM_2050, n.trees=best_inter_MNEC_recyle[[i]])
  RCP2.6_NEM_pred_2050s$Ynpk_pred_2051<-predict(brt_MNEC_recyle[[i]],RCP2.6_NEM_2051, n.trees=best_inter_MNEC_recyle[[i]])
  RCP2.6_NEM_pred_2050s$Ynpk_pred_2052<-predict(brt_MNEC_recyle[[i]],RCP2.6_NEM_2052, n.trees=best_inter_MNEC_recyle[[i]])
  RCP2.6_NEM_pred_2050s$Ynpk_pred_2053<-predict(brt_MNEC_recyle[[i]],RCP2.6_NEM_2053, n.trees=best_inter_MNEC_recyle[[i]])
  RCP2.6_NEM_pred_2050s$Ynpk_pred_2054<-predict(brt_MNEC_recyle[[i]],RCP2.6_NEM_2054, n.trees=best_inter_MNEC_recyle[[i]])
  RCP2.6_NEM_pred_2050s$Ynpk_pred_2055<-predict(brt_MNEC_recyle[[i]],RCP2.6_NEM_2055, n.trees=best_inter_MNEC_recyle[[i]])
  RCP2.6_NEM_pred_2050s$Ynpk_pred_2056<-predict(brt_MNEC_recyle[[i]],RCP2.6_NEM_2056, n.trees=best_inter_MNEC_recyle[[i]])
  RCP2.6_NEM_pred_2050s$Ynpk_pred_2057<-predict(brt_MNEC_recyle[[i]],RCP2.6_NEM_2057, n.trees=best_inter_MNEC_recyle[[i]])
  RCP2.6_NEM_pred_2050s$Ynpk_pred_2058<-predict(brt_MNEC_recyle[[i]],RCP2.6_NEM_2058, n.trees=best_inter_MNEC_recyle[[i]])
  RCP2.6_NEM_pred_2050s$Ynpk_pred_2059<-predict(brt_MNEC_recyle[[i]],RCP2.6_NEM_2059, n.trees=best_inter_MNEC_recyle[[i]])
  RCP2.6_NEM_pred_2050s$Ynpk_pred_40.59<-predict(brt_MNEC_recyle[[i]],RCP2.6_NEM_40.59, n.trees=best_inter_MNEC_recyle[[i]])
  
  RCP2.6_NEM_pred_2050s$average<- apply(RCP2.6_NEM_pred_2050s[,7:26], 1, mean)
  RCP2.6_NEM_pred_2050s_recyle[[i]] <- RCP2.6_NEM_pred_2050s
}

RCP2.6_NEM_pred_2050s_recyle_mean<-RCP2.6_NEM_pred_2050s_recyle[[1]]
RCP2.6_NEM_pred_2050s_recyle_sd<-RCP2.6_NEM_pred_2050s_recyle[[1]]
for(i in 7:28){
  da=as.matrix(as.data.frame(sapply(1:50,function(x){RCP2.6_NEM_pred_2050s_recyle[[x]][i]})))
  RCP2.6_NEM_pred_2050s_recyle_mean[,i]=rowMeans(da)
  RCP2.6_NEM_pred_2050s_recyle_sd[,i]=apply(da,1,sd)
}

###RCP2.6 during 2080-2099

RCP2.6_NEM_2100s<-subset(RCP2.6_2100s, Crop == "northeast maize")
RCP2.6_NEM_2080 <- RCP2.6_NEM_2100s %>%
  select(Soil.quality, Tave_2080, Tmax_2080, Tmin_2080, GDD.10_2080, 
         PRE_2080, RAD_2080,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NEM_2080)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NEM_2081 <- RCP2.6_NEM_2100s %>%
  select(Soil.quality, Tave_2081, Tmax_2081, Tmin_2081, GDD.10_2081, 
         PRE_2081, RAD_2081, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NEM_2081)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NEM_2082 <- RCP2.6_NEM_2100s %>%
  select(Soil.quality, Tave_2082, Tmax_2082, Tmin_2082, GDD.10_2082, 
         PRE_2082, RAD_2082, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NEM_2082)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NEM_2083 <- RCP2.6_NEM_2100s %>%
  select(Soil.quality, Tave_2083, Tmax_2083, Tmin_2083, GDD.10_2083, 
         PRE_2083, RAD_2083, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NEM_2083)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NEM_2084 <- RCP2.6_NEM_2100s %>%
  select(Soil.quality, Tave_2084, Tmax_2084, Tmin_2084, GDD.10_2084, 
         PRE_2084, RAD_2084, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NEM_2084)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NEM_2085 <- RCP2.6_NEM_2100s %>%
  select(Soil.quality, Tave_2085, Tmax_2085, Tmin_2085, GDD.10_2085, 
         PRE_2085, RAD_2085,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NEM_2085)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NEM_2086 <- RCP2.6_NEM_2100s %>%
  select(Soil.quality, Tave_2086, Tmax_2086, Tmin_2086, GDD.10_2086, 
         PRE_2086, RAD_2086, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NEM_2086)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NEM_2087 <- RCP2.6_NEM_2100s %>%
  select(Soil.quality, Tave_2087, Tmax_2087, Tmin_2087, GDD.10_2087, 
         PRE_2087, RAD_2087, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NEM_2087)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NEM_2088 <- RCP2.6_NEM_2100s %>%
  select(Soil.quality, Tave_2088, Tmax_2088, Tmin_2088, GDD.10_2088, 
         PRE_2088, RAD_2088, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NEM_2088)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NEM_2089 <- RCP2.6_NEM_2100s %>%
  select(Soil.quality, Tave_2089, Tmax_2089, Tmin_2089, GDD.10_2089, 
         PRE_2089, RAD_2089, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NEM_2089)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NEM_2090 <- RCP2.6_NEM_2100s %>%
  select(Soil.quality, Tave_2090, Tmax_2090, Tmin_2090, GDD.10_2090, 
         PRE_2090, RAD_2090,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NEM_2090)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NEM_2091 <- RCP2.6_NEM_2100s %>%
  select(Soil.quality, Tave_2091, Tmax_2091, Tmin_2091, GDD.10_2091, 
         PRE_2091, RAD_2091, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NEM_2091)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NEM_2092 <- RCP2.6_NEM_2100s %>%
  select(Soil.quality, Tave_2092, Tmax_2092, Tmin_2092, GDD.10_2092, 
         PRE_2092, RAD_2092, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NEM_2092)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NEM_2093 <- RCP2.6_NEM_2100s %>%
  select(Soil.quality, Tave_2093, Tmax_2093, Tmin_2093, GDD.10_2093, 
         PRE_2093, RAD_2093,Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NEM_2093)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NEM_2094 <- RCP2.6_NEM_2100s %>%
  select(Soil.quality, Tave_2094, Tmax_2094, Tmin_2094, GDD.10_2094, 
         PRE_2094, RAD_2094,Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NEM_2094)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NEM_2095 <- RCP2.6_NEM_2100s %>%
  select(Soil.quality, Tave_2095, Tmax_2095, Tmin_2095, GDD.10_2095, 
         PRE_2095, RAD_2095, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NEM_2095)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NEM_2096 <- RCP2.6_NEM_2100s %>%
  select(Soil.quality, Tave_2096, Tmax_2096, Tmin_2096, GDD.10_2096, 
         PRE_2096, RAD_2096, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NEM_2096)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NEM_2097 <- RCP2.6_NEM_2100s %>%
  select(Soil.quality, Tave_2097, Tmax_2097, Tmin_2097, GDD.10_2097, 
         PRE_2097, RAD_2097,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NEM_2097)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NEM_2098 <- RCP2.6_NEM_2100s %>%
  select(Soil.quality, Tave_2098, Tmax_2098, Tmin_2098, GDD.10_2098, 
         PRE_2098, RAD_2098,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NEM_2098)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NEM_2099 <- RCP2.6_NEM_2100s %>%
  select(Soil.quality, Tave_2099, Tmax_2099, Tmin_2099, GDD.10_2099, 
         PRE_2099, RAD_2099, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NEM_2099)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NEM_80.99 <- RCP2.6_NEM_2100s %>%
  select(Soil.quality, Tave_2080.2099, Tmax_2080.2099, Tmin_2080.2099, GDD.10_2080.2099, 
         PRE_2080.2099, RAD_2080.2099, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NEM_80.99)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                            "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                            "N", "P2O5", "K2O", "Yck", "Ynpk")


###predict yield by GBRT model

RCP2.6_NEM_pred_2100s_recyle<-NULL

RCP2.6_NEM_pred_2100s <- RCP2.6_NEM_2100s %>%
  select(Soil.quality, Crop, NO, Year, Province, 
         NO.Station)

for (i in 1:50) {
  RCP2.6_NEM_pred_2100s$Ynpk_pred_2080<-predict(brt_MNEC_recyle[[i]],RCP2.6_NEM_2080, n.trees=best_inter_MNEC_recyle[[i]])
  RCP2.6_NEM_pred_2100s$Ynpk_pred_2081<-predict(brt_MNEC_recyle[[i]],RCP2.6_NEM_2081, n.trees=best_inter_MNEC_recyle[[i]])
  RCP2.6_NEM_pred_2100s$Ynpk_pred_2082<-predict(brt_MNEC_recyle[[i]],RCP2.6_NEM_2082, n.trees=best_inter_MNEC_recyle[[i]])
  RCP2.6_NEM_pred_2100s$Ynpk_pred_2083<-predict(brt_MNEC_recyle[[i]],RCP2.6_NEM_2083, n.trees=best_inter_MNEC_recyle[[i]])
  RCP2.6_NEM_pred_2100s$Ynpk_pred_2084<-predict(brt_MNEC_recyle[[i]],RCP2.6_NEM_2084, n.trees=best_inter_MNEC_recyle[[i]])
  RCP2.6_NEM_pred_2100s$Ynpk_pred_2085<-predict(brt_MNEC_recyle[[i]],RCP2.6_NEM_2085, n.trees=best_inter_MNEC_recyle[[i]])
  RCP2.6_NEM_pred_2100s$Ynpk_pred_2086<-predict(brt_MNEC_recyle[[i]],RCP2.6_NEM_2086, n.trees=best_inter_MNEC_recyle[[i]])
  RCP2.6_NEM_pred_2100s$Ynpk_pred_2087<-predict(brt_MNEC_recyle[[i]],RCP2.6_NEM_2087, n.trees=best_inter_MNEC_recyle[[i]])
  RCP2.6_NEM_pred_2100s$Ynpk_pred_2088<-predict(brt_MNEC_recyle[[i]],RCP2.6_NEM_2088, n.trees=best_inter_MNEC_recyle[[i]])
  RCP2.6_NEM_pred_2100s$Ynpk_pred_2089<-predict(brt_MNEC_recyle[[i]],RCP2.6_NEM_2089, n.trees=best_inter_MNEC_recyle[[i]])
  RCP2.6_NEM_pred_2100s$Ynpk_pred_2090<-predict(brt_MNEC_recyle[[i]],RCP2.6_NEM_2090, n.trees=best_inter_MNEC_recyle[[i]])
  RCP2.6_NEM_pred_2100s$Ynpk_pred_2091<-predict(brt_MNEC_recyle[[i]],RCP2.6_NEM_2091, n.trees=best_inter_MNEC_recyle[[i]])
  RCP2.6_NEM_pred_2100s$Ynpk_pred_2092<-predict(brt_MNEC_recyle[[i]],RCP2.6_NEM_2092, n.trees=best_inter_MNEC_recyle[[i]])
  RCP2.6_NEM_pred_2100s$Ynpk_pred_2093<-predict(brt_MNEC_recyle[[i]],RCP2.6_NEM_2093, n.trees=best_inter_MNEC_recyle[[i]])
  RCP2.6_NEM_pred_2100s$Ynpk_pred_2094<-predict(brt_MNEC_recyle[[i]],RCP2.6_NEM_2094, n.trees=best_inter_MNEC_recyle[[i]])
  RCP2.6_NEM_pred_2100s$Ynpk_pred_2095<-predict(brt_MNEC_recyle[[i]],RCP2.6_NEM_2095, n.trees=best_inter_MNEC_recyle[[i]])
  RCP2.6_NEM_pred_2100s$Ynpk_pred_2096<-predict(brt_MNEC_recyle[[i]],RCP2.6_NEM_2096, n.trees=best_inter_MNEC_recyle[[i]])
  RCP2.6_NEM_pred_2100s$Ynpk_pred_2097<-predict(brt_MNEC_recyle[[i]],RCP2.6_NEM_2097, n.trees=best_inter_MNEC_recyle[[i]])
  RCP2.6_NEM_pred_2100s$Ynpk_pred_2098<-predict(brt_MNEC_recyle[[i]],RCP2.6_NEM_2098, n.trees=best_inter_MNEC_recyle[[i]])
  RCP2.6_NEM_pred_2100s$Ynpk_pred_2099<-predict(brt_MNEC_recyle[[i]],RCP2.6_NEM_2099, n.trees=best_inter_MNEC_recyle[[i]])
  RCP2.6_NEM_pred_2100s$Ynpk_pred_80.99<-predict(brt_MNEC_recyle[[i]],RCP2.6_NEM_80.99, n.trees=best_inter_MNEC_recyle[[i]])
  
  RCP2.6_NEM_pred_2100s$average<- apply(RCP2.6_NEM_pred_2100s[,7:26], 1, mean)
  RCP2.6_NEM_pred_2100s_recyle[[i]] <- RCP2.6_NEM_pred_2100s
}

RCP2.6_NEM_pred_2100s_recyle_mean<-RCP2.6_NEM_pred_2100s_recyle[[1]]
RCP2.6_NEM_pred_2100s_recyle_sd<-RCP2.6_NEM_pred_2100s_recyle[[1]]
for(i in 7:28){
  da=as.matrix(as.data.frame(sapply(1:50,function(x){RCP2.6_NEM_pred_2100s_recyle[[x]][i]})))
  RCP2.6_NEM_pred_2100s_recyle_mean[,i]=rowMeans(da)
  RCP2.6_NEM_pred_2100s_recyle_sd[,i]=apply(da,1,sd)
}


###RCP8.5 during 2040-2099

RCP8.5_NEM_2050s<-subset(RCP8.5_2050s, Crop == "northeast maize")
RCP8.5_NEM_2040 <- RCP8.5_NEM_2050s %>%
  select(Soil.quality, Tave_2040, Tmax_2040, Tmin_2040, GDD.10_2040, 
         PRE_2040, RAD_2040, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NEM_2040)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NEM_2041 <- RCP8.5_NEM_2050s %>%
  select(Soil.quality, Tave_2041, Tmax_2041, Tmin_2041, GDD.10_2041, 
         PRE_2041, RAD_2041, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NEM_2041)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NEM_2042 <- RCP8.5_NEM_2050s %>%
  select(Soil.quality, Tave_2042, Tmax_2042, Tmin_2042, GDD.10_2042, 
         PRE_2042, RAD_2042, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NEM_2042)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NEM_2043 <- RCP8.5_NEM_2050s %>%
  select(Soil.quality, Tave_2043, Tmax_2043, Tmin_2043, GDD.10_2043, 
         PRE_2043, RAD_2043, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NEM_2043)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NEM_2044 <- RCP8.5_NEM_2050s %>%
  select(Soil.quality, Tave_2044, Tmax_2044, Tmin_2044, GDD.10_2044, 
         PRE_2044, RAD_2044, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NEM_2044)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NEM_2045 <- RCP8.5_NEM_2050s %>%
  select(Soil.quality, Tave_2045, Tmax_2045, Tmin_2045, GDD.10_2045, 
         PRE_2045, RAD_2045, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NEM_2045)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NEM_2046 <- RCP8.5_NEM_2050s %>%
  select(Soil.quality, Tave_2046, Tmax_2046, Tmin_2046, GDD.10_2046, 
         PRE_2046, RAD_2046,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NEM_2046)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NEM_2047 <- RCP8.5_NEM_2050s %>%
  select(Soil.quality, Tave_2047, Tmax_2047, Tmin_2047, GDD.10_2047, 
         PRE_2047, RAD_2047, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NEM_2047)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NEM_2048 <- RCP8.5_NEM_2050s %>%
  select(Soil.quality, Tave_2048, Tmax_2048, Tmin_2048, GDD.10_2048, 
         PRE_2048, RAD_2048, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NEM_2048)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NEM_2049 <- RCP8.5_NEM_2050s %>%
  select(Soil.quality, Tave_2049, Tmax_2049, Tmin_2049, GDD.10_2049, 
         PRE_2049, RAD_2049, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NEM_2049)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NEM_2050 <- RCP8.5_NEM_2050s %>%
  select(Soil.quality, Tave_2050, Tmax_2050, Tmin_2050, GDD.10_2050, 
         PRE_2050, RAD_2050, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NEM_2050)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NEM_2051 <- RCP8.5_NEM_2050s %>%
  select(Soil.quality, Tave_2051, Tmax_2051, Tmin_2051, GDD.10_2051, 
         PRE_2051, RAD_2051,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NEM_2051)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NEM_2052 <- RCP8.5_NEM_2050s %>%
  select(Soil.quality, Tave_2052, Tmax_2052, Tmin_2052, GDD.10_2052, 
         PRE_2052, RAD_2052, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NEM_2052)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NEM_2053 <- RCP8.5_NEM_2050s %>%
  select(Soil.quality, Tave_2053, Tmax_2053, Tmin_2053, GDD.10_2053, 
         PRE_2053, RAD_2053, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NEM_2053)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NEM_2054 <- RCP8.5_NEM_2050s %>%
  select(Soil.quality, Tave_2054, Tmax_2054, Tmin_2054, GDD.10_2054, 
         PRE_2054, RAD_2054,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NEM_2054)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NEM_2055 <- RCP8.5_NEM_2050s %>%
  select(Soil.quality, Tave_2055, Tmax_2055, Tmin_2055, GDD.10_2055, 
         PRE_2055, RAD_2055, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NEM_2055)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NEM_2056 <- RCP8.5_NEM_2050s %>%
  select(Soil.quality, Tave_2056, Tmax_2056, Tmin_2056, GDD.10_2056, 
         PRE_2056, RAD_2056, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NEM_2056)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NEM_2057 <- RCP8.5_NEM_2050s %>%
  select(Soil.quality, Tave_2057, Tmax_2057, Tmin_2057, GDD.10_2057, 
         PRE_2057, RAD_2057, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NEM_2057)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NEM_2058 <- RCP8.5_NEM_2050s %>%
  select(Soil.quality, Tave_2058, Tmax_2058, Tmin_2058, GDD.10_2058, 
         PRE_2058, RAD_2058, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NEM_2058)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NEM_2059 <- RCP8.5_NEM_2050s %>%
  select(Soil.quality, Tave_2059, Tmax_2059, Tmin_2059, GDD.10_2059, 
         PRE_2059, RAD_2059, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NEM_2059)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NEM_40.59 <- RCP8.5_NEM_2050s %>%
  select(Soil.quality, Tave_2040.2059, Tmax_2040.2059, Tmin_2040.2059, GDD.10_2040.2059, 
         PRE_2040.2059, RAD_2040.2059,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NEM_40.59)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                            "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                            "N", "P2O5", "K2O", "Yck", "Ynpk")

###predict yield by GBRT model

RCP8.5_NEM_pred_2050s_recyle<-NULL

RCP8.5_NEM_pred_2050s <- RCP8.5_NEM_2050s %>%
  select(Soil.quality, Crop, NO, Year, Province, 
         NO.Station)

for (i in 1:50) {
  RCP8.5_NEM_pred_2050s$Ynpk_pred_2040<-predict(brt_MNEC_recyle[[i]],RCP8.5_NEM_2040, n.trees=best_inter_MNEC_recyle[[i]])
  RCP8.5_NEM_pred_2050s$Ynpk_pred_2041<-predict(brt_MNEC_recyle[[i]],RCP8.5_NEM_2041, n.trees=best_inter_MNEC_recyle[[i]])
  RCP8.5_NEM_pred_2050s$Ynpk_pred_2042<-predict(brt_MNEC_recyle[[i]],RCP8.5_NEM_2042, n.trees=best_inter_MNEC_recyle[[i]])
  RCP8.5_NEM_pred_2050s$Ynpk_pred_2043<-predict(brt_MNEC_recyle[[i]],RCP8.5_NEM_2043, n.trees=best_inter_MNEC_recyle[[i]])
  RCP8.5_NEM_pred_2050s$Ynpk_pred_2044<-predict(brt_MNEC_recyle[[i]],RCP8.5_NEM_2044, n.trees=best_inter_MNEC_recyle[[i]])
  RCP8.5_NEM_pred_2050s$Ynpk_pred_2045<-predict(brt_MNEC_recyle[[i]],RCP8.5_NEM_2045, n.trees=best_inter_MNEC_recyle[[i]])
  RCP8.5_NEM_pred_2050s$Ynpk_pred_2046<-predict(brt_MNEC_recyle[[i]],RCP8.5_NEM_2046, n.trees=best_inter_MNEC_recyle[[i]])
  RCP8.5_NEM_pred_2050s$Ynpk_pred_2047<-predict(brt_MNEC_recyle[[i]],RCP8.5_NEM_2047, n.trees=best_inter_MNEC_recyle[[i]])
  RCP8.5_NEM_pred_2050s$Ynpk_pred_2048<-predict(brt_MNEC_recyle[[i]],RCP8.5_NEM_2048, n.trees=best_inter_MNEC_recyle[[i]])
  RCP8.5_NEM_pred_2050s$Ynpk_pred_2049<-predict(brt_MNEC_recyle[[i]],RCP8.5_NEM_2049, n.trees=best_inter_MNEC_recyle[[i]])
  RCP8.5_NEM_pred_2050s$Ynpk_pred_2050<-predict(brt_MNEC_recyle[[i]],RCP8.5_NEM_2050, n.trees=best_inter_MNEC_recyle[[i]])
  RCP8.5_NEM_pred_2050s$Ynpk_pred_2051<-predict(brt_MNEC_recyle[[i]],RCP8.5_NEM_2051, n.trees=best_inter_MNEC_recyle[[i]])
  RCP8.5_NEM_pred_2050s$Ynpk_pred_2052<-predict(brt_MNEC_recyle[[i]],RCP8.5_NEM_2052, n.trees=best_inter_MNEC_recyle[[i]])
  RCP8.5_NEM_pred_2050s$Ynpk_pred_2053<-predict(brt_MNEC_recyle[[i]],RCP8.5_NEM_2053, n.trees=best_inter_MNEC_recyle[[i]])
  RCP8.5_NEM_pred_2050s$Ynpk_pred_2054<-predict(brt_MNEC_recyle[[i]],RCP8.5_NEM_2054, n.trees=best_inter_MNEC_recyle[[i]])
  RCP8.5_NEM_pred_2050s$Ynpk_pred_2055<-predict(brt_MNEC_recyle[[i]],RCP8.5_NEM_2055, n.trees=best_inter_MNEC_recyle[[i]])
  RCP8.5_NEM_pred_2050s$Ynpk_pred_2056<-predict(brt_MNEC_recyle[[i]],RCP8.5_NEM_2056, n.trees=best_inter_MNEC_recyle[[i]])
  RCP8.5_NEM_pred_2050s$Ynpk_pred_2057<-predict(brt_MNEC_recyle[[i]],RCP8.5_NEM_2057, n.trees=best_inter_MNEC_recyle[[i]])
  RCP8.5_NEM_pred_2050s$Ynpk_pred_2058<-predict(brt_MNEC_recyle[[i]],RCP8.5_NEM_2058, n.trees=best_inter_MNEC_recyle[[i]])
  RCP8.5_NEM_pred_2050s$Ynpk_pred_2059<-predict(brt_MNEC_recyle[[i]],RCP8.5_NEM_2059, n.trees=best_inter_MNEC_recyle[[i]])
  RCP8.5_NEM_pred_2050s$Ynpk_pred_40.59<-predict(brt_MNEC_recyle[[i]],RCP8.5_NEM_40.59, n.trees=best_inter_MNEC_recyle[[i]])
  
  RCP8.5_NEM_pred_2050s$average<- apply(RCP8.5_NEM_pred_2050s[,7:26], 1, mean)
  RCP8.5_NEM_pred_2050s_recyle[[i]] <- RCP8.5_NEM_pred_2050s
}

RCP8.5_NEM_pred_2050s_recyle_mean<-RCP8.5_NEM_pred_2050s_recyle[[1]]
RCP8.5_NEM_pred_2050s_recyle_sd<-RCP8.5_NEM_pred_2050s_recyle[[1]]
for(i in 7:28){
  da=as.matrix(as.data.frame(sapply(1:50,function(x){RCP8.5_NEM_pred_2050s_recyle[[x]][i]})))
  RCP8.5_NEM_pred_2050s_recyle_mean[,i]=rowMeans(da)
  RCP8.5_NEM_pred_2050s_recyle_sd[,i]=apply(da,1,sd)
}

###RCP8.5 during 2080-2099

RCP8.5_NEM_2100s<-subset(RCP8.5_2100s, Crop == "northeast maize")
RCP8.5_NEM_2080 <- RCP8.5_NEM_2100s %>%
  select(Soil.quality, Tave_2080, Tmax_2080, Tmin_2080, GDD.10_2080, 
         PRE_2080, RAD_2080,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NEM_2080)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NEM_2081 <- RCP8.5_NEM_2100s %>%
  select(Soil.quality, Tave_2081, Tmax_2081, Tmin_2081, GDD.10_2081, 
         PRE_2081, RAD_2081, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NEM_2081)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NEM_2082 <- RCP8.5_NEM_2100s %>%
  select(Soil.quality, Tave_2082, Tmax_2082, Tmin_2082, GDD.10_2082, 
         PRE_2082, RAD_2082, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NEM_2082)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NEM_2083 <- RCP8.5_NEM_2100s %>%
  select(Soil.quality, Tave_2083, Tmax_2083, Tmin_2083, GDD.10_2083, 
         PRE_2083, RAD_2083, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NEM_2083)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NEM_2084 <- RCP8.5_NEM_2100s %>%
  select(Soil.quality, Tave_2084, Tmax_2084, Tmin_2084, GDD.10_2084, 
         PRE_2084, RAD_2084, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NEM_2084)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NEM_2085 <- RCP8.5_NEM_2100s %>%
  select(Soil.quality, Tave_2085, Tmax_2085, Tmin_2085, GDD.10_2085, 
         PRE_2085, RAD_2085, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NEM_2085)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NEM_2086 <- RCP8.5_NEM_2100s %>%
  select(Soil.quality, Tave_2086, Tmax_2086, Tmin_2086, GDD.10_2086, 
         PRE_2086, RAD_2086,Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NEM_2086)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NEM_2087 <- RCP8.5_NEM_2100s %>%
  select(Soil.quality, Tave_2087, Tmax_2087, Tmin_2087, GDD.10_2087, 
         PRE_2087, RAD_2087, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NEM_2087)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NEM_2088 <- RCP8.5_NEM_2100s %>%
  select(Soil.quality, Tave_2088, Tmax_2088, Tmin_2088, GDD.10_2088, 
         PRE_2088, RAD_2088, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NEM_2088)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NEM_2089 <- RCP8.5_NEM_2100s %>%
  select(Soil.quality, Tave_2089, Tmax_2089, Tmin_2089, GDD.10_2089, 
         PRE_2089, RAD_2089, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NEM_2089)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NEM_2090 <- RCP8.5_NEM_2100s %>%
  select(Soil.quality, Tave_2090, Tmax_2090, Tmin_2090, GDD.10_2090, 
         PRE_2090, RAD_2090, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NEM_2090)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NEM_2091 <- RCP8.5_NEM_2100s %>%
  select(Soil.quality, Tave_2091, Tmax_2091, Tmin_2091, GDD.10_2091, 
         PRE_2091, RAD_2091,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NEM_2091)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NEM_2092 <- RCP8.5_NEM_2100s %>%
  select(Soil.quality, Tave_2092, Tmax_2092, Tmin_2092, GDD.10_2092, 
         PRE_2092, RAD_2092, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NEM_2092)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NEM_2093 <- RCP8.5_NEM_2100s %>%
  select(Soil.quality, Tave_2093, Tmax_2093, Tmin_2093, GDD.10_2093, 
         PRE_2093, RAD_2093, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NEM_2093)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NEM_2094 <- RCP8.5_NEM_2100s %>%
  select(Soil.quality, Tave_2094, Tmax_2094, Tmin_2094, GDD.10_2094, 
         PRE_2094, RAD_2094, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NEM_2094)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NEM_2095 <- RCP8.5_NEM_2100s %>%
  select(Soil.quality, Tave_2095, Tmax_2095, Tmin_2095, GDD.10_2095, 
         PRE_2095, RAD_2095, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NEM_2095)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NEM_2096 <- RCP8.5_NEM_2100s %>%
  select(Soil.quality, Tave_2096, Tmax_2096, Tmin_2096, GDD.10_2096, 
         PRE_2096, RAD_2096,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NEM_2096)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NEM_2097 <- RCP8.5_NEM_2100s %>%
  select(Soil.quality, Tave_2097, Tmax_2097, Tmin_2097, GDD.10_2097, 
         PRE_2097, RAD_2097, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NEM_2097)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NEM_2098 <- RCP8.5_NEM_2100s %>%
  select(Soil.quality, Tave_2098, Tmax_2098, Tmin_2098, GDD.10_2098, 
         PRE_2098, RAD_2098, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NEM_2098)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NEM_2099 <- RCP8.5_NEM_2100s %>%
  select(Soil.quality, Tave_2099, Tmax_2099, Tmin_2099, GDD.10_2099, 
         PRE_2099, RAD_2099, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NEM_2099)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NEM_80.99 <- RCP8.5_NEM_2100s %>%
  select(Soil.quality, Tave_2080.2099, Tmax_2080.2099, Tmin_2080.2099, GDD.10_2080.2099, 
         PRE_2080.2099, RAD_2080.2099, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NEM_80.99)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                            "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                            "N", "P2O5", "K2O", "Yck", "Ynpk")


###predict yield by GBRT model
RCP8.5_NEM_pred_2100s_recyle<-NULL

RCP8.5_NEM_pred_2100s <- RCP8.5_NEM_2100s %>%
  select(Soil.quality, Crop, NO, Year, Province, 
         NO.Station)

for (i in 1:50) {
  RCP8.5_NEM_pred_2100s$Ynpk_pred_2080<-predict(brt_MNEC_recyle[[i]],RCP8.5_NEM_2080, n.trees=best_inter_MNEC_recyle[[i]])
  RCP8.5_NEM_pred_2100s$Ynpk_pred_2081<-predict(brt_MNEC_recyle[[i]],RCP8.5_NEM_2081, n.trees=best_inter_MNEC_recyle[[i]])
  RCP8.5_NEM_pred_2100s$Ynpk_pred_2082<-predict(brt_MNEC_recyle[[i]],RCP8.5_NEM_2082, n.trees=best_inter_MNEC_recyle[[i]])
  RCP8.5_NEM_pred_2100s$Ynpk_pred_2083<-predict(brt_MNEC_recyle[[i]],RCP8.5_NEM_2083, n.trees=best_inter_MNEC_recyle[[i]])
  RCP8.5_NEM_pred_2100s$Ynpk_pred_2084<-predict(brt_MNEC_recyle[[i]],RCP8.5_NEM_2084, n.trees=best_inter_MNEC_recyle[[i]])
  RCP8.5_NEM_pred_2100s$Ynpk_pred_2085<-predict(brt_MNEC_recyle[[i]],RCP8.5_NEM_2085, n.trees=best_inter_MNEC_recyle[[i]])
  RCP8.5_NEM_pred_2100s$Ynpk_pred_2086<-predict(brt_MNEC_recyle[[i]],RCP8.5_NEM_2086, n.trees=best_inter_MNEC_recyle[[i]])
  RCP8.5_NEM_pred_2100s$Ynpk_pred_2087<-predict(brt_MNEC_recyle[[i]],RCP8.5_NEM_2087, n.trees=best_inter_MNEC_recyle[[i]])
  RCP8.5_NEM_pred_2100s$Ynpk_pred_2088<-predict(brt_MNEC_recyle[[i]],RCP8.5_NEM_2088, n.trees=best_inter_MNEC_recyle[[i]])
  RCP8.5_NEM_pred_2100s$Ynpk_pred_2089<-predict(brt_MNEC_recyle[[i]],RCP8.5_NEM_2089, n.trees=best_inter_MNEC_recyle[[i]])
  RCP8.5_NEM_pred_2100s$Ynpk_pred_2090<-predict(brt_MNEC_recyle[[i]],RCP8.5_NEM_2090, n.trees=best_inter_MNEC_recyle[[i]])
  RCP8.5_NEM_pred_2100s$Ynpk_pred_2091<-predict(brt_MNEC_recyle[[i]],RCP8.5_NEM_2091, n.trees=best_inter_MNEC_recyle[[i]])
  RCP8.5_NEM_pred_2100s$Ynpk_pred_2092<-predict(brt_MNEC_recyle[[i]],RCP8.5_NEM_2092, n.trees=best_inter_MNEC_recyle[[i]])
  RCP8.5_NEM_pred_2100s$Ynpk_pred_2093<-predict(brt_MNEC_recyle[[i]],RCP8.5_NEM_2093, n.trees=best_inter_MNEC_recyle[[i]])
  RCP8.5_NEM_pred_2100s$Ynpk_pred_2094<-predict(brt_MNEC_recyle[[i]],RCP8.5_NEM_2094, n.trees=best_inter_MNEC_recyle[[i]])
  RCP8.5_NEM_pred_2100s$Ynpk_pred_2095<-predict(brt_MNEC_recyle[[i]],RCP8.5_NEM_2095, n.trees=best_inter_MNEC_recyle[[i]])
  RCP8.5_NEM_pred_2100s$Ynpk_pred_2096<-predict(brt_MNEC_recyle[[i]],RCP8.5_NEM_2096, n.trees=best_inter_MNEC_recyle[[i]])
  RCP8.5_NEM_pred_2100s$Ynpk_pred_2097<-predict(brt_MNEC_recyle[[i]],RCP8.5_NEM_2097, n.trees=best_inter_MNEC_recyle[[i]])
  RCP8.5_NEM_pred_2100s$Ynpk_pred_2098<-predict(brt_MNEC_recyle[[i]],RCP8.5_NEM_2098, n.trees=best_inter_MNEC_recyle[[i]])
  RCP8.5_NEM_pred_2100s$Ynpk_pred_2099<-predict(brt_MNEC_recyle[[i]],RCP8.5_NEM_2099, n.trees=best_inter_MNEC_recyle[[i]])
  RCP8.5_NEM_pred_2100s$Ynpk_pred_80.99<-predict(brt_MNEC_recyle[[i]],RCP8.5_NEM_80.99, n.trees=best_inter_MNEC_recyle[[i]])
  
  RCP8.5_NEM_pred_2100s$average<- apply(RCP8.5_NEM_pred_2100s[,7:26], 1, mean)
  RCP8.5_NEM_pred_2100s_recyle[[i]] <- RCP8.5_NEM_pred_2100s
}

RCP8.5_NEM_pred_2100s_recyle_mean<-RCP8.5_NEM_pred_2100s_recyle[[1]]
RCP8.5_NEM_pred_2100s_recyle_sd<-RCP8.5_NEM_pred_2100s_recyle[[1]]
for(i in 7:28){
  da=as.matrix(as.data.frame(sapply(1:50,function(x){RCP8.5_NEM_pred_2100s_recyle[[x]][i]})))
  RCP8.5_NEM_pred_2100s_recyle_mean[,i]=rowMeans(da)
  RCP8.5_NEM_pred_2100s_recyle_sd[,i]=apply(da,1,sd)
}

###summarise predicted yield

sum_RCP2.6_NEM_2050s_year<- cbind(base_NEM_pred_all[,c("Soil.quality","Crop","NO","average")],RCP2.6_NEM_pred_2050s[,7:28])
names(sum_RCP2.6_NEM_2050s_year)[1:4]<- c("Soil quality", "Crop", "No", "Yield_base")
names(sum_RCP2.6_NEM_2050s_year)[26]<- "Yield_RCP"
sum_RCP2.6_NEM_2050s_year$Yield_change <- sum_RCP2.6_NEM_2050s_year$`Yield_RCP` - sum_RCP2.6_NEM_2050s_year$`Yield_base`
sum_RCP2.6_NEM_2050s_year$Yield_change_per <- (sum_RCP2.6_NEM_2050s_year$Yield_change / sum_RCP2.6_NEM_2050s_year$Yield_base)*100
sum_RCP2.6_NEM_2050s_year$RCP <- "RCP2.6"
sum_RCP2.6_NEM_2050s_year$Time <- "2050"

sum_RCP8.5_NEM_2050s_year<- cbind(base_NEM_pred_all[,c("Soil.quality","Crop","NO","average")],RCP8.5_NEM_pred_2050s[,7:28])
names(sum_RCP8.5_NEM_2050s_year)[1:4]<- c("Soil quality", "Crop", "No", "Yield_base")
names(sum_RCP8.5_NEM_2050s_year)[26]<- "Yield_RCP"
sum_RCP8.5_NEM_2050s_year$Yield_change <- sum_RCP8.5_NEM_2050s_year$`Yield_RCP` - sum_RCP8.5_NEM_2050s_year$`Yield_base`
sum_RCP8.5_NEM_2050s_year$Yield_change_per <- (sum_RCP8.5_NEM_2050s_year$Yield_change / sum_RCP8.5_NEM_2050s_year$Yield_base)*100
sum_RCP8.5_NEM_2050s_year$RCP <- "RCP8.5"
sum_RCP8.5_NEM_2050s_year$Time <- "2050"

sum_RCP2.6_NEM_2100s_year<- cbind(base_NEM_pred_all[,c("Soil.quality","Crop","NO","average")],RCP2.6_NEM_pred_2100s[,7:28])
names(sum_RCP2.6_NEM_2100s_year)[1:4]<- c("Soil quality", "Crop", "No", "Yield_base")
names(sum_RCP2.6_NEM_2100s_year)[26]<- "Yield_RCP"
sum_RCP2.6_NEM_2100s_year$Yield_change <- sum_RCP2.6_NEM_2100s_year$`Yield_RCP` - sum_RCP2.6_NEM_2100s_year$`Yield_base`
sum_RCP2.6_NEM_2100s_year$Yield_change_per <- (sum_RCP2.6_NEM_2100s_year$Yield_change / sum_RCP2.6_NEM_2100s_year$Yield_base)*100
sum_RCP2.6_NEM_2100s_year$RCP <- "RCP2.6"
sum_RCP2.6_NEM_2100s_year$Time <- "2100"

sum_RCP8.5_NEM_2100s_year<- cbind(base_NEM_pred_all[,c("Soil.quality","Crop","NO","average")],RCP8.5_NEM_pred_2100s[,7:28])
names(sum_RCP8.5_NEM_2100s_year)[1:4]<- c("Soil quality", "Crop", "No", "Yield_base")
names(sum_RCP8.5_NEM_2100s_year)[26]<- "Yield_RCP"
sum_RCP8.5_NEM_2100s_year$Yield_change <- sum_RCP8.5_NEM_2100s_year$`Yield_RCP` - sum_RCP8.5_NEM_2100s_year$`Yield_base`
sum_RCP8.5_NEM_2100s_year$Yield_change_per <- (sum_RCP8.5_NEM_2100s_year$Yield_change / sum_RCP8.5_NEM_2100s_year$Yield_base)*100
sum_RCP8.5_NEM_2100s_year$RCP <- "RCP8.5"
sum_RCP8.5_NEM_2100s_year$Time <- "2100"

sum_RCP_NEM_2050s_year <- rbind(sum_RCP2.6_NEM_2050s_year,sum_RCP8.5_NEM_2050s_year)
sum_RCP_NEM_2100s_year<-rbind(sum_RCP2.6_NEM_2100s_year,sum_RCP8.5_NEM_2100s_year)

write.csv(sum_RCP_NEM_2050s_year,"./results/sum_RCP_NEM_2050s_year.csv")
write.csv(sum_RCP_NEM_2100s_year,"./results/sum_RCP_NEM_2100s_year.csv")

###comparison of yield change between high and low quality soils 
###T test

t.test(Yield_change~`Soil quality` ,sum_RCP2.6_NEM_2050s_year) #RCP2.6-2050s
t.test(Yield_change~`Soil quality` ,sum_RCP8.5_NEM_2050s_year) #RCP8.5-2050s
t.test(Yield_change~`Soil quality` ,sum_RCP2.6_NEM_2100s_year) #RCP2.6-2100s
t.test(Yield_change~`Soil quality` ,sum_RCP8.5_NEM_2100s_year) #RCP8.5-2100s

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

for (i in 1:50) {
  
  set.seed(i)
  train_MNCP <- sample(nrow(M_NCP), 0.9*nrow(M_NCP))
  M_NCP_train <- M_NCP[train_MNCP,] 
  M_NCP_test <- M_NCP[-train_MNCP,]
  
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
}

#####Predicted yield under different climate scenarios
###baseline period 1986-2005
base_NM<-subset(base, Crop == "north maize")
base_NM_1986 <- base_NM %>%
  select(Soil.quality, Tave_1986, Tmax_1986, Tmin_1986, GDD.10_1986, 
         PRE_1986, RAD_1986, Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NM_1986)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NM_1987 <- base_NM %>%
  select(Soil.quality, Tave_1987, Tmax_1987, Tmin_1987, GDD.10_1987, 
         PRE_1987, RAD_1987, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NM_1987)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NM_1988 <- base_NM %>%
  select(Soil.quality, Tave_1988, Tmax_1988, Tmin_1988, GDD.10_1988, 
         PRE_1988, RAD_1988, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NM_1988)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NM_1989 <- base_NM %>%
  select(Soil.quality, Tave_1989, Tmax_1989, Tmin_1989, GDD.10_1989, 
         PRE_1989, RAD_1989, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NM_1989)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NM_1990 <- base_NM %>%
  select(Soil.quality, Tave_1990, Tmax_1990, Tmin_1990, GDD.10_1990, 
         PRE_1990, RAD_1990, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NM_1990)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NM_1991 <- base_NM %>%
  select(Soil.quality, Tave_1991, Tmax_1991, Tmin_1991, GDD.10_1991, 
         PRE_1991, RAD_1991, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NM_1991)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NM_1992 <- base_NM %>%
  select(Soil.quality, Tave_1992, Tmax_1992, Tmin_1992, GDD.10_1992, 
         PRE_1992, RAD_1992,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NM_1992)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NM_1993 <- base_NM %>%
  select(Soil.quality, Tave_1993, Tmax_1993, Tmin_1993, GDD.10_1993, 
         PRE_1993, RAD_1993, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NM_1993)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NM_1994 <- base_NM %>%
  select(Soil.quality, Tave_1994, Tmax_1994, Tmin_1994, GDD.10_1994, 
         PRE_1994, RAD_1994, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NM_1994)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NM_1995 <- base_NM %>%
  select(Soil.quality, Tave_1995, Tmax_1995, Tmin_1995, GDD.10_1995, 
         PRE_1995, RAD_1995, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NM_1995)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NM_1996 <- base_NM %>%
  select(Soil.quality, Tave_1996, Tmax_1996, Tmin_1996, GDD.10_1996, 
         PRE_1996, RAD_1996, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NM_1996)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NM_1997 <- base_NM %>%
  select(Soil.quality, Tave_1997, Tmax_1997, Tmin_1997, GDD.10_1997, 
         PRE_1997, RAD_1997, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NM_1997)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NM_1998 <- base_NM %>%
  select(Soil.quality, Tave_1998, Tmax_1998, Tmin_1998, GDD.10_1998, 
         PRE_1998, RAD_1998, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NM_1998)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NM_1999 <- base_NM %>%
  select(Soil.quality, Tave_1999, Tmax_1999, Tmin_1999, GDD.10_1999, 
         PRE_1999, RAD_1999, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NM_1999)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NM_2000 <- base_NM %>%
  select(Soil.quality, Tave_2000, Tmax_2000, Tmin_2000, GDD.10_2000, 
         PRE_2000, RAD_2000, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NM_2000)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NM_2001 <- base_NM %>%
  select(Soil.quality, Tave_2001, Tmax_2001, Tmin_2001, GDD.10_2001, 
         PRE_2001, RAD_2001, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NM_2001)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NM_2002 <- base_NM %>%
  select(Soil.quality, Tave_2002, Tmax_2002, Tmin_2002, GDD.10_2002, 
         PRE_2002, RAD_2002, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NM_2002)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NM_2003 <- base_NM %>%
  select(Soil.quality, Tave_2003, Tmax_2003, Tmin_2003, GDD.10_2003, 
         PRE_2003, RAD_2003, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NM_2003)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NM_2004 <- base_NM %>%
  select(Soil.quality, Tave_2004, Tmax_2004, Tmin_2004, GDD.10_2004, 
         PRE_2004, RAD_2004, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NM_2004)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NM_2005 <- base_NM %>%
  select(Soil.quality, Tave_2005, Tmax_2005, Tmin_2005, GDD.10_2005, 
         PRE_2005, RAD_2005, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NM_2005)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_NM_86.05 <- base_NM %>%
  select(Soil.quality, Tave_1986.2005, Tmax_1986.2005, Tmin_1986.2005, GDD.10_1986.2005, 
         PRE_1986.2005, RAD_1986.2005, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_NM_86.05)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                         "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                         "N", "P2O5", "K2O", "Yck", "Ynpk")

###predict yield by GBRT model

base_NM_pred_all_recyle<-NULL

base_NM_pred_all <- base_NM %>%
  select(Soil.quality, Crop, NO, Year,Province, 
         NO.Station)

for (i in 1:50) {
  base_NM_pred_all$Ynpk_pred_1986<-predict(brt_MNCP_recyle[[i]],base_NM_1986, n.trees=best_inter_MNCP_recyle[[i]])
  base_NM_pred_all$Ynpk_pred_1987<-predict(brt_MNCP_recyle[[i]],base_NM_1987, n.trees=best_inter_MNCP_recyle[[i]])
  base_NM_pred_all$Ynpk_pred_1988<-predict(brt_MNCP_recyle[[i]],base_NM_1988, n.trees=best_inter_MNCP_recyle[[i]])
  base_NM_pred_all$Ynpk_pred_1989<-predict(brt_MNCP_recyle[[i]],base_NM_1989, n.trees=best_inter_MNCP_recyle[[i]])
  base_NM_pred_all$Ynpk_pred_1990<-predict(brt_MNCP_recyle[[i]],base_NM_1990, n.trees=best_inter_MNCP_recyle[[i]])
  base_NM_pred_all$Ynpk_pred_1991<-predict(brt_MNCP_recyle[[i]],base_NM_1991, n.trees=best_inter_MNCP_recyle[[i]])
  base_NM_pred_all$Ynpk_pred_1992<-predict(brt_MNCP_recyle[[i]],base_NM_1992, n.trees=best_inter_MNCP_recyle[[i]])
  base_NM_pred_all$Ynpk_pred_1993<-predict(brt_MNCP_recyle[[i]],base_NM_1993, n.trees=best_inter_MNCP_recyle[[i]])
  base_NM_pred_all$Ynpk_pred_1994<-predict(brt_MNCP_recyle[[i]],base_NM_1994, n.trees=best_inter_MNCP_recyle[[i]])
  base_NM_pred_all$Ynpk_pred_1995<-predict(brt_MNCP_recyle[[i]],base_NM_1995, n.trees=best_inter_MNCP_recyle[[i]])
  base_NM_pred_all$Ynpk_pred_1996<-predict(brt_MNCP_recyle[[i]],base_NM_1996, n.trees=best_inter_MNCP_recyle[[i]])
  base_NM_pred_all$Ynpk_pred_1997<-predict(brt_MNCP_recyle[[i]],base_NM_1997, n.trees=best_inter_MNCP_recyle[[i]])
  base_NM_pred_all$Ynpk_pred_1998<-predict(brt_MNCP_recyle[[i]],base_NM_1998, n.trees=best_inter_MNCP_recyle[[i]])
  base_NM_pred_all$Ynpk_pred_1999<-predict(brt_MNCP_recyle[[i]],base_NM_1999, n.trees=best_inter_MNCP_recyle[[i]])
  base_NM_pred_all$Ynpk_pred_2000<-predict(brt_MNCP_recyle[[i]],base_NM_2000, n.trees=best_inter_MNCP_recyle[[i]])
  base_NM_pred_all$Ynpk_pred_2001<-predict(brt_MNCP_recyle[[i]],base_NM_2001, n.trees=best_inter_MNCP_recyle[[i]])
  base_NM_pred_all$Ynpk_pred_2002<-predict(brt_MNCP_recyle[[i]],base_NM_2002, n.trees=best_inter_MNCP_recyle[[i]])
  base_NM_pred_all$Ynpk_pred_2003<-predict(brt_MNCP_recyle[[i]],base_NM_2003, n.trees=best_inter_MNCP_recyle[[i]])
  base_NM_pred_all$Ynpk_pred_2004<-predict(brt_MNCP_recyle[[i]],base_NM_2004, n.trees=best_inter_MNCP_recyle[[i]])
  base_NM_pred_all$Ynpk_pred_2005<-predict(brt_MNCP_recyle[[i]],base_NM_2005, n.trees=best_inter_MNCP_recyle[[i]])
  base_NM_pred_all$Ynpk_pred_86.05<-predict(brt_MNCP_recyle[[i]],base_NM_86.05, n.trees=best_inter_MNCP_recyle[[i]])
  
  base_NM_pred_all$average<- apply(base_NM_pred_all[,7:26], 1, mean)
  base_NM_pred_all_recyle[[i]] <- base_NM_pred_all
}

base_NM_pred_all_recyle_mean<-base_NM_pred_all_recyle[[1]]
base_NM_pred_all_recyle_sd<-base_NM_pred_all_recyle[[1]]
for(i in 7:28){
  da=as.matrix(as.data.frame(sapply(1:50,function(x){base_NM_pred_all_recyle[[x]][i]})))
  base_NM_pred_all_recyle_mean[,i]=rowMeans(da)
  base_NM_pred_all_recyle_sd[,i]=apply(da,1,sd)
}

###RCP2.6 during 2040-2059

RCP2.6_NM_2050s<-subset(RCP2.6_2050s, Crop == "north maize")
RCP2.6_NM_2040 <- RCP2.6_NM_2050s %>%
  select(Soil.quality, Tave_2040, Tmax_2040, Tmin_2040, GDD.10_2040, 
         PRE_2040, RAD_2040, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NM_2040)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NM_2041 <- RCP2.6_NM_2050s %>%
  select(Soil.quality, Tave_2041, Tmax_2041, Tmin_2041, GDD.10_2041, 
         PRE_2041, RAD_2041, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NM_2041)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NM_2042 <- RCP2.6_NM_2050s %>%
  select(Soil.quality, Tave_2042, Tmax_2042, Tmin_2042, GDD.10_2042, 
         PRE_2042, RAD_2042, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NM_2042)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NM_2043 <- RCP2.6_NM_2050s %>%
  select(Soil.quality, Tave_2043, Tmax_2043, Tmin_2043, GDD.10_2043, 
         PRE_2043, RAD_2043, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NM_2043)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NM_2044 <- RCP2.6_NM_2050s %>%
  select(Soil.quality, Tave_2044, Tmax_2044, Tmin_2044, GDD.10_2044, 
         PRE_2044, RAD_2044, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NM_2044)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NM_2045 <- RCP2.6_NM_2050s %>%
  select(Soil.quality, Tave_2045, Tmax_2045, Tmin_2045, GDD.10_2045, 
         PRE_2045, RAD_2045, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NM_2045)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NM_2046 <- RCP2.6_NM_2050s %>%
  select(Soil.quality, Tave_2046, Tmax_2046, Tmin_2046, GDD.10_2046, 
         PRE_2046, RAD_2046, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NM_2046)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NM_2047 <- RCP2.6_NM_2050s %>%
  select(Soil.quality, Tave_2047, Tmax_2047, Tmin_2047, GDD.10_2047, 
         PRE_2047, RAD_2047, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NM_2047)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NM_2048 <- RCP2.6_NM_2050s %>%
  select(Soil.quality, Tave_2048, Tmax_2048, Tmin_2048, GDD.10_2048, 
         PRE_2048, RAD_2048, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NM_2048)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NM_2049 <- RCP2.6_NM_2050s %>%
  select(Soil.quality, Tave_2049, Tmax_2049, Tmin_2049, GDD.10_2049, 
         PRE_2049, RAD_2049, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NM_2049)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NM_2050 <- RCP2.6_NM_2050s %>%
  select(Soil.quality, Tave_2050, Tmax_2050, Tmin_2050, GDD.10_2050, 
         PRE_2050, RAD_2050, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NM_2050)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NM_2051 <- RCP2.6_NM_2050s %>%
  select(Soil.quality, Tave_2051, Tmax_2051, Tmin_2051, GDD.10_2051, 
         PRE_2051, RAD_2051, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NM_2051)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NM_2052 <- RCP2.6_NM_2050s %>%
  select(Soil.quality, Tave_2052, Tmax_2052, Tmin_2052, GDD.10_2052, 
         PRE_2052, RAD_2052, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NM_2052)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NM_2053 <- RCP2.6_NM_2050s %>%
  select(Soil.quality, Tave_2053, Tmax_2053, Tmin_2053, GDD.10_2053, 
         PRE_2053, RAD_2053, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NM_2053)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NM_2054 <- RCP2.6_NM_2050s %>%
  select(Soil.quality, Tave_2054, Tmax_2054, Tmin_2054, GDD.10_2054, 
         PRE_2054, RAD_2054, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NM_2054)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NM_2055 <- RCP2.6_NM_2050s %>%
  select(Soil.quality, Tave_2055, Tmax_2055, Tmin_2055, GDD.10_2055, 
         PRE_2055, RAD_2055, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NM_2055)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NM_2056 <- RCP2.6_NM_2050s %>%
  select(Soil.quality, Tave_2056, Tmax_2056, Tmin_2056, GDD.10_2056, 
         PRE_2056, RAD_2056, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NM_2056)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NM_2057 <- RCP2.6_NM_2050s %>%
  select(Soil.quality, Tave_2057, Tmax_2057, Tmin_2057, GDD.10_2057, 
         PRE_2057, RAD_2057, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NM_2057)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NM_2058 <- RCP2.6_NM_2050s %>%
  select(Soil.quality, Tave_2058, Tmax_2058, Tmin_2058, GDD.10_2058, 
         PRE_2058, RAD_2058, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NM_2058)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NM_2059 <- RCP2.6_NM_2050s %>%
  select(Soil.quality, Tave_2059, Tmax_2059, Tmin_2059, GDD.10_2059, 
         PRE_2059, RAD_2059, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NM_2059)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NM_40.59 <- RCP2.6_NM_2050s %>%
  select(Soil.quality, Tave_2040.2059, Tmax_2040.2059, Tmin_2040.2059, GDD.10_2040.2059, 
         PRE_2040.2059, RAD_2040.2059, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NM_40.59)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

###predict yield by GBRT model

RCP2.6_NM_pred_2050s_recyle<-NULL

RCP2.6_NM_pred_2050s <- RCP2.6_NM_2050s %>%
  select(Soil.quality, Crop, NO, Year, Province, 
         NO.Station)

for (i in 1:50) {
  RCP2.6_NM_pred_2050s$Ynpk_pred_2040<-predict(brt_MNCP_recyle[[i]],RCP2.6_NM_2040, n.trees=best_inter_MNCP_recyle[[i]])
  RCP2.6_NM_pred_2050s$Ynpk_pred_2041<-predict(brt_MNCP_recyle[[i]],RCP2.6_NM_2041, n.trees=best_inter_MNCP_recyle[[i]])
  RCP2.6_NM_pred_2050s$Ynpk_pred_2042<-predict(brt_MNCP_recyle[[i]],RCP2.6_NM_2042, n.trees=best_inter_MNCP_recyle[[i]])
  RCP2.6_NM_pred_2050s$Ynpk_pred_2043<-predict(brt_MNCP_recyle[[i]],RCP2.6_NM_2043, n.trees=best_inter_MNCP_recyle[[i]])
  RCP2.6_NM_pred_2050s$Ynpk_pred_2044<-predict(brt_MNCP_recyle[[i]],RCP2.6_NM_2044, n.trees=best_inter_MNCP_recyle[[i]])
  RCP2.6_NM_pred_2050s$Ynpk_pred_2045<-predict(brt_MNCP_recyle[[i]],RCP2.6_NM_2045, n.trees=best_inter_MNCP_recyle[[i]])
  RCP2.6_NM_pred_2050s$Ynpk_pred_2046<-predict(brt_MNCP_recyle[[i]],RCP2.6_NM_2046, n.trees=best_inter_MNCP_recyle[[i]])
  RCP2.6_NM_pred_2050s$Ynpk_pred_2047<-predict(brt_MNCP_recyle[[i]],RCP2.6_NM_2047, n.trees=best_inter_MNCP_recyle[[i]])
  RCP2.6_NM_pred_2050s$Ynpk_pred_2048<-predict(brt_MNCP_recyle[[i]],RCP2.6_NM_2048, n.trees=best_inter_MNCP_recyle[[i]])
  RCP2.6_NM_pred_2050s$Ynpk_pred_2049<-predict(brt_MNCP_recyle[[i]],RCP2.6_NM_2049, n.trees=best_inter_MNCP_recyle[[i]])
  RCP2.6_NM_pred_2050s$Ynpk_pred_2050<-predict(brt_MNCP_recyle[[i]],RCP2.6_NM_2050, n.trees=best_inter_MNCP_recyle[[i]])
  RCP2.6_NM_pred_2050s$Ynpk_pred_2051<-predict(brt_MNCP_recyle[[i]],RCP2.6_NM_2051, n.trees=best_inter_MNCP_recyle[[i]])
  RCP2.6_NM_pred_2050s$Ynpk_pred_2052<-predict(brt_MNCP_recyle[[i]],RCP2.6_NM_2052, n.trees=best_inter_MNCP_recyle[[i]])
  RCP2.6_NM_pred_2050s$Ynpk_pred_2053<-predict(brt_MNCP_recyle[[i]],RCP2.6_NM_2053, n.trees=best_inter_MNCP_recyle[[i]])
  RCP2.6_NM_pred_2050s$Ynpk_pred_2054<-predict(brt_MNCP_recyle[[i]],RCP2.6_NM_2054, n.trees=best_inter_MNCP_recyle[[i]])
  RCP2.6_NM_pred_2050s$Ynpk_pred_2055<-predict(brt_MNCP_recyle[[i]],RCP2.6_NM_2055, n.trees=best_inter_MNCP_recyle[[i]])
  RCP2.6_NM_pred_2050s$Ynpk_pred_2056<-predict(brt_MNCP_recyle[[i]],RCP2.6_NM_2056, n.trees=best_inter_MNCP_recyle[[i]])
  RCP2.6_NM_pred_2050s$Ynpk_pred_2057<-predict(brt_MNCP_recyle[[i]],RCP2.6_NM_2057, n.trees=best_inter_MNCP_recyle[[i]])
  RCP2.6_NM_pred_2050s$Ynpk_pred_2058<-predict(brt_MNCP_recyle[[i]],RCP2.6_NM_2058, n.trees=best_inter_MNCP_recyle[[i]])
  RCP2.6_NM_pred_2050s$Ynpk_pred_2059<-predict(brt_MNCP_recyle[[i]],RCP2.6_NM_2059, n.trees=best_inter_MNCP_recyle[[i]])
  RCP2.6_NM_pred_2050s$Ynpk_pred_40.59<-predict(brt_MNCP_recyle[[i]],RCP2.6_NM_40.59, n.trees=best_inter_MNCP_recyle[[i]])
  
  RCP2.6_NM_pred_2050s$average<- apply(RCP2.6_NM_pred_2050s[,7:26], 1, mean)
  RCP2.6_NM_pred_2050s_recyle[[i]] <- RCP2.6_NM_pred_2050s
}

RCP2.6_NM_pred_2050s_recyle_mean<-RCP2.6_NM_pred_2050s_recyle[[1]]
RCP2.6_NM_pred_2050s_recyle_sd<-RCP2.6_NM_pred_2050s_recyle[[1]]
for(i in 7:28){
  da=as.matrix(as.data.frame(sapply(1:50,function(x){RCP2.6_NM_pred_2050s_recyle[[x]][i]})))
  RCP2.6_NM_pred_2050s_recyle_mean[,i]=rowMeans(da)
  RCP2.6_NM_pred_2050s_recyle_sd[,i]=apply(da,1,sd)
}

###RCP2.6 during 2080-2099

RCP2.6_NM_2100s<-subset(RCP2.6_2100s, Crop == "north maize")
RCP2.6_NM_2080 <- RCP2.6_NM_2100s %>%
  select(Soil.quality, Tave_2080, Tmax_2080, Tmin_2080, GDD.10_2080, 
         PRE_2080, RAD_2080,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NM_2080)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NM_2081 <- RCP2.6_NM_2100s %>%
  select(Soil.quality, Tave_2081, Tmax_2081, Tmin_2081, GDD.10_2081, 
         PRE_2081, RAD_2081, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NM_2081)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NM_2082 <- RCP2.6_NM_2100s %>%
  select(Soil.quality, Tave_2082, Tmax_2082, Tmin_2082, GDD.10_2082, 
         PRE_2082, RAD_2082, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NM_2082)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NM_2083 <- RCP2.6_NM_2100s %>%
  select(Soil.quality, Tave_2083, Tmax_2083, Tmin_2083, GDD.10_2083, 
         PRE_2083, RAD_2083, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NM_2083)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NM_2084 <- RCP2.6_NM_2100s %>%
  select(Soil.quality, Tave_2084, Tmax_2084, Tmin_2084, GDD.10_2084, 
         PRE_2084, RAD_2084, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NM_2084)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NM_2085 <- RCP2.6_NM_2100s %>%
  select(Soil.quality, Tave_2085, Tmax_2085, Tmin_2085, GDD.10_2085, 
         PRE_2085, RAD_2085,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NM_2085)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NM_2086 <- RCP2.6_NM_2100s %>%
  select(Soil.quality, Tave_2086, Tmax_2086, Tmin_2086, GDD.10_2086, 
         PRE_2086, RAD_2086, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NM_2086)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NM_2087 <- RCP2.6_NM_2100s %>%
  select(Soil.quality, Tave_2087, Tmax_2087, Tmin_2087, GDD.10_2087, 
         PRE_2087, RAD_2087, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NM_2087)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NM_2088 <- RCP2.6_NM_2100s %>%
  select(Soil.quality, Tave_2088, Tmax_2088, Tmin_2088, GDD.10_2088, 
         PRE_2088, RAD_2088, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NM_2088)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NM_2089 <- RCP2.6_NM_2100s %>%
  select(Soil.quality, Tave_2089, Tmax_2089, Tmin_2089, GDD.10_2089, 
         PRE_2089, RAD_2089, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NM_2089)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NM_2090 <- RCP2.6_NM_2100s %>%
  select(Soil.quality, Tave_2090, Tmax_2090, Tmin_2090, GDD.10_2090, 
         PRE_2090, RAD_2090,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NM_2090)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NM_2091 <- RCP2.6_NM_2100s %>%
  select(Soil.quality, Tave_2091, Tmax_2091, Tmin_2091, GDD.10_2091, 
         PRE_2091, RAD_2091, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NM_2091)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NM_2092 <- RCP2.6_NM_2100s %>%
  select(Soil.quality, Tave_2092, Tmax_2092, Tmin_2092, GDD.10_2092, 
         PRE_2092, RAD_2092, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NM_2092)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NM_2093 <- RCP2.6_NM_2100s %>%
  select(Soil.quality, Tave_2093, Tmax_2093, Tmin_2093, GDD.10_2093, 
         PRE_2093, RAD_2093,Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NM_2093)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NM_2094 <- RCP2.6_NM_2100s %>%
  select(Soil.quality, Tave_2094, Tmax_2094, Tmin_2094, GDD.10_2094, 
         PRE_2094, RAD_2094,Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NM_2094)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NM_2095 <- RCP2.6_NM_2100s %>%
  select(Soil.quality, Tave_2095, Tmax_2095, Tmin_2095, GDD.10_2095, 
         PRE_2095, RAD_2095, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NM_2095)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NM_2096 <- RCP2.6_NM_2100s %>%
  select(Soil.quality, Tave_2096, Tmax_2096, Tmin_2096, GDD.10_2096, 
         PRE_2096, RAD_2096, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NM_2096)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NM_2097 <- RCP2.6_NM_2100s %>%
  select(Soil.quality, Tave_2097, Tmax_2097, Tmin_2097, GDD.10_2097, 
         PRE_2097, RAD_2097,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NM_2097)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NM_2098 <- RCP2.6_NM_2100s %>%
  select(Soil.quality, Tave_2098, Tmax_2098, Tmin_2098, GDD.10_2098, 
         PRE_2098, RAD_2098,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NM_2098)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NM_2099 <- RCP2.6_NM_2100s %>%
  select(Soil.quality, Tave_2099, Tmax_2099, Tmin_2099, GDD.10_2099, 
         PRE_2099, RAD_2099, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NM_2099)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_NM_80.99 <- RCP2.6_NM_2100s %>%
  select(Soil.quality, Tave_2080.2099, Tmax_2080.2099, Tmin_2080.2099, GDD.10_2080.2099, 
         PRE_2080.2099, RAD_2080.2099, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_NM_80.99)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")


###predict yield by GBRT model

RCP2.6_NM_pred_2100s_recyle<-NULL

RCP2.6_NM_pred_2100s <- RCP2.6_NM_2100s %>%
  select(Soil.quality, Crop, NO, Year, Province, 
         NO.Station)

for (i in 1:50) {
  RCP2.6_NM_pred_2100s$Ynpk_pred_2080<-predict(brt_MNCP_recyle[[i]],RCP2.6_NM_2080, n.trees=best_inter_MNCP_recyle[[i]])
  RCP2.6_NM_pred_2100s$Ynpk_pred_2081<-predict(brt_MNCP_recyle[[i]],RCP2.6_NM_2081, n.trees=best_inter_MNCP_recyle[[i]])
  RCP2.6_NM_pred_2100s$Ynpk_pred_2082<-predict(brt_MNCP_recyle[[i]],RCP2.6_NM_2082, n.trees=best_inter_MNCP_recyle[[i]])
  RCP2.6_NM_pred_2100s$Ynpk_pred_2083<-predict(brt_MNCP_recyle[[i]],RCP2.6_NM_2083, n.trees=best_inter_MNCP_recyle[[i]])
  RCP2.6_NM_pred_2100s$Ynpk_pred_2084<-predict(brt_MNCP_recyle[[i]],RCP2.6_NM_2084, n.trees=best_inter_MNCP_recyle[[i]])
  RCP2.6_NM_pred_2100s$Ynpk_pred_2085<-predict(brt_MNCP_recyle[[i]],RCP2.6_NM_2085, n.trees=best_inter_MNCP_recyle[[i]])
  RCP2.6_NM_pred_2100s$Ynpk_pred_2086<-predict(brt_MNCP_recyle[[i]],RCP2.6_NM_2086, n.trees=best_inter_MNCP_recyle[[i]])
  RCP2.6_NM_pred_2100s$Ynpk_pred_2087<-predict(brt_MNCP_recyle[[i]],RCP2.6_NM_2087, n.trees=best_inter_MNCP_recyle[[i]])
  RCP2.6_NM_pred_2100s$Ynpk_pred_2088<-predict(brt_MNCP_recyle[[i]],RCP2.6_NM_2088, n.trees=best_inter_MNCP_recyle[[i]])
  RCP2.6_NM_pred_2100s$Ynpk_pred_2089<-predict(brt_MNCP_recyle[[i]],RCP2.6_NM_2089, n.trees=best_inter_MNCP_recyle[[i]])
  RCP2.6_NM_pred_2100s$Ynpk_pred_2090<-predict(brt_MNCP_recyle[[i]],RCP2.6_NM_2090, n.trees=best_inter_MNCP_recyle[[i]])
  RCP2.6_NM_pred_2100s$Ynpk_pred_2091<-predict(brt_MNCP_recyle[[i]],RCP2.6_NM_2091, n.trees=best_inter_MNCP_recyle[[i]])
  RCP2.6_NM_pred_2100s$Ynpk_pred_2092<-predict(brt_MNCP_recyle[[i]],RCP2.6_NM_2092, n.trees=best_inter_MNCP_recyle[[i]])
  RCP2.6_NM_pred_2100s$Ynpk_pred_2093<-predict(brt_MNCP_recyle[[i]],RCP2.6_NM_2093, n.trees=best_inter_MNCP_recyle[[i]])
  RCP2.6_NM_pred_2100s$Ynpk_pred_2094<-predict(brt_MNCP_recyle[[i]],RCP2.6_NM_2094, n.trees=best_inter_MNCP_recyle[[i]])
  RCP2.6_NM_pred_2100s$Ynpk_pred_2095<-predict(brt_MNCP_recyle[[i]],RCP2.6_NM_2095, n.trees=best_inter_MNCP_recyle[[i]])
  RCP2.6_NM_pred_2100s$Ynpk_pred_2096<-predict(brt_MNCP_recyle[[i]],RCP2.6_NM_2096, n.trees=best_inter_MNCP_recyle[[i]])
  RCP2.6_NM_pred_2100s$Ynpk_pred_2097<-predict(brt_MNCP_recyle[[i]],RCP2.6_NM_2097, n.trees=best_inter_MNCP_recyle[[i]])
  RCP2.6_NM_pred_2100s$Ynpk_pred_2098<-predict(brt_MNCP_recyle[[i]],RCP2.6_NM_2098, n.trees=best_inter_MNCP_recyle[[i]])
  RCP2.6_NM_pred_2100s$Ynpk_pred_2099<-predict(brt_MNCP_recyle[[i]],RCP2.6_NM_2099, n.trees=best_inter_MNCP_recyle[[i]])
  RCP2.6_NM_pred_2100s$Ynpk_pred_80.99<-predict(brt_MNCP_recyle[[i]],RCP2.6_NM_80.99, n.trees=best_inter_MNCP_recyle[[i]])
  
  RCP2.6_NM_pred_2100s$average<- apply(RCP2.6_NM_pred_2100s[,7:26], 1, mean)
  RCP2.6_NM_pred_2100s_recyle[[i]] <- RCP2.6_NM_pred_2100s
}

RCP2.6_NM_pred_2100s_recyle_mean<-RCP2.6_NM_pred_2100s_recyle[[1]]
RCP2.6_NM_pred_2100s_recyle_sd<-RCP2.6_NM_pred_2100s_recyle[[1]]
for(i in 7:28){
  da=as.matrix(as.data.frame(sapply(1:50,function(x){RCP2.6_NM_pred_2100s_recyle[[x]][i]})))
  RCP2.6_NM_pred_2100s_recyle_mean[,i]=rowMeans(da)
  RCP2.6_NM_pred_2100s_recyle_sd[,i]=apply(da,1,sd)
}


###RCP8.5 during 2040-2099

RCP8.5_NM_2050s<-subset(RCP8.5_2050s, Crop == "north maize")
RCP8.5_NM_2040 <- RCP8.5_NM_2050s %>%
  select(Soil.quality, Tave_2040, Tmax_2040, Tmin_2040, GDD.10_2040, 
         PRE_2040, RAD_2040, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NM_2040)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NM_2041 <- RCP8.5_NM_2050s %>%
  select(Soil.quality, Tave_2041, Tmax_2041, Tmin_2041, GDD.10_2041, 
         PRE_2041, RAD_2041, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NM_2041)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NM_2042 <- RCP8.5_NM_2050s %>%
  select(Soil.quality, Tave_2042, Tmax_2042, Tmin_2042, GDD.10_2042, 
         PRE_2042, RAD_2042, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NM_2042)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NM_2043 <- RCP8.5_NM_2050s %>%
  select(Soil.quality, Tave_2043, Tmax_2043, Tmin_2043, GDD.10_2043, 
         PRE_2043, RAD_2043, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NM_2043)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NM_2044 <- RCP8.5_NM_2050s %>%
  select(Soil.quality, Tave_2044, Tmax_2044, Tmin_2044, GDD.10_2044, 
         PRE_2044, RAD_2044, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NM_2044)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NM_2045 <- RCP8.5_NM_2050s %>%
  select(Soil.quality, Tave_2045, Tmax_2045, Tmin_2045, GDD.10_2045, 
         PRE_2045, RAD_2045, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NM_2045)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NM_2046 <- RCP8.5_NM_2050s %>%
  select(Soil.quality, Tave_2046, Tmax_2046, Tmin_2046, GDD.10_2046, 
         PRE_2046, RAD_2046,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NM_2046)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NM_2047 <- RCP8.5_NM_2050s %>%
  select(Soil.quality, Tave_2047, Tmax_2047, Tmin_2047, GDD.10_2047, 
         PRE_2047, RAD_2047, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NM_2047)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NM_2048 <- RCP8.5_NM_2050s %>%
  select(Soil.quality, Tave_2048, Tmax_2048, Tmin_2048, GDD.10_2048, 
         PRE_2048, RAD_2048, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NM_2048)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NM_2049 <- RCP8.5_NM_2050s %>%
  select(Soil.quality, Tave_2049, Tmax_2049, Tmin_2049, GDD.10_2049, 
         PRE_2049, RAD_2049, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NM_2049)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NM_2050 <- RCP8.5_NM_2050s %>%
  select(Soil.quality, Tave_2050, Tmax_2050, Tmin_2050, GDD.10_2050, 
         PRE_2050, RAD_2050, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NM_2050)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NM_2051 <- RCP8.5_NM_2050s %>%
  select(Soil.quality, Tave_2051, Tmax_2051, Tmin_2051, GDD.10_2051, 
         PRE_2051, RAD_2051,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NM_2051)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NM_2052 <- RCP8.5_NM_2050s %>%
  select(Soil.quality, Tave_2052, Tmax_2052, Tmin_2052, GDD.10_2052, 
         PRE_2052, RAD_2052, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NM_2052)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NM_2053 <- RCP8.5_NM_2050s %>%
  select(Soil.quality, Tave_2053, Tmax_2053, Tmin_2053, GDD.10_2053, 
         PRE_2053, RAD_2053, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NM_2053)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NM_2054 <- RCP8.5_NM_2050s %>%
  select(Soil.quality, Tave_2054, Tmax_2054, Tmin_2054, GDD.10_2054, 
         PRE_2054, RAD_2054,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NM_2054)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NM_2055 <- RCP8.5_NM_2050s %>%
  select(Soil.quality, Tave_2055, Tmax_2055, Tmin_2055, GDD.10_2055, 
         PRE_2055, RAD_2055, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NM_2055)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NM_2056 <- RCP8.5_NM_2050s %>%
  select(Soil.quality, Tave_2056, Tmax_2056, Tmin_2056, GDD.10_2056, 
         PRE_2056, RAD_2056, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NM_2056)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NM_2057 <- RCP8.5_NM_2050s %>%
  select(Soil.quality, Tave_2057, Tmax_2057, Tmin_2057, GDD.10_2057, 
         PRE_2057, RAD_2057, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NM_2057)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NM_2058 <- RCP8.5_NM_2050s %>%
  select(Soil.quality, Tave_2058, Tmax_2058, Tmin_2058, GDD.10_2058, 
         PRE_2058, RAD_2058, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NM_2058)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NM_2059 <- RCP8.5_NM_2050s %>%
  select(Soil.quality, Tave_2059, Tmax_2059, Tmin_2059, GDD.10_2059, 
         PRE_2059, RAD_2059, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NM_2059)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NM_40.59 <- RCP8.5_NM_2050s %>%
  select(Soil.quality, Tave_2040.2059, Tmax_2040.2059, Tmin_2040.2059, GDD.10_2040.2059, 
         PRE_2040.2059, RAD_2040.2059,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NM_40.59)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

###predict yield by GBRT model

RCP8.5_NM_pred_2050s_recyle<-NULL

RCP8.5_NM_pred_2050s <- RCP8.5_NM_2050s %>%
  select(Soil.quality, Crop, NO, Year, Province, 
         NO.Station)

for (i in 1:50) {
  RCP8.5_NM_pred_2050s$Ynpk_pred_2040<-predict(brt_MNCP_recyle[[i]],RCP8.5_NM_2040, n.trees=best_inter_MNCP_recyle[[i]])
  RCP8.5_NM_pred_2050s$Ynpk_pred_2041<-predict(brt_MNCP_recyle[[i]],RCP8.5_NM_2041, n.trees=best_inter_MNCP_recyle[[i]])
  RCP8.5_NM_pred_2050s$Ynpk_pred_2042<-predict(brt_MNCP_recyle[[i]],RCP8.5_NM_2042, n.trees=best_inter_MNCP_recyle[[i]])
  RCP8.5_NM_pred_2050s$Ynpk_pred_2043<-predict(brt_MNCP_recyle[[i]],RCP8.5_NM_2043, n.trees=best_inter_MNCP_recyle[[i]])
  RCP8.5_NM_pred_2050s$Ynpk_pred_2044<-predict(brt_MNCP_recyle[[i]],RCP8.5_NM_2044, n.trees=best_inter_MNCP_recyle[[i]])
  RCP8.5_NM_pred_2050s$Ynpk_pred_2045<-predict(brt_MNCP_recyle[[i]],RCP8.5_NM_2045, n.trees=best_inter_MNCP_recyle[[i]])
  RCP8.5_NM_pred_2050s$Ynpk_pred_2046<-predict(brt_MNCP_recyle[[i]],RCP8.5_NM_2046, n.trees=best_inter_MNCP_recyle[[i]])
  RCP8.5_NM_pred_2050s$Ynpk_pred_2047<-predict(brt_MNCP_recyle[[i]],RCP8.5_NM_2047, n.trees=best_inter_MNCP_recyle[[i]])
  RCP8.5_NM_pred_2050s$Ynpk_pred_2048<-predict(brt_MNCP_recyle[[i]],RCP8.5_NM_2048, n.trees=best_inter_MNCP_recyle[[i]])
  RCP8.5_NM_pred_2050s$Ynpk_pred_2049<-predict(brt_MNCP_recyle[[i]],RCP8.5_NM_2049, n.trees=best_inter_MNCP_recyle[[i]])
  RCP8.5_NM_pred_2050s$Ynpk_pred_2050<-predict(brt_MNCP_recyle[[i]],RCP8.5_NM_2050, n.trees=best_inter_MNCP_recyle[[i]])
  RCP8.5_NM_pred_2050s$Ynpk_pred_2051<-predict(brt_MNCP_recyle[[i]],RCP8.5_NM_2051, n.trees=best_inter_MNCP_recyle[[i]])
  RCP8.5_NM_pred_2050s$Ynpk_pred_2052<-predict(brt_MNCP_recyle[[i]],RCP8.5_NM_2052, n.trees=best_inter_MNCP_recyle[[i]])
  RCP8.5_NM_pred_2050s$Ynpk_pred_2053<-predict(brt_MNCP_recyle[[i]],RCP8.5_NM_2053, n.trees=best_inter_MNCP_recyle[[i]])
  RCP8.5_NM_pred_2050s$Ynpk_pred_2054<-predict(brt_MNCP_recyle[[i]],RCP8.5_NM_2054, n.trees=best_inter_MNCP_recyle[[i]])
  RCP8.5_NM_pred_2050s$Ynpk_pred_2055<-predict(brt_MNCP_recyle[[i]],RCP8.5_NM_2055, n.trees=best_inter_MNCP_recyle[[i]])
  RCP8.5_NM_pred_2050s$Ynpk_pred_2056<-predict(brt_MNCP_recyle[[i]],RCP8.5_NM_2056, n.trees=best_inter_MNCP_recyle[[i]])
  RCP8.5_NM_pred_2050s$Ynpk_pred_2057<-predict(brt_MNCP_recyle[[i]],RCP8.5_NM_2057, n.trees=best_inter_MNCP_recyle[[i]])
  RCP8.5_NM_pred_2050s$Ynpk_pred_2058<-predict(brt_MNCP_recyle[[i]],RCP8.5_NM_2058, n.trees=best_inter_MNCP_recyle[[i]])
  RCP8.5_NM_pred_2050s$Ynpk_pred_2059<-predict(brt_MNCP_recyle[[i]],RCP8.5_NM_2059, n.trees=best_inter_MNCP_recyle[[i]])
  RCP8.5_NM_pred_2050s$Ynpk_pred_40.59<-predict(brt_MNCP_recyle[[i]],RCP8.5_NM_40.59, n.trees=best_inter_MNCP_recyle[[i]])
  
  RCP8.5_NM_pred_2050s$average<- apply(RCP8.5_NM_pred_2050s[,7:26], 1, mean)
  RCP8.5_NM_pred_2050s_recyle[[i]] <- RCP8.5_NM_pred_2050s
}

RCP8.5_NM_pred_2050s_recyle_mean<-RCP8.5_NM_pred_2050s_recyle[[1]]
RCP8.5_NM_pred_2050s_recyle_sd<-RCP8.5_NM_pred_2050s_recyle[[1]]
for(i in 7:28){
  da=as.matrix(as.data.frame(sapply(1:50,function(x){RCP8.5_NM_pred_2050s_recyle[[x]][i]})))
  RCP8.5_NM_pred_2050s_recyle_mean[,i]=rowMeans(da)
  RCP8.5_NM_pred_2050s_recyle_sd[,i]=apply(da,1,sd)
}

###RCP8.5 during 2080-2099

RCP8.5_NM_2100s<-subset(RCP8.5_2100s, Crop == "north maize")
RCP8.5_NM_2080 <- RCP8.5_NM_2100s %>%
  select(Soil.quality, Tave_2080, Tmax_2080, Tmin_2080, GDD.10_2080, 
         PRE_2080, RAD_2080,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NM_2080)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NM_2081 <- RCP8.5_NM_2100s %>%
  select(Soil.quality, Tave_2081, Tmax_2081, Tmin_2081, GDD.10_2081, 
         PRE_2081, RAD_2081, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NM_2081)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NM_2082 <- RCP8.5_NM_2100s %>%
  select(Soil.quality, Tave_2082, Tmax_2082, Tmin_2082, GDD.10_2082, 
         PRE_2082, RAD_2082, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NM_2082)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NM_2083 <- RCP8.5_NM_2100s %>%
  select(Soil.quality, Tave_2083, Tmax_2083, Tmin_2083, GDD.10_2083, 
         PRE_2083, RAD_2083, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NM_2083)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NM_2084 <- RCP8.5_NM_2100s %>%
  select(Soil.quality, Tave_2084, Tmax_2084, Tmin_2084, GDD.10_2084, 
         PRE_2084, RAD_2084, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NM_2084)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NM_2085 <- RCP8.5_NM_2100s %>%
  select(Soil.quality, Tave_2085, Tmax_2085, Tmin_2085, GDD.10_2085, 
         PRE_2085, RAD_2085, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NM_2085)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NM_2086 <- RCP8.5_NM_2100s %>%
  select(Soil.quality, Tave_2086, Tmax_2086, Tmin_2086, GDD.10_2086, 
         PRE_2086, RAD_2086,Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NM_2086)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NM_2087 <- RCP8.5_NM_2100s %>%
  select(Soil.quality, Tave_2087, Tmax_2087, Tmin_2087, GDD.10_2087, 
         PRE_2087, RAD_2087, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NM_2087)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NM_2088 <- RCP8.5_NM_2100s %>%
  select(Soil.quality, Tave_2088, Tmax_2088, Tmin_2088, GDD.10_2088, 
         PRE_2088, RAD_2088, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NM_2088)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NM_2089 <- RCP8.5_NM_2100s %>%
  select(Soil.quality, Tave_2089, Tmax_2089, Tmin_2089, GDD.10_2089, 
         PRE_2089, RAD_2089, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NM_2089)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NM_2090 <- RCP8.5_NM_2100s %>%
  select(Soil.quality, Tave_2090, Tmax_2090, Tmin_2090, GDD.10_2090, 
         PRE_2090, RAD_2090, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NM_2090)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NM_2091 <- RCP8.5_NM_2100s %>%
  select(Soil.quality, Tave_2091, Tmax_2091, Tmin_2091, GDD.10_2091, 
         PRE_2091, RAD_2091,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NM_2091)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NM_2092 <- RCP8.5_NM_2100s %>%
  select(Soil.quality, Tave_2092, Tmax_2092, Tmin_2092, GDD.10_2092, 
         PRE_2092, RAD_2092, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NM_2092)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NM_2093 <- RCP8.5_NM_2100s %>%
  select(Soil.quality, Tave_2093, Tmax_2093, Tmin_2093, GDD.10_2093, 
         PRE_2093, RAD_2093, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NM_2093)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NM_2094 <- RCP8.5_NM_2100s %>%
  select(Soil.quality, Tave_2094, Tmax_2094, Tmin_2094, GDD.10_2094, 
         PRE_2094, RAD_2094, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NM_2094)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NM_2095 <- RCP8.5_NM_2100s %>%
  select(Soil.quality, Tave_2095, Tmax_2095, Tmin_2095, GDD.10_2095, 
         PRE_2095, RAD_2095, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NM_2095)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NM_2096 <- RCP8.5_NM_2100s %>%
  select(Soil.quality, Tave_2096, Tmax_2096, Tmin_2096, GDD.10_2096, 
         PRE_2096, RAD_2096,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NM_2096)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NM_2097 <- RCP8.5_NM_2100s %>%
  select(Soil.quality, Tave_2097, Tmax_2097, Tmin_2097, GDD.10_2097, 
         PRE_2097, RAD_2097, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NM_2097)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NM_2098 <- RCP8.5_NM_2100s %>%
  select(Soil.quality, Tave_2098, Tmax_2098, Tmin_2098, GDD.10_2098, 
         PRE_2098, RAD_2098, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NM_2098)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NM_2099 <- RCP8.5_NM_2100s %>%
  select(Soil.quality, Tave_2099, Tmax_2099, Tmin_2099, GDD.10_2099, 
         PRE_2099, RAD_2099, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NM_2099)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_NM_80.99 <- RCP8.5_NM_2100s %>%
  select(Soil.quality, Tave_2080.2099, Tmax_2080.2099, Tmin_2080.2099, GDD.10_2080.2099, 
         PRE_2080.2099, RAD_2080.2099, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_NM_80.99)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")


###predict yield by GBRT model
RCP8.5_NM_pred_2100s_recyle<-NULL

RCP8.5_NM_pred_2100s <- RCP8.5_NM_2100s %>%
  select(Soil.quality, Crop, NO, Year, Province, 
         NO.Station)

for (i in 1:50) {
  RCP8.5_NM_pred_2100s$Ynpk_pred_2080<-predict(brt_MNCP_recyle[[i]],RCP8.5_NM_2080, n.trees=best_inter_MNCP_recyle[[i]])
  RCP8.5_NM_pred_2100s$Ynpk_pred_2081<-predict(brt_MNCP_recyle[[i]],RCP8.5_NM_2081, n.trees=best_inter_MNCP_recyle[[i]])
  RCP8.5_NM_pred_2100s$Ynpk_pred_2082<-predict(brt_MNCP_recyle[[i]],RCP8.5_NM_2082, n.trees=best_inter_MNCP_recyle[[i]])
  RCP8.5_NM_pred_2100s$Ynpk_pred_2083<-predict(brt_MNCP_recyle[[i]],RCP8.5_NM_2083, n.trees=best_inter_MNCP_recyle[[i]])
  RCP8.5_NM_pred_2100s$Ynpk_pred_2084<-predict(brt_MNCP_recyle[[i]],RCP8.5_NM_2084, n.trees=best_inter_MNCP_recyle[[i]])
  RCP8.5_NM_pred_2100s$Ynpk_pred_2085<-predict(brt_MNCP_recyle[[i]],RCP8.5_NM_2085, n.trees=best_inter_MNCP_recyle[[i]])
  RCP8.5_NM_pred_2100s$Ynpk_pred_2086<-predict(brt_MNCP_recyle[[i]],RCP8.5_NM_2086, n.trees=best_inter_MNCP_recyle[[i]])
  RCP8.5_NM_pred_2100s$Ynpk_pred_2087<-predict(brt_MNCP_recyle[[i]],RCP8.5_NM_2087, n.trees=best_inter_MNCP_recyle[[i]])
  RCP8.5_NM_pred_2100s$Ynpk_pred_2088<-predict(brt_MNCP_recyle[[i]],RCP8.5_NM_2088, n.trees=best_inter_MNCP_recyle[[i]])
  RCP8.5_NM_pred_2100s$Ynpk_pred_2089<-predict(brt_MNCP_recyle[[i]],RCP8.5_NM_2089, n.trees=best_inter_MNCP_recyle[[i]])
  RCP8.5_NM_pred_2100s$Ynpk_pred_2090<-predict(brt_MNCP_recyle[[i]],RCP8.5_NM_2090, n.trees=best_inter_MNCP_recyle[[i]])
  RCP8.5_NM_pred_2100s$Ynpk_pred_2091<-predict(brt_MNCP_recyle[[i]],RCP8.5_NM_2091, n.trees=best_inter_MNCP_recyle[[i]])
  RCP8.5_NM_pred_2100s$Ynpk_pred_2092<-predict(brt_MNCP_recyle[[i]],RCP8.5_NM_2092, n.trees=best_inter_MNCP_recyle[[i]])
  RCP8.5_NM_pred_2100s$Ynpk_pred_2093<-predict(brt_MNCP_recyle[[i]],RCP8.5_NM_2093, n.trees=best_inter_MNCP_recyle[[i]])
  RCP8.5_NM_pred_2100s$Ynpk_pred_2094<-predict(brt_MNCP_recyle[[i]],RCP8.5_NM_2094, n.trees=best_inter_MNCP_recyle[[i]])
  RCP8.5_NM_pred_2100s$Ynpk_pred_2095<-predict(brt_MNCP_recyle[[i]],RCP8.5_NM_2095, n.trees=best_inter_MNCP_recyle[[i]])
  RCP8.5_NM_pred_2100s$Ynpk_pred_2096<-predict(brt_MNCP_recyle[[i]],RCP8.5_NM_2096, n.trees=best_inter_MNCP_recyle[[i]])
  RCP8.5_NM_pred_2100s$Ynpk_pred_2097<-predict(brt_MNCP_recyle[[i]],RCP8.5_NM_2097, n.trees=best_inter_MNCP_recyle[[i]])
  RCP8.5_NM_pred_2100s$Ynpk_pred_2098<-predict(brt_MNCP_recyle[[i]],RCP8.5_NM_2098, n.trees=best_inter_MNCP_recyle[[i]])
  RCP8.5_NM_pred_2100s$Ynpk_pred_2099<-predict(brt_MNCP_recyle[[i]],RCP8.5_NM_2099, n.trees=best_inter_MNCP_recyle[[i]])
  RCP8.5_NM_pred_2100s$Ynpk_pred_80.99<-predict(brt_MNCP_recyle[[i]],RCP8.5_NM_80.99, n.trees=best_inter_MNCP_recyle[[i]])
  
  RCP8.5_NM_pred_2100s$average<- apply(RCP8.5_NM_pred_2100s[,7:26], 1, mean)
  RCP8.5_NM_pred_2100s_recyle[[i]] <- RCP8.5_NM_pred_2100s
}

RCP8.5_NM_pred_2100s_recyle_mean<-RCP8.5_NM_pred_2100s_recyle[[1]]
RCP8.5_NM_pred_2100s_recyle_sd<-RCP8.5_NM_pred_2100s_recyle[[1]]
for(i in 7:28){
  da=as.matrix(as.data.frame(sapply(1:50,function(x){RCP8.5_NM_pred_2100s_recyle[[x]][i]})))
  RCP8.5_NM_pred_2100s_recyle_mean[,i]=rowMeans(da)
  RCP8.5_NM_pred_2100s_recyle_sd[,i]=apply(da,1,sd)
}

###summarise predicted yield

sum_RCP2.6_NM_2050s_year<- cbind(base_NM_pred_all[,c("Soil.quality","Crop","NO","average")],RCP2.6_NM_pred_2050s[,7:28])
names(sum_RCP2.6_NM_2050s_year)[1:4]<- c("Soil quality", "Crop", "No", "Yield_base")
names(sum_RCP2.6_NM_2050s_year)[26]<- "Yield_RCP"
sum_RCP2.6_NM_2050s_year$Yield_change <- sum_RCP2.6_NM_2050s_year$`Yield_RCP` - sum_RCP2.6_NM_2050s_year$`Yield_base`
sum_RCP2.6_NM_2050s_year$Yield_change_per <- (sum_RCP2.6_NM_2050s_year$Yield_change / sum_RCP2.6_NM_2050s_year$Yield_base)*100
sum_RCP2.6_NM_2050s_year$RCP <- "RCP2.6"
sum_RCP2.6_NM_2050s_year$Time <- "2050"

sum_RCP8.5_NM_2050s_year<- cbind(base_NM_pred_all[,c("Soil.quality","Crop","NO","average")],RCP8.5_NM_pred_2050s[,7:28])
names(sum_RCP8.5_NM_2050s_year)[1:4]<- c("Soil quality", "Crop", "No", "Yield_base")
names(sum_RCP8.5_NM_2050s_year)[26]<- "Yield_RCP"
sum_RCP8.5_NM_2050s_year$Yield_change <- sum_RCP8.5_NM_2050s_year$`Yield_RCP` - sum_RCP8.5_NM_2050s_year$`Yield_base`
sum_RCP8.5_NM_2050s_year$Yield_change_per <- (sum_RCP8.5_NM_2050s_year$Yield_change / sum_RCP8.5_NM_2050s_year$Yield_base)*100
sum_RCP8.5_NM_2050s_year$RCP <- "RCP8.5"
sum_RCP8.5_NM_2050s_year$Time <- "2050"

sum_RCP2.6_NM_2100s_year<- cbind(base_NM_pred_all[,c("Soil.quality","Crop","NO","average")],RCP2.6_NM_pred_2100s[,7:28])
names(sum_RCP2.6_NM_2100s_year)[1:4]<- c("Soil quality", "Crop", "No", "Yield_base")
names(sum_RCP2.6_NM_2100s_year)[26]<- "Yield_RCP"
sum_RCP2.6_NM_2100s_year$Yield_change <- sum_RCP2.6_NM_2100s_year$`Yield_RCP` - sum_RCP2.6_NM_2100s_year$`Yield_base`
sum_RCP2.6_NM_2100s_year$Yield_change_per <- (sum_RCP2.6_NM_2100s_year$Yield_change / sum_RCP2.6_NM_2100s_year$Yield_base)*100
sum_RCP2.6_NM_2100s_year$RCP <- "RCP2.6"
sum_RCP2.6_NM_2100s_year$Time <- "2100"

sum_RCP8.5_NM_2100s_year<- cbind(base_NM_pred_all[,c("Soil.quality","Crop","NO","average")],RCP8.5_NM_pred_2100s[,7:28])
names(sum_RCP8.5_NM_2100s_year)[1:4]<- c("Soil quality", "Crop", "No", "Yield_base")
names(sum_RCP8.5_NM_2100s_year)[26]<- "Yield_RCP"
sum_RCP8.5_NM_2100s_year$Yield_change <- sum_RCP8.5_NM_2100s_year$`Yield_RCP` - sum_RCP8.5_NM_2100s_year$`Yield_base`
sum_RCP8.5_NM_2100s_year$Yield_change_per <- (sum_RCP8.5_NM_2100s_year$Yield_change / sum_RCP8.5_NM_2100s_year$Yield_base)*100
sum_RCP8.5_NM_2100s_year$RCP <- "RCP8.5"
sum_RCP8.5_NM_2100s_year$Time <- "2100"

sum_RCP_NM_2050s_year <- rbind(sum_RCP2.6_NM_2050s_year,sum_RCP8.5_NM_2050s_year)
sum_RCP_NM_2100s_year<-rbind(sum_RCP2.6_NM_2100s_year,sum_RCP8.5_NM_2100s_year)

write.csv(sum_RCP_NM_2050s_year,"./results/sum_RCP_NM_2050s_year.csv")
write.csv(sum_RCP_NM_2100s_year,"./results/sum_RCP_NM_2100s_year.csv")

###comparison of yield change between high and low quality soils 
###T test

t.test(Yield_change~`Soil quality` ,sum_RCP2.6_NM_2050s_year) #RCP2.6-2050s
t.test(Yield_change~`Soil quality` ,sum_RCP8.5_NM_2050s_year) #RCP8.5-2050s
t.test(Yield_change~`Soil quality` ,sum_RCP2.6_NM_2100s_year) #RCP2.6-2100s
t.test(Yield_change~`Soil quality` ,sum_RCP8.5_NM_2100s_year) #RCP8.5-2100s

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
}

#####Predicted yield under different climate scenarios
###baseline period 1986-2005
base_SM<-subset(base, Crop == "south maize")
base_SM_1986 <- base_SM %>%
  select(Soil.quality, Tave_1986, Tmax_1986, Tmin_1986, GDD.10_1986, 
         PRE_1986, RAD_1986, Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_SM_1986)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_SM_1987 <- base_SM %>%
  select(Soil.quality, Tave_1987, Tmax_1987, Tmin_1987, GDD.10_1987, 
         PRE_1987, RAD_1987, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_SM_1987)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_SM_1988 <- base_SM %>%
  select(Soil.quality, Tave_1988, Tmax_1988, Tmin_1988, GDD.10_1988, 
         PRE_1988, RAD_1988, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_SM_1988)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_SM_1989 <- base_SM %>%
  select(Soil.quality, Tave_1989, Tmax_1989, Tmin_1989, GDD.10_1989, 
         PRE_1989, RAD_1989, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_SM_1989)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_SM_1990 <- base_SM %>%
  select(Soil.quality, Tave_1990, Tmax_1990, Tmin_1990, GDD.10_1990, 
         PRE_1990, RAD_1990, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_SM_1990)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_SM_1991 <- base_SM %>%
  select(Soil.quality, Tave_1991, Tmax_1991, Tmin_1991, GDD.10_1991, 
         PRE_1991, RAD_1991, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_SM_1991)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_SM_1992 <- base_SM %>%
  select(Soil.quality, Tave_1992, Tmax_1992, Tmin_1992, GDD.10_1992, 
         PRE_1992, RAD_1992,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_SM_1992)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_SM_1993 <- base_SM %>%
  select(Soil.quality, Tave_1993, Tmax_1993, Tmin_1993, GDD.10_1993, 
         PRE_1993, RAD_1993, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_SM_1993)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_SM_1994 <- base_SM %>%
  select(Soil.quality, Tave_1994, Tmax_1994, Tmin_1994, GDD.10_1994, 
         PRE_1994, RAD_1994, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_SM_1994)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_SM_1995 <- base_SM %>%
  select(Soil.quality, Tave_1995, Tmax_1995, Tmin_1995, GDD.10_1995, 
         PRE_1995, RAD_1995, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_SM_1995)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_SM_1996 <- base_SM %>%
  select(Soil.quality, Tave_1996, Tmax_1996, Tmin_1996, GDD.10_1996, 
         PRE_1996, RAD_1996, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_SM_1996)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_SM_1997 <- base_SM %>%
  select(Soil.quality, Tave_1997, Tmax_1997, Tmin_1997, GDD.10_1997, 
         PRE_1997, RAD_1997, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_SM_1997)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_SM_1998 <- base_SM %>%
  select(Soil.quality, Tave_1998, Tmax_1998, Tmin_1998, GDD.10_1998, 
         PRE_1998, RAD_1998, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_SM_1998)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_SM_1999 <- base_SM %>%
  select(Soil.quality, Tave_1999, Tmax_1999, Tmin_1999, GDD.10_1999, 
         PRE_1999, RAD_1999, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_SM_1999)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_SM_2000 <- base_SM %>%
  select(Soil.quality, Tave_2000, Tmax_2000, Tmin_2000, GDD.10_2000, 
         PRE_2000, RAD_2000, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_SM_2000)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_SM_2001 <- base_SM %>%
  select(Soil.quality, Tave_2001, Tmax_2001, Tmin_2001, GDD.10_2001, 
         PRE_2001, RAD_2001, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_SM_2001)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_SM_2002 <- base_SM %>%
  select(Soil.quality, Tave_2002, Tmax_2002, Tmin_2002, GDD.10_2002, 
         PRE_2002, RAD_2002, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_SM_2002)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_SM_2003 <- base_SM %>%
  select(Soil.quality, Tave_2003, Tmax_2003, Tmin_2003, GDD.10_2003, 
         PRE_2003, RAD_2003, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_SM_2003)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_SM_2004 <- base_SM %>%
  select(Soil.quality, Tave_2004, Tmax_2004, Tmin_2004, GDD.10_2004, 
         PRE_2004, RAD_2004, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_SM_2004)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_SM_2005 <- base_SM %>%
  select(Soil.quality, Tave_2005, Tmax_2005, Tmin_2005, GDD.10_2005, 
         PRE_2005, RAD_2005, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_SM_2005)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_SM_86.05 <- base_SM %>%
  select(Soil.quality, Tave_1986.2005, Tmax_1986.2005, Tmin_1986.2005, GDD.10_1986.2005, 
         PRE_1986.2005, RAD_1986.2005, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_SM_86.05)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                         "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                         "N", "P2O5", "K2O", "Yck", "Ynpk")

###predict yield by GBRT model

base_SM_pred_all_recyle<-NULL

base_SM_pred_all <- base_SM %>%
  select(Soil.quality, Crop, NO, Year,Province, 
         NO.Station)

for (i in 1:50) {
  base_SM_pred_all$Ynpk_pred_1986<-predict(brt_MSWC_recyle[[i]],base_SM_1986, n.trees=best_inter_MSWC_recyle[[i]])
  base_SM_pred_all$Ynpk_pred_1987<-predict(brt_MSWC_recyle[[i]],base_SM_1987, n.trees=best_inter_MSWC_recyle[[i]])
  base_SM_pred_all$Ynpk_pred_1988<-predict(brt_MSWC_recyle[[i]],base_SM_1988, n.trees=best_inter_MSWC_recyle[[i]])
  base_SM_pred_all$Ynpk_pred_1989<-predict(brt_MSWC_recyle[[i]],base_SM_1989, n.trees=best_inter_MSWC_recyle[[i]])
  base_SM_pred_all$Ynpk_pred_1990<-predict(brt_MSWC_recyle[[i]],base_SM_1990, n.trees=best_inter_MSWC_recyle[[i]])
  base_SM_pred_all$Ynpk_pred_1991<-predict(brt_MSWC_recyle[[i]],base_SM_1991, n.trees=best_inter_MSWC_recyle[[i]])
  base_SM_pred_all$Ynpk_pred_1992<-predict(brt_MSWC_recyle[[i]],base_SM_1992, n.trees=best_inter_MSWC_recyle[[i]])
  base_SM_pred_all$Ynpk_pred_1993<-predict(brt_MSWC_recyle[[i]],base_SM_1993, n.trees=best_inter_MSWC_recyle[[i]])
  base_SM_pred_all$Ynpk_pred_1994<-predict(brt_MSWC_recyle[[i]],base_SM_1994, n.trees=best_inter_MSWC_recyle[[i]])
  base_SM_pred_all$Ynpk_pred_1995<-predict(brt_MSWC_recyle[[i]],base_SM_1995, n.trees=best_inter_MSWC_recyle[[i]])
  base_SM_pred_all$Ynpk_pred_1996<-predict(brt_MSWC_recyle[[i]],base_SM_1996, n.trees=best_inter_MSWC_recyle[[i]])
  base_SM_pred_all$Ynpk_pred_1997<-predict(brt_MSWC_recyle[[i]],base_SM_1997, n.trees=best_inter_MSWC_recyle[[i]])
  base_SM_pred_all$Ynpk_pred_1998<-predict(brt_MSWC_recyle[[i]],base_SM_1998, n.trees=best_inter_MSWC_recyle[[i]])
  base_SM_pred_all$Ynpk_pred_1999<-predict(brt_MSWC_recyle[[i]],base_SM_1999, n.trees=best_inter_MSWC_recyle[[i]])
  base_SM_pred_all$Ynpk_pred_2000<-predict(brt_MSWC_recyle[[i]],base_SM_2000, n.trees=best_inter_MSWC_recyle[[i]])
  base_SM_pred_all$Ynpk_pred_2001<-predict(brt_MSWC_recyle[[i]],base_SM_2001, n.trees=best_inter_MSWC_recyle[[i]])
  base_SM_pred_all$Ynpk_pred_2002<-predict(brt_MSWC_recyle[[i]],base_SM_2002, n.trees=best_inter_MSWC_recyle[[i]])
  base_SM_pred_all$Ynpk_pred_2003<-predict(brt_MSWC_recyle[[i]],base_SM_2003, n.trees=best_inter_MSWC_recyle[[i]])
  base_SM_pred_all$Ynpk_pred_2004<-predict(brt_MSWC_recyle[[i]],base_SM_2004, n.trees=best_inter_MSWC_recyle[[i]])
  base_SM_pred_all$Ynpk_pred_2005<-predict(brt_MSWC_recyle[[i]],base_SM_2005, n.trees=best_inter_MSWC_recyle[[i]])
  base_SM_pred_all$Ynpk_pred_86.05<-predict(brt_MSWC_recyle[[i]],base_SM_86.05, n.trees=best_inter_MSWC_recyle[[i]])
  
  base_SM_pred_all$average<- apply(base_SM_pred_all[,7:26], 1, mean)
  base_SM_pred_all_recyle[[i]] <- base_SM_pred_all
}

base_SM_pred_all_recyle_mean<-base_SM_pred_all_recyle[[1]]
base_SM_pred_all_recyle_sd<-base_SM_pred_all_recyle[[1]]
for(i in 7:28){
  da=as.matrix(as.data.frame(sapply(1:50,function(x){base_SM_pred_all_recyle[[x]][i]})))
  base_SM_pred_all_recyle_mean[,i]=rowMeans(da)
  base_SM_pred_all_recyle_sd[,i]=apply(da,1,sd)
}

###RCP2.6 during 2040-2059

RCP2.6_SM_2050s<-subset(RCP2.6_2050s, Crop == "south maize")
RCP2.6_SM_2040 <- RCP2.6_SM_2050s %>%
  select(Soil.quality, Tave_2040, Tmax_2040, Tmin_2040, GDD.10_2040, 
         PRE_2040, RAD_2040, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SM_2040)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SM_2041 <- RCP2.6_SM_2050s %>%
  select(Soil.quality, Tave_2041, Tmax_2041, Tmin_2041, GDD.10_2041, 
         PRE_2041, RAD_2041, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SM_2041)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SM_2042 <- RCP2.6_SM_2050s %>%
  select(Soil.quality, Tave_2042, Tmax_2042, Tmin_2042, GDD.10_2042, 
         PRE_2042, RAD_2042, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SM_2042)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SM_2043 <- RCP2.6_SM_2050s %>%
  select(Soil.quality, Tave_2043, Tmax_2043, Tmin_2043, GDD.10_2043, 
         PRE_2043, RAD_2043, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SM_2043)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SM_2044 <- RCP2.6_SM_2050s %>%
  select(Soil.quality, Tave_2044, Tmax_2044, Tmin_2044, GDD.10_2044, 
         PRE_2044, RAD_2044, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SM_2044)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SM_2045 <- RCP2.6_SM_2050s %>%
  select(Soil.quality, Tave_2045, Tmax_2045, Tmin_2045, GDD.10_2045, 
         PRE_2045, RAD_2045, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SM_2045)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SM_2046 <- RCP2.6_SM_2050s %>%
  select(Soil.quality, Tave_2046, Tmax_2046, Tmin_2046, GDD.10_2046, 
         PRE_2046, RAD_2046, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SM_2046)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SM_2047 <- RCP2.6_SM_2050s %>%
  select(Soil.quality, Tave_2047, Tmax_2047, Tmin_2047, GDD.10_2047, 
         PRE_2047, RAD_2047, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SM_2047)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SM_2048 <- RCP2.6_SM_2050s %>%
  select(Soil.quality, Tave_2048, Tmax_2048, Tmin_2048, GDD.10_2048, 
         PRE_2048, RAD_2048, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SM_2048)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SM_2049 <- RCP2.6_SM_2050s %>%
  select(Soil.quality, Tave_2049, Tmax_2049, Tmin_2049, GDD.10_2049, 
         PRE_2049, RAD_2049, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SM_2049)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SM_2050 <- RCP2.6_SM_2050s %>%
  select(Soil.quality, Tave_2050, Tmax_2050, Tmin_2050, GDD.10_2050, 
         PRE_2050, RAD_2050, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SM_2050)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SM_2051 <- RCP2.6_SM_2050s %>%
  select(Soil.quality, Tave_2051, Tmax_2051, Tmin_2051, GDD.10_2051, 
         PRE_2051, RAD_2051, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SM_2051)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SM_2052 <- RCP2.6_SM_2050s %>%
  select(Soil.quality, Tave_2052, Tmax_2052, Tmin_2052, GDD.10_2052, 
         PRE_2052, RAD_2052, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SM_2052)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SM_2053 <- RCP2.6_SM_2050s %>%
  select(Soil.quality, Tave_2053, Tmax_2053, Tmin_2053, GDD.10_2053, 
         PRE_2053, RAD_2053, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SM_2053)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SM_2054 <- RCP2.6_SM_2050s %>%
  select(Soil.quality, Tave_2054, Tmax_2054, Tmin_2054, GDD.10_2054, 
         PRE_2054, RAD_2054, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SM_2054)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SM_2055 <- RCP2.6_SM_2050s %>%
  select(Soil.quality, Tave_2055, Tmax_2055, Tmin_2055, GDD.10_2055, 
         PRE_2055, RAD_2055, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SM_2055)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SM_2056 <- RCP2.6_SM_2050s %>%
  select(Soil.quality, Tave_2056, Tmax_2056, Tmin_2056, GDD.10_2056, 
         PRE_2056, RAD_2056, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SM_2056)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SM_2057 <- RCP2.6_SM_2050s %>%
  select(Soil.quality, Tave_2057, Tmax_2057, Tmin_2057, GDD.10_2057, 
         PRE_2057, RAD_2057, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SM_2057)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SM_2058 <- RCP2.6_SM_2050s %>%
  select(Soil.quality, Tave_2058, Tmax_2058, Tmin_2058, GDD.10_2058, 
         PRE_2058, RAD_2058, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SM_2058)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SM_2059 <- RCP2.6_SM_2050s %>%
  select(Soil.quality, Tave_2059, Tmax_2059, Tmin_2059, GDD.10_2059, 
         PRE_2059, RAD_2059, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SM_2059)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SM_40.59 <- RCP2.6_SM_2050s %>%
  select(Soil.quality, Tave_2040.2059, Tmax_2040.2059, Tmin_2040.2059, GDD.10_2040.2059, 
         PRE_2040.2059, RAD_2040.2059, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SM_40.59)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

###predict yield by GBRT model

RCP2.6_SM_pred_2050s_recyle<-NULL

RCP2.6_SM_pred_2050s <- RCP2.6_SM_2050s %>%
  select(Soil.quality, Crop, NO, Year, Province, 
         NO.Station)

for (i in 1:50) {
  RCP2.6_SM_pred_2050s$Ynpk_pred_2040<-predict(brt_MSWC_recyle[[i]],RCP2.6_SM_2040, n.trees=best_inter_MSWC_recyle[[i]])
  RCP2.6_SM_pred_2050s$Ynpk_pred_2041<-predict(brt_MSWC_recyle[[i]],RCP2.6_SM_2041, n.trees=best_inter_MSWC_recyle[[i]])
  RCP2.6_SM_pred_2050s$Ynpk_pred_2042<-predict(brt_MSWC_recyle[[i]],RCP2.6_SM_2042, n.trees=best_inter_MSWC_recyle[[i]])
  RCP2.6_SM_pred_2050s$Ynpk_pred_2043<-predict(brt_MSWC_recyle[[i]],RCP2.6_SM_2043, n.trees=best_inter_MSWC_recyle[[i]])
  RCP2.6_SM_pred_2050s$Ynpk_pred_2044<-predict(brt_MSWC_recyle[[i]],RCP2.6_SM_2044, n.trees=best_inter_MSWC_recyle[[i]])
  RCP2.6_SM_pred_2050s$Ynpk_pred_2045<-predict(brt_MSWC_recyle[[i]],RCP2.6_SM_2045, n.trees=best_inter_MSWC_recyle[[i]])
  RCP2.6_SM_pred_2050s$Ynpk_pred_2046<-predict(brt_MSWC_recyle[[i]],RCP2.6_SM_2046, n.trees=best_inter_MSWC_recyle[[i]])
  RCP2.6_SM_pred_2050s$Ynpk_pred_2047<-predict(brt_MSWC_recyle[[i]],RCP2.6_SM_2047, n.trees=best_inter_MSWC_recyle[[i]])
  RCP2.6_SM_pred_2050s$Ynpk_pred_2048<-predict(brt_MSWC_recyle[[i]],RCP2.6_SM_2048, n.trees=best_inter_MSWC_recyle[[i]])
  RCP2.6_SM_pred_2050s$Ynpk_pred_2049<-predict(brt_MSWC_recyle[[i]],RCP2.6_SM_2049, n.trees=best_inter_MSWC_recyle[[i]])
  RCP2.6_SM_pred_2050s$Ynpk_pred_2050<-predict(brt_MSWC_recyle[[i]],RCP2.6_SM_2050, n.trees=best_inter_MSWC_recyle[[i]])
  RCP2.6_SM_pred_2050s$Ynpk_pred_2051<-predict(brt_MSWC_recyle[[i]],RCP2.6_SM_2051, n.trees=best_inter_MSWC_recyle[[i]])
  RCP2.6_SM_pred_2050s$Ynpk_pred_2052<-predict(brt_MSWC_recyle[[i]],RCP2.6_SM_2052, n.trees=best_inter_MSWC_recyle[[i]])
  RCP2.6_SM_pred_2050s$Ynpk_pred_2053<-predict(brt_MSWC_recyle[[i]],RCP2.6_SM_2053, n.trees=best_inter_MSWC_recyle[[i]])
  RCP2.6_SM_pred_2050s$Ynpk_pred_2054<-predict(brt_MSWC_recyle[[i]],RCP2.6_SM_2054, n.trees=best_inter_MSWC_recyle[[i]])
  RCP2.6_SM_pred_2050s$Ynpk_pred_2055<-predict(brt_MSWC_recyle[[i]],RCP2.6_SM_2055, n.trees=best_inter_MSWC_recyle[[i]])
  RCP2.6_SM_pred_2050s$Ynpk_pred_2056<-predict(brt_MSWC_recyle[[i]],RCP2.6_SM_2056, n.trees=best_inter_MSWC_recyle[[i]])
  RCP2.6_SM_pred_2050s$Ynpk_pred_2057<-predict(brt_MSWC_recyle[[i]],RCP2.6_SM_2057, n.trees=best_inter_MSWC_recyle[[i]])
  RCP2.6_SM_pred_2050s$Ynpk_pred_2058<-predict(brt_MSWC_recyle[[i]],RCP2.6_SM_2058, n.trees=best_inter_MSWC_recyle[[i]])
  RCP2.6_SM_pred_2050s$Ynpk_pred_2059<-predict(brt_MSWC_recyle[[i]],RCP2.6_SM_2059, n.trees=best_inter_MSWC_recyle[[i]])
  RCP2.6_SM_pred_2050s$Ynpk_pred_40.59<-predict(brt_MSWC_recyle[[i]],RCP2.6_SM_40.59, n.trees=best_inter_MSWC_recyle[[i]])
  
  RCP2.6_SM_pred_2050s$average<- apply(RCP2.6_SM_pred_2050s[,7:26], 1, mean)
  RCP2.6_SM_pred_2050s_recyle[[i]] <- RCP2.6_SM_pred_2050s
}

RCP2.6_SM_pred_2050s_recyle_mean<-RCP2.6_SM_pred_2050s_recyle[[1]]
RCP2.6_SM_pred_2050s_recyle_sd<-RCP2.6_SM_pred_2050s_recyle[[1]]
for(i in 7:28){
  da=as.matrix(as.data.frame(sapply(1:50,function(x){RCP2.6_SM_pred_2050s_recyle[[x]][i]})))
  RCP2.6_SM_pred_2050s_recyle_mean[,i]=rowMeans(da)
  RCP2.6_SM_pred_2050s_recyle_sd[,i]=apply(da,1,sd)
}

###RCP2.6 during 2080-2099

RCP2.6_SM_2100s<-subset(RCP2.6_2100s, Crop == "south maize")
RCP2.6_SM_2080 <- RCP2.6_SM_2100s %>%
  select(Soil.quality, Tave_2080, Tmax_2080, Tmin_2080, GDD.10_2080, 
         PRE_2080, RAD_2080,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SM_2080)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SM_2081 <- RCP2.6_SM_2100s %>%
  select(Soil.quality, Tave_2081, Tmax_2081, Tmin_2081, GDD.10_2081, 
         PRE_2081, RAD_2081, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SM_2081)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SM_2082 <- RCP2.6_SM_2100s %>%
  select(Soil.quality, Tave_2082, Tmax_2082, Tmin_2082, GDD.10_2082, 
         PRE_2082, RAD_2082, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SM_2082)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SM_2083 <- RCP2.6_SM_2100s %>%
  select(Soil.quality, Tave_2083, Tmax_2083, Tmin_2083, GDD.10_2083, 
         PRE_2083, RAD_2083, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SM_2083)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SM_2084 <- RCP2.6_SM_2100s %>%
  select(Soil.quality, Tave_2084, Tmax_2084, Tmin_2084, GDD.10_2084, 
         PRE_2084, RAD_2084, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SM_2084)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SM_2085 <- RCP2.6_SM_2100s %>%
  select(Soil.quality, Tave_2085, Tmax_2085, Tmin_2085, GDD.10_2085, 
         PRE_2085, RAD_2085,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SM_2085)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SM_2086 <- RCP2.6_SM_2100s %>%
  select(Soil.quality, Tave_2086, Tmax_2086, Tmin_2086, GDD.10_2086, 
         PRE_2086, RAD_2086, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SM_2086)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SM_2087 <- RCP2.6_SM_2100s %>%
  select(Soil.quality, Tave_2087, Tmax_2087, Tmin_2087, GDD.10_2087, 
         PRE_2087, RAD_2087, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SM_2087)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SM_2088 <- RCP2.6_SM_2100s %>%
  select(Soil.quality, Tave_2088, Tmax_2088, Tmin_2088, GDD.10_2088, 
         PRE_2088, RAD_2088, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SM_2088)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SM_2089 <- RCP2.6_SM_2100s %>%
  select(Soil.quality, Tave_2089, Tmax_2089, Tmin_2089, GDD.10_2089, 
         PRE_2089, RAD_2089, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SM_2089)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SM_2090 <- RCP2.6_SM_2100s %>%
  select(Soil.quality, Tave_2090, Tmax_2090, Tmin_2090, GDD.10_2090, 
         PRE_2090, RAD_2090,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SM_2090)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SM_2091 <- RCP2.6_SM_2100s %>%
  select(Soil.quality, Tave_2091, Tmax_2091, Tmin_2091, GDD.10_2091, 
         PRE_2091, RAD_2091, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SM_2091)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SM_2092 <- RCP2.6_SM_2100s %>%
  select(Soil.quality, Tave_2092, Tmax_2092, Tmin_2092, GDD.10_2092, 
         PRE_2092, RAD_2092, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SM_2092)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SM_2093 <- RCP2.6_SM_2100s %>%
  select(Soil.quality, Tave_2093, Tmax_2093, Tmin_2093, GDD.10_2093, 
         PRE_2093, RAD_2093,Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SM_2093)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SM_2094 <- RCP2.6_SM_2100s %>%
  select(Soil.quality, Tave_2094, Tmax_2094, Tmin_2094, GDD.10_2094, 
         PRE_2094, RAD_2094,Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SM_2094)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SM_2095 <- RCP2.6_SM_2100s %>%
  select(Soil.quality, Tave_2095, Tmax_2095, Tmin_2095, GDD.10_2095, 
         PRE_2095, RAD_2095, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SM_2095)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SM_2096 <- RCP2.6_SM_2100s %>%
  select(Soil.quality, Tave_2096, Tmax_2096, Tmin_2096, GDD.10_2096, 
         PRE_2096, RAD_2096, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SM_2096)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SM_2097 <- RCP2.6_SM_2100s %>%
  select(Soil.quality, Tave_2097, Tmax_2097, Tmin_2097, GDD.10_2097, 
         PRE_2097, RAD_2097,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SM_2097)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SM_2098 <- RCP2.6_SM_2100s %>%
  select(Soil.quality, Tave_2098, Tmax_2098, Tmin_2098, GDD.10_2098, 
         PRE_2098, RAD_2098,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SM_2098)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SM_2099 <- RCP2.6_SM_2100s %>%
  select(Soil.quality, Tave_2099, Tmax_2099, Tmin_2099, GDD.10_2099, 
         PRE_2099, RAD_2099, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SM_2099)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SM_80.99 <- RCP2.6_SM_2100s %>%
  select(Soil.quality, Tave_2080.2099, Tmax_2080.2099, Tmin_2080.2099, GDD.10_2080.2099, 
         PRE_2080.2099, RAD_2080.2099, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SM_80.99)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")


###predict yield by GBRT model

RCP2.6_SM_pred_2100s_recyle<-NULL

RCP2.6_SM_pred_2100s <- RCP2.6_SM_2100s %>%
  select(Soil.quality, Crop, NO, Year, Province, 
         NO.Station)

for (i in 1:50) {
  RCP2.6_SM_pred_2100s$Ynpk_pred_2080<-predict(brt_MSWC_recyle[[i]],RCP2.6_SM_2080, n.trees=best_inter_MSWC_recyle[[i]])
  RCP2.6_SM_pred_2100s$Ynpk_pred_2081<-predict(brt_MSWC_recyle[[i]],RCP2.6_SM_2081, n.trees=best_inter_MSWC_recyle[[i]])
  RCP2.6_SM_pred_2100s$Ynpk_pred_2082<-predict(brt_MSWC_recyle[[i]],RCP2.6_SM_2082, n.trees=best_inter_MSWC_recyle[[i]])
  RCP2.6_SM_pred_2100s$Ynpk_pred_2083<-predict(brt_MSWC_recyle[[i]],RCP2.6_SM_2083, n.trees=best_inter_MSWC_recyle[[i]])
  RCP2.6_SM_pred_2100s$Ynpk_pred_2084<-predict(brt_MSWC_recyle[[i]],RCP2.6_SM_2084, n.trees=best_inter_MSWC_recyle[[i]])
  RCP2.6_SM_pred_2100s$Ynpk_pred_2085<-predict(brt_MSWC_recyle[[i]],RCP2.6_SM_2085, n.trees=best_inter_MSWC_recyle[[i]])
  RCP2.6_SM_pred_2100s$Ynpk_pred_2086<-predict(brt_MSWC_recyle[[i]],RCP2.6_SM_2086, n.trees=best_inter_MSWC_recyle[[i]])
  RCP2.6_SM_pred_2100s$Ynpk_pred_2087<-predict(brt_MSWC_recyle[[i]],RCP2.6_SM_2087, n.trees=best_inter_MSWC_recyle[[i]])
  RCP2.6_SM_pred_2100s$Ynpk_pred_2088<-predict(brt_MSWC_recyle[[i]],RCP2.6_SM_2088, n.trees=best_inter_MSWC_recyle[[i]])
  RCP2.6_SM_pred_2100s$Ynpk_pred_2089<-predict(brt_MSWC_recyle[[i]],RCP2.6_SM_2089, n.trees=best_inter_MSWC_recyle[[i]])
  RCP2.6_SM_pred_2100s$Ynpk_pred_2090<-predict(brt_MSWC_recyle[[i]],RCP2.6_SM_2090, n.trees=best_inter_MSWC_recyle[[i]])
  RCP2.6_SM_pred_2100s$Ynpk_pred_2091<-predict(brt_MSWC_recyle[[i]],RCP2.6_SM_2091, n.trees=best_inter_MSWC_recyle[[i]])
  RCP2.6_SM_pred_2100s$Ynpk_pred_2092<-predict(brt_MSWC_recyle[[i]],RCP2.6_SM_2092, n.trees=best_inter_MSWC_recyle[[i]])
  RCP2.6_SM_pred_2100s$Ynpk_pred_2093<-predict(brt_MSWC_recyle[[i]],RCP2.6_SM_2093, n.trees=best_inter_MSWC_recyle[[i]])
  RCP2.6_SM_pred_2100s$Ynpk_pred_2094<-predict(brt_MSWC_recyle[[i]],RCP2.6_SM_2094, n.trees=best_inter_MSWC_recyle[[i]])
  RCP2.6_SM_pred_2100s$Ynpk_pred_2095<-predict(brt_MSWC_recyle[[i]],RCP2.6_SM_2095, n.trees=best_inter_MSWC_recyle[[i]])
  RCP2.6_SM_pred_2100s$Ynpk_pred_2096<-predict(brt_MSWC_recyle[[i]],RCP2.6_SM_2096, n.trees=best_inter_MSWC_recyle[[i]])
  RCP2.6_SM_pred_2100s$Ynpk_pred_2097<-predict(brt_MSWC_recyle[[i]],RCP2.6_SM_2097, n.trees=best_inter_MSWC_recyle[[i]])
  RCP2.6_SM_pred_2100s$Ynpk_pred_2098<-predict(brt_MSWC_recyle[[i]],RCP2.6_SM_2098, n.trees=best_inter_MSWC_recyle[[i]])
  RCP2.6_SM_pred_2100s$Ynpk_pred_2099<-predict(brt_MSWC_recyle[[i]],RCP2.6_SM_2099, n.trees=best_inter_MSWC_recyle[[i]])
  RCP2.6_SM_pred_2100s$Ynpk_pred_80.99<-predict(brt_MSWC_recyle[[i]],RCP2.6_SM_80.99, n.trees=best_inter_MSWC_recyle[[i]])
  
  RCP2.6_SM_pred_2100s$average<- apply(RCP2.6_SM_pred_2100s[,7:26], 1, mean)
  RCP2.6_SM_pred_2100s_recyle[[i]] <- RCP2.6_SM_pred_2100s
}

RCP2.6_SM_pred_2100s_recyle_mean<-RCP2.6_SM_pred_2100s_recyle[[1]]
RCP2.6_SM_pred_2100s_recyle_sd<-RCP2.6_SM_pred_2100s_recyle[[1]]
for(i in 7:28){
  da=as.matrix(as.data.frame(sapply(1:50,function(x){RCP2.6_SM_pred_2100s_recyle[[x]][i]})))
  RCP2.6_SM_pred_2100s_recyle_mean[,i]=rowMeans(da)
  RCP2.6_SM_pred_2100s_recyle_sd[,i]=apply(da,1,sd)
}


###RCP8.5 during 2040-2099

RCP8.5_SM_2050s<-subset(RCP8.5_2050s, Crop == "south maize")
RCP8.5_SM_2040 <- RCP8.5_SM_2050s %>%
  select(Soil.quality, Tave_2040, Tmax_2040, Tmin_2040, GDD.10_2040, 
         PRE_2040, RAD_2040, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SM_2040)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SM_2041 <- RCP8.5_SM_2050s %>%
  select(Soil.quality, Tave_2041, Tmax_2041, Tmin_2041, GDD.10_2041, 
         PRE_2041, RAD_2041, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SM_2041)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SM_2042 <- RCP8.5_SM_2050s %>%
  select(Soil.quality, Tave_2042, Tmax_2042, Tmin_2042, GDD.10_2042, 
         PRE_2042, RAD_2042, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SM_2042)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SM_2043 <- RCP8.5_SM_2050s %>%
  select(Soil.quality, Tave_2043, Tmax_2043, Tmin_2043, GDD.10_2043, 
         PRE_2043, RAD_2043, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SM_2043)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SM_2044 <- RCP8.5_SM_2050s %>%
  select(Soil.quality, Tave_2044, Tmax_2044, Tmin_2044, GDD.10_2044, 
         PRE_2044, RAD_2044, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SM_2044)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SM_2045 <- RCP8.5_SM_2050s %>%
  select(Soil.quality, Tave_2045, Tmax_2045, Tmin_2045, GDD.10_2045, 
         PRE_2045, RAD_2045, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SM_2045)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SM_2046 <- RCP8.5_SM_2050s %>%
  select(Soil.quality, Tave_2046, Tmax_2046, Tmin_2046, GDD.10_2046, 
         PRE_2046, RAD_2046,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SM_2046)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SM_2047 <- RCP8.5_SM_2050s %>%
  select(Soil.quality, Tave_2047, Tmax_2047, Tmin_2047, GDD.10_2047, 
         PRE_2047, RAD_2047, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SM_2047)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SM_2048 <- RCP8.5_SM_2050s %>%
  select(Soil.quality, Tave_2048, Tmax_2048, Tmin_2048, GDD.10_2048, 
         PRE_2048, RAD_2048, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SM_2048)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SM_2049 <- RCP8.5_SM_2050s %>%
  select(Soil.quality, Tave_2049, Tmax_2049, Tmin_2049, GDD.10_2049, 
         PRE_2049, RAD_2049, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SM_2049)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SM_2050 <- RCP8.5_SM_2050s %>%
  select(Soil.quality, Tave_2050, Tmax_2050, Tmin_2050, GDD.10_2050, 
         PRE_2050, RAD_2050, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SM_2050)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SM_2051 <- RCP8.5_SM_2050s %>%
  select(Soil.quality, Tave_2051, Tmax_2051, Tmin_2051, GDD.10_2051, 
         PRE_2051, RAD_2051,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SM_2051)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SM_2052 <- RCP8.5_SM_2050s %>%
  select(Soil.quality, Tave_2052, Tmax_2052, Tmin_2052, GDD.10_2052, 
         PRE_2052, RAD_2052, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SM_2052)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SM_2053 <- RCP8.5_SM_2050s %>%
  select(Soil.quality, Tave_2053, Tmax_2053, Tmin_2053, GDD.10_2053, 
         PRE_2053, RAD_2053, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SM_2053)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SM_2054 <- RCP8.5_SM_2050s %>%
  select(Soil.quality, Tave_2054, Tmax_2054, Tmin_2054, GDD.10_2054, 
         PRE_2054, RAD_2054,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SM_2054)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SM_2055 <- RCP8.5_SM_2050s %>%
  select(Soil.quality, Tave_2055, Tmax_2055, Tmin_2055, GDD.10_2055, 
         PRE_2055, RAD_2055, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SM_2055)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SM_2056 <- RCP8.5_SM_2050s %>%
  select(Soil.quality, Tave_2056, Tmax_2056, Tmin_2056, GDD.10_2056, 
         PRE_2056, RAD_2056, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SM_2056)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SM_2057 <- RCP8.5_SM_2050s %>%
  select(Soil.quality, Tave_2057, Tmax_2057, Tmin_2057, GDD.10_2057, 
         PRE_2057, RAD_2057, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SM_2057)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SM_2058 <- RCP8.5_SM_2050s %>%
  select(Soil.quality, Tave_2058, Tmax_2058, Tmin_2058, GDD.10_2058, 
         PRE_2058, RAD_2058, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SM_2058)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SM_2059 <- RCP8.5_SM_2050s %>%
  select(Soil.quality, Tave_2059, Tmax_2059, Tmin_2059, GDD.10_2059, 
         PRE_2059, RAD_2059, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SM_2059)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SM_40.59 <- RCP8.5_SM_2050s %>%
  select(Soil.quality, Tave_2040.2059, Tmax_2040.2059, Tmin_2040.2059, GDD.10_2040.2059, 
         PRE_2040.2059, RAD_2040.2059,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SM_40.59)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

###predict yield by GBRT model

RCP8.5_SM_pred_2050s_recyle<-NULL

RCP8.5_SM_pred_2050s <- RCP8.5_SM_2050s %>%
  select(Soil.quality, Crop, NO, Year, Province, 
         NO.Station)

for (i in 1:50) {
  RCP8.5_SM_pred_2050s$Ynpk_pred_2040<-predict(brt_MSWC_recyle[[i]],RCP8.5_SM_2040, n.trees=best_inter_MSWC_recyle[[i]])
  RCP8.5_SM_pred_2050s$Ynpk_pred_2041<-predict(brt_MSWC_recyle[[i]],RCP8.5_SM_2041, n.trees=best_inter_MSWC_recyle[[i]])
  RCP8.5_SM_pred_2050s$Ynpk_pred_2042<-predict(brt_MSWC_recyle[[i]],RCP8.5_SM_2042, n.trees=best_inter_MSWC_recyle[[i]])
  RCP8.5_SM_pred_2050s$Ynpk_pred_2043<-predict(brt_MSWC_recyle[[i]],RCP8.5_SM_2043, n.trees=best_inter_MSWC_recyle[[i]])
  RCP8.5_SM_pred_2050s$Ynpk_pred_2044<-predict(brt_MSWC_recyle[[i]],RCP8.5_SM_2044, n.trees=best_inter_MSWC_recyle[[i]])
  RCP8.5_SM_pred_2050s$Ynpk_pred_2045<-predict(brt_MSWC_recyle[[i]],RCP8.5_SM_2045, n.trees=best_inter_MSWC_recyle[[i]])
  RCP8.5_SM_pred_2050s$Ynpk_pred_2046<-predict(brt_MSWC_recyle[[i]],RCP8.5_SM_2046, n.trees=best_inter_MSWC_recyle[[i]])
  RCP8.5_SM_pred_2050s$Ynpk_pred_2047<-predict(brt_MSWC_recyle[[i]],RCP8.5_SM_2047, n.trees=best_inter_MSWC_recyle[[i]])
  RCP8.5_SM_pred_2050s$Ynpk_pred_2048<-predict(brt_MSWC_recyle[[i]],RCP8.5_SM_2048, n.trees=best_inter_MSWC_recyle[[i]])
  RCP8.5_SM_pred_2050s$Ynpk_pred_2049<-predict(brt_MSWC_recyle[[i]],RCP8.5_SM_2049, n.trees=best_inter_MSWC_recyle[[i]])
  RCP8.5_SM_pred_2050s$Ynpk_pred_2050<-predict(brt_MSWC_recyle[[i]],RCP8.5_SM_2050, n.trees=best_inter_MSWC_recyle[[i]])
  RCP8.5_SM_pred_2050s$Ynpk_pred_2051<-predict(brt_MSWC_recyle[[i]],RCP8.5_SM_2051, n.trees=best_inter_MSWC_recyle[[i]])
  RCP8.5_SM_pred_2050s$Ynpk_pred_2052<-predict(brt_MSWC_recyle[[i]],RCP8.5_SM_2052, n.trees=best_inter_MSWC_recyle[[i]])
  RCP8.5_SM_pred_2050s$Ynpk_pred_2053<-predict(brt_MSWC_recyle[[i]],RCP8.5_SM_2053, n.trees=best_inter_MSWC_recyle[[i]])
  RCP8.5_SM_pred_2050s$Ynpk_pred_2054<-predict(brt_MSWC_recyle[[i]],RCP8.5_SM_2054, n.trees=best_inter_MSWC_recyle[[i]])
  RCP8.5_SM_pred_2050s$Ynpk_pred_2055<-predict(brt_MSWC_recyle[[i]],RCP8.5_SM_2055, n.trees=best_inter_MSWC_recyle[[i]])
  RCP8.5_SM_pred_2050s$Ynpk_pred_2056<-predict(brt_MSWC_recyle[[i]],RCP8.5_SM_2056, n.trees=best_inter_MSWC_recyle[[i]])
  RCP8.5_SM_pred_2050s$Ynpk_pred_2057<-predict(brt_MSWC_recyle[[i]],RCP8.5_SM_2057, n.trees=best_inter_MSWC_recyle[[i]])
  RCP8.5_SM_pred_2050s$Ynpk_pred_2058<-predict(brt_MSWC_recyle[[i]],RCP8.5_SM_2058, n.trees=best_inter_MSWC_recyle[[i]])
  RCP8.5_SM_pred_2050s$Ynpk_pred_2059<-predict(brt_MSWC_recyle[[i]],RCP8.5_SM_2059, n.trees=best_inter_MSWC_recyle[[i]])
  RCP8.5_SM_pred_2050s$Ynpk_pred_40.59<-predict(brt_MSWC_recyle[[i]],RCP8.5_SM_40.59, n.trees=best_inter_MSWC_recyle[[i]])
  
  RCP8.5_SM_pred_2050s$average<- apply(RCP8.5_SM_pred_2050s[,7:26], 1, mean)
  RCP8.5_SM_pred_2050s_recyle[[i]] <- RCP8.5_SM_pred_2050s
}

RCP8.5_SM_pred_2050s_recyle_mean<-RCP8.5_SM_pred_2050s_recyle[[1]]
RCP8.5_SM_pred_2050s_recyle_sd<-RCP8.5_SM_pred_2050s_recyle[[1]]
for(i in 7:28){
  da=as.matrix(as.data.frame(sapply(1:50,function(x){RCP8.5_SM_pred_2050s_recyle[[x]][i]})))
  RCP8.5_SM_pred_2050s_recyle_mean[,i]=rowMeans(da)
  RCP8.5_SM_pred_2050s_recyle_sd[,i]=apply(da,1,sd)
}

###RCP8.5 during 2080-2099

RCP8.5_SM_2100s<-subset(RCP8.5_2100s, Crop == "south maize")
RCP8.5_SM_2080 <- RCP8.5_SM_2100s %>%
  select(Soil.quality, Tave_2080, Tmax_2080, Tmin_2080, GDD.10_2080, 
         PRE_2080, RAD_2080,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SM_2080)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SM_2081 <- RCP8.5_SM_2100s %>%
  select(Soil.quality, Tave_2081, Tmax_2081, Tmin_2081, GDD.10_2081, 
         PRE_2081, RAD_2081, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SM_2081)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SM_2082 <- RCP8.5_SM_2100s %>%
  select(Soil.quality, Tave_2082, Tmax_2082, Tmin_2082, GDD.10_2082, 
         PRE_2082, RAD_2082, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SM_2082)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SM_2083 <- RCP8.5_SM_2100s %>%
  select(Soil.quality, Tave_2083, Tmax_2083, Tmin_2083, GDD.10_2083, 
         PRE_2083, RAD_2083, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SM_2083)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SM_2084 <- RCP8.5_SM_2100s %>%
  select(Soil.quality, Tave_2084, Tmax_2084, Tmin_2084, GDD.10_2084, 
         PRE_2084, RAD_2084, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SM_2084)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SM_2085 <- RCP8.5_SM_2100s %>%
  select(Soil.quality, Tave_2085, Tmax_2085, Tmin_2085, GDD.10_2085, 
         PRE_2085, RAD_2085, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SM_2085)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SM_2086 <- RCP8.5_SM_2100s %>%
  select(Soil.quality, Tave_2086, Tmax_2086, Tmin_2086, GDD.10_2086, 
         PRE_2086, RAD_2086,Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SM_2086)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SM_2087 <- RCP8.5_SM_2100s %>%
  select(Soil.quality, Tave_2087, Tmax_2087, Tmin_2087, GDD.10_2087, 
         PRE_2087, RAD_2087, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SM_2087)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SM_2088 <- RCP8.5_SM_2100s %>%
  select(Soil.quality, Tave_2088, Tmax_2088, Tmin_2088, GDD.10_2088, 
         PRE_2088, RAD_2088, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SM_2088)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SM_2089 <- RCP8.5_SM_2100s %>%
  select(Soil.quality, Tave_2089, Tmax_2089, Tmin_2089, GDD.10_2089, 
         PRE_2089, RAD_2089, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SM_2089)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SM_2090 <- RCP8.5_SM_2100s %>%
  select(Soil.quality, Tave_2090, Tmax_2090, Tmin_2090, GDD.10_2090, 
         PRE_2090, RAD_2090, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SM_2090)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SM_2091 <- RCP8.5_SM_2100s %>%
  select(Soil.quality, Tave_2091, Tmax_2091, Tmin_2091, GDD.10_2091, 
         PRE_2091, RAD_2091,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SM_2091)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SM_2092 <- RCP8.5_SM_2100s %>%
  select(Soil.quality, Tave_2092, Tmax_2092, Tmin_2092, GDD.10_2092, 
         PRE_2092, RAD_2092, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SM_2092)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SM_2093 <- RCP8.5_SM_2100s %>%
  select(Soil.quality, Tave_2093, Tmax_2093, Tmin_2093, GDD.10_2093, 
         PRE_2093, RAD_2093, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SM_2093)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SM_2094 <- RCP8.5_SM_2100s %>%
  select(Soil.quality, Tave_2094, Tmax_2094, Tmin_2094, GDD.10_2094, 
         PRE_2094, RAD_2094, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SM_2094)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SM_2095 <- RCP8.5_SM_2100s %>%
  select(Soil.quality, Tave_2095, Tmax_2095, Tmin_2095, GDD.10_2095, 
         PRE_2095, RAD_2095, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SM_2095)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SM_2096 <- RCP8.5_SM_2100s %>%
  select(Soil.quality, Tave_2096, Tmax_2096, Tmin_2096, GDD.10_2096, 
         PRE_2096, RAD_2096,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SM_2096)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SM_2097 <- RCP8.5_SM_2100s %>%
  select(Soil.quality, Tave_2097, Tmax_2097, Tmin_2097, GDD.10_2097, 
         PRE_2097, RAD_2097, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SM_2097)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SM_2098 <- RCP8.5_SM_2100s %>%
  select(Soil.quality, Tave_2098, Tmax_2098, Tmin_2098, GDD.10_2098, 
         PRE_2098, RAD_2098, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SM_2098)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SM_2099 <- RCP8.5_SM_2100s %>%
  select(Soil.quality, Tave_2099, Tmax_2099, Tmin_2099, GDD.10_2099, 
         PRE_2099, RAD_2099, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SM_2099)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SM_80.99 <- RCP8.5_SM_2100s %>%
  select(Soil.quality, Tave_2080.2099, Tmax_2080.2099, Tmin_2080.2099, GDD.10_2080.2099, 
         PRE_2080.2099, RAD_2080.2099, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SM_80.99)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")


###predict yield by GBRT model
RCP8.5_SM_pred_2100s_recyle<-NULL

RCP8.5_SM_pred_2100s <- RCP8.5_SM_2100s %>%
  select(Soil.quality, Crop, NO, Year, Province, 
         NO.Station)

for (i in 1:50) {
  RCP8.5_SM_pred_2100s$Ynpk_pred_2080<-predict(brt_MSWC_recyle[[i]],RCP8.5_SM_2080, n.trees=best_inter_MSWC_recyle[[i]])
  RCP8.5_SM_pred_2100s$Ynpk_pred_2081<-predict(brt_MSWC_recyle[[i]],RCP8.5_SM_2081, n.trees=best_inter_MSWC_recyle[[i]])
  RCP8.5_SM_pred_2100s$Ynpk_pred_2082<-predict(brt_MSWC_recyle[[i]],RCP8.5_SM_2082, n.trees=best_inter_MSWC_recyle[[i]])
  RCP8.5_SM_pred_2100s$Ynpk_pred_2083<-predict(brt_MSWC_recyle[[i]],RCP8.5_SM_2083, n.trees=best_inter_MSWC_recyle[[i]])
  RCP8.5_SM_pred_2100s$Ynpk_pred_2084<-predict(brt_MSWC_recyle[[i]],RCP8.5_SM_2084, n.trees=best_inter_MSWC_recyle[[i]])
  RCP8.5_SM_pred_2100s$Ynpk_pred_2085<-predict(brt_MSWC_recyle[[i]],RCP8.5_SM_2085, n.trees=best_inter_MSWC_recyle[[i]])
  RCP8.5_SM_pred_2100s$Ynpk_pred_2086<-predict(brt_MSWC_recyle[[i]],RCP8.5_SM_2086, n.trees=best_inter_MSWC_recyle[[i]])
  RCP8.5_SM_pred_2100s$Ynpk_pred_2087<-predict(brt_MSWC_recyle[[i]],RCP8.5_SM_2087, n.trees=best_inter_MSWC_recyle[[i]])
  RCP8.5_SM_pred_2100s$Ynpk_pred_2088<-predict(brt_MSWC_recyle[[i]],RCP8.5_SM_2088, n.trees=best_inter_MSWC_recyle[[i]])
  RCP8.5_SM_pred_2100s$Ynpk_pred_2089<-predict(brt_MSWC_recyle[[i]],RCP8.5_SM_2089, n.trees=best_inter_MSWC_recyle[[i]])
  RCP8.5_SM_pred_2100s$Ynpk_pred_2090<-predict(brt_MSWC_recyle[[i]],RCP8.5_SM_2090, n.trees=best_inter_MSWC_recyle[[i]])
  RCP8.5_SM_pred_2100s$Ynpk_pred_2091<-predict(brt_MSWC_recyle[[i]],RCP8.5_SM_2091, n.trees=best_inter_MSWC_recyle[[i]])
  RCP8.5_SM_pred_2100s$Ynpk_pred_2092<-predict(brt_MSWC_recyle[[i]],RCP8.5_SM_2092, n.trees=best_inter_MSWC_recyle[[i]])
  RCP8.5_SM_pred_2100s$Ynpk_pred_2093<-predict(brt_MSWC_recyle[[i]],RCP8.5_SM_2093, n.trees=best_inter_MSWC_recyle[[i]])
  RCP8.5_SM_pred_2100s$Ynpk_pred_2094<-predict(brt_MSWC_recyle[[i]],RCP8.5_SM_2094, n.trees=best_inter_MSWC_recyle[[i]])
  RCP8.5_SM_pred_2100s$Ynpk_pred_2095<-predict(brt_MSWC_recyle[[i]],RCP8.5_SM_2095, n.trees=best_inter_MSWC_recyle[[i]])
  RCP8.5_SM_pred_2100s$Ynpk_pred_2096<-predict(brt_MSWC_recyle[[i]],RCP8.5_SM_2096, n.trees=best_inter_MSWC_recyle[[i]])
  RCP8.5_SM_pred_2100s$Ynpk_pred_2097<-predict(brt_MSWC_recyle[[i]],RCP8.5_SM_2097, n.trees=best_inter_MSWC_recyle[[i]])
  RCP8.5_SM_pred_2100s$Ynpk_pred_2098<-predict(brt_MSWC_recyle[[i]],RCP8.5_SM_2098, n.trees=best_inter_MSWC_recyle[[i]])
  RCP8.5_SM_pred_2100s$Ynpk_pred_2099<-predict(brt_MSWC_recyle[[i]],RCP8.5_SM_2099, n.trees=best_inter_MSWC_recyle[[i]])
  RCP8.5_SM_pred_2100s$Ynpk_pred_80.99<-predict(brt_MSWC_recyle[[i]],RCP8.5_SM_80.99, n.trees=best_inter_MSWC_recyle[[i]])
  
  RCP8.5_SM_pred_2100s$average<- apply(RCP8.5_SM_pred_2100s[,7:26], 1, mean)
  RCP8.5_SM_pred_2100s_recyle[[i]] <- RCP8.5_SM_pred_2100s
}

RCP8.5_SM_pred_2100s_recyle_mean<-RCP8.5_SM_pred_2100s_recyle[[1]]
RCP8.5_SM_pred_2100s_recyle_sd<-RCP8.5_SM_pred_2100s_recyle[[1]]
for(i in 7:28){
  da=as.matrix(as.data.frame(sapply(1:50,function(x){RCP8.5_SM_pred_2100s_recyle[[x]][i]})))
  RCP8.5_SM_pred_2100s_recyle_mean[,i]=rowMeans(da)
  RCP8.5_SM_pred_2100s_recyle_sd[,i]=apply(da,1,sd)
}

###summarise predicted yield

sum_RCP2.6_SM_2050s_year<- cbind(base_SM_pred_all[,c("Soil.quality","Crop","NO","average")],RCP2.6_SM_pred_2050s[,7:28])
names(sum_RCP2.6_SM_2050s_year)[1:4]<- c("Soil quality", "Crop", "No", "Yield_base")
names(sum_RCP2.6_SM_2050s_year)[26]<- "Yield_RCP"
sum_RCP2.6_SM_2050s_year$Yield_change <- sum_RCP2.6_SM_2050s_year$`Yield_RCP` - sum_RCP2.6_SM_2050s_year$`Yield_base`
sum_RCP2.6_SM_2050s_year$Yield_change_per <- (sum_RCP2.6_SM_2050s_year$Yield_change / sum_RCP2.6_SM_2050s_year$Yield_base)*100
sum_RCP2.6_SM_2050s_year$RCP <- "RCP2.6"
sum_RCP2.6_SM_2050s_year$Time <- "2050"

sum_RCP8.5_SM_2050s_year<- cbind(base_SM_pred_all[,c("Soil.quality","Crop","NO","average")],RCP8.5_SM_pred_2050s[,7:28])
names(sum_RCP8.5_SM_2050s_year)[1:4]<- c("Soil quality", "Crop", "No", "Yield_base")
names(sum_RCP8.5_SM_2050s_year)[26]<- "Yield_RCP"
sum_RCP8.5_SM_2050s_year$Yield_change <- sum_RCP8.5_SM_2050s_year$`Yield_RCP` - sum_RCP8.5_SM_2050s_year$`Yield_base`
sum_RCP8.5_SM_2050s_year$Yield_change_per <- (sum_RCP8.5_SM_2050s_year$Yield_change / sum_RCP8.5_SM_2050s_year$Yield_base)*100
sum_RCP8.5_SM_2050s_year$RCP <- "RCP8.5"
sum_RCP8.5_SM_2050s_year$Time <- "2050"

sum_RCP2.6_SM_2100s_year<- cbind(base_SM_pred_all[,c("Soil.quality","Crop","NO","average")],RCP2.6_SM_pred_2100s[,7:28])
names(sum_RCP2.6_SM_2100s_year)[1:4]<- c("Soil quality", "Crop", "No", "Yield_base")
names(sum_RCP2.6_SM_2100s_year)[26]<- "Yield_RCP"
sum_RCP2.6_SM_2100s_year$Yield_change <- sum_RCP2.6_SM_2100s_year$`Yield_RCP` - sum_RCP2.6_SM_2100s_year$`Yield_base`
sum_RCP2.6_SM_2100s_year$Yield_change_per <- (sum_RCP2.6_SM_2100s_year$Yield_change / sum_RCP2.6_SM_2100s_year$Yield_base)*100
sum_RCP2.6_SM_2100s_year$RCP <- "RCP2.6"
sum_RCP2.6_SM_2100s_year$Time <- "2100"

sum_RCP8.5_SM_2100s_year<- cbind(base_SM_pred_all[,c("Soil.quality","Crop","NO","average")],RCP8.5_SM_pred_2100s[,7:28])
names(sum_RCP8.5_SM_2100s_year)[1:4]<- c("Soil quality", "Crop", "No", "Yield_base")
names(sum_RCP8.5_SM_2100s_year)[26]<- "Yield_RCP"
sum_RCP8.5_SM_2100s_year$Yield_change <- sum_RCP8.5_SM_2100s_year$`Yield_RCP` - sum_RCP8.5_SM_2100s_year$`Yield_base`
sum_RCP8.5_SM_2100s_year$Yield_change_per <- (sum_RCP8.5_SM_2100s_year$Yield_change / sum_RCP8.5_SM_2100s_year$Yield_base)*100
sum_RCP8.5_SM_2100s_year$RCP <- "RCP8.5"
sum_RCP8.5_SM_2100s_year$Time <- "2100"

sum_RCP_SM_2050s_year <- rbind(sum_RCP2.6_SM_2050s_year,sum_RCP8.5_SM_2050s_year)
sum_RCP_SM_2100s_year<-rbind(sum_RCP2.6_SM_2100s_year,sum_RCP8.5_SM_2100s_year)

write.csv(sum_RCP_SM_2050s_year,"./results/sum_RCP_SM_2050s_year.csv")
write.csv(sum_RCP_SM_2100s_year,"./results/sum_RCP_SM_2100s_year.csv")

###comparison of yield change between high and low quality soils 
###T test

t.test(Yield_change~`Soil quality` ,sum_RCP2.6_SM_2050s_year) #RCP2.6-2050s
t.test(Yield_change~`Soil quality` ,sum_RCP8.5_SM_2050s_year) #RCP8.5-2050s
t.test(Yield_change~`Soil quality` ,sum_RCP2.6_SM_2100s_year) #RCP2.6-2100s
t.test(Yield_change~`Soil quality` ,sum_RCP8.5_SM_2100s_year) #RCP8.5-2100s

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
  brt_SRYZB<-gbm(Ynpk ~ Tmax + Tmin + GDD.10 + PRE +  RAD + Soil.type +Cultivar
                 + Soil.texture + SOM + OP + AK + pH + N + P2O5 + K2O,
                 data = sub_SR_YZB_train, distribution = "gaussian", n.trees = 3000,
                 interaction.depth = 9, shrinkage = 0.01, 
                 bag.fraction = 0.5,cv.folds = 10)
  
  brt_SRYZB_recyle[[i]]<-brt_SRYZB
  print(brt_SRYZB)
  best_inter_SRYZB<-gbm.perf(brt_SRYZB,method = "cv")
  best_inter_SRYZB_recyle[[i]]<-best_inter_SRYZB
  summary_SRYZB_record<-summary(brt_SRYZB,n.trees=best_inter_SRYZB)
  summary_SRYZB_recyle[[i]] <-summary_SRYZB_record
}

#####Predicted yield under different climate scenarios
###baseline period 1986-2005
base_SSR<-subset(base, Crop == "single rice")
base_SSR_1986 <- base_SSR %>%
  select(Soil.quality, Tave_1986, Tmax_1986, Tmin_1986, GDD.10_1986, 
         PRE_1986, RAD_1986, Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_SSR_1986)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                         "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                         "N", "P2O5", "K2O", "Yck", "Ynpk")

base_SSR_1987 <- base_SSR %>%
  select(Soil.quality, Tave_1987, Tmax_1987, Tmin_1987, GDD.10_1987, 
         PRE_1987, RAD_1987, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_SSR_1987)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                         "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                         "N", "P2O5", "K2O", "Yck", "Ynpk")

base_SSR_1988 <- base_SSR %>%
  select(Soil.quality, Tave_1988, Tmax_1988, Tmin_1988, GDD.10_1988, 
         PRE_1988, RAD_1988, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_SSR_1988)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                         "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                         "N", "P2O5", "K2O", "Yck", "Ynpk")

base_SSR_1989 <- base_SSR %>%
  select(Soil.quality, Tave_1989, Tmax_1989, Tmin_1989, GDD.10_1989, 
         PRE_1989, RAD_1989, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_SSR_1989)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                         "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                         "N", "P2O5", "K2O", "Yck", "Ynpk")

base_SSR_1990 <- base_SSR %>%
  select(Soil.quality, Tave_1990, Tmax_1990, Tmin_1990, GDD.10_1990, 
         PRE_1990, RAD_1990, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_SSR_1990)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                         "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                         "N", "P2O5", "K2O", "Yck", "Ynpk")

base_SSR_1991 <- base_SSR %>%
  select(Soil.quality, Tave_1991, Tmax_1991, Tmin_1991, GDD.10_1991, 
         PRE_1991, RAD_1991, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_SSR_1991)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                         "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                         "N", "P2O5", "K2O", "Yck", "Ynpk")

base_SSR_1992 <- base_SSR %>%
  select(Soil.quality, Tave_1992, Tmax_1992, Tmin_1992, GDD.10_1992, 
         PRE_1992, RAD_1992,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_SSR_1992)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                         "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                         "N", "P2O5", "K2O", "Yck", "Ynpk")

base_SSR_1993 <- base_SSR %>%
  select(Soil.quality, Tave_1993, Tmax_1993, Tmin_1993, GDD.10_1993, 
         PRE_1993, RAD_1993, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_SSR_1993)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                         "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                         "N", "P2O5", "K2O", "Yck", "Ynpk")

base_SSR_1994 <- base_SSR %>%
  select(Soil.quality, Tave_1994, Tmax_1994, Tmin_1994, GDD.10_1994, 
         PRE_1994, RAD_1994, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_SSR_1994)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                         "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                         "N", "P2O5", "K2O", "Yck", "Ynpk")

base_SSR_1995 <- base_SSR %>%
  select(Soil.quality, Tave_1995, Tmax_1995, Tmin_1995, GDD.10_1995, 
         PRE_1995, RAD_1995, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_SSR_1995)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                         "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                         "N", "P2O5", "K2O", "Yck", "Ynpk")

base_SSR_1996 <- base_SSR %>%
  select(Soil.quality, Tave_1996, Tmax_1996, Tmin_1996, GDD.10_1996, 
         PRE_1996, RAD_1996, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_SSR_1996)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                         "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                         "N", "P2O5", "K2O", "Yck", "Ynpk")

base_SSR_1997 <- base_SSR %>%
  select(Soil.quality, Tave_1997, Tmax_1997, Tmin_1997, GDD.10_1997, 
         PRE_1997, RAD_1997, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_SSR_1997)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                         "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                         "N", "P2O5", "K2O", "Yck", "Ynpk")

base_SSR_1998 <- base_SSR %>%
  select(Soil.quality, Tave_1998, Tmax_1998, Tmin_1998, GDD.10_1998, 
         PRE_1998, RAD_1998, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_SSR_1998)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                         "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                         "N", "P2O5", "K2O", "Yck", "Ynpk")

base_SSR_1999 <- base_SSR %>%
  select(Soil.quality, Tave_1999, Tmax_1999, Tmin_1999, GDD.10_1999, 
         PRE_1999, RAD_1999, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_SSR_1999)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                         "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                         "N", "P2O5", "K2O", "Yck", "Ynpk")

base_SSR_2000 <- base_SSR %>%
  select(Soil.quality, Tave_2000, Tmax_2000, Tmin_2000, GDD.10_2000, 
         PRE_2000, RAD_2000, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_SSR_2000)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                         "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                         "N", "P2O5", "K2O", "Yck", "Ynpk")

base_SSR_2001 <- base_SSR %>%
  select(Soil.quality, Tave_2001, Tmax_2001, Tmin_2001, GDD.10_2001, 
         PRE_2001, RAD_2001, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_SSR_2001)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                         "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                         "N", "P2O5", "K2O", "Yck", "Ynpk")

base_SSR_2002 <- base_SSR %>%
  select(Soil.quality, Tave_2002, Tmax_2002, Tmin_2002, GDD.10_2002, 
         PRE_2002, RAD_2002, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_SSR_2002)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                         "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                         "N", "P2O5", "K2O", "Yck", "Ynpk")

base_SSR_2003 <- base_SSR %>%
  select(Soil.quality, Tave_2003, Tmax_2003, Tmin_2003, GDD.10_2003, 
         PRE_2003, RAD_2003, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_SSR_2003)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                         "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                         "N", "P2O5", "K2O", "Yck", "Ynpk")

base_SSR_2004 <- base_SSR %>%
  select(Soil.quality, Tave_2004, Tmax_2004, Tmin_2004, GDD.10_2004, 
         PRE_2004, RAD_2004, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_SSR_2004)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                         "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                         "N", "P2O5", "K2O", "Yck", "Ynpk")

base_SSR_2005 <- base_SSR %>%
  select(Soil.quality, Tave_2005, Tmax_2005, Tmin_2005, GDD.10_2005, 
         PRE_2005, RAD_2005, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_SSR_2005)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                         "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                         "N", "P2O5", "K2O", "Yck", "Ynpk")

base_SSR_86.05 <- base_SSR %>%
  select(Soil.quality, Tave_1986.2005, Tmax_1986.2005, Tmin_1986.2005, GDD.10_1986.2005, 
         PRE_1986.2005, RAD_1986.2005, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_SSR_86.05)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

###predict yield by GBRT model

base_SSR_pred_all_recyle<-NULL

base_SSR_pred_all <- base_SSR %>%
  select(Soil.quality, Crop, NO, Year,Province, 
         NO.Station)

for (i in 1:50) {
  base_SSR_pred_all$Ynpk_pred_1986<-predict(brt_SRYZB_recyle[[i]],base_SSR_1986, n.trees=best_inter_SRYZB_recyle[[i]])
  base_SSR_pred_all$Ynpk_pred_1987<-predict(brt_SRYZB_recyle[[i]],base_SSR_1987, n.trees=best_inter_SRYZB_recyle[[i]])
  base_SSR_pred_all$Ynpk_pred_1988<-predict(brt_SRYZB_recyle[[i]],base_SSR_1988, n.trees=best_inter_SRYZB_recyle[[i]])
  base_SSR_pred_all$Ynpk_pred_1989<-predict(brt_SRYZB_recyle[[i]],base_SSR_1989, n.trees=best_inter_SRYZB_recyle[[i]])
  base_SSR_pred_all$Ynpk_pred_1990<-predict(brt_SRYZB_recyle[[i]],base_SSR_1990, n.trees=best_inter_SRYZB_recyle[[i]])
  base_SSR_pred_all$Ynpk_pred_1991<-predict(brt_SRYZB_recyle[[i]],base_SSR_1991, n.trees=best_inter_SRYZB_recyle[[i]])
  base_SSR_pred_all$Ynpk_pred_1992<-predict(brt_SRYZB_recyle[[i]],base_SSR_1992, n.trees=best_inter_SRYZB_recyle[[i]])
  base_SSR_pred_all$Ynpk_pred_1993<-predict(brt_SRYZB_recyle[[i]],base_SSR_1993, n.trees=best_inter_SRYZB_recyle[[i]])
  base_SSR_pred_all$Ynpk_pred_1994<-predict(brt_SRYZB_recyle[[i]],base_SSR_1994, n.trees=best_inter_SRYZB_recyle[[i]])
  base_SSR_pred_all$Ynpk_pred_1995<-predict(brt_SRYZB_recyle[[i]],base_SSR_1995, n.trees=best_inter_SRYZB_recyle[[i]])
  base_SSR_pred_all$Ynpk_pred_1996<-predict(brt_SRYZB_recyle[[i]],base_SSR_1996, n.trees=best_inter_SRYZB_recyle[[i]])
  base_SSR_pred_all$Ynpk_pred_1997<-predict(brt_SRYZB_recyle[[i]],base_SSR_1997, n.trees=best_inter_SRYZB_recyle[[i]])
  base_SSR_pred_all$Ynpk_pred_1998<-predict(brt_SRYZB_recyle[[i]],base_SSR_1998, n.trees=best_inter_SRYZB_recyle[[i]])
  base_SSR_pred_all$Ynpk_pred_1999<-predict(brt_SRYZB_recyle[[i]],base_SSR_1999, n.trees=best_inter_SRYZB_recyle[[i]])
  base_SSR_pred_all$Ynpk_pred_2000<-predict(brt_SRYZB_recyle[[i]],base_SSR_2000, n.trees=best_inter_SRYZB_recyle[[i]])
  base_SSR_pred_all$Ynpk_pred_2001<-predict(brt_SRYZB_recyle[[i]],base_SSR_2001, n.trees=best_inter_SRYZB_recyle[[i]])
  base_SSR_pred_all$Ynpk_pred_2002<-predict(brt_SRYZB_recyle[[i]],base_SSR_2002, n.trees=best_inter_SRYZB_recyle[[i]])
  base_SSR_pred_all$Ynpk_pred_2003<-predict(brt_SRYZB_recyle[[i]],base_SSR_2003, n.trees=best_inter_SRYZB_recyle[[i]])
  base_SSR_pred_all$Ynpk_pred_2004<-predict(brt_SRYZB_recyle[[i]],base_SSR_2004, n.trees=best_inter_SRYZB_recyle[[i]])
  base_SSR_pred_all$Ynpk_pred_2005<-predict(brt_SRYZB_recyle[[i]],base_SSR_2005, n.trees=best_inter_SRYZB_recyle[[i]])
  base_SSR_pred_all$Ynpk_pred_86.05<-predict(brt_SRYZB_recyle[[i]],base_SSR_86.05, n.trees=best_inter_SRYZB_recyle[[i]])
  
  base_SSR_pred_all$average<- apply(base_SSR_pred_all[,7:26], 1, mean)
  base_SSR_pred_all_recyle[[i]] <- base_SSR_pred_all
}

base_SSR_pred_all_recyle_mean<-base_SSR_pred_all_recyle[[1]]
base_SSR_pred_all_recyle_sd<-base_SSR_pred_all_recyle[[1]]
for(i in 7:28){
  da=as.matrix(as.data.frame(sapply(1:50,function(x){base_SSR_pred_all_recyle[[x]][i]})))
  base_SSR_pred_all_recyle_mean[,i]=rowMeans(da)
  base_SSR_pred_all_recyle_sd[,i]=apply(da,1,sd)
}

###RCP2.6 during 2040-2059

RCP2.6_SSR_2050s<-subset(RCP2.6_2050s, Crop == "single rice")
RCP2.6_SSR_2040 <- RCP2.6_SSR_2050s %>%
  select(Soil.quality, Tave_2040, Tmax_2040, Tmin_2040, GDD.10_2040, 
         PRE_2040, RAD_2040, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SSR_2040)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SSR_2041 <- RCP2.6_SSR_2050s %>%
  select(Soil.quality, Tave_2041, Tmax_2041, Tmin_2041, GDD.10_2041, 
         PRE_2041, RAD_2041, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SSR_2041)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SSR_2042 <- RCP2.6_SSR_2050s %>%
  select(Soil.quality, Tave_2042, Tmax_2042, Tmin_2042, GDD.10_2042, 
         PRE_2042, RAD_2042, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SSR_2042)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SSR_2043 <- RCP2.6_SSR_2050s %>%
  select(Soil.quality, Tave_2043, Tmax_2043, Tmin_2043, GDD.10_2043, 
         PRE_2043, RAD_2043, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SSR_2043)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SSR_2044 <- RCP2.6_SSR_2050s %>%
  select(Soil.quality, Tave_2044, Tmax_2044, Tmin_2044, GDD.10_2044, 
         PRE_2044, RAD_2044, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SSR_2044)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SSR_2045 <- RCP2.6_SSR_2050s %>%
  select(Soil.quality, Tave_2045, Tmax_2045, Tmin_2045, GDD.10_2045, 
         PRE_2045, RAD_2045, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SSR_2045)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SSR_2046 <- RCP2.6_SSR_2050s %>%
  select(Soil.quality, Tave_2046, Tmax_2046, Tmin_2046, GDD.10_2046, 
         PRE_2046, RAD_2046, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SSR_2046)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SSR_2047 <- RCP2.6_SSR_2050s %>%
  select(Soil.quality, Tave_2047, Tmax_2047, Tmin_2047, GDD.10_2047, 
         PRE_2047, RAD_2047, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SSR_2047)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SSR_2048 <- RCP2.6_SSR_2050s %>%
  select(Soil.quality, Tave_2048, Tmax_2048, Tmin_2048, GDD.10_2048, 
         PRE_2048, RAD_2048, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SSR_2048)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SSR_2049 <- RCP2.6_SSR_2050s %>%
  select(Soil.quality, Tave_2049, Tmax_2049, Tmin_2049, GDD.10_2049, 
         PRE_2049, RAD_2049, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SSR_2049)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SSR_2050 <- RCP2.6_SSR_2050s %>%
  select(Soil.quality, Tave_2050, Tmax_2050, Tmin_2050, GDD.10_2050, 
         PRE_2050, RAD_2050, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SSR_2050)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SSR_2051 <- RCP2.6_SSR_2050s %>%
  select(Soil.quality, Tave_2051, Tmax_2051, Tmin_2051, GDD.10_2051, 
         PRE_2051, RAD_2051, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SSR_2051)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SSR_2052 <- RCP2.6_SSR_2050s %>%
  select(Soil.quality, Tave_2052, Tmax_2052, Tmin_2052, GDD.10_2052, 
         PRE_2052, RAD_2052, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SSR_2052)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SSR_2053 <- RCP2.6_SSR_2050s %>%
  select(Soil.quality, Tave_2053, Tmax_2053, Tmin_2053, GDD.10_2053, 
         PRE_2053, RAD_2053, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SSR_2053)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SSR_2054 <- RCP2.6_SSR_2050s %>%
  select(Soil.quality, Tave_2054, Tmax_2054, Tmin_2054, GDD.10_2054, 
         PRE_2054, RAD_2054, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SSR_2054)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SSR_2055 <- RCP2.6_SSR_2050s %>%
  select(Soil.quality, Tave_2055, Tmax_2055, Tmin_2055, GDD.10_2055, 
         PRE_2055, RAD_2055, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SSR_2055)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SSR_2056 <- RCP2.6_SSR_2050s %>%
  select(Soil.quality, Tave_2056, Tmax_2056, Tmin_2056, GDD.10_2056, 
         PRE_2056, RAD_2056, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SSR_2056)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SSR_2057 <- RCP2.6_SSR_2050s %>%
  select(Soil.quality, Tave_2057, Tmax_2057, Tmin_2057, GDD.10_2057, 
         PRE_2057, RAD_2057, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SSR_2057)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SSR_2058 <- RCP2.6_SSR_2050s %>%
  select(Soil.quality, Tave_2058, Tmax_2058, Tmin_2058, GDD.10_2058, 
         PRE_2058, RAD_2058, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SSR_2058)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SSR_2059 <- RCP2.6_SSR_2050s %>%
  select(Soil.quality, Tave_2059, Tmax_2059, Tmin_2059, GDD.10_2059, 
         PRE_2059, RAD_2059, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SSR_2059)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SSR_40.59 <- RCP2.6_SSR_2050s %>%
  select(Soil.quality, Tave_2040.2059, Tmax_2040.2059, Tmin_2040.2059, GDD.10_2040.2059, 
         PRE_2040.2059, RAD_2040.2059, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SSR_40.59)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                            "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                            "N", "P2O5", "K2O", "Yck", "Ynpk")

###predict yield by GBRT model

RCP2.6_SSR_pred_2050s_recyle<-NULL

RCP2.6_SSR_pred_2050s <- RCP2.6_SSR_2050s %>%
  select(Soil.quality, Crop, NO, Year, Province, 
         NO.Station)

for (i in 1:50) {
  RCP2.6_SSR_pred_2050s$Ynpk_pred_2040<-predict(brt_SRYZB_recyle[[i]],RCP2.6_SSR_2040, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP2.6_SSR_pred_2050s$Ynpk_pred_2041<-predict(brt_SRYZB_recyle[[i]],RCP2.6_SSR_2041, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP2.6_SSR_pred_2050s$Ynpk_pred_2042<-predict(brt_SRYZB_recyle[[i]],RCP2.6_SSR_2042, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP2.6_SSR_pred_2050s$Ynpk_pred_2043<-predict(brt_SRYZB_recyle[[i]],RCP2.6_SSR_2043, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP2.6_SSR_pred_2050s$Ynpk_pred_2044<-predict(brt_SRYZB_recyle[[i]],RCP2.6_SSR_2044, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP2.6_SSR_pred_2050s$Ynpk_pred_2045<-predict(brt_SRYZB_recyle[[i]],RCP2.6_SSR_2045, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP2.6_SSR_pred_2050s$Ynpk_pred_2046<-predict(brt_SRYZB_recyle[[i]],RCP2.6_SSR_2046, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP2.6_SSR_pred_2050s$Ynpk_pred_2047<-predict(brt_SRYZB_recyle[[i]],RCP2.6_SSR_2047, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP2.6_SSR_pred_2050s$Ynpk_pred_2048<-predict(brt_SRYZB_recyle[[i]],RCP2.6_SSR_2048, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP2.6_SSR_pred_2050s$Ynpk_pred_2049<-predict(brt_SRYZB_recyle[[i]],RCP2.6_SSR_2049, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP2.6_SSR_pred_2050s$Ynpk_pred_2050<-predict(brt_SRYZB_recyle[[i]],RCP2.6_SSR_2050, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP2.6_SSR_pred_2050s$Ynpk_pred_2051<-predict(brt_SRYZB_recyle[[i]],RCP2.6_SSR_2051, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP2.6_SSR_pred_2050s$Ynpk_pred_2052<-predict(brt_SRYZB_recyle[[i]],RCP2.6_SSR_2052, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP2.6_SSR_pred_2050s$Ynpk_pred_2053<-predict(brt_SRYZB_recyle[[i]],RCP2.6_SSR_2053, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP2.6_SSR_pred_2050s$Ynpk_pred_2054<-predict(brt_SRYZB_recyle[[i]],RCP2.6_SSR_2054, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP2.6_SSR_pred_2050s$Ynpk_pred_2055<-predict(brt_SRYZB_recyle[[i]],RCP2.6_SSR_2055, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP2.6_SSR_pred_2050s$Ynpk_pred_2056<-predict(brt_SRYZB_recyle[[i]],RCP2.6_SSR_2056, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP2.6_SSR_pred_2050s$Ynpk_pred_2057<-predict(brt_SRYZB_recyle[[i]],RCP2.6_SSR_2057, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP2.6_SSR_pred_2050s$Ynpk_pred_2058<-predict(brt_SRYZB_recyle[[i]],RCP2.6_SSR_2058, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP2.6_SSR_pred_2050s$Ynpk_pred_2059<-predict(brt_SRYZB_recyle[[i]],RCP2.6_SSR_2059, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP2.6_SSR_pred_2050s$Ynpk_pred_40.59<-predict(brt_SRYZB_recyle[[i]],RCP2.6_SSR_40.59, n.trees=best_inter_SRYZB_recyle[[i]])
  
  RCP2.6_SSR_pred_2050s$average<- apply(RCP2.6_SSR_pred_2050s[,7:26], 1, mean)
  RCP2.6_SSR_pred_2050s_recyle[[i]] <- RCP2.6_SSR_pred_2050s
}

RCP2.6_SSR_pred_2050s_recyle_mean<-RCP2.6_SSR_pred_2050s_recyle[[1]]
RCP2.6_SSR_pred_2050s_recyle_sd<-RCP2.6_SSR_pred_2050s_recyle[[1]]
for(i in 7:28){
  da=as.matrix(as.data.frame(sapply(1:50,function(x){RCP2.6_SSR_pred_2050s_recyle[[x]][i]})))
  RCP2.6_SSR_pred_2050s_recyle_mean[,i]=rowMeans(da)
  RCP2.6_SSR_pred_2050s_recyle_sd[,i]=apply(da,1,sd)
}

###RCP2.6 during 2080-2099

RCP2.6_SSR_2100s<-subset(RCP2.6_2100s, Crop == "single rice")
RCP2.6_SSR_2080 <- RCP2.6_SSR_2100s %>%
  select(Soil.quality, Tave_2080, Tmax_2080, Tmin_2080, GDD.10_2080, 
         PRE_2080, RAD_2080,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SSR_2080)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SSR_2081 <- RCP2.6_SSR_2100s %>%
  select(Soil.quality, Tave_2081, Tmax_2081, Tmin_2081, GDD.10_2081, 
         PRE_2081, RAD_2081, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SSR_2081)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SSR_2082 <- RCP2.6_SSR_2100s %>%
  select(Soil.quality, Tave_2082, Tmax_2082, Tmin_2082, GDD.10_2082, 
         PRE_2082, RAD_2082, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SSR_2082)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SSR_2083 <- RCP2.6_SSR_2100s %>%
  select(Soil.quality, Tave_2083, Tmax_2083, Tmin_2083, GDD.10_2083, 
         PRE_2083, RAD_2083, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SSR_2083)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SSR_2084 <- RCP2.6_SSR_2100s %>%
  select(Soil.quality, Tave_2084, Tmax_2084, Tmin_2084, GDD.10_2084, 
         PRE_2084, RAD_2084, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SSR_2084)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SSR_2085 <- RCP2.6_SSR_2100s %>%
  select(Soil.quality, Tave_2085, Tmax_2085, Tmin_2085, GDD.10_2085, 
         PRE_2085, RAD_2085,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SSR_2085)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SSR_2086 <- RCP2.6_SSR_2100s %>%
  select(Soil.quality, Tave_2086, Tmax_2086, Tmin_2086, GDD.10_2086, 
         PRE_2086, RAD_2086, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SSR_2086)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SSR_2087 <- RCP2.6_SSR_2100s %>%
  select(Soil.quality, Tave_2087, Tmax_2087, Tmin_2087, GDD.10_2087, 
         PRE_2087, RAD_2087, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SSR_2087)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SSR_2088 <- RCP2.6_SSR_2100s %>%
  select(Soil.quality, Tave_2088, Tmax_2088, Tmin_2088, GDD.10_2088, 
         PRE_2088, RAD_2088, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SSR_2088)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SSR_2089 <- RCP2.6_SSR_2100s %>%
  select(Soil.quality, Tave_2089, Tmax_2089, Tmin_2089, GDD.10_2089, 
         PRE_2089, RAD_2089, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SSR_2089)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SSR_2090 <- RCP2.6_SSR_2100s %>%
  select(Soil.quality, Tave_2090, Tmax_2090, Tmin_2090, GDD.10_2090, 
         PRE_2090, RAD_2090,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SSR_2090)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SSR_2091 <- RCP2.6_SSR_2100s %>%
  select(Soil.quality, Tave_2091, Tmax_2091, Tmin_2091, GDD.10_2091, 
         PRE_2091, RAD_2091, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SSR_2091)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SSR_2092 <- RCP2.6_SSR_2100s %>%
  select(Soil.quality, Tave_2092, Tmax_2092, Tmin_2092, GDD.10_2092, 
         PRE_2092, RAD_2092, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SSR_2092)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SSR_2093 <- RCP2.6_SSR_2100s %>%
  select(Soil.quality, Tave_2093, Tmax_2093, Tmin_2093, GDD.10_2093, 
         PRE_2093, RAD_2093,Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SSR_2093)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SSR_2094 <- RCP2.6_SSR_2100s %>%
  select(Soil.quality, Tave_2094, Tmax_2094, Tmin_2094, GDD.10_2094, 
         PRE_2094, RAD_2094,Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SSR_2094)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SSR_2095 <- RCP2.6_SSR_2100s %>%
  select(Soil.quality, Tave_2095, Tmax_2095, Tmin_2095, GDD.10_2095, 
         PRE_2095, RAD_2095, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SSR_2095)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SSR_2096 <- RCP2.6_SSR_2100s %>%
  select(Soil.quality, Tave_2096, Tmax_2096, Tmin_2096, GDD.10_2096, 
         PRE_2096, RAD_2096, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SSR_2096)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SSR_2097 <- RCP2.6_SSR_2100s %>%
  select(Soil.quality, Tave_2097, Tmax_2097, Tmin_2097, GDD.10_2097, 
         PRE_2097, RAD_2097,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SSR_2097)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SSR_2098 <- RCP2.6_SSR_2100s %>%
  select(Soil.quality, Tave_2098, Tmax_2098, Tmin_2098, GDD.10_2098, 
         PRE_2098, RAD_2098,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SSR_2098)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SSR_2099 <- RCP2.6_SSR_2100s %>%
  select(Soil.quality, Tave_2099, Tmax_2099, Tmin_2099, GDD.10_2099, 
         PRE_2099, RAD_2099, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SSR_2099)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_SSR_80.99 <- RCP2.6_SSR_2100s %>%
  select(Soil.quality, Tave_2080.2099, Tmax_2080.2099, Tmin_2080.2099, GDD.10_2080.2099, 
         PRE_2080.2099, RAD_2080.2099, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_SSR_80.99)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                            "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                            "N", "P2O5", "K2O", "Yck", "Ynpk")


###predict yield by GBRT model

RCP2.6_SSR_pred_2100s_recyle<-NULL

RCP2.6_SSR_pred_2100s <- RCP2.6_SSR_2100s %>%
  select(Soil.quality, Crop, NO, Year, Province, 
         NO.Station)

for (i in 1:50) {
  RCP2.6_SSR_pred_2100s$Ynpk_pred_2080<-predict(brt_SRYZB_recyle[[i]],RCP2.6_SSR_2080, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP2.6_SSR_pred_2100s$Ynpk_pred_2081<-predict(brt_SRYZB_recyle[[i]],RCP2.6_SSR_2081, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP2.6_SSR_pred_2100s$Ynpk_pred_2082<-predict(brt_SRYZB_recyle[[i]],RCP2.6_SSR_2082, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP2.6_SSR_pred_2100s$Ynpk_pred_2083<-predict(brt_SRYZB_recyle[[i]],RCP2.6_SSR_2083, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP2.6_SSR_pred_2100s$Ynpk_pred_2084<-predict(brt_SRYZB_recyle[[i]],RCP2.6_SSR_2084, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP2.6_SSR_pred_2100s$Ynpk_pred_2085<-predict(brt_SRYZB_recyle[[i]],RCP2.6_SSR_2085, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP2.6_SSR_pred_2100s$Ynpk_pred_2086<-predict(brt_SRYZB_recyle[[i]],RCP2.6_SSR_2086, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP2.6_SSR_pred_2100s$Ynpk_pred_2087<-predict(brt_SRYZB_recyle[[i]],RCP2.6_SSR_2087, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP2.6_SSR_pred_2100s$Ynpk_pred_2088<-predict(brt_SRYZB_recyle[[i]],RCP2.6_SSR_2088, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP2.6_SSR_pred_2100s$Ynpk_pred_2089<-predict(brt_SRYZB_recyle[[i]],RCP2.6_SSR_2089, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP2.6_SSR_pred_2100s$Ynpk_pred_2090<-predict(brt_SRYZB_recyle[[i]],RCP2.6_SSR_2090, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP2.6_SSR_pred_2100s$Ynpk_pred_2091<-predict(brt_SRYZB_recyle[[i]],RCP2.6_SSR_2091, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP2.6_SSR_pred_2100s$Ynpk_pred_2092<-predict(brt_SRYZB_recyle[[i]],RCP2.6_SSR_2092, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP2.6_SSR_pred_2100s$Ynpk_pred_2093<-predict(brt_SRYZB_recyle[[i]],RCP2.6_SSR_2093, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP2.6_SSR_pred_2100s$Ynpk_pred_2094<-predict(brt_SRYZB_recyle[[i]],RCP2.6_SSR_2094, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP2.6_SSR_pred_2100s$Ynpk_pred_2095<-predict(brt_SRYZB_recyle[[i]],RCP2.6_SSR_2095, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP2.6_SSR_pred_2100s$Ynpk_pred_2096<-predict(brt_SRYZB_recyle[[i]],RCP2.6_SSR_2096, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP2.6_SSR_pred_2100s$Ynpk_pred_2097<-predict(brt_SRYZB_recyle[[i]],RCP2.6_SSR_2097, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP2.6_SSR_pred_2100s$Ynpk_pred_2098<-predict(brt_SRYZB_recyle[[i]],RCP2.6_SSR_2098, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP2.6_SSR_pred_2100s$Ynpk_pred_2099<-predict(brt_SRYZB_recyle[[i]],RCP2.6_SSR_2099, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP2.6_SSR_pred_2100s$Ynpk_pred_80.99<-predict(brt_SRYZB_recyle[[i]],RCP2.6_SSR_80.99, n.trees=best_inter_SRYZB_recyle[[i]])
  
  RCP2.6_SSR_pred_2100s$average<- apply(RCP2.6_SSR_pred_2100s[,7:26], 1, mean)
  RCP2.6_SSR_pred_2100s_recyle[[i]] <- RCP2.6_SSR_pred_2100s
}

RCP2.6_SSR_pred_2100s_recyle_mean<-RCP2.6_SSR_pred_2100s_recyle[[1]]
RCP2.6_SSR_pred_2100s_recyle_sd<-RCP2.6_SSR_pred_2100s_recyle[[1]]
for(i in 7:28){
  da=as.matrix(as.data.frame(sapply(1:50,function(x){RCP2.6_SSR_pred_2100s_recyle[[x]][i]})))
  RCP2.6_SSR_pred_2100s_recyle_mean[,i]=rowMeans(da)
  RCP2.6_SSR_pred_2100s_recyle_sd[,i]=apply(da,1,sd)
}


###RCP8.5 during 2040-2099

RCP8.5_SSR_2050s<-subset(RCP8.5_2050s, Crop == "single rice")
RCP8.5_SSR_2040 <- RCP8.5_SSR_2050s %>%
  select(Soil.quality, Tave_2040, Tmax_2040, Tmin_2040, GDD.10_2040, 
         PRE_2040, RAD_2040, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SSR_2040)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SSR_2041 <- RCP8.5_SSR_2050s %>%
  select(Soil.quality, Tave_2041, Tmax_2041, Tmin_2041, GDD.10_2041, 
         PRE_2041, RAD_2041, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SSR_2041)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SSR_2042 <- RCP8.5_SSR_2050s %>%
  select(Soil.quality, Tave_2042, Tmax_2042, Tmin_2042, GDD.10_2042, 
         PRE_2042, RAD_2042, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SSR_2042)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SSR_2043 <- RCP8.5_SSR_2050s %>%
  select(Soil.quality, Tave_2043, Tmax_2043, Tmin_2043, GDD.10_2043, 
         PRE_2043, RAD_2043, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SSR_2043)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SSR_2044 <- RCP8.5_SSR_2050s %>%
  select(Soil.quality, Tave_2044, Tmax_2044, Tmin_2044, GDD.10_2044, 
         PRE_2044, RAD_2044, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SSR_2044)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SSR_2045 <- RCP8.5_SSR_2050s %>%
  select(Soil.quality, Tave_2045, Tmax_2045, Tmin_2045, GDD.10_2045, 
         PRE_2045, RAD_2045, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SSR_2045)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SSR_2046 <- RCP8.5_SSR_2050s %>%
  select(Soil.quality, Tave_2046, Tmax_2046, Tmin_2046, GDD.10_2046, 
         PRE_2046, RAD_2046,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SSR_2046)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SSR_2047 <- RCP8.5_SSR_2050s %>%
  select(Soil.quality, Tave_2047, Tmax_2047, Tmin_2047, GDD.10_2047, 
         PRE_2047, RAD_2047, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SSR_2047)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SSR_2048 <- RCP8.5_SSR_2050s %>%
  select(Soil.quality, Tave_2048, Tmax_2048, Tmin_2048, GDD.10_2048, 
         PRE_2048, RAD_2048, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SSR_2048)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SSR_2049 <- RCP8.5_SSR_2050s %>%
  select(Soil.quality, Tave_2049, Tmax_2049, Tmin_2049, GDD.10_2049, 
         PRE_2049, RAD_2049, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SSR_2049)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SSR_2050 <- RCP8.5_SSR_2050s %>%
  select(Soil.quality, Tave_2050, Tmax_2050, Tmin_2050, GDD.10_2050, 
         PRE_2050, RAD_2050, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SSR_2050)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SSR_2051 <- RCP8.5_SSR_2050s %>%
  select(Soil.quality, Tave_2051, Tmax_2051, Tmin_2051, GDD.10_2051, 
         PRE_2051, RAD_2051,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SSR_2051)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SSR_2052 <- RCP8.5_SSR_2050s %>%
  select(Soil.quality, Tave_2052, Tmax_2052, Tmin_2052, GDD.10_2052, 
         PRE_2052, RAD_2052, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SSR_2052)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SSR_2053 <- RCP8.5_SSR_2050s %>%
  select(Soil.quality, Tave_2053, Tmax_2053, Tmin_2053, GDD.10_2053, 
         PRE_2053, RAD_2053, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SSR_2053)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SSR_2054 <- RCP8.5_SSR_2050s %>%
  select(Soil.quality, Tave_2054, Tmax_2054, Tmin_2054, GDD.10_2054, 
         PRE_2054, RAD_2054,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SSR_2054)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SSR_2055 <- RCP8.5_SSR_2050s %>%
  select(Soil.quality, Tave_2055, Tmax_2055, Tmin_2055, GDD.10_2055, 
         PRE_2055, RAD_2055, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SSR_2055)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SSR_2056 <- RCP8.5_SSR_2050s %>%
  select(Soil.quality, Tave_2056, Tmax_2056, Tmin_2056, GDD.10_2056, 
         PRE_2056, RAD_2056, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SSR_2056)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SSR_2057 <- RCP8.5_SSR_2050s %>%
  select(Soil.quality, Tave_2057, Tmax_2057, Tmin_2057, GDD.10_2057, 
         PRE_2057, RAD_2057, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SSR_2057)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SSR_2058 <- RCP8.5_SSR_2050s %>%
  select(Soil.quality, Tave_2058, Tmax_2058, Tmin_2058, GDD.10_2058, 
         PRE_2058, RAD_2058, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SSR_2058)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SSR_2059 <- RCP8.5_SSR_2050s %>%
  select(Soil.quality, Tave_2059, Tmax_2059, Tmin_2059, GDD.10_2059, 
         PRE_2059, RAD_2059, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SSR_2059)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SSR_40.59 <- RCP8.5_SSR_2050s %>%
  select(Soil.quality, Tave_2040.2059, Tmax_2040.2059, Tmin_2040.2059, GDD.10_2040.2059, 
         PRE_2040.2059, RAD_2040.2059,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SSR_40.59)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                            "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                            "N", "P2O5", "K2O", "Yck", "Ynpk")

###predict yield by GBRT model

RCP8.5_SSR_pred_2050s_recyle<-NULL

RCP8.5_SSR_pred_2050s <- RCP8.5_SSR_2050s %>%
  select(Soil.quality, Crop, NO, Year, Province, 
         NO.Station)

for (i in 1:50) {
  RCP8.5_SSR_pred_2050s$Ynpk_pred_2040<-predict(brt_SRYZB_recyle[[i]],RCP8.5_SSR_2040, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP8.5_SSR_pred_2050s$Ynpk_pred_2041<-predict(brt_SRYZB_recyle[[i]],RCP8.5_SSR_2041, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP8.5_SSR_pred_2050s$Ynpk_pred_2042<-predict(brt_SRYZB_recyle[[i]],RCP8.5_SSR_2042, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP8.5_SSR_pred_2050s$Ynpk_pred_2043<-predict(brt_SRYZB_recyle[[i]],RCP8.5_SSR_2043, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP8.5_SSR_pred_2050s$Ynpk_pred_2044<-predict(brt_SRYZB_recyle[[i]],RCP8.5_SSR_2044, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP8.5_SSR_pred_2050s$Ynpk_pred_2045<-predict(brt_SRYZB_recyle[[i]],RCP8.5_SSR_2045, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP8.5_SSR_pred_2050s$Ynpk_pred_2046<-predict(brt_SRYZB_recyle[[i]],RCP8.5_SSR_2046, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP8.5_SSR_pred_2050s$Ynpk_pred_2047<-predict(brt_SRYZB_recyle[[i]],RCP8.5_SSR_2047, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP8.5_SSR_pred_2050s$Ynpk_pred_2048<-predict(brt_SRYZB_recyle[[i]],RCP8.5_SSR_2048, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP8.5_SSR_pred_2050s$Ynpk_pred_2049<-predict(brt_SRYZB_recyle[[i]],RCP8.5_SSR_2049, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP8.5_SSR_pred_2050s$Ynpk_pred_2050<-predict(brt_SRYZB_recyle[[i]],RCP8.5_SSR_2050, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP8.5_SSR_pred_2050s$Ynpk_pred_2051<-predict(brt_SRYZB_recyle[[i]],RCP8.5_SSR_2051, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP8.5_SSR_pred_2050s$Ynpk_pred_2052<-predict(brt_SRYZB_recyle[[i]],RCP8.5_SSR_2052, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP8.5_SSR_pred_2050s$Ynpk_pred_2053<-predict(brt_SRYZB_recyle[[i]],RCP8.5_SSR_2053, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP8.5_SSR_pred_2050s$Ynpk_pred_2054<-predict(brt_SRYZB_recyle[[i]],RCP8.5_SSR_2054, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP8.5_SSR_pred_2050s$Ynpk_pred_2055<-predict(brt_SRYZB_recyle[[i]],RCP8.5_SSR_2055, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP8.5_SSR_pred_2050s$Ynpk_pred_2056<-predict(brt_SRYZB_recyle[[i]],RCP8.5_SSR_2056, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP8.5_SSR_pred_2050s$Ynpk_pred_2057<-predict(brt_SRYZB_recyle[[i]],RCP8.5_SSR_2057, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP8.5_SSR_pred_2050s$Ynpk_pred_2058<-predict(brt_SRYZB_recyle[[i]],RCP8.5_SSR_2058, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP8.5_SSR_pred_2050s$Ynpk_pred_2059<-predict(brt_SRYZB_recyle[[i]],RCP8.5_SSR_2059, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP8.5_SSR_pred_2050s$Ynpk_pred_40.59<-predict(brt_SRYZB_recyle[[i]],RCP8.5_SSR_40.59, n.trees=best_inter_SRYZB_recyle[[i]])
  
  RCP8.5_SSR_pred_2050s$average<- apply(RCP8.5_SSR_pred_2050s[,7:26], 1, mean)
  RCP8.5_SSR_pred_2050s_recyle[[i]] <- RCP8.5_SSR_pred_2050s
}

RCP8.5_SSR_pred_2050s_recyle_mean<-RCP8.5_SSR_pred_2050s_recyle[[1]]
RCP8.5_SSR_pred_2050s_recyle_sd<-RCP8.5_SSR_pred_2050s_recyle[[1]]
for(i in 7:28){
  da=as.matrix(as.data.frame(sapply(1:50,function(x){RCP8.5_SSR_pred_2050s_recyle[[x]][i]})))
  RCP8.5_SSR_pred_2050s_recyle_mean[,i]=rowMeans(da)
  RCP8.5_SSR_pred_2050s_recyle_sd[,i]=apply(da,1,sd)
}

###RCP8.5 during 2080-2099

RCP8.5_SSR_2100s<-subset(RCP8.5_2100s, Crop == "single rice")
RCP8.5_SSR_2080 <- RCP8.5_SSR_2100s %>%
  select(Soil.quality, Tave_2080, Tmax_2080, Tmin_2080, GDD.10_2080, 
         PRE_2080, RAD_2080,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SSR_2080)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SSR_2081 <- RCP8.5_SSR_2100s %>%
  select(Soil.quality, Tave_2081, Tmax_2081, Tmin_2081, GDD.10_2081, 
         PRE_2081, RAD_2081, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SSR_2081)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SSR_2082 <- RCP8.5_SSR_2100s %>%
  select(Soil.quality, Tave_2082, Tmax_2082, Tmin_2082, GDD.10_2082, 
         PRE_2082, RAD_2082, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SSR_2082)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SSR_2083 <- RCP8.5_SSR_2100s %>%
  select(Soil.quality, Tave_2083, Tmax_2083, Tmin_2083, GDD.10_2083, 
         PRE_2083, RAD_2083, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SSR_2083)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SSR_2084 <- RCP8.5_SSR_2100s %>%
  select(Soil.quality, Tave_2084, Tmax_2084, Tmin_2084, GDD.10_2084, 
         PRE_2084, RAD_2084, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SSR_2084)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SSR_2085 <- RCP8.5_SSR_2100s %>%
  select(Soil.quality, Tave_2085, Tmax_2085, Tmin_2085, GDD.10_2085, 
         PRE_2085, RAD_2085, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SSR_2085)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SSR_2086 <- RCP8.5_SSR_2100s %>%
  select(Soil.quality, Tave_2086, Tmax_2086, Tmin_2086, GDD.10_2086, 
         PRE_2086, RAD_2086,Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SSR_2086)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SSR_2087 <- RCP8.5_SSR_2100s %>%
  select(Soil.quality, Tave_2087, Tmax_2087, Tmin_2087, GDD.10_2087, 
         PRE_2087, RAD_2087, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SSR_2087)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SSR_2088 <- RCP8.5_SSR_2100s %>%
  select(Soil.quality, Tave_2088, Tmax_2088, Tmin_2088, GDD.10_2088, 
         PRE_2088, RAD_2088, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SSR_2088)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SSR_2089 <- RCP8.5_SSR_2100s %>%
  select(Soil.quality, Tave_2089, Tmax_2089, Tmin_2089, GDD.10_2089, 
         PRE_2089, RAD_2089, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SSR_2089)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SSR_2090 <- RCP8.5_SSR_2100s %>%
  select(Soil.quality, Tave_2090, Tmax_2090, Tmin_2090, GDD.10_2090, 
         PRE_2090, RAD_2090, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SSR_2090)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SSR_2091 <- RCP8.5_SSR_2100s %>%
  select(Soil.quality, Tave_2091, Tmax_2091, Tmin_2091, GDD.10_2091, 
         PRE_2091, RAD_2091,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SSR_2091)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SSR_2092 <- RCP8.5_SSR_2100s %>%
  select(Soil.quality, Tave_2092, Tmax_2092, Tmin_2092, GDD.10_2092, 
         PRE_2092, RAD_2092, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SSR_2092)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SSR_2093 <- RCP8.5_SSR_2100s %>%
  select(Soil.quality, Tave_2093, Tmax_2093, Tmin_2093, GDD.10_2093, 
         PRE_2093, RAD_2093, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SSR_2093)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SSR_2094 <- RCP8.5_SSR_2100s %>%
  select(Soil.quality, Tave_2094, Tmax_2094, Tmin_2094, GDD.10_2094, 
         PRE_2094, RAD_2094, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SSR_2094)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SSR_2095 <- RCP8.5_SSR_2100s %>%
  select(Soil.quality, Tave_2095, Tmax_2095, Tmin_2095, GDD.10_2095, 
         PRE_2095, RAD_2095, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SSR_2095)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SSR_2096 <- RCP8.5_SSR_2100s %>%
  select(Soil.quality, Tave_2096, Tmax_2096, Tmin_2096, GDD.10_2096, 
         PRE_2096, RAD_2096,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SSR_2096)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SSR_2097 <- RCP8.5_SSR_2100s %>%
  select(Soil.quality, Tave_2097, Tmax_2097, Tmin_2097, GDD.10_2097, 
         PRE_2097, RAD_2097, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SSR_2097)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SSR_2098 <- RCP8.5_SSR_2100s %>%
  select(Soil.quality, Tave_2098, Tmax_2098, Tmin_2098, GDD.10_2098, 
         PRE_2098, RAD_2098, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SSR_2098)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SSR_2099 <- RCP8.5_SSR_2100s %>%
  select(Soil.quality, Tave_2099, Tmax_2099, Tmin_2099, GDD.10_2099, 
         PRE_2099, RAD_2099, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SSR_2099)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_SSR_80.99 <- RCP8.5_SSR_2100s %>%
  select(Soil.quality, Tave_2080.2099, Tmax_2080.2099, Tmin_2080.2099, GDD.10_2080.2099, 
         PRE_2080.2099, RAD_2080.2099, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_SSR_80.99)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                            "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                            "N", "P2O5", "K2O", "Yck", "Ynpk")


###predict yield by GBRT model
RCP8.5_SSR_pred_2100s_recyle<-NULL

RCP8.5_SSR_pred_2100s <- RCP8.5_SSR_2100s %>%
  select(Soil.quality, Crop, NO, Year, Province, 
         NO.Station)

for (i in 1:50) {
  RCP8.5_SSR_pred_2100s$Ynpk_pred_2080<-predict(brt_SRYZB_recyle[[i]],RCP8.5_SSR_2080, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP8.5_SSR_pred_2100s$Ynpk_pred_2081<-predict(brt_SRYZB_recyle[[i]],RCP8.5_SSR_2081, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP8.5_SSR_pred_2100s$Ynpk_pred_2082<-predict(brt_SRYZB_recyle[[i]],RCP8.5_SSR_2082, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP8.5_SSR_pred_2100s$Ynpk_pred_2083<-predict(brt_SRYZB_recyle[[i]],RCP8.5_SSR_2083, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP8.5_SSR_pred_2100s$Ynpk_pred_2084<-predict(brt_SRYZB_recyle[[i]],RCP8.5_SSR_2084, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP8.5_SSR_pred_2100s$Ynpk_pred_2085<-predict(brt_SRYZB_recyle[[i]],RCP8.5_SSR_2085, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP8.5_SSR_pred_2100s$Ynpk_pred_2086<-predict(brt_SRYZB_recyle[[i]],RCP8.5_SSR_2086, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP8.5_SSR_pred_2100s$Ynpk_pred_2087<-predict(brt_SRYZB_recyle[[i]],RCP8.5_SSR_2087, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP8.5_SSR_pred_2100s$Ynpk_pred_2088<-predict(brt_SRYZB_recyle[[i]],RCP8.5_SSR_2088, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP8.5_SSR_pred_2100s$Ynpk_pred_2089<-predict(brt_SRYZB_recyle[[i]],RCP8.5_SSR_2089, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP8.5_SSR_pred_2100s$Ynpk_pred_2090<-predict(brt_SRYZB_recyle[[i]],RCP8.5_SSR_2090, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP8.5_SSR_pred_2100s$Ynpk_pred_2091<-predict(brt_SRYZB_recyle[[i]],RCP8.5_SSR_2091, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP8.5_SSR_pred_2100s$Ynpk_pred_2092<-predict(brt_SRYZB_recyle[[i]],RCP8.5_SSR_2092, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP8.5_SSR_pred_2100s$Ynpk_pred_2093<-predict(brt_SRYZB_recyle[[i]],RCP8.5_SSR_2093, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP8.5_SSR_pred_2100s$Ynpk_pred_2094<-predict(brt_SRYZB_recyle[[i]],RCP8.5_SSR_2094, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP8.5_SSR_pred_2100s$Ynpk_pred_2095<-predict(brt_SRYZB_recyle[[i]],RCP8.5_SSR_2095, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP8.5_SSR_pred_2100s$Ynpk_pred_2096<-predict(brt_SRYZB_recyle[[i]],RCP8.5_SSR_2096, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP8.5_SSR_pred_2100s$Ynpk_pred_2097<-predict(brt_SRYZB_recyle[[i]],RCP8.5_SSR_2097, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP8.5_SSR_pred_2100s$Ynpk_pred_2098<-predict(brt_SRYZB_recyle[[i]],RCP8.5_SSR_2098, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP8.5_SSR_pred_2100s$Ynpk_pred_2099<-predict(brt_SRYZB_recyle[[i]],RCP8.5_SSR_2099, n.trees=best_inter_SRYZB_recyle[[i]])
  RCP8.5_SSR_pred_2100s$Ynpk_pred_80.99<-predict(brt_SRYZB_recyle[[i]],RCP8.5_SSR_80.99, n.trees=best_inter_SRYZB_recyle[[i]])
  
  RCP8.5_SSR_pred_2100s$average<- apply(RCP8.5_SSR_pred_2100s[,7:26], 1, mean)
  RCP8.5_SSR_pred_2100s_recyle[[i]] <- RCP8.5_SSR_pred_2100s
}

RCP8.5_SSR_pred_2100s_recyle_mean<-RCP8.5_SSR_pred_2100s_recyle[[1]]
RCP8.5_SSR_pred_2100s_recyle_sd<-RCP8.5_SSR_pred_2100s_recyle[[1]]
for(i in 7:28){
  da=as.matrix(as.data.frame(sapply(1:50,function(x){RCP8.5_SSR_pred_2100s_recyle[[x]][i]})))
  RCP8.5_SSR_pred_2100s_recyle_mean[,i]=rowMeans(da)
  RCP8.5_SSR_pred_2100s_recyle_sd[,i]=apply(da,1,sd)
}

###summarise predicted yield

sum_RCP2.6_SSR_2050s_year<- cbind(base_SSR_pred_all[,c("Soil.quality","Crop","NO","average")],RCP2.6_SSR_pred_2050s[,7:28])
names(sum_RCP2.6_SSR_2050s_year)[1:4]<- c("Soil quality", "Crop", "No", "Yield_base")
names(sum_RCP2.6_SSR_2050s_year)[26]<- "Yield_RCP"
sum_RCP2.6_SSR_2050s_year$Yield_change <- sum_RCP2.6_SSR_2050s_year$`Yield_RCP` - sum_RCP2.6_SSR_2050s_year$`Yield_base`
sum_RCP2.6_SSR_2050s_year$Yield_change_per <- (sum_RCP2.6_SSR_2050s_year$Yield_change / sum_RCP2.6_SSR_2050s_year$Yield_base)*100
sum_RCP2.6_SSR_2050s_year$RCP <- "RCP2.6"
sum_RCP2.6_SSR_2050s_year$Time <- "2050"

sum_RCP8.5_SSR_2050s_year<- cbind(base_SSR_pred_all[,c("Soil.quality","Crop","NO","average")],RCP8.5_SSR_pred_2050s[,7:28])
names(sum_RCP8.5_SSR_2050s_year)[1:4]<- c("Soil quality", "Crop", "No", "Yield_base")
names(sum_RCP8.5_SSR_2050s_year)[26]<- "Yield_RCP"
sum_RCP8.5_SSR_2050s_year$Yield_change <- sum_RCP8.5_SSR_2050s_year$`Yield_RCP` - sum_RCP8.5_SSR_2050s_year$`Yield_base`
sum_RCP8.5_SSR_2050s_year$Yield_change_per <- (sum_RCP8.5_SSR_2050s_year$Yield_change / sum_RCP8.5_SSR_2050s_year$Yield_base)*100
sum_RCP8.5_SSR_2050s_year$RCP <- "RCP8.5"
sum_RCP8.5_SSR_2050s_year$Time <- "2050"

sum_RCP2.6_SSR_2100s_year<- cbind(base_SSR_pred_all[,c("Soil.quality","Crop","NO","average")],RCP2.6_SSR_pred_2100s[,7:28])
names(sum_RCP2.6_SSR_2100s_year)[1:4]<- c("Soil quality", "Crop", "No", "Yield_base")
names(sum_RCP2.6_SSR_2100s_year)[26]<- "Yield_RCP"
sum_RCP2.6_SSR_2100s_year$Yield_change <- sum_RCP2.6_SSR_2100s_year$`Yield_RCP` - sum_RCP2.6_SSR_2100s_year$`Yield_base`
sum_RCP2.6_SSR_2100s_year$Yield_change_per <- (sum_RCP2.6_SSR_2100s_year$Yield_change / sum_RCP2.6_SSR_2100s_year$Yield_base)*100
sum_RCP2.6_SSR_2100s_year$RCP <- "RCP2.6"
sum_RCP2.6_SSR_2100s_year$Time <- "2100"

sum_RCP8.5_SSR_2100s_year<- cbind(base_SSR_pred_all[,c("Soil.quality","Crop","NO","average")],RCP8.5_SSR_pred_2100s[,7:28])
names(sum_RCP8.5_SSR_2100s_year)[1:4]<- c("Soil quality", "Crop", "No", "Yield_base")
names(sum_RCP8.5_SSR_2100s_year)[26]<- "Yield_RCP"
sum_RCP8.5_SSR_2100s_year$Yield_change <- sum_RCP8.5_SSR_2100s_year$`Yield_RCP` - sum_RCP8.5_SSR_2100s_year$`Yield_base`
sum_RCP8.5_SSR_2100s_year$Yield_change_per <- (sum_RCP8.5_SSR_2100s_year$Yield_change / sum_RCP8.5_SSR_2100s_year$Yield_base)*100
sum_RCP8.5_SSR_2100s_year$RCP <- "RCP8.5"
sum_RCP8.5_SSR_2100s_year$Time <- "2100"

sum_RCP_SSR_2050s_year <- rbind(sum_RCP2.6_SSR_2050s_year,sum_RCP8.5_SSR_2050s_year)
sum_RCP_SSR_2100s_year<-rbind(sum_RCP2.6_SSR_2100s_year,sum_RCP8.5_SSR_2100s_year)

write.csv(sum_RCP_SSR_2050s_year,"./results/sum_RCP_SSR_2050s_year.csv")
write.csv(sum_RCP_SSR_2100s_year,"./results/sum_RCP_SSR_2100s_year.csv")

###comparison of yield change between high and low quality soils 
###T test

t.test(Yield_change~`Soil quality` ,sum_RCP2.6_SSR_2050s_year) #RCP2.6-2050s
t.test(Yield_change~`Soil quality` ,sum_RCP8.5_SSR_2050s_year) #RCP8.5-2050s
t.test(Yield_change~`Soil quality` ,sum_RCP2.6_SSR_2100s_year) #RCP2.6-2100s
t.test(Yield_change~`Soil quality` ,sum_RCP8.5_SSR_2100s_year) #RCP8.5-2100s

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

for (i in 1:50) {
  # set train and test data(random 10%)
  set.seed(i)
  trainingRow_ERSC <- createDataPartition(ER_SC$Province,p=0.9, list = F)
  ER_SC_train <- ER_SC[trainingRow_ERSC,] 
  ER_SC_test <- ER_SC[-trainingRow_ERSC,] 
  

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
                interaction.depth = 7, shrinkage = 0.005, 
                bag.fraction = 0.5,cv.folds = 10)
  
  brt_ERSC_recyle[[i]]<-brt_ERSC
  print(brt_ERSC)
  best_inter_ERSC<-gbm.perf(brt_ERSC,method = "cv")
  best_inter_ERSC_recyle[[i]]<-best_inter_ERSC
  summary_ERSC_record<-summary(brt_ERSC,n.trees=best_inter_ERSC)
  summary_ERSC_recyle[[i]] <-summary_ERSC_record
}

#####Predicted yield under different climate scenarios
###baseline period 1986-2005
base_ER<-subset(base, Crop == "early rice")
base_ER_1986 <- base_ER %>%
  select(Soil.quality, Tave_1986, Tmax_1986, Tmin_1986, GDD.10_1986, 
         PRE_1986, RAD_1986, Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_ER_1986)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_ER_1987 <- base_ER %>%
  select(Soil.quality, Tave_1987, Tmax_1987, Tmin_1987, GDD.10_1987, 
         PRE_1987, RAD_1987, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_ER_1987)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_ER_1988 <- base_ER %>%
  select(Soil.quality, Tave_1988, Tmax_1988, Tmin_1988, GDD.10_1988, 
         PRE_1988, RAD_1988, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_ER_1988)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_ER_1989 <- base_ER %>%
  select(Soil.quality, Tave_1989, Tmax_1989, Tmin_1989, GDD.10_1989, 
         PRE_1989, RAD_1989, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_ER_1989)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_ER_1990 <- base_ER %>%
  select(Soil.quality, Tave_1990, Tmax_1990, Tmin_1990, GDD.10_1990, 
         PRE_1990, RAD_1990, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_ER_1990)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_ER_1991 <- base_ER %>%
  select(Soil.quality, Tave_1991, Tmax_1991, Tmin_1991, GDD.10_1991, 
         PRE_1991, RAD_1991, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_ER_1991)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_ER_1992 <- base_ER %>%
  select(Soil.quality, Tave_1992, Tmax_1992, Tmin_1992, GDD.10_1992, 
         PRE_1992, RAD_1992,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_ER_1992)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_ER_1993 <- base_ER %>%
  select(Soil.quality, Tave_1993, Tmax_1993, Tmin_1993, GDD.10_1993, 
         PRE_1993, RAD_1993, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_ER_1993)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_ER_1994 <- base_ER %>%
  select(Soil.quality, Tave_1994, Tmax_1994, Tmin_1994, GDD.10_1994, 
         PRE_1994, RAD_1994, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_ER_1994)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_ER_1995 <- base_ER %>%
  select(Soil.quality, Tave_1995, Tmax_1995, Tmin_1995, GDD.10_1995, 
         PRE_1995, RAD_1995, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_ER_1995)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_ER_1996 <- base_ER %>%
  select(Soil.quality, Tave_1996, Tmax_1996, Tmin_1996, GDD.10_1996, 
         PRE_1996, RAD_1996, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_ER_1996)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_ER_1997 <- base_ER %>%
  select(Soil.quality, Tave_1997, Tmax_1997, Tmin_1997, GDD.10_1997, 
         PRE_1997, RAD_1997, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_ER_1997)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_ER_1998 <- base_ER %>%
  select(Soil.quality, Tave_1998, Tmax_1998, Tmin_1998, GDD.10_1998, 
         PRE_1998, RAD_1998, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_ER_1998)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_ER_1999 <- base_ER %>%
  select(Soil.quality, Tave_1999, Tmax_1999, Tmin_1999, GDD.10_1999, 
         PRE_1999, RAD_1999, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_ER_1999)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_ER_2000 <- base_ER %>%
  select(Soil.quality, Tave_2000, Tmax_2000, Tmin_2000, GDD.10_2000, 
         PRE_2000, RAD_2000, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_ER_2000)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_ER_2001 <- base_ER %>%
  select(Soil.quality, Tave_2001, Tmax_2001, Tmin_2001, GDD.10_2001, 
         PRE_2001, RAD_2001, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_ER_2001)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_ER_2002 <- base_ER %>%
  select(Soil.quality, Tave_2002, Tmax_2002, Tmin_2002, GDD.10_2002, 
         PRE_2002, RAD_2002, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_ER_2002)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_ER_2003 <- base_ER %>%
  select(Soil.quality, Tave_2003, Tmax_2003, Tmin_2003, GDD.10_2003, 
         PRE_2003, RAD_2003, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_ER_2003)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_ER_2004 <- base_ER %>%
  select(Soil.quality, Tave_2004, Tmax_2004, Tmin_2004, GDD.10_2004, 
         PRE_2004, RAD_2004, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_ER_2004)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_ER_2005 <- base_ER %>%
  select(Soil.quality, Tave_2005, Tmax_2005, Tmin_2005, GDD.10_2005, 
         PRE_2005, RAD_2005, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_ER_2005)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_ER_86.05 <- base_ER %>%
  select(Soil.quality, Tave_1986.2005, Tmax_1986.2005, Tmin_1986.2005, GDD.10_1986.2005, 
         PRE_1986.2005, RAD_1986.2005, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_ER_86.05)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                         "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                         "N", "P2O5", "K2O", "Yck", "Ynpk")

###predict yield by GBRT model

base_ER_pred_all_recyle<-NULL

base_ER_pred_all <- base_ER %>%
  select(Soil.quality, Crop, NO, Year,Province, 
         NO.Station)

for (i in 1:50) {
  base_ER_pred_all$Ynpk_pred_1986<-predict(brt_ERSC_recyle[[i]],base_ER_1986, n.trees=best_inter_ERSC_recyle[[i]])
  base_ER_pred_all$Ynpk_pred_1987<-predict(brt_ERSC_recyle[[i]],base_ER_1987, n.trees=best_inter_ERSC_recyle[[i]])
  base_ER_pred_all$Ynpk_pred_1988<-predict(brt_ERSC_recyle[[i]],base_ER_1988, n.trees=best_inter_ERSC_recyle[[i]])
  base_ER_pred_all$Ynpk_pred_1989<-predict(brt_ERSC_recyle[[i]],base_ER_1989, n.trees=best_inter_ERSC_recyle[[i]])
  base_ER_pred_all$Ynpk_pred_1990<-predict(brt_ERSC_recyle[[i]],base_ER_1990, n.trees=best_inter_ERSC_recyle[[i]])
  base_ER_pred_all$Ynpk_pred_1991<-predict(brt_ERSC_recyle[[i]],base_ER_1991, n.trees=best_inter_ERSC_recyle[[i]])
  base_ER_pred_all$Ynpk_pred_1992<-predict(brt_ERSC_recyle[[i]],base_ER_1992, n.trees=best_inter_ERSC_recyle[[i]])
  base_ER_pred_all$Ynpk_pred_1993<-predict(brt_ERSC_recyle[[i]],base_ER_1993, n.trees=best_inter_ERSC_recyle[[i]])
  base_ER_pred_all$Ynpk_pred_1994<-predict(brt_ERSC_recyle[[i]],base_ER_1994, n.trees=best_inter_ERSC_recyle[[i]])
  base_ER_pred_all$Ynpk_pred_1995<-predict(brt_ERSC_recyle[[i]],base_ER_1995, n.trees=best_inter_ERSC_recyle[[i]])
  base_ER_pred_all$Ynpk_pred_1996<-predict(brt_ERSC_recyle[[i]],base_ER_1996, n.trees=best_inter_ERSC_recyle[[i]])
  base_ER_pred_all$Ynpk_pred_1997<-predict(brt_ERSC_recyle[[i]],base_ER_1997, n.trees=best_inter_ERSC_recyle[[i]])
  base_ER_pred_all$Ynpk_pred_1998<-predict(brt_ERSC_recyle[[i]],base_ER_1998, n.trees=best_inter_ERSC_recyle[[i]])
  base_ER_pred_all$Ynpk_pred_1999<-predict(brt_ERSC_recyle[[i]],base_ER_1999, n.trees=best_inter_ERSC_recyle[[i]])
  base_ER_pred_all$Ynpk_pred_2000<-predict(brt_ERSC_recyle[[i]],base_ER_2000, n.trees=best_inter_ERSC_recyle[[i]])
  base_ER_pred_all$Ynpk_pred_2001<-predict(brt_ERSC_recyle[[i]],base_ER_2001, n.trees=best_inter_ERSC_recyle[[i]])
  base_ER_pred_all$Ynpk_pred_2002<-predict(brt_ERSC_recyle[[i]],base_ER_2002, n.trees=best_inter_ERSC_recyle[[i]])
  base_ER_pred_all$Ynpk_pred_2003<-predict(brt_ERSC_recyle[[i]],base_ER_2003, n.trees=best_inter_ERSC_recyle[[i]])
  base_ER_pred_all$Ynpk_pred_2004<-predict(brt_ERSC_recyle[[i]],base_ER_2004, n.trees=best_inter_ERSC_recyle[[i]])
  base_ER_pred_all$Ynpk_pred_2005<-predict(brt_ERSC_recyle[[i]],base_ER_2005, n.trees=best_inter_ERSC_recyle[[i]])
  base_ER_pred_all$Ynpk_pred_86.05<-predict(brt_ERSC_recyle[[i]],base_ER_86.05, n.trees=best_inter_ERSC_recyle[[i]])
  
  base_ER_pred_all$average<- apply(base_ER_pred_all[,7:26], 1, mean)
  base_ER_pred_all_recyle[[i]] <- base_ER_pred_all
}

base_ER_pred_all_recyle_mean<-base_ER_pred_all_recyle[[1]]
base_ER_pred_all_recyle_sd<-base_ER_pred_all_recyle[[1]]
for(i in 7:28){
  da=as.matrix(as.data.frame(sapply(1:50,function(x){base_ER_pred_all_recyle[[x]][i]})))
  base_ER_pred_all_recyle_mean[,i]=rowMeans(da)
  base_ER_pred_all_recyle_sd[,i]=apply(da,1,sd)
}

###RCP2.6 during 2040-2059

RCP2.6_ER_2050s<-subset(RCP2.6_2050s, Crop == "early rice")
RCP2.6_ER_2040 <- RCP2.6_ER_2050s %>%
  select(Soil.quality, Tave_2040, Tmax_2040, Tmin_2040, GDD.10_2040, 
         PRE_2040, RAD_2040, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_ER_2040)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_ER_2041 <- RCP2.6_ER_2050s %>%
  select(Soil.quality, Tave_2041, Tmax_2041, Tmin_2041, GDD.10_2041, 
         PRE_2041, RAD_2041, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_ER_2041)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_ER_2042 <- RCP2.6_ER_2050s %>%
  select(Soil.quality, Tave_2042, Tmax_2042, Tmin_2042, GDD.10_2042, 
         PRE_2042, RAD_2042, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_ER_2042)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_ER_2043 <- RCP2.6_ER_2050s %>%
  select(Soil.quality, Tave_2043, Tmax_2043, Tmin_2043, GDD.10_2043, 
         PRE_2043, RAD_2043, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_ER_2043)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_ER_2044 <- RCP2.6_ER_2050s %>%
  select(Soil.quality, Tave_2044, Tmax_2044, Tmin_2044, GDD.10_2044, 
         PRE_2044, RAD_2044, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_ER_2044)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_ER_2045 <- RCP2.6_ER_2050s %>%
  select(Soil.quality, Tave_2045, Tmax_2045, Tmin_2045, GDD.10_2045, 
         PRE_2045, RAD_2045, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_ER_2045)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_ER_2046 <- RCP2.6_ER_2050s %>%
  select(Soil.quality, Tave_2046, Tmax_2046, Tmin_2046, GDD.10_2046, 
         PRE_2046, RAD_2046, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_ER_2046)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_ER_2047 <- RCP2.6_ER_2050s %>%
  select(Soil.quality, Tave_2047, Tmax_2047, Tmin_2047, GDD.10_2047, 
         PRE_2047, RAD_2047, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_ER_2047)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_ER_2048 <- RCP2.6_ER_2050s %>%
  select(Soil.quality, Tave_2048, Tmax_2048, Tmin_2048, GDD.10_2048, 
         PRE_2048, RAD_2048, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_ER_2048)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_ER_2049 <- RCP2.6_ER_2050s %>%
  select(Soil.quality, Tave_2049, Tmax_2049, Tmin_2049, GDD.10_2049, 
         PRE_2049, RAD_2049, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_ER_2049)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_ER_2050 <- RCP2.6_ER_2050s %>%
  select(Soil.quality, Tave_2050, Tmax_2050, Tmin_2050, GDD.10_2050, 
         PRE_2050, RAD_2050, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_ER_2050)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_ER_2051 <- RCP2.6_ER_2050s %>%
  select(Soil.quality, Tave_2051, Tmax_2051, Tmin_2051, GDD.10_2051, 
         PRE_2051, RAD_2051, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_ER_2051)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_ER_2052 <- RCP2.6_ER_2050s %>%
  select(Soil.quality, Tave_2052, Tmax_2052, Tmin_2052, GDD.10_2052, 
         PRE_2052, RAD_2052, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_ER_2052)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_ER_2053 <- RCP2.6_ER_2050s %>%
  select(Soil.quality, Tave_2053, Tmax_2053, Tmin_2053, GDD.10_2053, 
         PRE_2053, RAD_2053, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_ER_2053)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_ER_2054 <- RCP2.6_ER_2050s %>%
  select(Soil.quality, Tave_2054, Tmax_2054, Tmin_2054, GDD.10_2054, 
         PRE_2054, RAD_2054, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_ER_2054)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_ER_2055 <- RCP2.6_ER_2050s %>%
  select(Soil.quality, Tave_2055, Tmax_2055, Tmin_2055, GDD.10_2055, 
         PRE_2055, RAD_2055, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_ER_2055)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_ER_2056 <- RCP2.6_ER_2050s %>%
  select(Soil.quality, Tave_2056, Tmax_2056, Tmin_2056, GDD.10_2056, 
         PRE_2056, RAD_2056, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_ER_2056)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_ER_2057 <- RCP2.6_ER_2050s %>%
  select(Soil.quality, Tave_2057, Tmax_2057, Tmin_2057, GDD.10_2057, 
         PRE_2057, RAD_2057, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_ER_2057)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_ER_2058 <- RCP2.6_ER_2050s %>%
  select(Soil.quality, Tave_2058, Tmax_2058, Tmin_2058, GDD.10_2058, 
         PRE_2058, RAD_2058, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_ER_2058)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_ER_2059 <- RCP2.6_ER_2050s %>%
  select(Soil.quality, Tave_2059, Tmax_2059, Tmin_2059, GDD.10_2059, 
         PRE_2059, RAD_2059, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_ER_2059)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_ER_40.59 <- RCP2.6_ER_2050s %>%
  select(Soil.quality, Tave_2040.2059, Tmax_2040.2059, Tmin_2040.2059, GDD.10_2040.2059, 
         PRE_2040.2059, RAD_2040.2059, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_ER_40.59)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

###predict yield by GBRT model

RCP2.6_ER_pred_2050s_recyle<-NULL

RCP2.6_ER_pred_2050s <- RCP2.6_ER_2050s %>%
  select(Soil.quality, Crop, NO, Year, Province, 
         NO.Station)

for (i in 1:50) {
  RCP2.6_ER_pred_2050s$Ynpk_pred_2040<-predict(brt_ERSC_recyle[[i]],RCP2.6_ER_2040, n.trees=best_inter_ERSC_recyle[[i]])
  RCP2.6_ER_pred_2050s$Ynpk_pred_2041<-predict(brt_ERSC_recyle[[i]],RCP2.6_ER_2041, n.trees=best_inter_ERSC_recyle[[i]])
  RCP2.6_ER_pred_2050s$Ynpk_pred_2042<-predict(brt_ERSC_recyle[[i]],RCP2.6_ER_2042, n.trees=best_inter_ERSC_recyle[[i]])
  RCP2.6_ER_pred_2050s$Ynpk_pred_2043<-predict(brt_ERSC_recyle[[i]],RCP2.6_ER_2043, n.trees=best_inter_ERSC_recyle[[i]])
  RCP2.6_ER_pred_2050s$Ynpk_pred_2044<-predict(brt_ERSC_recyle[[i]],RCP2.6_ER_2044, n.trees=best_inter_ERSC_recyle[[i]])
  RCP2.6_ER_pred_2050s$Ynpk_pred_2045<-predict(brt_ERSC_recyle[[i]],RCP2.6_ER_2045, n.trees=best_inter_ERSC_recyle[[i]])
  RCP2.6_ER_pred_2050s$Ynpk_pred_2046<-predict(brt_ERSC_recyle[[i]],RCP2.6_ER_2046, n.trees=best_inter_ERSC_recyle[[i]])
  RCP2.6_ER_pred_2050s$Ynpk_pred_2047<-predict(brt_ERSC_recyle[[i]],RCP2.6_ER_2047, n.trees=best_inter_ERSC_recyle[[i]])
  RCP2.6_ER_pred_2050s$Ynpk_pred_2048<-predict(brt_ERSC_recyle[[i]],RCP2.6_ER_2048, n.trees=best_inter_ERSC_recyle[[i]])
  RCP2.6_ER_pred_2050s$Ynpk_pred_2049<-predict(brt_ERSC_recyle[[i]],RCP2.6_ER_2049, n.trees=best_inter_ERSC_recyle[[i]])
  RCP2.6_ER_pred_2050s$Ynpk_pred_2050<-predict(brt_ERSC_recyle[[i]],RCP2.6_ER_2050, n.trees=best_inter_ERSC_recyle[[i]])
  RCP2.6_ER_pred_2050s$Ynpk_pred_2051<-predict(brt_ERSC_recyle[[i]],RCP2.6_ER_2051, n.trees=best_inter_ERSC_recyle[[i]])
  RCP2.6_ER_pred_2050s$Ynpk_pred_2052<-predict(brt_ERSC_recyle[[i]],RCP2.6_ER_2052, n.trees=best_inter_ERSC_recyle[[i]])
  RCP2.6_ER_pred_2050s$Ynpk_pred_2053<-predict(brt_ERSC_recyle[[i]],RCP2.6_ER_2053, n.trees=best_inter_ERSC_recyle[[i]])
  RCP2.6_ER_pred_2050s$Ynpk_pred_2054<-predict(brt_ERSC_recyle[[i]],RCP2.6_ER_2054, n.trees=best_inter_ERSC_recyle[[i]])
  RCP2.6_ER_pred_2050s$Ynpk_pred_2055<-predict(brt_ERSC_recyle[[i]],RCP2.6_ER_2055, n.trees=best_inter_ERSC_recyle[[i]])
  RCP2.6_ER_pred_2050s$Ynpk_pred_2056<-predict(brt_ERSC_recyle[[i]],RCP2.6_ER_2056, n.trees=best_inter_ERSC_recyle[[i]])
  RCP2.6_ER_pred_2050s$Ynpk_pred_2057<-predict(brt_ERSC_recyle[[i]],RCP2.6_ER_2057, n.trees=best_inter_ERSC_recyle[[i]])
  RCP2.6_ER_pred_2050s$Ynpk_pred_2058<-predict(brt_ERSC_recyle[[i]],RCP2.6_ER_2058, n.trees=best_inter_ERSC_recyle[[i]])
  RCP2.6_ER_pred_2050s$Ynpk_pred_2059<-predict(brt_ERSC_recyle[[i]],RCP2.6_ER_2059, n.trees=best_inter_ERSC_recyle[[i]])
  RCP2.6_ER_pred_2050s$Ynpk_pred_40.59<-predict(brt_ERSC_recyle[[i]],RCP2.6_ER_40.59, n.trees=best_inter_ERSC_recyle[[i]])
  
  RCP2.6_ER_pred_2050s$average<- apply(RCP2.6_ER_pred_2050s[,7:26], 1, mean)
  RCP2.6_ER_pred_2050s_recyle[[i]] <- RCP2.6_ER_pred_2050s
}

RCP2.6_ER_pred_2050s_recyle_mean<-RCP2.6_ER_pred_2050s_recyle[[1]]
RCP2.6_ER_pred_2050s_recyle_sd<-RCP2.6_ER_pred_2050s_recyle[[1]]
for(i in 7:28){
  da=as.matrix(as.data.frame(sapply(1:50,function(x){RCP2.6_ER_pred_2050s_recyle[[x]][i]})))
  RCP2.6_ER_pred_2050s_recyle_mean[,i]=rowMeans(da)
  RCP2.6_ER_pred_2050s_recyle_sd[,i]=apply(da,1,sd)
}

###RCP2.6 during 2080-2099

RCP2.6_ER_2100s<-subset(RCP2.6_2100s, Crop == "early rice")
RCP2.6_ER_2080 <- RCP2.6_ER_2100s %>%
  select(Soil.quality, Tave_2080, Tmax_2080, Tmin_2080, GDD.10_2080, 
         PRE_2080, RAD_2080,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_ER_2080)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_ER_2081 <- RCP2.6_ER_2100s %>%
  select(Soil.quality, Tave_2081, Tmax_2081, Tmin_2081, GDD.10_2081, 
         PRE_2081, RAD_2081, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_ER_2081)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_ER_2082 <- RCP2.6_ER_2100s %>%
  select(Soil.quality, Tave_2082, Tmax_2082, Tmin_2082, GDD.10_2082, 
         PRE_2082, RAD_2082, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_ER_2082)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_ER_2083 <- RCP2.6_ER_2100s %>%
  select(Soil.quality, Tave_2083, Tmax_2083, Tmin_2083, GDD.10_2083, 
         PRE_2083, RAD_2083, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_ER_2083)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_ER_2084 <- RCP2.6_ER_2100s %>%
  select(Soil.quality, Tave_2084, Tmax_2084, Tmin_2084, GDD.10_2084, 
         PRE_2084, RAD_2084, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_ER_2084)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_ER_2085 <- RCP2.6_ER_2100s %>%
  select(Soil.quality, Tave_2085, Tmax_2085, Tmin_2085, GDD.10_2085, 
         PRE_2085, RAD_2085,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_ER_2085)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_ER_2086 <- RCP2.6_ER_2100s %>%
  select(Soil.quality, Tave_2086, Tmax_2086, Tmin_2086, GDD.10_2086, 
         PRE_2086, RAD_2086, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_ER_2086)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_ER_2087 <- RCP2.6_ER_2100s %>%
  select(Soil.quality, Tave_2087, Tmax_2087, Tmin_2087, GDD.10_2087, 
         PRE_2087, RAD_2087, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_ER_2087)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_ER_2088 <- RCP2.6_ER_2100s %>%
  select(Soil.quality, Tave_2088, Tmax_2088, Tmin_2088, GDD.10_2088, 
         PRE_2088, RAD_2088, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_ER_2088)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_ER_2089 <- RCP2.6_ER_2100s %>%
  select(Soil.quality, Tave_2089, Tmax_2089, Tmin_2089, GDD.10_2089, 
         PRE_2089, RAD_2089, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_ER_2089)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_ER_2090 <- RCP2.6_ER_2100s %>%
  select(Soil.quality, Tave_2090, Tmax_2090, Tmin_2090, GDD.10_2090, 
         PRE_2090, RAD_2090,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_ER_2090)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_ER_2091 <- RCP2.6_ER_2100s %>%
  select(Soil.quality, Tave_2091, Tmax_2091, Tmin_2091, GDD.10_2091, 
         PRE_2091, RAD_2091, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_ER_2091)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_ER_2092 <- RCP2.6_ER_2100s %>%
  select(Soil.quality, Tave_2092, Tmax_2092, Tmin_2092, GDD.10_2092, 
         PRE_2092, RAD_2092, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_ER_2092)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_ER_2093 <- RCP2.6_ER_2100s %>%
  select(Soil.quality, Tave_2093, Tmax_2093, Tmin_2093, GDD.10_2093, 
         PRE_2093, RAD_2093,Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_ER_2093)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_ER_2094 <- RCP2.6_ER_2100s %>%
  select(Soil.quality, Tave_2094, Tmax_2094, Tmin_2094, GDD.10_2094, 
         PRE_2094, RAD_2094,Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_ER_2094)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_ER_2095 <- RCP2.6_ER_2100s %>%
  select(Soil.quality, Tave_2095, Tmax_2095, Tmin_2095, GDD.10_2095, 
         PRE_2095, RAD_2095, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_ER_2095)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_ER_2096 <- RCP2.6_ER_2100s %>%
  select(Soil.quality, Tave_2096, Tmax_2096, Tmin_2096, GDD.10_2096, 
         PRE_2096, RAD_2096, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_ER_2096)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_ER_2097 <- RCP2.6_ER_2100s %>%
  select(Soil.quality, Tave_2097, Tmax_2097, Tmin_2097, GDD.10_2097, 
         PRE_2097, RAD_2097,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_ER_2097)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_ER_2098 <- RCP2.6_ER_2100s %>%
  select(Soil.quality, Tave_2098, Tmax_2098, Tmin_2098, GDD.10_2098, 
         PRE_2098, RAD_2098,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_ER_2098)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_ER_2099 <- RCP2.6_ER_2100s %>%
  select(Soil.quality, Tave_2099, Tmax_2099, Tmin_2099, GDD.10_2099, 
         PRE_2099, RAD_2099, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_ER_2099)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_ER_80.99 <- RCP2.6_ER_2100s %>%
  select(Soil.quality, Tave_2080.2099, Tmax_2080.2099, Tmin_2080.2099, GDD.10_2080.2099, 
         PRE_2080.2099, RAD_2080.2099, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_ER_80.99)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")


###predict yield by GBRT model

RCP2.6_ER_pred_2100s_recyle<-NULL

RCP2.6_ER_pred_2100s <- RCP2.6_ER_2100s %>%
  select(Soil.quality, Crop, NO, Year, Province, 
         NO.Station)

for (i in 1:50) {
  RCP2.6_ER_pred_2100s$Ynpk_pred_2080<-predict(brt_ERSC_recyle[[i]],RCP2.6_ER_2080, n.trees=best_inter_ERSC_recyle[[i]])
  RCP2.6_ER_pred_2100s$Ynpk_pred_2081<-predict(brt_ERSC_recyle[[i]],RCP2.6_ER_2081, n.trees=best_inter_ERSC_recyle[[i]])
  RCP2.6_ER_pred_2100s$Ynpk_pred_2082<-predict(brt_ERSC_recyle[[i]],RCP2.6_ER_2082, n.trees=best_inter_ERSC_recyle[[i]])
  RCP2.6_ER_pred_2100s$Ynpk_pred_2083<-predict(brt_ERSC_recyle[[i]],RCP2.6_ER_2083, n.trees=best_inter_ERSC_recyle[[i]])
  RCP2.6_ER_pred_2100s$Ynpk_pred_2084<-predict(brt_ERSC_recyle[[i]],RCP2.6_ER_2084, n.trees=best_inter_ERSC_recyle[[i]])
  RCP2.6_ER_pred_2100s$Ynpk_pred_2085<-predict(brt_ERSC_recyle[[i]],RCP2.6_ER_2085, n.trees=best_inter_ERSC_recyle[[i]])
  RCP2.6_ER_pred_2100s$Ynpk_pred_2086<-predict(brt_ERSC_recyle[[i]],RCP2.6_ER_2086, n.trees=best_inter_ERSC_recyle[[i]])
  RCP2.6_ER_pred_2100s$Ynpk_pred_2087<-predict(brt_ERSC_recyle[[i]],RCP2.6_ER_2087, n.trees=best_inter_ERSC_recyle[[i]])
  RCP2.6_ER_pred_2100s$Ynpk_pred_2088<-predict(brt_ERSC_recyle[[i]],RCP2.6_ER_2088, n.trees=best_inter_ERSC_recyle[[i]])
  RCP2.6_ER_pred_2100s$Ynpk_pred_2089<-predict(brt_ERSC_recyle[[i]],RCP2.6_ER_2089, n.trees=best_inter_ERSC_recyle[[i]])
  RCP2.6_ER_pred_2100s$Ynpk_pred_2090<-predict(brt_ERSC_recyle[[i]],RCP2.6_ER_2090, n.trees=best_inter_ERSC_recyle[[i]])
  RCP2.6_ER_pred_2100s$Ynpk_pred_2091<-predict(brt_ERSC_recyle[[i]],RCP2.6_ER_2091, n.trees=best_inter_ERSC_recyle[[i]])
  RCP2.6_ER_pred_2100s$Ynpk_pred_2092<-predict(brt_ERSC_recyle[[i]],RCP2.6_ER_2092, n.trees=best_inter_ERSC_recyle[[i]])
  RCP2.6_ER_pred_2100s$Ynpk_pred_2093<-predict(brt_ERSC_recyle[[i]],RCP2.6_ER_2093, n.trees=best_inter_ERSC_recyle[[i]])
  RCP2.6_ER_pred_2100s$Ynpk_pred_2094<-predict(brt_ERSC_recyle[[i]],RCP2.6_ER_2094, n.trees=best_inter_ERSC_recyle[[i]])
  RCP2.6_ER_pred_2100s$Ynpk_pred_2095<-predict(brt_ERSC_recyle[[i]],RCP2.6_ER_2095, n.trees=best_inter_ERSC_recyle[[i]])
  RCP2.6_ER_pred_2100s$Ynpk_pred_2096<-predict(brt_ERSC_recyle[[i]],RCP2.6_ER_2096, n.trees=best_inter_ERSC_recyle[[i]])
  RCP2.6_ER_pred_2100s$Ynpk_pred_2097<-predict(brt_ERSC_recyle[[i]],RCP2.6_ER_2097, n.trees=best_inter_ERSC_recyle[[i]])
  RCP2.6_ER_pred_2100s$Ynpk_pred_2098<-predict(brt_ERSC_recyle[[i]],RCP2.6_ER_2098, n.trees=best_inter_ERSC_recyle[[i]])
  RCP2.6_ER_pred_2100s$Ynpk_pred_2099<-predict(brt_ERSC_recyle[[i]],RCP2.6_ER_2099, n.trees=best_inter_ERSC_recyle[[i]])
  RCP2.6_ER_pred_2100s$Ynpk_pred_80.99<-predict(brt_ERSC_recyle[[i]],RCP2.6_ER_80.99, n.trees=best_inter_ERSC_recyle[[i]])
  
  RCP2.6_ER_pred_2100s$average<- apply(RCP2.6_ER_pred_2100s[,7:26], 1, mean)
  RCP2.6_ER_pred_2100s_recyle[[i]] <- RCP2.6_ER_pred_2100s
}

RCP2.6_ER_pred_2100s_recyle_mean<-RCP2.6_ER_pred_2100s_recyle[[1]]
RCP2.6_ER_pred_2100s_recyle_sd<-RCP2.6_ER_pred_2100s_recyle[[1]]
for(i in 7:28){
  da=as.matrix(as.data.frame(sapply(1:50,function(x){RCP2.6_ER_pred_2100s_recyle[[x]][i]})))
  RCP2.6_ER_pred_2100s_recyle_mean[,i]=rowMeans(da)
  RCP2.6_ER_pred_2100s_recyle_sd[,i]=apply(da,1,sd)
}


###RCP8.5 during 2040-2099

RCP8.5_ER_2050s<-subset(RCP8.5_2050s, Crop == "early rice")
RCP8.5_ER_2040 <- RCP8.5_ER_2050s %>%
  select(Soil.quality, Tave_2040, Tmax_2040, Tmin_2040, GDD.10_2040, 
         PRE_2040, RAD_2040, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_ER_2040)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_ER_2041 <- RCP8.5_ER_2050s %>%
  select(Soil.quality, Tave_2041, Tmax_2041, Tmin_2041, GDD.10_2041, 
         PRE_2041, RAD_2041, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_ER_2041)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_ER_2042 <- RCP8.5_ER_2050s %>%
  select(Soil.quality, Tave_2042, Tmax_2042, Tmin_2042, GDD.10_2042, 
         PRE_2042, RAD_2042, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_ER_2042)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_ER_2043 <- RCP8.5_ER_2050s %>%
  select(Soil.quality, Tave_2043, Tmax_2043, Tmin_2043, GDD.10_2043, 
         PRE_2043, RAD_2043, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_ER_2043)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_ER_2044 <- RCP8.5_ER_2050s %>%
  select(Soil.quality, Tave_2044, Tmax_2044, Tmin_2044, GDD.10_2044, 
         PRE_2044, RAD_2044, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_ER_2044)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_ER_2045 <- RCP8.5_ER_2050s %>%
  select(Soil.quality, Tave_2045, Tmax_2045, Tmin_2045, GDD.10_2045, 
         PRE_2045, RAD_2045, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_ER_2045)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_ER_2046 <- RCP8.5_ER_2050s %>%
  select(Soil.quality, Tave_2046, Tmax_2046, Tmin_2046, GDD.10_2046, 
         PRE_2046, RAD_2046,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_ER_2046)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_ER_2047 <- RCP8.5_ER_2050s %>%
  select(Soil.quality, Tave_2047, Tmax_2047, Tmin_2047, GDD.10_2047, 
         PRE_2047, RAD_2047, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_ER_2047)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_ER_2048 <- RCP8.5_ER_2050s %>%
  select(Soil.quality, Tave_2048, Tmax_2048, Tmin_2048, GDD.10_2048, 
         PRE_2048, RAD_2048, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_ER_2048)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_ER_2049 <- RCP8.5_ER_2050s %>%
  select(Soil.quality, Tave_2049, Tmax_2049, Tmin_2049, GDD.10_2049, 
         PRE_2049, RAD_2049, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_ER_2049)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_ER_2050 <- RCP8.5_ER_2050s %>%
  select(Soil.quality, Tave_2050, Tmax_2050, Tmin_2050, GDD.10_2050, 
         PRE_2050, RAD_2050, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_ER_2050)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_ER_2051 <- RCP8.5_ER_2050s %>%
  select(Soil.quality, Tave_2051, Tmax_2051, Tmin_2051, GDD.10_2051, 
         PRE_2051, RAD_2051,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_ER_2051)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_ER_2052 <- RCP8.5_ER_2050s %>%
  select(Soil.quality, Tave_2052, Tmax_2052, Tmin_2052, GDD.10_2052, 
         PRE_2052, RAD_2052, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_ER_2052)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_ER_2053 <- RCP8.5_ER_2050s %>%
  select(Soil.quality, Tave_2053, Tmax_2053, Tmin_2053, GDD.10_2053, 
         PRE_2053, RAD_2053, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_ER_2053)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_ER_2054 <- RCP8.5_ER_2050s %>%
  select(Soil.quality, Tave_2054, Tmax_2054, Tmin_2054, GDD.10_2054, 
         PRE_2054, RAD_2054,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_ER_2054)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_ER_2055 <- RCP8.5_ER_2050s %>%
  select(Soil.quality, Tave_2055, Tmax_2055, Tmin_2055, GDD.10_2055, 
         PRE_2055, RAD_2055, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_ER_2055)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_ER_2056 <- RCP8.5_ER_2050s %>%
  select(Soil.quality, Tave_2056, Tmax_2056, Tmin_2056, GDD.10_2056, 
         PRE_2056, RAD_2056, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_ER_2056)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_ER_2057 <- RCP8.5_ER_2050s %>%
  select(Soil.quality, Tave_2057, Tmax_2057, Tmin_2057, GDD.10_2057, 
         PRE_2057, RAD_2057, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_ER_2057)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_ER_2058 <- RCP8.5_ER_2050s %>%
  select(Soil.quality, Tave_2058, Tmax_2058, Tmin_2058, GDD.10_2058, 
         PRE_2058, RAD_2058, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_ER_2058)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_ER_2059 <- RCP8.5_ER_2050s %>%
  select(Soil.quality, Tave_2059, Tmax_2059, Tmin_2059, GDD.10_2059, 
         PRE_2059, RAD_2059, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_ER_2059)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_ER_40.59 <- RCP8.5_ER_2050s %>%
  select(Soil.quality, Tave_2040.2059, Tmax_2040.2059, Tmin_2040.2059, GDD.10_2040.2059, 
         PRE_2040.2059, RAD_2040.2059,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_ER_40.59)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

###predict yield by GBRT model

RCP8.5_ER_pred_2050s_recyle<-NULL

RCP8.5_ER_pred_2050s <- RCP8.5_ER_2050s %>%
  select(Soil.quality, Crop, NO, Year, Province, 
         NO.Station)

for (i in 1:50) {
  RCP8.5_ER_pred_2050s$Ynpk_pred_2040<-predict(brt_ERSC_recyle[[i]],RCP8.5_ER_2040, n.trees=best_inter_ERSC_recyle[[i]])
  RCP8.5_ER_pred_2050s$Ynpk_pred_2041<-predict(brt_ERSC_recyle[[i]],RCP8.5_ER_2041, n.trees=best_inter_ERSC_recyle[[i]])
  RCP8.5_ER_pred_2050s$Ynpk_pred_2042<-predict(brt_ERSC_recyle[[i]],RCP8.5_ER_2042, n.trees=best_inter_ERSC_recyle[[i]])
  RCP8.5_ER_pred_2050s$Ynpk_pred_2043<-predict(brt_ERSC_recyle[[i]],RCP8.5_ER_2043, n.trees=best_inter_ERSC_recyle[[i]])
  RCP8.5_ER_pred_2050s$Ynpk_pred_2044<-predict(brt_ERSC_recyle[[i]],RCP8.5_ER_2044, n.trees=best_inter_ERSC_recyle[[i]])
  RCP8.5_ER_pred_2050s$Ynpk_pred_2045<-predict(brt_ERSC_recyle[[i]],RCP8.5_ER_2045, n.trees=best_inter_ERSC_recyle[[i]])
  RCP8.5_ER_pred_2050s$Ynpk_pred_2046<-predict(brt_ERSC_recyle[[i]],RCP8.5_ER_2046, n.trees=best_inter_ERSC_recyle[[i]])
  RCP8.5_ER_pred_2050s$Ynpk_pred_2047<-predict(brt_ERSC_recyle[[i]],RCP8.5_ER_2047, n.trees=best_inter_ERSC_recyle[[i]])
  RCP8.5_ER_pred_2050s$Ynpk_pred_2048<-predict(brt_ERSC_recyle[[i]],RCP8.5_ER_2048, n.trees=best_inter_ERSC_recyle[[i]])
  RCP8.5_ER_pred_2050s$Ynpk_pred_2049<-predict(brt_ERSC_recyle[[i]],RCP8.5_ER_2049, n.trees=best_inter_ERSC_recyle[[i]])
  RCP8.5_ER_pred_2050s$Ynpk_pred_2050<-predict(brt_ERSC_recyle[[i]],RCP8.5_ER_2050, n.trees=best_inter_ERSC_recyle[[i]])
  RCP8.5_ER_pred_2050s$Ynpk_pred_2051<-predict(brt_ERSC_recyle[[i]],RCP8.5_ER_2051, n.trees=best_inter_ERSC_recyle[[i]])
  RCP8.5_ER_pred_2050s$Ynpk_pred_2052<-predict(brt_ERSC_recyle[[i]],RCP8.5_ER_2052, n.trees=best_inter_ERSC_recyle[[i]])
  RCP8.5_ER_pred_2050s$Ynpk_pred_2053<-predict(brt_ERSC_recyle[[i]],RCP8.5_ER_2053, n.trees=best_inter_ERSC_recyle[[i]])
  RCP8.5_ER_pred_2050s$Ynpk_pred_2054<-predict(brt_ERSC_recyle[[i]],RCP8.5_ER_2054, n.trees=best_inter_ERSC_recyle[[i]])
  RCP8.5_ER_pred_2050s$Ynpk_pred_2055<-predict(brt_ERSC_recyle[[i]],RCP8.5_ER_2055, n.trees=best_inter_ERSC_recyle[[i]])
  RCP8.5_ER_pred_2050s$Ynpk_pred_2056<-predict(brt_ERSC_recyle[[i]],RCP8.5_ER_2056, n.trees=best_inter_ERSC_recyle[[i]])
  RCP8.5_ER_pred_2050s$Ynpk_pred_2057<-predict(brt_ERSC_recyle[[i]],RCP8.5_ER_2057, n.trees=best_inter_ERSC_recyle[[i]])
  RCP8.5_ER_pred_2050s$Ynpk_pred_2058<-predict(brt_ERSC_recyle[[i]],RCP8.5_ER_2058, n.trees=best_inter_ERSC_recyle[[i]])
  RCP8.5_ER_pred_2050s$Ynpk_pred_2059<-predict(brt_ERSC_recyle[[i]],RCP8.5_ER_2059, n.trees=best_inter_ERSC_recyle[[i]])
  RCP8.5_ER_pred_2050s$Ynpk_pred_40.59<-predict(brt_ERSC_recyle[[i]],RCP8.5_ER_40.59, n.trees=best_inter_ERSC_recyle[[i]])
  
  RCP8.5_ER_pred_2050s$average<- apply(RCP8.5_ER_pred_2050s[,7:26], 1, mean)
  RCP8.5_ER_pred_2050s_recyle[[i]] <- RCP8.5_ER_pred_2050s
}

RCP8.5_ER_pred_2050s_recyle_mean<-RCP8.5_ER_pred_2050s_recyle[[1]]
RCP8.5_ER_pred_2050s_recyle_sd<-RCP8.5_ER_pred_2050s_recyle[[1]]
for(i in 7:28){
  da=as.matrix(as.data.frame(sapply(1:50,function(x){RCP8.5_ER_pred_2050s_recyle[[x]][i]})))
  RCP8.5_ER_pred_2050s_recyle_mean[,i]=rowMeans(da)
  RCP8.5_ER_pred_2050s_recyle_sd[,i]=apply(da,1,sd)
}

###RCP8.5 during 2080-2099

RCP8.5_ER_2100s<-subset(RCP8.5_2100s, Crop == "early rice")
RCP8.5_ER_2080 <- RCP8.5_ER_2100s %>%
  select(Soil.quality, Tave_2080, Tmax_2080, Tmin_2080, GDD.10_2080, 
         PRE_2080, RAD_2080,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_ER_2080)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_ER_2081 <- RCP8.5_ER_2100s %>%
  select(Soil.quality, Tave_2081, Tmax_2081, Tmin_2081, GDD.10_2081, 
         PRE_2081, RAD_2081, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_ER_2081)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_ER_2082 <- RCP8.5_ER_2100s %>%
  select(Soil.quality, Tave_2082, Tmax_2082, Tmin_2082, GDD.10_2082, 
         PRE_2082, RAD_2082, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_ER_2082)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_ER_2083 <- RCP8.5_ER_2100s %>%
  select(Soil.quality, Tave_2083, Tmax_2083, Tmin_2083, GDD.10_2083, 
         PRE_2083, RAD_2083, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_ER_2083)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_ER_2084 <- RCP8.5_ER_2100s %>%
  select(Soil.quality, Tave_2084, Tmax_2084, Tmin_2084, GDD.10_2084, 
         PRE_2084, RAD_2084, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_ER_2084)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_ER_2085 <- RCP8.5_ER_2100s %>%
  select(Soil.quality, Tave_2085, Tmax_2085, Tmin_2085, GDD.10_2085, 
         PRE_2085, RAD_2085, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_ER_2085)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_ER_2086 <- RCP8.5_ER_2100s %>%
  select(Soil.quality, Tave_2086, Tmax_2086, Tmin_2086, GDD.10_2086, 
         PRE_2086, RAD_2086,Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_ER_2086)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_ER_2087 <- RCP8.5_ER_2100s %>%
  select(Soil.quality, Tave_2087, Tmax_2087, Tmin_2087, GDD.10_2087, 
         PRE_2087, RAD_2087, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_ER_2087)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_ER_2088 <- RCP8.5_ER_2100s %>%
  select(Soil.quality, Tave_2088, Tmax_2088, Tmin_2088, GDD.10_2088, 
         PRE_2088, RAD_2088, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_ER_2088)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_ER_2089 <- RCP8.5_ER_2100s %>%
  select(Soil.quality, Tave_2089, Tmax_2089, Tmin_2089, GDD.10_2089, 
         PRE_2089, RAD_2089, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_ER_2089)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_ER_2090 <- RCP8.5_ER_2100s %>%
  select(Soil.quality, Tave_2090, Tmax_2090, Tmin_2090, GDD.10_2090, 
         PRE_2090, RAD_2090, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_ER_2090)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_ER_2091 <- RCP8.5_ER_2100s %>%
  select(Soil.quality, Tave_2091, Tmax_2091, Tmin_2091, GDD.10_2091, 
         PRE_2091, RAD_2091,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_ER_2091)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_ER_2092 <- RCP8.5_ER_2100s %>%
  select(Soil.quality, Tave_2092, Tmax_2092, Tmin_2092, GDD.10_2092, 
         PRE_2092, RAD_2092, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_ER_2092)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_ER_2093 <- RCP8.5_ER_2100s %>%
  select(Soil.quality, Tave_2093, Tmax_2093, Tmin_2093, GDD.10_2093, 
         PRE_2093, RAD_2093, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_ER_2093)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_ER_2094 <- RCP8.5_ER_2100s %>%
  select(Soil.quality, Tave_2094, Tmax_2094, Tmin_2094, GDD.10_2094, 
         PRE_2094, RAD_2094, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_ER_2094)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_ER_2095 <- RCP8.5_ER_2100s %>%
  select(Soil.quality, Tave_2095, Tmax_2095, Tmin_2095, GDD.10_2095, 
         PRE_2095, RAD_2095, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_ER_2095)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_ER_2096 <- RCP8.5_ER_2100s %>%
  select(Soil.quality, Tave_2096, Tmax_2096, Tmin_2096, GDD.10_2096, 
         PRE_2096, RAD_2096,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_ER_2096)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_ER_2097 <- RCP8.5_ER_2100s %>%
  select(Soil.quality, Tave_2097, Tmax_2097, Tmin_2097, GDD.10_2097, 
         PRE_2097, RAD_2097, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_ER_2097)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_ER_2098 <- RCP8.5_ER_2100s %>%
  select(Soil.quality, Tave_2098, Tmax_2098, Tmin_2098, GDD.10_2098, 
         PRE_2098, RAD_2098, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_ER_2098)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_ER_2099 <- RCP8.5_ER_2100s %>%
  select(Soil.quality, Tave_2099, Tmax_2099, Tmin_2099, GDD.10_2099, 
         PRE_2099, RAD_2099, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_ER_2099)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_ER_80.99 <- RCP8.5_ER_2100s %>%
  select(Soil.quality, Tave_2080.2099, Tmax_2080.2099, Tmin_2080.2099, GDD.10_2080.2099, 
         PRE_2080.2099, RAD_2080.2099, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_ER_80.99)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")


###predict yield by GBRT model
RCP8.5_ER_pred_2100s_recyle<-NULL

RCP8.5_ER_pred_2100s <- RCP8.5_ER_2100s %>%
  select(Soil.quality, Crop, NO, Year, Province, 
         NO.Station)

for (i in 1:50) {
  RCP8.5_ER_pred_2100s$Ynpk_pred_2080<-predict(brt_ERSC_recyle[[i]],RCP8.5_ER_2080, n.trees=best_inter_ERSC_recyle[[i]])
  RCP8.5_ER_pred_2100s$Ynpk_pred_2081<-predict(brt_ERSC_recyle[[i]],RCP8.5_ER_2081, n.trees=best_inter_ERSC_recyle[[i]])
  RCP8.5_ER_pred_2100s$Ynpk_pred_2082<-predict(brt_ERSC_recyle[[i]],RCP8.5_ER_2082, n.trees=best_inter_ERSC_recyle[[i]])
  RCP8.5_ER_pred_2100s$Ynpk_pred_2083<-predict(brt_ERSC_recyle[[i]],RCP8.5_ER_2083, n.trees=best_inter_ERSC_recyle[[i]])
  RCP8.5_ER_pred_2100s$Ynpk_pred_2084<-predict(brt_ERSC_recyle[[i]],RCP8.5_ER_2084, n.trees=best_inter_ERSC_recyle[[i]])
  RCP8.5_ER_pred_2100s$Ynpk_pred_2085<-predict(brt_ERSC_recyle[[i]],RCP8.5_ER_2085, n.trees=best_inter_ERSC_recyle[[i]])
  RCP8.5_ER_pred_2100s$Ynpk_pred_2086<-predict(brt_ERSC_recyle[[i]],RCP8.5_ER_2086, n.trees=best_inter_ERSC_recyle[[i]])
  RCP8.5_ER_pred_2100s$Ynpk_pred_2087<-predict(brt_ERSC_recyle[[i]],RCP8.5_ER_2087, n.trees=best_inter_ERSC_recyle[[i]])
  RCP8.5_ER_pred_2100s$Ynpk_pred_2088<-predict(brt_ERSC_recyle[[i]],RCP8.5_ER_2088, n.trees=best_inter_ERSC_recyle[[i]])
  RCP8.5_ER_pred_2100s$Ynpk_pred_2089<-predict(brt_ERSC_recyle[[i]],RCP8.5_ER_2089, n.trees=best_inter_ERSC_recyle[[i]])
  RCP8.5_ER_pred_2100s$Ynpk_pred_2090<-predict(brt_ERSC_recyle[[i]],RCP8.5_ER_2090, n.trees=best_inter_ERSC_recyle[[i]])
  RCP8.5_ER_pred_2100s$Ynpk_pred_2091<-predict(brt_ERSC_recyle[[i]],RCP8.5_ER_2091, n.trees=best_inter_ERSC_recyle[[i]])
  RCP8.5_ER_pred_2100s$Ynpk_pred_2092<-predict(brt_ERSC_recyle[[i]],RCP8.5_ER_2092, n.trees=best_inter_ERSC_recyle[[i]])
  RCP8.5_ER_pred_2100s$Ynpk_pred_2093<-predict(brt_ERSC_recyle[[i]],RCP8.5_ER_2093, n.trees=best_inter_ERSC_recyle[[i]])
  RCP8.5_ER_pred_2100s$Ynpk_pred_2094<-predict(brt_ERSC_recyle[[i]],RCP8.5_ER_2094, n.trees=best_inter_ERSC_recyle[[i]])
  RCP8.5_ER_pred_2100s$Ynpk_pred_2095<-predict(brt_ERSC_recyle[[i]],RCP8.5_ER_2095, n.trees=best_inter_ERSC_recyle[[i]])
  RCP8.5_ER_pred_2100s$Ynpk_pred_2096<-predict(brt_ERSC_recyle[[i]],RCP8.5_ER_2096, n.trees=best_inter_ERSC_recyle[[i]])
  RCP8.5_ER_pred_2100s$Ynpk_pred_2097<-predict(brt_ERSC_recyle[[i]],RCP8.5_ER_2097, n.trees=best_inter_ERSC_recyle[[i]])
  RCP8.5_ER_pred_2100s$Ynpk_pred_2098<-predict(brt_ERSC_recyle[[i]],RCP8.5_ER_2098, n.trees=best_inter_ERSC_recyle[[i]])
  RCP8.5_ER_pred_2100s$Ynpk_pred_2099<-predict(brt_ERSC_recyle[[i]],RCP8.5_ER_2099, n.trees=best_inter_ERSC_recyle[[i]])
  RCP8.5_ER_pred_2100s$Ynpk_pred_80.99<-predict(brt_ERSC_recyle[[i]],RCP8.5_ER_80.99, n.trees=best_inter_ERSC_recyle[[i]])
  
  RCP8.5_ER_pred_2100s$average<- apply(RCP8.5_ER_pred_2100s[,7:26], 1, mean)
  RCP8.5_ER_pred_2100s_recyle[[i]] <- RCP8.5_ER_pred_2100s
}

RCP8.5_ER_pred_2100s_recyle_mean<-RCP8.5_ER_pred_2100s_recyle[[1]]
RCP8.5_ER_pred_2100s_recyle_sd<-RCP8.5_ER_pred_2100s_recyle[[1]]
for(i in 7:28){
  da=as.matrix(as.data.frame(sapply(1:50,function(x){RCP8.5_ER_pred_2100s_recyle[[x]][i]})))
  RCP8.5_ER_pred_2100s_recyle_mean[,i]=rowMeans(da)
  RCP8.5_ER_pred_2100s_recyle_sd[,i]=apply(da,1,sd)
}

###summarise predicted yield

sum_RCP2.6_ER_2050s_year<- cbind(base_ER_pred_all[,c("Soil.quality","Crop","NO","average")],RCP2.6_ER_pred_2050s[,7:28])
names(sum_RCP2.6_ER_2050s_year)[1:4]<- c("Soil quality", "Crop", "No", "Yield_base")
names(sum_RCP2.6_ER_2050s_year)[26]<- "Yield_RCP"
sum_RCP2.6_ER_2050s_year$Yield_change <- sum_RCP2.6_ER_2050s_year$`Yield_RCP` - sum_RCP2.6_ER_2050s_year$`Yield_base`
sum_RCP2.6_ER_2050s_year$Yield_change_per <- (sum_RCP2.6_ER_2050s_year$Yield_change / sum_RCP2.6_ER_2050s_year$Yield_base)*100
sum_RCP2.6_ER_2050s_year$RCP <- "RCP2.6"
sum_RCP2.6_ER_2050s_year$Time <- "2050"

sum_RCP8.5_ER_2050s_year<- cbind(base_ER_pred_all[,c("Soil.quality","Crop","NO","average")],RCP8.5_ER_pred_2050s[,7:28])
names(sum_RCP8.5_ER_2050s_year)[1:4]<- c("Soil quality", "Crop", "No", "Yield_base")
names(sum_RCP8.5_ER_2050s_year)[26]<- "Yield_RCP"
sum_RCP8.5_ER_2050s_year$Yield_change <- sum_RCP8.5_ER_2050s_year$`Yield_RCP` - sum_RCP8.5_ER_2050s_year$`Yield_base`
sum_RCP8.5_ER_2050s_year$Yield_change_per <- (sum_RCP8.5_ER_2050s_year$Yield_change / sum_RCP8.5_ER_2050s_year$Yield_base)*100
sum_RCP8.5_ER_2050s_year$RCP <- "RCP8.5"
sum_RCP8.5_ER_2050s_year$Time <- "2050"

sum_RCP2.6_ER_2100s_year<- cbind(base_ER_pred_all[,c("Soil.quality","Crop","NO","average")],RCP2.6_ER_pred_2100s[,7:28])
names(sum_RCP2.6_ER_2100s_year)[1:4]<- c("Soil quality", "Crop", "No", "Yield_base")
names(sum_RCP2.6_ER_2100s_year)[26]<- "Yield_RCP"
sum_RCP2.6_ER_2100s_year$Yield_change <- sum_RCP2.6_ER_2100s_year$`Yield_RCP` - sum_RCP2.6_ER_2100s_year$`Yield_base`
sum_RCP2.6_ER_2100s_year$Yield_change_per <- (sum_RCP2.6_ER_2100s_year$Yield_change / sum_RCP2.6_ER_2100s_year$Yield_base)*100
sum_RCP2.6_ER_2100s_year$RCP <- "RCP2.6"
sum_RCP2.6_ER_2100s_year$Time <- "2100"

sum_RCP8.5_ER_2100s_year<- cbind(base_ER_pred_all[,c("Soil.quality","Crop","NO","average")],RCP8.5_ER_pred_2100s[,7:28])
names(sum_RCP8.5_ER_2100s_year)[1:4]<- c("Soil quality", "Crop", "No", "Yield_base")
names(sum_RCP8.5_ER_2100s_year)[26]<- "Yield_RCP"
sum_RCP8.5_ER_2100s_year$Yield_change <- sum_RCP8.5_ER_2100s_year$`Yield_RCP` - sum_RCP8.5_ER_2100s_year$`Yield_base`
sum_RCP8.5_ER_2100s_year$Yield_change_per <- (sum_RCP8.5_ER_2100s_year$Yield_change / sum_RCP8.5_ER_2100s_year$Yield_base)*100
sum_RCP8.5_ER_2100s_year$RCP <- "RCP8.5"
sum_RCP8.5_ER_2100s_year$Time <- "2100"

sum_RCP_ER_2050s_year <- rbind(sum_RCP2.6_ER_2050s_year,sum_RCP8.5_ER_2050s_year)
sum_RCP_ER_2100s_year<-rbind(sum_RCP2.6_ER_2100s_year,sum_RCP8.5_ER_2100s_year)

write.csv(sum_RCP_ER_2050s_year,"./results/sum_RCP_ER_2050s_year.csv")
write.csv(sum_RCP_ER_2100s_year,"./results/sum_RCP_ER_2100s_year.csv")

###comparison of yield change between high and low quality soils 
###T test

t.test(Yield_change~`Soil quality` ,sum_RCP2.6_ER_2050s_year) #RCP2.6-2050s
t.test(Yield_change~`Soil quality` ,sum_RCP8.5_ER_2050s_year) #RCP8.5-2050s
t.test(Yield_change~`Soil quality` ,sum_RCP2.6_ER_2100s_year) #RCP2.6-2100s
t.test(Yield_change~`Soil quality` ,sum_RCP8.5_ER_2100s_year) #RCP8.5-2100s

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
}

#####Predicted yield under different climate scenarios
###baseline period 1986-2005
base_LR<-subset(base, Crop == "late rice")
base_LR_1986 <- base_LR %>%
  select(Soil.quality, Tave_1986, Tmax_1986, Tmin_1986, GDD.10_1986, 
         PRE_1986, RAD_1986, Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_LR_1986)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_LR_1987 <- base_LR %>%
  select(Soil.quality, Tave_1987, Tmax_1987, Tmin_1987, GDD.10_1987, 
         PRE_1987, RAD_1987, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_LR_1987)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_LR_1988 <- base_LR %>%
  select(Soil.quality, Tave_1988, Tmax_1988, Tmin_1988, GDD.10_1988, 
         PRE_1988, RAD_1988, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_LR_1988)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_LR_1989 <- base_LR %>%
  select(Soil.quality, Tave_1989, Tmax_1989, Tmin_1989, GDD.10_1989, 
         PRE_1989, RAD_1989, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_LR_1989)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_LR_1990 <- base_LR %>%
  select(Soil.quality, Tave_1990, Tmax_1990, Tmin_1990, GDD.10_1990, 
         PRE_1990, RAD_1990, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_LR_1990)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_LR_1991 <- base_LR %>%
  select(Soil.quality, Tave_1991, Tmax_1991, Tmin_1991, GDD.10_1991, 
         PRE_1991, RAD_1991, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_LR_1991)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_LR_1992 <- base_LR %>%
  select(Soil.quality, Tave_1992, Tmax_1992, Tmin_1992, GDD.10_1992, 
         PRE_1992, RAD_1992,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_LR_1992)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_LR_1993 <- base_LR %>%
  select(Soil.quality, Tave_1993, Tmax_1993, Tmin_1993, GDD.10_1993, 
         PRE_1993, RAD_1993, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_LR_1993)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_LR_1994 <- base_LR %>%
  select(Soil.quality, Tave_1994, Tmax_1994, Tmin_1994, GDD.10_1994, 
         PRE_1994, RAD_1994, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_LR_1994)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_LR_1995 <- base_LR %>%
  select(Soil.quality, Tave_1995, Tmax_1995, Tmin_1995, GDD.10_1995, 
         PRE_1995, RAD_1995, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_LR_1995)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_LR_1996 <- base_LR %>%
  select(Soil.quality, Tave_1996, Tmax_1996, Tmin_1996, GDD.10_1996, 
         PRE_1996, RAD_1996, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_LR_1996)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_LR_1997 <- base_LR %>%
  select(Soil.quality, Tave_1997, Tmax_1997, Tmin_1997, GDD.10_1997, 
         PRE_1997, RAD_1997, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_LR_1997)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_LR_1998 <- base_LR %>%
  select(Soil.quality, Tave_1998, Tmax_1998, Tmin_1998, GDD.10_1998, 
         PRE_1998, RAD_1998, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_LR_1998)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_LR_1999 <- base_LR %>%
  select(Soil.quality, Tave_1999, Tmax_1999, Tmin_1999, GDD.10_1999, 
         PRE_1999, RAD_1999, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_LR_1999)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_LR_2000 <- base_LR %>%
  select(Soil.quality, Tave_2000, Tmax_2000, Tmin_2000, GDD.10_2000, 
         PRE_2000, RAD_2000, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_LR_2000)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_LR_2001 <- base_LR %>%
  select(Soil.quality, Tave_2001, Tmax_2001, Tmin_2001, GDD.10_2001, 
         PRE_2001, RAD_2001, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_LR_2001)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_LR_2002 <- base_LR %>%
  select(Soil.quality, Tave_2002, Tmax_2002, Tmin_2002, GDD.10_2002, 
         PRE_2002, RAD_2002, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_LR_2002)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_LR_2003 <- base_LR %>%
  select(Soil.quality, Tave_2003, Tmax_2003, Tmin_2003, GDD.10_2003, 
         PRE_2003, RAD_2003, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_LR_2003)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_LR_2004 <- base_LR %>%
  select(Soil.quality, Tave_2004, Tmax_2004, Tmin_2004, GDD.10_2004, 
         PRE_2004, RAD_2004, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_LR_2004)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_LR_2005 <- base_LR %>%
  select(Soil.quality, Tave_2005, Tmax_2005, Tmin_2005, GDD.10_2005, 
         PRE_2005, RAD_2005, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_LR_2005)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                        "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                        "N", "P2O5", "K2O", "Yck", "Ynpk")

base_LR_86.05 <- base_LR %>%
  select(Soil.quality, Tave_1986.2005, Tmax_1986.2005, Tmin_1986.2005, GDD.10_1986.2005, 
         PRE_1986.2005, RAD_1986.2005, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(base_LR_86.05)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                         "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                         "N", "P2O5", "K2O", "Yck", "Ynpk")

###predict yield by GBRT model

base_LR_pred_all_recyle<-NULL

base_LR_pred_all <- base_LR %>%
  select(Soil.quality, Crop, NO, Year,Province, 
         NO.Station)

for (i in 1:50) {
  base_LR_pred_all$Ynpk_pred_1986<-predict(brt_LRSC_recyle[[i]],base_LR_1986, n.trees=best_inter_LRSC_recyle[[i]])
  base_LR_pred_all$Ynpk_pred_1987<-predict(brt_LRSC_recyle[[i]],base_LR_1987, n.trees=best_inter_LRSC_recyle[[i]])
  base_LR_pred_all$Ynpk_pred_1988<-predict(brt_LRSC_recyle[[i]],base_LR_1988, n.trees=best_inter_LRSC_recyle[[i]])
  base_LR_pred_all$Ynpk_pred_1989<-predict(brt_LRSC_recyle[[i]],base_LR_1989, n.trees=best_inter_LRSC_recyle[[i]])
  base_LR_pred_all$Ynpk_pred_1990<-predict(brt_LRSC_recyle[[i]],base_LR_1990, n.trees=best_inter_LRSC_recyle[[i]])
  base_LR_pred_all$Ynpk_pred_1991<-predict(brt_LRSC_recyle[[i]],base_LR_1991, n.trees=best_inter_LRSC_recyle[[i]])
  base_LR_pred_all$Ynpk_pred_1992<-predict(brt_LRSC_recyle[[i]],base_LR_1992, n.trees=best_inter_LRSC_recyle[[i]])
  base_LR_pred_all$Ynpk_pred_1993<-predict(brt_LRSC_recyle[[i]],base_LR_1993, n.trees=best_inter_LRSC_recyle[[i]])
  base_LR_pred_all$Ynpk_pred_1994<-predict(brt_LRSC_recyle[[i]],base_LR_1994, n.trees=best_inter_LRSC_recyle[[i]])
  base_LR_pred_all$Ynpk_pred_1995<-predict(brt_LRSC_recyle[[i]],base_LR_1995, n.trees=best_inter_LRSC_recyle[[i]])
  base_LR_pred_all$Ynpk_pred_1996<-predict(brt_LRSC_recyle[[i]],base_LR_1996, n.trees=best_inter_LRSC_recyle[[i]])
  base_LR_pred_all$Ynpk_pred_1997<-predict(brt_LRSC_recyle[[i]],base_LR_1997, n.trees=best_inter_LRSC_recyle[[i]])
  base_LR_pred_all$Ynpk_pred_1998<-predict(brt_LRSC_recyle[[i]],base_LR_1998, n.trees=best_inter_LRSC_recyle[[i]])
  base_LR_pred_all$Ynpk_pred_1999<-predict(brt_LRSC_recyle[[i]],base_LR_1999, n.trees=best_inter_LRSC_recyle[[i]])
  base_LR_pred_all$Ynpk_pred_2000<-predict(brt_LRSC_recyle[[i]],base_LR_2000, n.trees=best_inter_LRSC_recyle[[i]])
  base_LR_pred_all$Ynpk_pred_2001<-predict(brt_LRSC_recyle[[i]],base_LR_2001, n.trees=best_inter_LRSC_recyle[[i]])
  base_LR_pred_all$Ynpk_pred_2002<-predict(brt_LRSC_recyle[[i]],base_LR_2002, n.trees=best_inter_LRSC_recyle[[i]])
  base_LR_pred_all$Ynpk_pred_2003<-predict(brt_LRSC_recyle[[i]],base_LR_2003, n.trees=best_inter_LRSC_recyle[[i]])
  base_LR_pred_all$Ynpk_pred_2004<-predict(brt_LRSC_recyle[[i]],base_LR_2004, n.trees=best_inter_LRSC_recyle[[i]])
  base_LR_pred_all$Ynpk_pred_2005<-predict(brt_LRSC_recyle[[i]],base_LR_2005, n.trees=best_inter_LRSC_recyle[[i]])
  base_LR_pred_all$Ynpk_pred_86.05<-predict(brt_LRSC_recyle[[i]],base_LR_86.05, n.trees=best_inter_LRSC_recyle[[i]])
  
  base_LR_pred_all$average<- apply(base_LR_pred_all[,7:26], 1, mean)
  base_LR_pred_all_recyle[[i]] <- base_LR_pred_all
}

base_LR_pred_all_recyle_mean<-base_LR_pred_all_recyle[[1]]
base_LR_pred_all_recyle_sd<-base_LR_pred_all_recyle[[1]]
for(i in 7:28){
  da=as.matrix(as.data.frame(sapply(1:50,function(x){base_LR_pred_all_recyle[[x]][i]})))
  base_LR_pred_all_recyle_mean[,i]=rowMeans(da)
  base_LR_pred_all_recyle_sd[,i]=apply(da,1,sd)
}

###RCP2.6 during 2040-2059

RCP2.6_LR_2050s<-subset(RCP2.6_2050s, Crop == "late rice")
RCP2.6_LR_2040 <- RCP2.6_LR_2050s %>%
  select(Soil.quality, Tave_2040, Tmax_2040, Tmin_2040, GDD.10_2040, 
         PRE_2040, RAD_2040, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_LR_2040)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_LR_2041 <- RCP2.6_LR_2050s %>%
  select(Soil.quality, Tave_2041, Tmax_2041, Tmin_2041, GDD.10_2041, 
         PRE_2041, RAD_2041, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_LR_2041)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_LR_2042 <- RCP2.6_LR_2050s %>%
  select(Soil.quality, Tave_2042, Tmax_2042, Tmin_2042, GDD.10_2042, 
         PRE_2042, RAD_2042, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_LR_2042)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_LR_2043 <- RCP2.6_LR_2050s %>%
  select(Soil.quality, Tave_2043, Tmax_2043, Tmin_2043, GDD.10_2043, 
         PRE_2043, RAD_2043, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_LR_2043)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_LR_2044 <- RCP2.6_LR_2050s %>%
  select(Soil.quality, Tave_2044, Tmax_2044, Tmin_2044, GDD.10_2044, 
         PRE_2044, RAD_2044, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_LR_2044)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_LR_2045 <- RCP2.6_LR_2050s %>%
  select(Soil.quality, Tave_2045, Tmax_2045, Tmin_2045, GDD.10_2045, 
         PRE_2045, RAD_2045, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_LR_2045)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_LR_2046 <- RCP2.6_LR_2050s %>%
  select(Soil.quality, Tave_2046, Tmax_2046, Tmin_2046, GDD.10_2046, 
         PRE_2046, RAD_2046, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_LR_2046)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_LR_2047 <- RCP2.6_LR_2050s %>%
  select(Soil.quality, Tave_2047, Tmax_2047, Tmin_2047, GDD.10_2047, 
         PRE_2047, RAD_2047, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_LR_2047)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_LR_2048 <- RCP2.6_LR_2050s %>%
  select(Soil.quality, Tave_2048, Tmax_2048, Tmin_2048, GDD.10_2048, 
         PRE_2048, RAD_2048, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_LR_2048)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_LR_2049 <- RCP2.6_LR_2050s %>%
  select(Soil.quality, Tave_2049, Tmax_2049, Tmin_2049, GDD.10_2049, 
         PRE_2049, RAD_2049, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_LR_2049)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_LR_2050 <- RCP2.6_LR_2050s %>%
  select(Soil.quality, Tave_2050, Tmax_2050, Tmin_2050, GDD.10_2050, 
         PRE_2050, RAD_2050, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_LR_2050)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_LR_2051 <- RCP2.6_LR_2050s %>%
  select(Soil.quality, Tave_2051, Tmax_2051, Tmin_2051, GDD.10_2051, 
         PRE_2051, RAD_2051, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_LR_2051)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_LR_2052 <- RCP2.6_LR_2050s %>%
  select(Soil.quality, Tave_2052, Tmax_2052, Tmin_2052, GDD.10_2052, 
         PRE_2052, RAD_2052, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_LR_2052)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_LR_2053 <- RCP2.6_LR_2050s %>%
  select(Soil.quality, Tave_2053, Tmax_2053, Tmin_2053, GDD.10_2053, 
         PRE_2053, RAD_2053, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_LR_2053)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_LR_2054 <- RCP2.6_LR_2050s %>%
  select(Soil.quality, Tave_2054, Tmax_2054, Tmin_2054, GDD.10_2054, 
         PRE_2054, RAD_2054, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_LR_2054)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_LR_2055 <- RCP2.6_LR_2050s %>%
  select(Soil.quality, Tave_2055, Tmax_2055, Tmin_2055, GDD.10_2055, 
         PRE_2055, RAD_2055, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_LR_2055)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_LR_2056 <- RCP2.6_LR_2050s %>%
  select(Soil.quality, Tave_2056, Tmax_2056, Tmin_2056, GDD.10_2056, 
         PRE_2056, RAD_2056, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_LR_2056)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_LR_2057 <- RCP2.6_LR_2050s %>%
  select(Soil.quality, Tave_2057, Tmax_2057, Tmin_2057, GDD.10_2057, 
         PRE_2057, RAD_2057, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_LR_2057)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_LR_2058 <- RCP2.6_LR_2050s %>%
  select(Soil.quality, Tave_2058, Tmax_2058, Tmin_2058, GDD.10_2058, 
         PRE_2058, RAD_2058, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_LR_2058)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_LR_2059 <- RCP2.6_LR_2050s %>%
  select(Soil.quality, Tave_2059, Tmax_2059, Tmin_2059, GDD.10_2059, 
         PRE_2059, RAD_2059, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_LR_2059)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_LR_40.59 <- RCP2.6_LR_2050s %>%
  select(Soil.quality, Tave_2040.2059, Tmax_2040.2059, Tmin_2040.2059, GDD.10_2040.2059, 
         PRE_2040.2059, RAD_2040.2059, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_LR_40.59)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

###predict yield by GBRT model

RCP2.6_LR_pred_2050s_recyle<-NULL

RCP2.6_LR_pred_2050s <- RCP2.6_LR_2050s %>%
  select(Soil.quality, Crop, NO, Year, Province, 
         NO.Station)

for (i in 1:50) {
  RCP2.6_LR_pred_2050s$Ynpk_pred_2040<-predict(brt_LRSC_recyle[[i]],RCP2.6_LR_2040, n.trees=best_inter_LRSC_recyle[[i]])
  RCP2.6_LR_pred_2050s$Ynpk_pred_2041<-predict(brt_LRSC_recyle[[i]],RCP2.6_LR_2041, n.trees=best_inter_LRSC_recyle[[i]])
  RCP2.6_LR_pred_2050s$Ynpk_pred_2042<-predict(brt_LRSC_recyle[[i]],RCP2.6_LR_2042, n.trees=best_inter_LRSC_recyle[[i]])
  RCP2.6_LR_pred_2050s$Ynpk_pred_2043<-predict(brt_LRSC_recyle[[i]],RCP2.6_LR_2043, n.trees=best_inter_LRSC_recyle[[i]])
  RCP2.6_LR_pred_2050s$Ynpk_pred_2044<-predict(brt_LRSC_recyle[[i]],RCP2.6_LR_2044, n.trees=best_inter_LRSC_recyle[[i]])
  RCP2.6_LR_pred_2050s$Ynpk_pred_2045<-predict(brt_LRSC_recyle[[i]],RCP2.6_LR_2045, n.trees=best_inter_LRSC_recyle[[i]])
  RCP2.6_LR_pred_2050s$Ynpk_pred_2046<-predict(brt_LRSC_recyle[[i]],RCP2.6_LR_2046, n.trees=best_inter_LRSC_recyle[[i]])
  RCP2.6_LR_pred_2050s$Ynpk_pred_2047<-predict(brt_LRSC_recyle[[i]],RCP2.6_LR_2047, n.trees=best_inter_LRSC_recyle[[i]])
  RCP2.6_LR_pred_2050s$Ynpk_pred_2048<-predict(brt_LRSC_recyle[[i]],RCP2.6_LR_2048, n.trees=best_inter_LRSC_recyle[[i]])
  RCP2.6_LR_pred_2050s$Ynpk_pred_2049<-predict(brt_LRSC_recyle[[i]],RCP2.6_LR_2049, n.trees=best_inter_LRSC_recyle[[i]])
  RCP2.6_LR_pred_2050s$Ynpk_pred_2050<-predict(brt_LRSC_recyle[[i]],RCP2.6_LR_2050, n.trees=best_inter_LRSC_recyle[[i]])
  RCP2.6_LR_pred_2050s$Ynpk_pred_2051<-predict(brt_LRSC_recyle[[i]],RCP2.6_LR_2051, n.trees=best_inter_LRSC_recyle[[i]])
  RCP2.6_LR_pred_2050s$Ynpk_pred_2052<-predict(brt_LRSC_recyle[[i]],RCP2.6_LR_2052, n.trees=best_inter_LRSC_recyle[[i]])
  RCP2.6_LR_pred_2050s$Ynpk_pred_2053<-predict(brt_LRSC_recyle[[i]],RCP2.6_LR_2053, n.trees=best_inter_LRSC_recyle[[i]])
  RCP2.6_LR_pred_2050s$Ynpk_pred_2054<-predict(brt_LRSC_recyle[[i]],RCP2.6_LR_2054, n.trees=best_inter_LRSC_recyle[[i]])
  RCP2.6_LR_pred_2050s$Ynpk_pred_2055<-predict(brt_LRSC_recyle[[i]],RCP2.6_LR_2055, n.trees=best_inter_LRSC_recyle[[i]])
  RCP2.6_LR_pred_2050s$Ynpk_pred_2056<-predict(brt_LRSC_recyle[[i]],RCP2.6_LR_2056, n.trees=best_inter_LRSC_recyle[[i]])
  RCP2.6_LR_pred_2050s$Ynpk_pred_2057<-predict(brt_LRSC_recyle[[i]],RCP2.6_LR_2057, n.trees=best_inter_LRSC_recyle[[i]])
  RCP2.6_LR_pred_2050s$Ynpk_pred_2058<-predict(brt_LRSC_recyle[[i]],RCP2.6_LR_2058, n.trees=best_inter_LRSC_recyle[[i]])
  RCP2.6_LR_pred_2050s$Ynpk_pred_2059<-predict(brt_LRSC_recyle[[i]],RCP2.6_LR_2059, n.trees=best_inter_LRSC_recyle[[i]])
  RCP2.6_LR_pred_2050s$Ynpk_pred_40.59<-predict(brt_LRSC_recyle[[i]],RCP2.6_LR_40.59, n.trees=best_inter_LRSC_recyle[[i]])
  
  RCP2.6_LR_pred_2050s$average<- apply(RCP2.6_LR_pred_2050s[,7:26], 1, mean)
  RCP2.6_LR_pred_2050s_recyle[[i]] <- RCP2.6_LR_pred_2050s
}

RCP2.6_LR_pred_2050s_recyle_mean<-RCP2.6_LR_pred_2050s_recyle[[1]]
RCP2.6_LR_pred_2050s_recyle_sd<-RCP2.6_LR_pred_2050s_recyle[[1]]
for(i in 7:28){
  da=as.matrix(as.data.frame(sapply(1:50,function(x){RCP2.6_LR_pred_2050s_recyle[[x]][i]})))
  RCP2.6_LR_pred_2050s_recyle_mean[,i]=rowMeans(da)
  RCP2.6_LR_pred_2050s_recyle_sd[,i]=apply(da,1,sd)
}

###RCP2.6 during 2080-2099

RCP2.6_LR_2100s<-subset(RCP2.6_2100s, Crop == "late rice")
RCP2.6_LR_2080 <- RCP2.6_LR_2100s %>%
  select(Soil.quality, Tave_2080, Tmax_2080, Tmin_2080, GDD.10_2080, 
         PRE_2080, RAD_2080,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_LR_2080)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_LR_2081 <- RCP2.6_LR_2100s %>%
  select(Soil.quality, Tave_2081, Tmax_2081, Tmin_2081, GDD.10_2081, 
         PRE_2081, RAD_2081, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_LR_2081)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_LR_2082 <- RCP2.6_LR_2100s %>%
  select(Soil.quality, Tave_2082, Tmax_2082, Tmin_2082, GDD.10_2082, 
         PRE_2082, RAD_2082, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_LR_2082)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_LR_2083 <- RCP2.6_LR_2100s %>%
  select(Soil.quality, Tave_2083, Tmax_2083, Tmin_2083, GDD.10_2083, 
         PRE_2083, RAD_2083, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_LR_2083)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_LR_2084 <- RCP2.6_LR_2100s %>%
  select(Soil.quality, Tave_2084, Tmax_2084, Tmin_2084, GDD.10_2084, 
         PRE_2084, RAD_2084, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_LR_2084)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_LR_2085 <- RCP2.6_LR_2100s %>%
  select(Soil.quality, Tave_2085, Tmax_2085, Tmin_2085, GDD.10_2085, 
         PRE_2085, RAD_2085,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_LR_2085)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_LR_2086 <- RCP2.6_LR_2100s %>%
  select(Soil.quality, Tave_2086, Tmax_2086, Tmin_2086, GDD.10_2086, 
         PRE_2086, RAD_2086, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_LR_2086)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_LR_2087 <- RCP2.6_LR_2100s %>%
  select(Soil.quality, Tave_2087, Tmax_2087, Tmin_2087, GDD.10_2087, 
         PRE_2087, RAD_2087, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_LR_2087)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_LR_2088 <- RCP2.6_LR_2100s %>%
  select(Soil.quality, Tave_2088, Tmax_2088, Tmin_2088, GDD.10_2088, 
         PRE_2088, RAD_2088, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_LR_2088)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_LR_2089 <- RCP2.6_LR_2100s %>%
  select(Soil.quality, Tave_2089, Tmax_2089, Tmin_2089, GDD.10_2089, 
         PRE_2089, RAD_2089, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_LR_2089)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_LR_2090 <- RCP2.6_LR_2100s %>%
  select(Soil.quality, Tave_2090, Tmax_2090, Tmin_2090, GDD.10_2090, 
         PRE_2090, RAD_2090,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_LR_2090)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_LR_2091 <- RCP2.6_LR_2100s %>%
  select(Soil.quality, Tave_2091, Tmax_2091, Tmin_2091, GDD.10_2091, 
         PRE_2091, RAD_2091, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_LR_2091)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_LR_2092 <- RCP2.6_LR_2100s %>%
  select(Soil.quality, Tave_2092, Tmax_2092, Tmin_2092, GDD.10_2092, 
         PRE_2092, RAD_2092, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_LR_2092)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_LR_2093 <- RCP2.6_LR_2100s %>%
  select(Soil.quality, Tave_2093, Tmax_2093, Tmin_2093, GDD.10_2093, 
         PRE_2093, RAD_2093,Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_LR_2093)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_LR_2094 <- RCP2.6_LR_2100s %>%
  select(Soil.quality, Tave_2094, Tmax_2094, Tmin_2094, GDD.10_2094, 
         PRE_2094, RAD_2094,Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_LR_2094)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_LR_2095 <- RCP2.6_LR_2100s %>%
  select(Soil.quality, Tave_2095, Tmax_2095, Tmin_2095, GDD.10_2095, 
         PRE_2095, RAD_2095, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_LR_2095)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_LR_2096 <- RCP2.6_LR_2100s %>%
  select(Soil.quality, Tave_2096, Tmax_2096, Tmin_2096, GDD.10_2096, 
         PRE_2096, RAD_2096, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_LR_2096)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_LR_2097 <- RCP2.6_LR_2100s %>%
  select(Soil.quality, Tave_2097, Tmax_2097, Tmin_2097, GDD.10_2097, 
         PRE_2097, RAD_2097,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_LR_2097)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_LR_2098 <- RCP2.6_LR_2100s %>%
  select(Soil.quality, Tave_2098, Tmax_2098, Tmin_2098, GDD.10_2098, 
         PRE_2098, RAD_2098,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_LR_2098)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_LR_2099 <- RCP2.6_LR_2100s %>%
  select(Soil.quality, Tave_2099, Tmax_2099, Tmin_2099, GDD.10_2099, 
         PRE_2099, RAD_2099, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_LR_2099)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP2.6_LR_80.99 <- RCP2.6_LR_2100s %>%
  select(Soil.quality, Tave_2080.2099, Tmax_2080.2099, Tmin_2080.2099, GDD.10_2080.2099, 
         PRE_2080.2099, RAD_2080.2099, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP2.6_LR_80.99)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")


###predict yield by GBRT model

RCP2.6_LR_pred_2100s_recyle<-NULL

RCP2.6_LR_pred_2100s <- RCP2.6_LR_2100s %>%
  select(Soil.quality, Crop, NO, Year, Province, 
         NO.Station)

for (i in 1:50) {
  RCP2.6_LR_pred_2100s$Ynpk_pred_2080<-predict(brt_LRSC_recyle[[i]],RCP2.6_LR_2080, n.trees=best_inter_LRSC_recyle[[i]])
  RCP2.6_LR_pred_2100s$Ynpk_pred_2081<-predict(brt_LRSC_recyle[[i]],RCP2.6_LR_2081, n.trees=best_inter_LRSC_recyle[[i]])
  RCP2.6_LR_pred_2100s$Ynpk_pred_2082<-predict(brt_LRSC_recyle[[i]],RCP2.6_LR_2082, n.trees=best_inter_LRSC_recyle[[i]])
  RCP2.6_LR_pred_2100s$Ynpk_pred_2083<-predict(brt_LRSC_recyle[[i]],RCP2.6_LR_2083, n.trees=best_inter_LRSC_recyle[[i]])
  RCP2.6_LR_pred_2100s$Ynpk_pred_2084<-predict(brt_LRSC_recyle[[i]],RCP2.6_LR_2084, n.trees=best_inter_LRSC_recyle[[i]])
  RCP2.6_LR_pred_2100s$Ynpk_pred_2085<-predict(brt_LRSC_recyle[[i]],RCP2.6_LR_2085, n.trees=best_inter_LRSC_recyle[[i]])
  RCP2.6_LR_pred_2100s$Ynpk_pred_2086<-predict(brt_LRSC_recyle[[i]],RCP2.6_LR_2086, n.trees=best_inter_LRSC_recyle[[i]])
  RCP2.6_LR_pred_2100s$Ynpk_pred_2087<-predict(brt_LRSC_recyle[[i]],RCP2.6_LR_2087, n.trees=best_inter_LRSC_recyle[[i]])
  RCP2.6_LR_pred_2100s$Ynpk_pred_2088<-predict(brt_LRSC_recyle[[i]],RCP2.6_LR_2088, n.trees=best_inter_LRSC_recyle[[i]])
  RCP2.6_LR_pred_2100s$Ynpk_pred_2089<-predict(brt_LRSC_recyle[[i]],RCP2.6_LR_2089, n.trees=best_inter_LRSC_recyle[[i]])
  RCP2.6_LR_pred_2100s$Ynpk_pred_2090<-predict(brt_LRSC_recyle[[i]],RCP2.6_LR_2090, n.trees=best_inter_LRSC_recyle[[i]])
  RCP2.6_LR_pred_2100s$Ynpk_pred_2091<-predict(brt_LRSC_recyle[[i]],RCP2.6_LR_2091, n.trees=best_inter_LRSC_recyle[[i]])
  RCP2.6_LR_pred_2100s$Ynpk_pred_2092<-predict(brt_LRSC_recyle[[i]],RCP2.6_LR_2092, n.trees=best_inter_LRSC_recyle[[i]])
  RCP2.6_LR_pred_2100s$Ynpk_pred_2093<-predict(brt_LRSC_recyle[[i]],RCP2.6_LR_2093, n.trees=best_inter_LRSC_recyle[[i]])
  RCP2.6_LR_pred_2100s$Ynpk_pred_2094<-predict(brt_LRSC_recyle[[i]],RCP2.6_LR_2094, n.trees=best_inter_LRSC_recyle[[i]])
  RCP2.6_LR_pred_2100s$Ynpk_pred_2095<-predict(brt_LRSC_recyle[[i]],RCP2.6_LR_2095, n.trees=best_inter_LRSC_recyle[[i]])
  RCP2.6_LR_pred_2100s$Ynpk_pred_2096<-predict(brt_LRSC_recyle[[i]],RCP2.6_LR_2096, n.trees=best_inter_LRSC_recyle[[i]])
  RCP2.6_LR_pred_2100s$Ynpk_pred_2097<-predict(brt_LRSC_recyle[[i]],RCP2.6_LR_2097, n.trees=best_inter_LRSC_recyle[[i]])
  RCP2.6_LR_pred_2100s$Ynpk_pred_2098<-predict(brt_LRSC_recyle[[i]],RCP2.6_LR_2098, n.trees=best_inter_LRSC_recyle[[i]])
  RCP2.6_LR_pred_2100s$Ynpk_pred_2099<-predict(brt_LRSC_recyle[[i]],RCP2.6_LR_2099, n.trees=best_inter_LRSC_recyle[[i]])
  RCP2.6_LR_pred_2100s$Ynpk_pred_80.99<-predict(brt_LRSC_recyle[[i]],RCP2.6_LR_80.99, n.trees=best_inter_LRSC_recyle[[i]])
  
  RCP2.6_LR_pred_2100s$average<- apply(RCP2.6_LR_pred_2100s[,7:26], 1, mean)
  RCP2.6_LR_pred_2100s_recyle[[i]] <- RCP2.6_LR_pred_2100s
}

RCP2.6_LR_pred_2100s_recyle_mean<-RCP2.6_LR_pred_2100s_recyle[[1]]
RCP2.6_LR_pred_2100s_recyle_sd<-RCP2.6_LR_pred_2100s_recyle[[1]]
for(i in 7:28){
  da=as.matrix(as.data.frame(sapply(1:50,function(x){RCP2.6_LR_pred_2100s_recyle[[x]][i]})))
  RCP2.6_LR_pred_2100s_recyle_mean[,i]=rowMeans(da)
  RCP2.6_LR_pred_2100s_recyle_sd[,i]=apply(da,1,sd)
}


###RCP8.5 during 2040-2099

RCP8.5_LR_2050s<-subset(RCP8.5_2050s, Crop == "late rice")
RCP8.5_LR_2040 <- RCP8.5_LR_2050s %>%
  select(Soil.quality, Tave_2040, Tmax_2040, Tmin_2040, GDD.10_2040, 
         PRE_2040, RAD_2040, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_LR_2040)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_LR_2041 <- RCP8.5_LR_2050s %>%
  select(Soil.quality, Tave_2041, Tmax_2041, Tmin_2041, GDD.10_2041, 
         PRE_2041, RAD_2041, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_LR_2041)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_LR_2042 <- RCP8.5_LR_2050s %>%
  select(Soil.quality, Tave_2042, Tmax_2042, Tmin_2042, GDD.10_2042, 
         PRE_2042, RAD_2042, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_LR_2042)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_LR_2043 <- RCP8.5_LR_2050s %>%
  select(Soil.quality, Tave_2043, Tmax_2043, Tmin_2043, GDD.10_2043, 
         PRE_2043, RAD_2043, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_LR_2043)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_LR_2044 <- RCP8.5_LR_2050s %>%
  select(Soil.quality, Tave_2044, Tmax_2044, Tmin_2044, GDD.10_2044, 
         PRE_2044, RAD_2044, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_LR_2044)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_LR_2045 <- RCP8.5_LR_2050s %>%
  select(Soil.quality, Tave_2045, Tmax_2045, Tmin_2045, GDD.10_2045, 
         PRE_2045, RAD_2045, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_LR_2045)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_LR_2046 <- RCP8.5_LR_2050s %>%
  select(Soil.quality, Tave_2046, Tmax_2046, Tmin_2046, GDD.10_2046, 
         PRE_2046, RAD_2046,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_LR_2046)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_LR_2047 <- RCP8.5_LR_2050s %>%
  select(Soil.quality, Tave_2047, Tmax_2047, Tmin_2047, GDD.10_2047, 
         PRE_2047, RAD_2047, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_LR_2047)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_LR_2048 <- RCP8.5_LR_2050s %>%
  select(Soil.quality, Tave_2048, Tmax_2048, Tmin_2048, GDD.10_2048, 
         PRE_2048, RAD_2048, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_LR_2048)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_LR_2049 <- RCP8.5_LR_2050s %>%
  select(Soil.quality, Tave_2049, Tmax_2049, Tmin_2049, GDD.10_2049, 
         PRE_2049, RAD_2049, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_LR_2049)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_LR_2050 <- RCP8.5_LR_2050s %>%
  select(Soil.quality, Tave_2050, Tmax_2050, Tmin_2050, GDD.10_2050, 
         PRE_2050, RAD_2050, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_LR_2050)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_LR_2051 <- RCP8.5_LR_2050s %>%
  select(Soil.quality, Tave_2051, Tmax_2051, Tmin_2051, GDD.10_2051, 
         PRE_2051, RAD_2051,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_LR_2051)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_LR_2052 <- RCP8.5_LR_2050s %>%
  select(Soil.quality, Tave_2052, Tmax_2052, Tmin_2052, GDD.10_2052, 
         PRE_2052, RAD_2052, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_LR_2052)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_LR_2053 <- RCP8.5_LR_2050s %>%
  select(Soil.quality, Tave_2053, Tmax_2053, Tmin_2053, GDD.10_2053, 
         PRE_2053, RAD_2053, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_LR_2053)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_LR_2054 <- RCP8.5_LR_2050s %>%
  select(Soil.quality, Tave_2054, Tmax_2054, Tmin_2054, GDD.10_2054, 
         PRE_2054, RAD_2054,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_LR_2054)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_LR_2055 <- RCP8.5_LR_2050s %>%
  select(Soil.quality, Tave_2055, Tmax_2055, Tmin_2055, GDD.10_2055, 
         PRE_2055, RAD_2055, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_LR_2055)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_LR_2056 <- RCP8.5_LR_2050s %>%
  select(Soil.quality, Tave_2056, Tmax_2056, Tmin_2056, GDD.10_2056, 
         PRE_2056, RAD_2056, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_LR_2056)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_LR_2057 <- RCP8.5_LR_2050s %>%
  select(Soil.quality, Tave_2057, Tmax_2057, Tmin_2057, GDD.10_2057, 
         PRE_2057, RAD_2057, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_LR_2057)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_LR_2058 <- RCP8.5_LR_2050s %>%
  select(Soil.quality, Tave_2058, Tmax_2058, Tmin_2058, GDD.10_2058, 
         PRE_2058, RAD_2058, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_LR_2058)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_LR_2059 <- RCP8.5_LR_2050s %>%
  select(Soil.quality, Tave_2059, Tmax_2059, Tmin_2059, GDD.10_2059, 
         PRE_2059, RAD_2059, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_LR_2059)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_LR_40.59 <- RCP8.5_LR_2050s %>%
  select(Soil.quality, Tave_2040.2059, Tmax_2040.2059, Tmin_2040.2059, GDD.10_2040.2059, 
         PRE_2040.2059, RAD_2040.2059,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_LR_40.59)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")

###predict yield by GBRT model

RCP8.5_LR_pred_2050s_recyle<-NULL

RCP8.5_LR_pred_2050s <- RCP8.5_LR_2050s %>%
  select(Soil.quality, Crop, NO, Year, Province, 
         NO.Station)

for (i in 1:50) {
  RCP8.5_LR_pred_2050s$Ynpk_pred_2040<-predict(brt_LRSC_recyle[[i]],RCP8.5_LR_2040, n.trees=best_inter_LRSC_recyle[[i]])
  RCP8.5_LR_pred_2050s$Ynpk_pred_2041<-predict(brt_LRSC_recyle[[i]],RCP8.5_LR_2041, n.trees=best_inter_LRSC_recyle[[i]])
  RCP8.5_LR_pred_2050s$Ynpk_pred_2042<-predict(brt_LRSC_recyle[[i]],RCP8.5_LR_2042, n.trees=best_inter_LRSC_recyle[[i]])
  RCP8.5_LR_pred_2050s$Ynpk_pred_2043<-predict(brt_LRSC_recyle[[i]],RCP8.5_LR_2043, n.trees=best_inter_LRSC_recyle[[i]])
  RCP8.5_LR_pred_2050s$Ynpk_pred_2044<-predict(brt_LRSC_recyle[[i]],RCP8.5_LR_2044, n.trees=best_inter_LRSC_recyle[[i]])
  RCP8.5_LR_pred_2050s$Ynpk_pred_2045<-predict(brt_LRSC_recyle[[i]],RCP8.5_LR_2045, n.trees=best_inter_LRSC_recyle[[i]])
  RCP8.5_LR_pred_2050s$Ynpk_pred_2046<-predict(brt_LRSC_recyle[[i]],RCP8.5_LR_2046, n.trees=best_inter_LRSC_recyle[[i]])
  RCP8.5_LR_pred_2050s$Ynpk_pred_2047<-predict(brt_LRSC_recyle[[i]],RCP8.5_LR_2047, n.trees=best_inter_LRSC_recyle[[i]])
  RCP8.5_LR_pred_2050s$Ynpk_pred_2048<-predict(brt_LRSC_recyle[[i]],RCP8.5_LR_2048, n.trees=best_inter_LRSC_recyle[[i]])
  RCP8.5_LR_pred_2050s$Ynpk_pred_2049<-predict(brt_LRSC_recyle[[i]],RCP8.5_LR_2049, n.trees=best_inter_LRSC_recyle[[i]])
  RCP8.5_LR_pred_2050s$Ynpk_pred_2050<-predict(brt_LRSC_recyle[[i]],RCP8.5_LR_2050, n.trees=best_inter_LRSC_recyle[[i]])
  RCP8.5_LR_pred_2050s$Ynpk_pred_2051<-predict(brt_LRSC_recyle[[i]],RCP8.5_LR_2051, n.trees=best_inter_LRSC_recyle[[i]])
  RCP8.5_LR_pred_2050s$Ynpk_pred_2052<-predict(brt_LRSC_recyle[[i]],RCP8.5_LR_2052, n.trees=best_inter_LRSC_recyle[[i]])
  RCP8.5_LR_pred_2050s$Ynpk_pred_2053<-predict(brt_LRSC_recyle[[i]],RCP8.5_LR_2053, n.trees=best_inter_LRSC_recyle[[i]])
  RCP8.5_LR_pred_2050s$Ynpk_pred_2054<-predict(brt_LRSC_recyle[[i]],RCP8.5_LR_2054, n.trees=best_inter_LRSC_recyle[[i]])
  RCP8.5_LR_pred_2050s$Ynpk_pred_2055<-predict(brt_LRSC_recyle[[i]],RCP8.5_LR_2055, n.trees=best_inter_LRSC_recyle[[i]])
  RCP8.5_LR_pred_2050s$Ynpk_pred_2056<-predict(brt_LRSC_recyle[[i]],RCP8.5_LR_2056, n.trees=best_inter_LRSC_recyle[[i]])
  RCP8.5_LR_pred_2050s$Ynpk_pred_2057<-predict(brt_LRSC_recyle[[i]],RCP8.5_LR_2057, n.trees=best_inter_LRSC_recyle[[i]])
  RCP8.5_LR_pred_2050s$Ynpk_pred_2058<-predict(brt_LRSC_recyle[[i]],RCP8.5_LR_2058, n.trees=best_inter_LRSC_recyle[[i]])
  RCP8.5_LR_pred_2050s$Ynpk_pred_2059<-predict(brt_LRSC_recyle[[i]],RCP8.5_LR_2059, n.trees=best_inter_LRSC_recyle[[i]])
  RCP8.5_LR_pred_2050s$Ynpk_pred_40.59<-predict(brt_LRSC_recyle[[i]],RCP8.5_LR_40.59, n.trees=best_inter_LRSC_recyle[[i]])
  
  RCP8.5_LR_pred_2050s$average<- apply(RCP8.5_LR_pred_2050s[,7:26], 1, mean)
  RCP8.5_LR_pred_2050s_recyle[[i]] <- RCP8.5_LR_pred_2050s
}

RCP8.5_LR_pred_2050s_recyle_mean<-RCP8.5_LR_pred_2050s_recyle[[1]]
RCP8.5_LR_pred_2050s_recyle_sd<-RCP8.5_LR_pred_2050s_recyle[[1]]
for(i in 7:28){
  da=as.matrix(as.data.frame(sapply(1:50,function(x){RCP8.5_LR_pred_2050s_recyle[[x]][i]})))
  RCP8.5_LR_pred_2050s_recyle_mean[,i]=rowMeans(da)
  RCP8.5_LR_pred_2050s_recyle_sd[,i]=apply(da,1,sd)
}

###RCP8.5 during 2080-2099

RCP8.5_LR_2100s<-subset(RCP8.5_2100s, Crop == "late rice")
RCP8.5_LR_2080 <- RCP8.5_LR_2100s %>%
  select(Soil.quality, Tave_2080, Tmax_2080, Tmin_2080, GDD.10_2080, 
         PRE_2080, RAD_2080,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_LR_2080)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_LR_2081 <- RCP8.5_LR_2100s %>%
  select(Soil.quality, Tave_2081, Tmax_2081, Tmin_2081, GDD.10_2081, 
         PRE_2081, RAD_2081, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_LR_2081)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_LR_2082 <- RCP8.5_LR_2100s %>%
  select(Soil.quality, Tave_2082, Tmax_2082, Tmin_2082, GDD.10_2082, 
         PRE_2082, RAD_2082, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_LR_2082)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_LR_2083 <- RCP8.5_LR_2100s %>%
  select(Soil.quality, Tave_2083, Tmax_2083, Tmin_2083, GDD.10_2083, 
         PRE_2083, RAD_2083, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_LR_2083)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_LR_2084 <- RCP8.5_LR_2100s %>%
  select(Soil.quality, Tave_2084, Tmax_2084, Tmin_2084, GDD.10_2084, 
         PRE_2084, RAD_2084, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_LR_2084)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_LR_2085 <- RCP8.5_LR_2100s %>%
  select(Soil.quality, Tave_2085, Tmax_2085, Tmin_2085, GDD.10_2085, 
         PRE_2085, RAD_2085, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_LR_2085)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_LR_2086 <- RCP8.5_LR_2100s %>%
  select(Soil.quality, Tave_2086, Tmax_2086, Tmin_2086, GDD.10_2086, 
         PRE_2086, RAD_2086,Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_LR_2086)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_LR_2087 <- RCP8.5_LR_2100s %>%
  select(Soil.quality, Tave_2087, Tmax_2087, Tmin_2087, GDD.10_2087, 
         PRE_2087, RAD_2087, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_LR_2087)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_LR_2088 <- RCP8.5_LR_2100s %>%
  select(Soil.quality, Tave_2088, Tmax_2088, Tmin_2088, GDD.10_2088, 
         PRE_2088, RAD_2088, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_LR_2088)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_LR_2089 <- RCP8.5_LR_2100s %>%
  select(Soil.quality, Tave_2089, Tmax_2089, Tmin_2089, GDD.10_2089, 
         PRE_2089, RAD_2089, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_LR_2089)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_LR_2090 <- RCP8.5_LR_2100s %>%
  select(Soil.quality, Tave_2090, Tmax_2090, Tmin_2090, GDD.10_2090, 
         PRE_2090, RAD_2090, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_LR_2090)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_LR_2091 <- RCP8.5_LR_2100s %>%
  select(Soil.quality, Tave_2091, Tmax_2091, Tmin_2091, GDD.10_2091, 
         PRE_2091, RAD_2091,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_LR_2091)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_LR_2092 <- RCP8.5_LR_2100s %>%
  select(Soil.quality, Tave_2092, Tmax_2092, Tmin_2092, GDD.10_2092, 
         PRE_2092, RAD_2092, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_LR_2092)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_LR_2093 <- RCP8.5_LR_2100s %>%
  select(Soil.quality, Tave_2093, Tmax_2093, Tmin_2093, GDD.10_2093, 
         PRE_2093, RAD_2093, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_LR_2093)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_LR_2094 <- RCP8.5_LR_2100s %>%
  select(Soil.quality, Tave_2094, Tmax_2094, Tmin_2094, GDD.10_2094, 
         PRE_2094, RAD_2094, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_LR_2094)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_LR_2095 <- RCP8.5_LR_2100s %>%
  select(Soil.quality, Tave_2095, Tmax_2095, Tmin_2095, GDD.10_2095, 
         PRE_2095, RAD_2095, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_LR_2095)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_LR_2096 <- RCP8.5_LR_2100s %>%
  select(Soil.quality, Tave_2096, Tmax_2096, Tmin_2096, GDD.10_2096, 
         PRE_2096, RAD_2096,Cultivar, Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_LR_2096)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_LR_2097 <- RCP8.5_LR_2100s %>%
  select(Soil.quality, Tave_2097, Tmax_2097, Tmin_2097, GDD.10_2097, 
         PRE_2097, RAD_2097, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_LR_2097)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_LR_2098 <- RCP8.5_LR_2100s %>%
  select(Soil.quality, Tave_2098, Tmax_2098, Tmin_2098, GDD.10_2098, 
         PRE_2098, RAD_2098, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_LR_2098)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_LR_2099 <- RCP8.5_LR_2100s %>%
  select(Soil.quality, Tave_2099, Tmax_2099, Tmin_2099, GDD.10_2099, 
         PRE_2099, RAD_2099, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_LR_2099)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                          "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                          "N", "P2O5", "K2O", "Yck", "Ynpk")

RCP8.5_LR_80.99 <- RCP8.5_LR_2100s %>%
  select(Soil.quality, Tave_2080.2099, Tmax_2080.2099, Tmin_2080.2099, GDD.10_2080.2099, 
         PRE_2080.2099, RAD_2080.2099, Cultivar,Soil.type, Soil.texture, SOM, OP, AK, pH,
         N, P2O5, K2O, Yck, Ynpk)
names(RCP8.5_LR_80.99)<- c("SQ", "Tave", "Tmax", "Tmin", "GDD.10", "PRE", "RAD","Cultivar",
                           "Soil.type", "Soil.texture", "SOM", "OP", "AK", "pH",
                           "N", "P2O5", "K2O", "Yck", "Ynpk")


###predict yield by GBRT model
RCP8.5_LR_pred_2100s_recyle<-NULL

RCP8.5_LR_pred_2100s <- RCP8.5_LR_2100s %>%
  select(Soil.quality, Crop, NO, Year, Province, 
         NO.Station)

for (i in 1:50) {
  RCP8.5_LR_pred_2100s$Ynpk_pred_2080<-predict(brt_LRSC_recyle[[i]],RCP8.5_LR_2080, n.trees=best_inter_LRSC_recyle[[i]])
  RCP8.5_LR_pred_2100s$Ynpk_pred_2081<-predict(brt_LRSC_recyle[[i]],RCP8.5_LR_2081, n.trees=best_inter_LRSC_recyle[[i]])
  RCP8.5_LR_pred_2100s$Ynpk_pred_2082<-predict(brt_LRSC_recyle[[i]],RCP8.5_LR_2082, n.trees=best_inter_LRSC_recyle[[i]])
  RCP8.5_LR_pred_2100s$Ynpk_pred_2083<-predict(brt_LRSC_recyle[[i]],RCP8.5_LR_2083, n.trees=best_inter_LRSC_recyle[[i]])
  RCP8.5_LR_pred_2100s$Ynpk_pred_2084<-predict(brt_LRSC_recyle[[i]],RCP8.5_LR_2084, n.trees=best_inter_LRSC_recyle[[i]])
  RCP8.5_LR_pred_2100s$Ynpk_pred_2085<-predict(brt_LRSC_recyle[[i]],RCP8.5_LR_2085, n.trees=best_inter_LRSC_recyle[[i]])
  RCP8.5_LR_pred_2100s$Ynpk_pred_2086<-predict(brt_LRSC_recyle[[i]],RCP8.5_LR_2086, n.trees=best_inter_LRSC_recyle[[i]])
  RCP8.5_LR_pred_2100s$Ynpk_pred_2087<-predict(brt_LRSC_recyle[[i]],RCP8.5_LR_2087, n.trees=best_inter_LRSC_recyle[[i]])
  RCP8.5_LR_pred_2100s$Ynpk_pred_2088<-predict(brt_LRSC_recyle[[i]],RCP8.5_LR_2088, n.trees=best_inter_LRSC_recyle[[i]])
  RCP8.5_LR_pred_2100s$Ynpk_pred_2089<-predict(brt_LRSC_recyle[[i]],RCP8.5_LR_2089, n.trees=best_inter_LRSC_recyle[[i]])
  RCP8.5_LR_pred_2100s$Ynpk_pred_2090<-predict(brt_LRSC_recyle[[i]],RCP8.5_LR_2090, n.trees=best_inter_LRSC_recyle[[i]])
  RCP8.5_LR_pred_2100s$Ynpk_pred_2091<-predict(brt_LRSC_recyle[[i]],RCP8.5_LR_2091, n.trees=best_inter_LRSC_recyle[[i]])
  RCP8.5_LR_pred_2100s$Ynpk_pred_2092<-predict(brt_LRSC_recyle[[i]],RCP8.5_LR_2092, n.trees=best_inter_LRSC_recyle[[i]])
  RCP8.5_LR_pred_2100s$Ynpk_pred_2093<-predict(brt_LRSC_recyle[[i]],RCP8.5_LR_2093, n.trees=best_inter_LRSC_recyle[[i]])
  RCP8.5_LR_pred_2100s$Ynpk_pred_2094<-predict(brt_LRSC_recyle[[i]],RCP8.5_LR_2094, n.trees=best_inter_LRSC_recyle[[i]])
  RCP8.5_LR_pred_2100s$Ynpk_pred_2095<-predict(brt_LRSC_recyle[[i]],RCP8.5_LR_2095, n.trees=best_inter_LRSC_recyle[[i]])
  RCP8.5_LR_pred_2100s$Ynpk_pred_2096<-predict(brt_LRSC_recyle[[i]],RCP8.5_LR_2096, n.trees=best_inter_LRSC_recyle[[i]])
  RCP8.5_LR_pred_2100s$Ynpk_pred_2097<-predict(brt_LRSC_recyle[[i]],RCP8.5_LR_2097, n.trees=best_inter_LRSC_recyle[[i]])
  RCP8.5_LR_pred_2100s$Ynpk_pred_2098<-predict(brt_LRSC_recyle[[i]],RCP8.5_LR_2098, n.trees=best_inter_LRSC_recyle[[i]])
  RCP8.5_LR_pred_2100s$Ynpk_pred_2099<-predict(brt_LRSC_recyle[[i]],RCP8.5_LR_2099, n.trees=best_inter_LRSC_recyle[[i]])
  RCP8.5_LR_pred_2100s$Ynpk_pred_80.99<-predict(brt_LRSC_recyle[[i]],RCP8.5_LR_80.99, n.trees=best_inter_LRSC_recyle[[i]])
  
  RCP8.5_LR_pred_2100s$average<- apply(RCP8.5_LR_pred_2100s[,7:26], 1, mean)
  RCP8.5_LR_pred_2100s_recyle[[i]] <- RCP8.5_LR_pred_2100s
}

RCP8.5_LR_pred_2100s_recyle_mean<-RCP8.5_LR_pred_2100s_recyle[[1]]
RCP8.5_LR_pred_2100s_recyle_sd<-RCP8.5_LR_pred_2100s_recyle[[1]]
for(i in 7:28){
  da=as.matrix(as.data.frame(sapply(1:50,function(x){RCP8.5_LR_pred_2100s_recyle[[x]][i]})))
  RCP8.5_LR_pred_2100s_recyle_mean[,i]=rowMeans(da)
  RCP8.5_LR_pred_2100s_recyle_sd[,i]=apply(da,1,sd)
}

###summarise predicted yield

sum_RCP2.6_LR_2050s_year<- cbind(base_LR_pred_all[,c("Soil.quality","Crop","NO","average")],RCP2.6_LR_pred_2050s[,7:28])
names(sum_RCP2.6_LR_2050s_year)[1:4]<- c("Soil quality", "Crop", "No", "Yield_base")
names(sum_RCP2.6_LR_2050s_year)[26]<- "Yield_RCP"
sum_RCP2.6_LR_2050s_year$Yield_change <- sum_RCP2.6_LR_2050s_year$`Yield_RCP` - sum_RCP2.6_LR_2050s_year$`Yield_base`
sum_RCP2.6_LR_2050s_year$Yield_change_per <- (sum_RCP2.6_LR_2050s_year$Yield_change / sum_RCP2.6_LR_2050s_year$Yield_base)*100
sum_RCP2.6_LR_2050s_year$RCP <- "RCP2.6"
sum_RCP2.6_LR_2050s_year$Time <- "2050"

sum_RCP8.5_LR_2050s_year<- cbind(base_LR_pred_all[,c("Soil.quality","Crop","NO","average")],RCP8.5_LR_pred_2050s[,7:28])
names(sum_RCP8.5_LR_2050s_year)[1:4]<- c("Soil quality", "Crop", "No", "Yield_base")
names(sum_RCP8.5_LR_2050s_year)[26]<- "Yield_RCP"
sum_RCP8.5_LR_2050s_year$Yield_change <- sum_RCP8.5_LR_2050s_year$`Yield_RCP` - sum_RCP8.5_LR_2050s_year$`Yield_base`
sum_RCP8.5_LR_2050s_year$Yield_change_per <- (sum_RCP8.5_LR_2050s_year$Yield_change / sum_RCP8.5_LR_2050s_year$Yield_base)*100
sum_RCP8.5_LR_2050s_year$RCP <- "RCP8.5"
sum_RCP8.5_LR_2050s_year$Time <- "2050"

sum_RCP2.6_LR_2100s_year<- cbind(base_LR_pred_all[,c("Soil.quality","Crop","NO","average")],RCP2.6_LR_pred_2100s[,7:28])
names(sum_RCP2.6_LR_2100s_year)[1:4]<- c("Soil quality", "Crop", "No", "Yield_base")
names(sum_RCP2.6_LR_2100s_year)[26]<- "Yield_RCP"
sum_RCP2.6_LR_2100s_year$Yield_change <- sum_RCP2.6_LR_2100s_year$`Yield_RCP` - sum_RCP2.6_LR_2100s_year$`Yield_base`
sum_RCP2.6_LR_2100s_year$Yield_change_per <- (sum_RCP2.6_LR_2100s_year$Yield_change / sum_RCP2.6_LR_2100s_year$Yield_base)*100
sum_RCP2.6_LR_2100s_year$RCP <- "RCP2.6"
sum_RCP2.6_LR_2100s_year$Time <- "2100"

sum_RCP8.5_LR_2100s_year<- cbind(base_LR_pred_all[,c("Soil.quality","Crop","NO","average")],RCP8.5_LR_pred_2100s[,7:28])
names(sum_RCP8.5_LR_2100s_year)[1:4]<- c("Soil quality", "Crop", "No", "Yield_base")
names(sum_RCP8.5_LR_2100s_year)[26]<- "Yield_RCP"
sum_RCP8.5_LR_2100s_year$Yield_change <- sum_RCP8.5_LR_2100s_year$`Yield_RCP` - sum_RCP8.5_LR_2100s_year$`Yield_base`
sum_RCP8.5_LR_2100s_year$Yield_change_per <- (sum_RCP8.5_LR_2100s_year$Yield_change / sum_RCP8.5_LR_2100s_year$Yield_base)*100
sum_RCP8.5_LR_2100s_year$RCP <- "RCP8.5"
sum_RCP8.5_LR_2100s_year$Time <- "2100"

sum_RCP_LR_2050s_year <- rbind(sum_RCP2.6_LR_2050s_year,sum_RCP8.5_LR_2050s_year)
sum_RCP_LR_2100s_year<-rbind(sum_RCP2.6_LR_2100s_year,sum_RCP8.5_LR_2100s_year)

write.csv(sum_RCP_LR_2050s_year,"./results/sum_RCP_LR_2050s_year.csv")
write.csv(sum_RCP_LR_2100s_year,"./results/sum_RCP_LR_2100s_year.csv")

###comparison of yield change between high and low quality soils 
###T test

t.test(Yield_change~`Soil quality` ,sum_RCP2.6_LR_2050s_year) #RCP2.6-2050s
t.test(Yield_change~`Soil quality` ,sum_RCP8.5_LR_2050s_year) #RCP8.5-2050s
t.test(Yield_change~`Soil quality` ,sum_RCP2.6_LR_2100s_year) #RCP2.6-2100s
t.test(Yield_change~`Soil quality` ,sum_RCP8.5_LR_2100s_year) #RCP8.5-2100s
