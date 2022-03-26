# set working directory
setwd("C:/Users/klchi/OneDrive/Desktop/Advance business forecast/Second Submission")
# setwd("C:/Users/klchi/OneDrive/Desktop/Advance business forecast/Mar22")

# load packages :
library(tidyverse)
library(lubridate)
library(janitor)
library(forecast)
library(imputeTS)
library(hts)
library(MAPA)
library(thief)


# ----------------------------------------------------------------------------------------------------


###### CLEANING DATA ######
# import, clean, and convert data to time series format:
source("clean_data.R")


# ----------------------------------------------------------------------------------------------------


###### TIME SERIES VALIDATION FOR MODEL SELECTION ######
# we will use the small period data

### ***** MODEL POOL: we may add more models in each submission

# array identifying best model for each ts:
best <- array(NA,length(load_ts1))

# for every time series:
for (i in 1:length(load_ts1)){
  y <- load_ts1[[i]]
  dy <- data.frame(y=as.vector(y),x=rep(0:23,length(y)/24))
  
  # split the data to train/validation sets:
  h <- 7*24 
  yt <- head(y, length(y) - h)
  yv <- tail(y, h)
  xt <- head(dy$x, length(y) - h)
  xv <- tail(dy$x, h)
  yt_thief <-ts(yt,freq=24)
  
  # multi-seasonal time series format, train-validation split:
  yms <- msts(y,seasonal.periods = c(24,24*7))
  ymst <- head(yms, length(yms)-h)
  ymsv <- tail(yms, h)
  
  # MODEL POOL
  models <- c("msts","tbats","thief","mapa",
              "determ","stoch","dynam-harm-","ae-comb")
  
  ### Multiple seasonalities (daily, hourly)
  fit1 <- stlm(ymst, lambda = 0)
  fc1 <- forecast(fit1, h=h)$mean
  
  ### TBATS with time of day as x-variable:
  fit2 <- tbats(log(ymst),use.box.cox=F,use.trend = TRUE,
                use.damped.trend = TRUE,xreg=xt)
  fc2 <- exp(forecast(fit2, h=h)$mean)
  
  ### Thief 
  fit3 <- thief(yt_thief,h=h,comb="struc", usemodel="ets")
  fc3 <- forecast(fit3, h=h)$mean
  
  ### MAPA
  fc4 <- mapasimple(yt, comb = "w.mean", fh = h)
  
  ### deterministic trend:
  # fit5 <- auto.arima(yt, d=0, xreg=1:length(yt))
  # fc5 <- forecast(fit5, xreg=length(yt)+1:h, h=h)$mean
  
  ### stochastic trend:
  # fit6 <- auto.arima(yt, d=1)
  # fc6 <- forecast(fit6, h=h)$mean
  
  ### dynamic-harmonic regression:
  # AICc <- array(NA,6)
  # for (j in 1:6) {
  #   x <- auto.arima(yt, xreg = fourier(yt, K = j), seasonal=FALSE, lambda=0)
  #   AICc[[j]] <- x[["aicc"]] 
  # }
  # models[7] <- paste0(models[7],which.min(AICc))
  # fit7 <- auto.arima(yt, xreg = fourier(yt, K=which.min(AICc)),
  #                    seasonal = FALSE, lambda = 0)
  # fc7 <- forecast(fit7,xreg=fourier(yt, K=which.min(AICc), h=h))$mean
  
  ### combination of ets-arima models:
  # fc8 <- 0.5*as.vector(forecast(auto.arima(yt),h=h)$mean) +
  #        0.5*as.vector(forecast(ets(yt),h=h)$mean)
  
  # evaluate accuracy using MAPE:
  # ?forecast::accuracy()
  MAPEs <- round(c(accuracy(fc1,ymsv)[5], accuracy(fc2,ymsv)[5],
                   accuracy(fc3,yv)[5], accuracy(fc4$forecast,yv)[5]),2)
  #                accuracy(fc5,yv)[5], accuracy(fc6,yv)[5],
  #                accuracy(fc7,yv)[5], accuracy(fc8,yv)[5]
  
  # choose model based on better MAPE value:
  best[i] <- models[which.min(MAPEs)]
}
beepr::beep(2) ; View(data.frame(country=names(load_ts1),best))

# clear environment :
rm(list=setdiff(ls(),c("best","load_ts1","load_ts2")))


# ----------------------------------------------------------------------------------------------------


###### HIERARCHICAL MODEL ######

# # GEOGRAPHICAL SPLIT: re-arrange columns to fit hierarchies:
# load_df1 <- do.call("data.frame",load_ts1)[,c(1,2,4,10,3,6,9,5,7,8)]
# # train/validation split:
# train <- head(load_df1,dim(load_df1)[1]-24*7)
# test <- tail(load_df1,24*7)
# # get the data in proper form:
# hier <- hts(train, nodes=list(3, c(4,3,3)))
# hiert <- hts(test, nodes=list(3, c(4,3,3)))
# 
# # forecasts --> methods = c("comb","bu","mo","tdgsa","tdgsf","tdfp")
# #               fmethods = c("ets","arima","rw")
# hierf <- forecast.gts(hier, h=24*7, method="tdgsa", fmethod="rw")
# # get MAPE for all forecasts:
# accuracy.gts(hierf,levels=2,test=hiert)[4,]
# load_df1 <- do.call("data.frame",load_ts1)[,c(1,4,7,3,6,9,5,8,2,10)]
# # train/validation split:
# train <- head(load_df1,dim(load_df1)[1]-24*7)
# test <- tail(load_df1,24*7)
# # get the data in proper form:
# hier <- hts(train, nodes=list(4, c(3,3,2,2)))
# hiert <- hts(test, nodes=list(4, c(3,3,2,2)))
# hier$labels$`Level 1` <- c("Central","Southwest","Southeast","Scandinavia")
# 
# # forecasts --> methods = c("comb","bu","mo","tdgsa","tdgsf","tdfp")
# #               fmethods = c("ets","arima","rw")
# hierf <- forecast.gts(hier, h=24*7, method="tdgsa", fmethod="rw")
# # get MAPE for all forecasts:
# accuracy.gts(hierf,levels=2,test=hiert)[4,]
# 
# 
# 
# # SPLIT BY GDP PER CAPITA: re-arrange columns to fit hierarchies:
# load_df1 <- do.call("data.frame",load_ts1)[,c(1,2,4,10,3,6,9,5,7,8)]
# # train/validation split:
# train <- head(load_df1,dim(load_df1)[1]-24*7)
# test <- tail(load_df1,24*7)
# # get the data in proper form:
# hier <- hts(train, nodes=list(3, c(4,3,3)))
# hiert <- hts(test, nodes=list(3, c(4,3,3)))
# 
# # forecasts --> methods = c("comb","bu","mo","tdgsa","tdgsf","tdfp")
# #               fmethods = c("ets","arima","rw")
# hierf <- forecast.gts(hier, h=24*7, method="tdgsa", fmethod="rw")
# # get MAPE for all forecasts:
# accuracy.gts(hierf,levels=2,test=hiert)[4,]


# ----------------------------------------------------------------------------------------------------   
   
   
###### Short period VS Long period VS Short-Long combination ######

### ***** adjust if loops, based on best values ***** ###

# forecast horizon:
h = 24*7

# 1:small, 2:large
# tables holding forecasts:
FC_1 <- array(NA,c(h,length(load_ts1)))
FC_2<- array(NA,c(h,length(load_ts2)))
FC_comb <- array(NA,c(h,length(load_ts1)))
colnames(FC_1) <- names(load_ts1)
colnames(FC_2) <- names(load_ts1)
colnames(FC_comb) <- names(load_ts1)

# choice array:
choice <- array(NA,length(load_ts1))

for (i in 1:length(load_ts1)) {
  
  # time series, train-validation split:
  y1 <- load_ts1[[i]]
  y2 <- load_ts2[[i]]
  y1t <- head(y1, length(y1)-h)
  y1v <- tail(y1, h)
  y2t <- head(y2, length(y1)-h)
  y2v <- tail(y2, h)
  
  # multi-seasonal time series format, train-validation split:
  y1ms <- msts(load_ts1[[i]],seasonal.periods = c(24,24*7))
  y1mst <- head(y1ms, length(y1ms)-h)
  y1msv <- tail(y1ms, h)

  y2ms <- msts(load_ts2[[i]],seasonal.periods = c(24,24*7))
  y2mst <- head(y2ms, length(y2ms)-h)
  y2msv <- tail(y2ms, h)
  
  x1<-rep(0:23,length(y1ms)/24)
  x1t <- head(x1, length(y1ms)-h)
  x1v <- tail(x1, h)
  
  x2<-rep(0:23,length(y2ms)/24)
  x2t <- head(x2, length(y2ms)-h)
  x2v <- tail(x2, h)
  
  best_forecast <- array(NA,3)
  period <-c("short","long","comb")
  
  # generate forecasts for 1-month and combination:
  if (best[i]=="msts") {
    # Multiple Seasonalities
    fit1 <- stlm(y1mst, lambda = 0)
    fit2 <- stlm(y2mst, lambda = 0)
    FC_1[,i] <- as.vector(forecast(fit1, h=h)$mean)
    FC_2[,i] <-as.vector(forecast(fit2, h=h)$mean)
    FC_comb[,i] <- 0.5*as.vector(forecast(fit1,h=h)$mean) + 
      0.5*as.vector(forecast(fit2,h=h)$mean)
  } else if (best[i]=="tbats") {
    # tbats
    fit1 <- tbats(log(y1mst),use.box.cox=F,use.trend = TRUE,
                  use.damped.trend = TRUE,xreg=x1t)
    fit2 <- tbats(log(y2mst),use.box.cox=F,use.trend = TRUE,
                  use.damped.trend = TRUE,xreg=x2t)
    FC_1[,i] <- as.vector(exp(forecast(fit1, h=h)$mean))
    FC_2[,i]<- as.vector(exp(forecast(fit2, h=h)$mean))
    FC_comb[,i] <- 0.5*as.vector(exp(forecast(fit1, h=h)$mean)) + 
      0.5*as.vector(exp(forecast(fit2, h=h)$mean))
  } else if (best[i]=="thief") {
    # thief
    fit1 <- thief(y1t,h=h,comb="struc", usemodel="ets")
    fit2 <- thief(y2t,h=h,comb="struc", usemodel="ets")
    FC_1[,i] <- as.vector(forecast(fit1, h=h)$mean)
    FC_2[,i] <-as.vector(forecast(fit2, h=h)$mean)
    FC_comb[,i] <- 0.5*as.vector(forecast(fit1,h=h)$mean) + 
      0.5*as.vector(forecast(fit2,h=h)$mean)    
  } else {
    # MAPA
    FC_1[,i] <- as.vector(mapasimple(y1t, comb = "w.mean", fh = h)$forecast)
    FC_2[,i] <-as.vector(mapasimple(y2t, comb = "w.mean", fh = h)$forecast)
    FC_comb[,i] <- 0.5*as.vector(mapasimple(y1t, comb = "w.mean", fh = h)$forecast) +
      -.5*as.vector(mapasimple(y2t, comb = "w.mean", fh = h)$forecast)
    
  }
  # put forecast in best_forecast array
  best_forecast[1] = accuracy(FC_1[,i],y1msv)[5] 
  best_forecast[2] = accuracy(FC_2[,i],y1msv)[5] 
  best_forecast[3] = accuracy(FC_comb[,i],y1msv)[5]
  # choose forecast based on MAPE value:
  choice[i]=period[which.min(best_forecast)]
}

beepr::beep(2) ; View(cbind(country=colnames(FC_1),best,choice))


# ----------------------------------------------------------------------------------------------------


###### PRODUCE FORECASTS ######

### --- adjust if loops, based on choice values!! --- ###

h <- 24*8
results <- array(NA, c(h,length(load_ts1)))
colnames(results) <- names(load_ts1)

for (i in 1:dim(results)[2]) {
  y1 <- load_ts1[[i]]
  y2 <-load_ts2[[i]]
  
  y1ms <- msts(load_ts1[[i]],seasonal.periods = c(24,24*7))
  y2ms <- msts(load_ts2[[i]][1:3648],seasonal.periods = c(24,24*7))
  
  x1 <- rep(0:23,length(y1ms)/24)
  x2 <- rep(0:23,length(y2ms)/24)
  
  if (best[i] == "msts") {
    if (choice[i] == "short") {
      results[,i] <- as.vector(forecast(stlm(y1ms, lambda = 0),h=h)$mean)
    }
    else if (choice[i]== "long"){
      results[,i] <- as.vector(forecast(stlm(y2ms, lambda = 0),h=h)$mean)
    }
    else {
      results[,i] <- 0.5*as.vector(forecast(stlm(y1ms, lambda = 0),h=h)$mean) +
        0.5*as.vector(forecast(stlm(y2ms, lambda = 0),h=h)$mean)
    }
  }
  else if (best[i] == "tbats") {
    if (choice[i] == "short") {
      results[,i] <- as.vector(exp(forecast(tbats(log(y1ms),use.box.cox=F,use.trend = TRUE,
                                                  use.damped.trend = TRUE,xreg=x1),h=h)$mean))
    }
    else if (choice[i]== "long"){
      results[,i] <- as.vector(exp(forecast(tbats(log(y2ms),use.box.cox=F,use.trend = TRUE,
                                                  use.damped.trend = TRUE,xreg=x2),h=h)$mean))
    }
    else {
      results[,i] <- 0.5*as.vector(exp(forecast(tbats(log(y1ms),use.box.cox=F,use.trend = TRUE,
                                                      use.damped.trend = TRUE,xreg=x1),h=h)$mean)) +
        0.5*0.5*as.vector(exp(forecast(tbats(log(y2ms),use.box.cox=F,use.trend = TRUE,
                                             use.damped.trend = TRUE,xreg=x2),h=h)$mean))
    }
  }
  else if (best[i] == "thief") {
    if (choice[i] == "short") {
      results[,i] <- as.vector(forecast(thief(y1,h=h,comb="struc", usemodel="ets"),h=h)$mean)
    }
    else if (choice[i]== "long"){
      results[,i] <- as.vector(forecast(thief(y2,h=h,comb="struc", usemodel="ets"),h=h)$mean)
    }
    else {
      results[,i] <- 0.5*as.vector(forecast(thief(y1,h=h,comb="struc", usemodel="ets"),h=h)$mean) +
        0.5*as.vector(forecast(thief(y2,h=h,comb="struc", usemodel="ets"),h=h)$mean)
    }
  }
  else {
    if (choice[i] == "short") { # MAPA
      results[,i] <- as.vector(mapasimple(y1, comb = "w.mean", fh = h)$forecast)
    }
    else if (choice[i]== "long"){
      results[,i] <- as.vector(mapasimple(y2, comb = "w.mean", fh = h)$forecast)
    }
    else {
      results[,i] <- 0.5*as.vector(mapasimple(y1, comb = "w.mean", fh = h)$forecast) +
        0.5*as.vector(mapasimple(y2, comb = "w.mean", fh = h)$forecast)
    }
  }
}

# clear environment :
rm(list=setdiff(ls(),c("best","results","choice"))) #,"load_ts1","load_ts2"

results <- data.frame(results)
names(results) <- c("Belgium","Finland","France","Germany","Greece",
                   "Italy","Poland","Romania","Spain","Sweden")
beepr::beep(2)
# export results as .csv file:
# remove first 24 rows
write.csv(data.frame(results[-(1:24),]),"MN50755_Group1_Formative_data_as_of_Mar24.csv",row.names=FALSE)
