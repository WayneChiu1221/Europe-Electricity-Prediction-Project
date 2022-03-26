# load packages :
library(tidyverse)
library(lubridate)
library(janitor)
library(forecast)
library(imputeTS)

# set working directory :

setwd("C:/Users/klchi/OneDrive/Desktop/Advance business forecast/Second Submission")
#setwd("~/University of Bath Files/MN50755/Coursework")


# country initials :
country = c("BE","FI","FR","DE","GR","IT","PO","RO","ES","SE")



###### 4 WEEKS OF DATA ######
load_data1 <- list()

# initialize the time frame that will be used to train the model :
start_date = "2022-02-25" # 2022-02-04  2022-02-25    2022-04-01
end_date = "2022-03-24"   # 2022-03-03  2022-03-24    2022-04-28

for (i in 1:length(country)) {
  # 2021 data:
  x1 <- read.csv(paste0(country[i],"-2021.csv")) %>% select(1,3)
  names(x1) <- c("Date_Time","Load")
  x1 <- x1 %>% 
    mutate(Load = as.numeric(Load),
           Date = dmy(gsub("\\.","\\-",str_sub(Date_Time,1,10))),
           Time = paste0(str_sub(Date_Time,12,16),"-",
                         str_sub(Date_Time,-5,-1))) %>% 
    filter(Date >= start_date) %>% 
    select(3,4,2)
  
  # 2022 data:
  x2 <- read_csv(paste0(country[i],"-2022.csv")) %>% select(1,3)
  names(x2) <- c("Date_Time","Load")
  x2 <- x2 %>% 
    mutate(Load = case_when(
      # is.na(Load) ~ 0,
      Load=="-" ~ "0",
      Load=="" ~ "0",
      Load=="N/A" ~ "NA",
      TRUE ~ Load
    ),
    Date = dmy(gsub("\\.","\\-",str_sub(Date_Time,1,10))),
    Time = paste0(str_sub(Date_Time,12,16),"-",
                  str_sub(Date_Time,-5,-1))) %>% 
    filter(Date <= end_date) %>% 
    filter(Date >= start_date) %>% 
    select(3,4,2) %>% 
    mutate(Load=as.numeric(Load))
  
  # merge 2011 and 2022 data together :
  x <- rbind(x1,x2) %>% 
    mutate(Time=str_sub(Time,1,5))
  
  # if we have data per 15':
  if (i %in% c(1,4,8)) {
    x <- x %>% group_by(count = (row_number() -1) %/% 4) %>%
      mutate(xLoad = round(sum(Load,na.rm=T),0)) %>% 
      select(1,2,5) %>% 
      rename(Load=xLoad) %>% 
      tibble() %>% 
      select(-1)
    x <- x[seq(1,dim(x)[1],4),] 
  }
  
  # data frame in its final form :
  x <- x %>% 
    mutate(Date_Time = paste0(Date," ",Time,":00")) %>% 
    select(4,3)
  load_data1[[i]] <- data.frame(x)
  load_data1[[i]] <- load_data1[[i]][!duplicated(load_data1[[i]]$Date_Time),]
  
}
# name every data frame by country initials :
names(load_data1) <- country

# check if there are any missing values (NA) : 
lapply(load_data1,function(x) {colSums(is.na(x))})

# treat missing values:
# 3, 5, 8, 9 have missing values. use imputeTS package for imputation:
# here, we applied simple moving average:
for (i in c(3,5,8,9)) {
  load_data1[[i]]$Load <- na_ma(load_data1[[i]]$Load,weighting = "simple")
}

# create a list to hold time series data :
load_ts1 <- list()
for (i in 1:length(load_data1)) {
  load_data1[[i]]$Date_Time <- ymd_hms(load_data1[[i]]$Date_Time)
  load_ts1[[i]] <- load_data1[[i]]$Load %>% ts(freq=24)
}
# name every time series by country initials :
names(load_ts1) <- country



###### 5 MONTHS OF DATA ######
load_data2 <- list()

# initialize the time frame that will be used to train the model :
start_date = "2021-10-24" # 2021-10-03  2021-10-24    2021-11-28
end_date = "2022-03-24"   # 2022-03-03  2022-03-24    2022-04-28

for (i in 1:length(country)) {
  # 2021 data:
  x1 <- read.csv(paste0(country[i],"-2021.csv")) %>% select(1,3)
  names(x1) <- c("Date_Time","Load")
  x1 <- x1 %>% 
    mutate(Load = as.numeric(Load),
           Date = dmy(gsub("\\.","\\-",str_sub(Date_Time,1,10))),
           Time = paste0(str_sub(Date_Time,12,16),"-",
                         str_sub(Date_Time,-5,-1))) %>% 
    filter(Date >= start_date) %>%  
    select(3,4,2)
  
  # 2022 data:
  x2 <- read_csv(paste0(country[i],"-2022.csv")) %>% select(1,3)
  names(x2) <- c("Date_Time","Load")
  x2 <- x2 %>% 
    mutate(Load = case_when(
      # is.na(Load) ~ 0,
      Load=="-" ~ "0",
      Load=="" ~ "0",
      Load=="N/A" ~ "NA",
      TRUE ~ Load
    ),
    Date = dmy(gsub("\\.","\\-",str_sub(Date_Time,1,10))),
    Time = paste0(str_sub(Date_Time,12,16),"-",
                  str_sub(Date_Time,-5,-1))) %>% 
    filter(Date <= end_date) %>% 
    filter(Date >= start_date) %>% 
    select(3,4,2) %>% 
    mutate(Load=as.numeric(Load))
  
  # merge 2011 and 2022 data together :
  x <- rbind(x1,x2) %>% 
    mutate(Time=str_sub(Time,1,5))
  
  # if we have data per 15':
  if (i %in% c(1,4,8)) {
    x <- x %>% group_by(count = (row_number() -1) %/% 4) %>%
      mutate(xLoad = round(sum(Load,na.rm=T),0)) %>% 
      select(1,2,5) %>% 
      rename(Load=xLoad) %>% 
      tibble() %>% 
      select(-1)
    x <- x[seq(1,dim(x)[1],4),] 
  }
  
  # data frame in its final form :
  x <- x %>% 
    mutate(Date_Time = paste0(Date," ",Time,":00")) %>% 
    select(4,3)
  load_data2[[i]] <- data.frame(x)
  load_data2[[i]] <- load_data2[[i]][!duplicated(load_data2[[i]]$Date_Time),]
  
}
# name every data frame by country initials :
names(load_data2) <- country

# check if there are any missing values (NA) : 
lapply(load_data2,function(x) {colSums(is.na(x))})

# treat missing values and 0s:
# 3, 5, 8, 9 have missing values. use imputeTS package for imputation:
# here, we applied simple moving average:
for (i in c(3,5,8,9)) {
  load_data2[[i]]$Load <- na_ma(load_data2[[i]]$Load,weighting = "simple")
}

for (i in 1:dim(load_data2[[1]])[1]) {
 for (j in 1:length(load_data2)) {
   if (load_data2[[j]]$Load[i] == 0) {
     load_data2[[j]]$Load[i] = mean(load_data2[[j]]$Load)
   }
 }
}

# create a list to hold time series data :
load_ts2 <- list()
for (i in 1:length(load_data2)) {
  load_data2[[i]]$Date_Time <- ymd_hms(load_data2[[i]]$Date_Time)
  load_ts2[[i]] <- load_data2[[i]]$Load %>% ts(freq=24)
}
# name every time series by country initials :
names(load_ts2) <- country

# check for outliers (replacements are made using linear interpolation):
for (i in 1:length(load_ts1)) {
  load_ts1[[i]] <- tsclean(load_ts1[[i]])
}
for (i in 1:length(load_ts2)) {
  load_ts2[[i]] <- tsclean(load_ts2[[i]])
}

# clear environment :
# keep 2 week and 5 month datasets:
rm(list=setdiff(ls(),c("load_ts1","load_ts2"))) #"load_data1","load_data2"