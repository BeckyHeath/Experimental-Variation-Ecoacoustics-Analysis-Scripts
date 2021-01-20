######################################################################
# Unsure if this is Redundant
# 

library("randomForest")
library("tidyr")
library(plyr)
library(MASS)
library(dplyr)
library(ggplot2)
library(ggfortify)
library(stringr)


#### LOAD dataFRAME AND FORMAT: ########################################################################################################
setwd("C:/Users/becky/Desktop/Research/(in Depth - needs to merge)/Compression Master (In Depth Analysis)/Final_Analysis")
data <- read.csv("Data_AudioSet_Fingerprint.csv")

data$site <- as.factor(data$site)

data <-
  data %>%
  mutate(time = str_pad(time, 4, side = 'left', pad = '0'))


#### SET FUNCTIONS: ####################################################################################################################

time_chunks_12 <- function(times){
  #print(times)
  x <- substr(times,1,2) #GET FIRST TWO CHARACTERS 
  print(x)
  if (x %in% c("22","23")) {
    times <- "Midnight1"
  } else if (x %in% c("00","01")) {
    times <- "Midnight2"
  } else if (x %in% c("02","03")) {
    times <- "Midnight3"
  } else if (x %in% c("04","05")) {
    times <- "Early AM1"
  } else if (x %in% c("06","07")) {
    times <- "Early AM2"
  } else if (x %in% c("08","09")) {
    times <- "Early AM3"
  } else if (x %in% c("10","11")) {
    times <- "Midday1"
  } else if (x %in% c("12","13")) {
    times <- "Midday2"
  } else if (x %in% c("14","15")) {
    times <- "Midday3"
  } else if (x %in% c("16","17")) {
    times <- "Dusk1"
  } else if (x %in% c("18","19")) {
    times <- "Dusk2"
  } else if (x %in% c("20","21")) {
    times <- "Dusk3"
  }
  
  
  #print(times)  
  #return(times)  
  
}

time_chunks_8 <- function(times){
  #print(times)
  x <- substr(times,1,2) #GET FIRST TWO CHARACTERS 
  print(x)
  if (x %in% c("23","00","01")) {
    times <- "Midnight"
  } else if (x %in% c("02","03","04")) {
    times <- "Early AM"
  } else if (x %in% c("05","06","07")) {
    times <- "Dawn"   
  } else if (x %in% c("08","09","10")) {
    times <- "Morning"
  } else if (x %in% c("11","12","13")) {
    times <- "Midday"
  } else if (x %in% c("14","15","16")) {
    times <- "Afternoon"
  } else if (x %in% c("17","18","19")) {
    times <- "Dusk"
  } else if (x %in% c("20","21","22")) {
    times <- "Evening"
  }
  
  #print(times)  
  #return(times)  
  
}

time_chunks_4 <- function(times){
  x <- substr(times,1,2) #GET FIRST TWO CHARACTERS 
  print(x)
  if (x %in% c("22","23","00","01", "02", "03")) {
    times <- "Quarter_Day_Midnight"
  } else if (x %in% c("04","05","06","07","08","09")) {
    times <- "Quarter_Day_Dawn"   
  } else if (x %in% c("10","11","12","13","14","15")) {
    times <- "Quarter_Day_Midday"
  } else if (x %in% c("16","17","18","19","20","21")) {
    times <- "Quarter_Day_Dusk"
  }
}

do_analysis <- function(data_hold, chunks) {
  
  ###################GENERATE METAdata:
  #Get Req_Time
  frame.size = as.character(data_hold$frame.size[1])
  #Get Compression
  compression = as.character(data_hold$compression[1])
  #Get time period Metadata
  time_period = as.character(data_hold$time[1])
  #Get time period Metadata
  file.size = as.character(data_hold$file.size[1])
  
  level.out = paste(index.type,frame.size, compression, chunks, time_period, sep= " ")

  ### TRAIN AND TEST THE MODEL, THEN GET ACCURACY
  test_data <- data_hold[!data_hold$date == '2019_2_28',]
  model_data <- data_hold[data_hold$date == '2019_2_28',]
  
  model_data <- subset(model_data, select=-c(id.no,file.size, frame.size, compression, date, time, req.freq))
  test_data <- subset(test_data, select=-c(id.no, file.size, frame.size, compression, date, time,req.freq))
  
  rf <- randomForest(site ~ ., data = model_data, mtry = 3, ntree=2000, importance = TRUE)
  
  rf.predict <- predict(rf, newdata = test_data)
  conf.rf=table(test_data$site, rf.predict)
  accuracy = sum(diag(conf.rf))/sum(conf.rf)
  precision = (conf.rf[1,1]/sum(conf.rf[,1]) + conf.rf[2,2]/sum(conf.rf[,2]) + conf.rf[3,3]/sum(conf.rf[,3]))/3
  recall = (conf.rf[1,1]/sum(conf.rf[1,]) + conf.rf[2,2]/sum(conf.rf[2,]) + conf.rf[3,3]/sum(conf.rf[3,]))/3
  x <- data.frame(compression, file.size, frame.size , accuracy, precision, recall, time_period, chunks, index.type)
  conf.filename = paste("Confusion_Matricies/",index.type,frame.size,compression,chunks,time_period,".csv" ,sep ="_")  
  tocsv <- cbind(conf.rf[,1],conf.rf[,2],conf.rf[,3])
  write.table(tocsv, file = conf.filename, sep = ",", append = TRUE, quote = FALSE,col.names = FALSE, row.names = TRUE)
  write.table(x, file = "RF_Accuracy_both_Aug_test.csv", sep = ",", append = TRUE, quote = FALSE,
              col.names = FALSE, row.names = FALSE)
  
}


#### SPLIT dataFRAMES AND RUN ANALYSIS:  ###############################################################################################


#Split dataframe depending on Compression:
data_comp <- data %>% group_split(compression)

index.type <- as.character("AudioSet")

#Do Analysis:
for(i in 1:length(data_comp)) {
  data_compx <- data_comp[[i]]
  chunks = as.character("None")
  
  #Split dataframe depending on FRAME SIZE: 
  data_frame <- data_compx %>% group_split(frame.size)
  
  #Do Analysis:
  for(j in 1:length(data_frame)) {
    data_framex <- data_frame[[j]]
    do_analysis(data_framex, chunks)
  }
  
}

data4 <- data
data8 <- data
data12<- data


#################################################################   4
#Put Time into CHUNKs
data4$time <- as.character(sapply(data$time, time_chunks_4))
data4$time <- unlist(data4$time)
chunks <- as.character("4")

#Split dataframe again depending on Req_Time:
data_reqtime <- data4 %>% group_split(frame.size)

#Do Analysis:
for(i in 1:length(data_reqtime)) {
  data_reqtimex <- data_reqtime[[i]]
  data_comp <- data_reqtimex %>% group_split(compression)
  
  for(j in 1:length(data_comp)) {
    data_compx <- data_comp[[j]]
    data_compx$time <- as.factor(data_compx$time)
    data_time <- data_compx %>% group_split(time)
    
    for(j in 1:length(data_time)) {
      data_timex <- data_time[[j]]
      do_analysis(data_timex, chunks)
    }
  }  
}

#################################################################   8
#Put Time into CHUNKs
data8$time <- sapply(data$time, time_chunks_8)
data8$time <- unlist(data8$time)
chunks <- as.character("8")

#Split dataframe again depending on Req_Time:
data_reqtime <- data8 %>% group_split(frame.size)

#Do Analysis:
for(i in 1:length(data_reqtime)) {
  data_reqtimex <- data_reqtime[[i]]
  data_comp <- data_reqtimex %>% group_split(compression)
  
  for(j in 1:length(data_comp)) {
    data_compx <- data_comp[[j]]
    
    data_time <- data_compx %>% group_split(time)
    
    for(j in 1:length(data_time)) {
      data_timex <- data_time[[j]]
      do_analysis(data_timex, chunks)
    }
  }  
}

#################################################################   12
#Put Time into CHUNKs
data12$time <- sapply(data$time, time_chunks_12)
data12$time <- unlist(data12$time)
chunks <- as.character("12")

#Split dataframe again depending on Req_Time:
data_reqtime <- data12 %>% group_split(frame.size)

#Do Analysis:
for(i in 1:length(data_reqtime)) {
  data_reqtimex <- data_reqtime[[i]]
  data_comp <- data_reqtimex %>% group_split(compression)
  
  for(j in 1:length(data_comp)) {
    data_compx <- data_comp[[j]]
    
    data_time <- data_compx %>% group_split(time)
    
    for(j in 1:length(data_time)) {
      data_timex <- data_time[[j]]
      do_analysis(data_timex, chunks)
    }
  }  
}



#### SPLIT dataFRAMES AND RUN Analytical_Indices ANALYSIS:  ###############################################################################################

data <- read.csv("Data_Analytical_Indices.csv")
data <- data[complete.cases(data),]
data$site <- as.factor(data$site)

index.type = as.character("Analytical_Indices")
data <-
  data %>%
  mutate(time = str_pad(time, 4, side = 'left', pad = '0'))

#Split dataframe depending on Compression:
data_comp <- data %>% group_split(compression)


#Do Analysis:
for(i in 1:length(data_comp)) {
  data_compx <- data_comp[[i]]
  chunks = as.character("None")
  
  #Split dataframe depending on FRAME SIZE: 
  data_frame <- data_compx %>% group_split(frame.size)
  
  #Do Analysis:
  for(j in 1:length(data_frame)) {
    data_framex <- data_frame[[j]]
    do_analysis(data_framex, chunks)
  }
  
}

data4 <- data
data8 <- data
data12<- data


#################################################################   4
#Put Time into CHUNKs
data4$time <- sapply(data$time, time_chunks_4)
data4$time <- unlist(data4$time)
chunks <- as.character("4")

#Split dataframe again depending on Req_Time:
data_reqtime <- data4 %>% group_split(frame.size)

#Do Analysis:
for(i in 1:length(data_reqtime)) {
  data_reqtimex <- data_reqtime[[i]]
  data_comp <- data_reqtimex %>% group_split(compression)
  
  for(j in 1:length(data_comp)) {
    data_compx <- data_comp[[j]]
    
    data_time <- data_compx %>% group_split(time)
    
    for(j in 1:length(data_time)) {
      data_timex <- data_time[[j]]
      do_analysis(data_timex, chunks)
    }
  }  
}

#################################################################   8
#Put Time into CHUNKs
data8$time <- sapply(data$time, time_chunks_8)
data8$time <- unlist(data8$time)
chunks <- as.character("8")

#Split dataframe again depending on Req_Time:
data_reqtime <- data8 %>% group_split(frame.size)

#Do Analysis:
for(i in 1:length(data_reqtime)) {
  data_reqtimex <- data_reqtime[[i]]
  data_comp <- data_reqtimex %>% group_split(compression)
  
  for(j in 1:length(data_comp)) {
    data_compx <- data_comp[[j]]
    
    data_time <- data_compx %>% group_split(time)
    
    for(j in 1:length(data_time)) {
      data_timex <- data_time[[j]]
      do_analysis(data_timex, chunks)
    }
  }  
}

#################################################################   12
#Put Time into CHUNKs
data12$time <- sapply(data$time, time_chunks_12)
data12$time <- unlist(data12$time)
chunks <- as.character("12")

#Split dataframe again depending on Req_Time:
data_reqtime <- data12 %>% group_split(frame.size)

#Do Analysis:
for(i in 1:length(data_reqtime)) {
  data_reqtimex <- data_reqtime[[i]]
  data_comp <- data_reqtimex %>% group_split(compression)
  
  for(j in 1:length(data_comp)) {
    data_compx <- data_comp[[j]]
    
    data_time <- data_compx %>% group_split(time)
    
    for(j in 1:length(data_time)) {
      data_timex <- data_time[[j]]
      do_analysis(data_timex, chunks)
    }
  }  
}


