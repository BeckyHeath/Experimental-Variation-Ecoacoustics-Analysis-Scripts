########################################################################### .
# Generate Confusion Matrixes for Each test case (save all in "Confusion 
# Matrixes") folder. Also Generate DF showing Accuracy, Precision and Recall
# of each test case. 
#
# This script also splits the data into TOD for Temporal Splitting Analysis
#
# Becky Heath 
# r.heath18@imperial.ac.uk
#
# Autumn/Winter 2020/2021
#
#

#### Load Packages and Set Working Directory ####
library("randomForest")
library("tidyr")
library(plyr)
library(MASS)
library(dplyr)
library(ggplot2)
library(ggfortify)
library(stringr)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#### Load dataframe and Format Time: ####
data <- read.csv("Dataframes/Data_AudioSet_Fingerprint.csv")

data$site <- as.factor(data$site)

data <-
  data %>%
  mutate(time = str_pad(time, 4, side = 'left', pad = '0'))


#### Define Functions #####
#
# These functions split the data depending of TOD into different 
# size "chunks" 
# 
# This also contains the "do analysis" function which runs the 
# random forest and saves the output CMs and accuracy metrics


# TODO Make sure these are copied into the other scripts
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
    times <- "Dawn1"
  } else if (x %in% c("06","07")) {
    times <- "Dawn2"
  } else if (x %in% c("08","09")) {
    times <- "Dawn3"
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
  if (x %in% c("22","23","00")) {
    times <- "Midnight1"
  } else if (x %in% c("01","02","03")) {
    times <- "Midnight2"
  } else if (x %in% c("04","05","06")) {
    times <- "Dawn1"   
  } else if (x %in% c("07","08","09")) {
    times <- "Dawn2"
  } else if (x %in% c("10","11","12")) {
    times <- "Midday1"
  } else if (x %in% c("13","14","15")) {
    times <- "Midday2"
  } else if (x %in% c("16","17","18")) {
    times <- "Dusk1"
  } else if (x %in% c("19","20","21")) {
    times <- "Dusk2"
  }
  
  #print(times)  
  #return(times)  
  
}

time_chunks_4 <- function(times){
  x <- substr(times,1,2) #GET FIRST TWO CHARACTERS 
  print(x)
  if (x %in% c("22","23","00","01", "02", "03")) {
    times <- "Midnight"
  } else if (x %in% c("04","05","06","07","08","09")) {
    times <- "Dawn"   
  } else if (x %in% c("10","11","12","13","14","15")) {
    times <- "Midday"
  } else if (x %in% c("16","17","18","19","20","21")) {
    times <- "Dusk"
  }
}

do_analysis <- function(data_hold, chunks) {
  
  # Generate metadata:
  frame.size = as.character(data_hold$frame.size[1])
  compression = as.character(data_hold$compression[1])
  time_period = as.character(data_hold$time[1])
  file.size = as.character(data_hold$file.size[1])
  
  level.out = paste(index.type,frame.size, compression, chunks, time_period, sep= " ")

  # Subset Data and Train the Model
  test_data <- data_hold[!data_hold$date == '2019_2_28',]
  model_data <- data_hold[data_hold$date == '2019_2_28',]
  
  model_data <- subset(model_data, select=-c(id.no,file.size, frame.size, compression, date, time, req.freq))
  test_data <- subset(test_data, select=-c(id.no, file.size, frame.size, compression, date, time,req.freq))
  
  rf <- randomForest(site ~ ., data = model_data, mtry = 3, ntree=2000, importance = TRUE)
  
  # Test the Model and Find Accuracy, Precision and Recall
  rf.predict <- predict(rf, newdata = test_data)
  conf.rf=table(test_data$site, rf.predict)
  accuracy = sum(diag(conf.rf))/sum(conf.rf)
  precision = (conf.rf[1,1]/sum(conf.rf[,1]) + conf.rf[2,2]/sum(conf.rf[,2]) + conf.rf[3,3]/sum(conf.rf[,3]))/3
  recall = (conf.rf[1,1]/sum(conf.rf[1,]) + conf.rf[2,2]/sum(conf.rf[2,]) + conf.rf[3,3]/sum(conf.rf[3,]))/3
  
  # Organise data to output to csv 
  x <- data.frame(compression, file.size, frame.size , accuracy, precision, recall, time_period, chunks, index.type)
  conf.filename = paste("Confusion_Matricies/",index.type,frame.size,compression,chunks,time_period,".csv" ,sep ="_")  
  tocsv <- cbind(conf.rf[,1],conf.rf[,2],conf.rf[,3])
  
  # Write Tables
  write.table(tocsv, file = conf.filename, sep = ",", append = TRUE, quote = FALSE,col.names = FALSE, row.names = TRUE)
  write.table(x, file = "Dataframes/RF_Accuracy_Both.csv", sep = ",", append = TRUE, quote = FALSE,
              col.names = FALSE, row.names = FALSE)

}

#### Split Dataframes and Run Analysis (AudioSet)  ####


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



#### Split Datframes and Run Analysis (Analytical Indices)  ###############################################################################################

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


