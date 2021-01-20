##### Graphing Master 
####  Becky Heath
####  Started 13th Feb 2020
####  Aim to make a Master Graphing Script for the Compression Paper

##### LOAD PACKAGES : ####
library(dplyr)
library(plyr)
library(dplyr)
library(ggpubr)
library(stringr)
library(evaluate)
library(ggplot2)
library(hrbrthemes)
library(scales)
library(wesanderson)
library(RColorBrewer)
library(patchwork)
library(ggfortify)
library(corrplot)
library(matrixcalc)
#library(tidyverse)
##### COLOUR SCHEME : ####
#palette <- wes_palette("Darjeeling1", n=5)
palette <- brewer.pal(n = 11, name = "Spectral")
col.1 <- as.character(palette[11])
col.2 <- as.character(palette[10])
col.3 <- as.character(palette[9])
col.4 <- as.character(palette[4])
col.5 <- as.character(palette[3])

fr.20 <- col.1
fr.10 <- col.2
fr.5 <- col.3
fr.2 <- col.4



#fr.20 <- as.character("#E86EB4")
#fr.10 <- as.character("#F9C867")
#fr.5 <- as.character("#43FF70")
#fr.2 <- as.character("#71DAFF")

col.p <- as.character("#00A08A")
col.s <- as.character("#F2AD00")
col.m <- as.character("#FF0000")


##### IMPORT DATA : ####
setwd("C:/Users/becky/Desktop/Research/(in Depth - needs to merge)/Compression Master (In Depth Analysis)/Final_Analysis")
dataset.index <- read.csv("Data_Analytical_Indices.csv")
dataset.audiosets <- read.csv("Data_AudioSet_Fingerprint.csv")
abs.dif.index <- read.csv("Difference_Data_Analytical_Indices.csv")
abs.dif.audiosets <- read.csv("Difference_Data_AudioSet_Fingerprint.csv")
accuracy.index <- read.csv("RF_Accuracy_Analytical_Indices.csv")
accuracy.audiosets <- read.csv("RF_Accuracy_AudioSet.csv")
accuracy.both <- read.csv("RF_Accuracy_both_Aug_test.csv")
median.index <- read.csv("Median_and_Quartiles_Analytical_Indices.csv")
median.audiosets <- read.csv("Median_and_Quartiles_AudioSet_Fingerprint.csv")
#corr.indices <- read.csv("Correlation_Analytical_Indices.csv")
#corr.audiosets <- read.csv("Correlation_AudioSet_Fingerprint.csv")

##### DEFINE FUNCTIONS : ####

reverselog_trans <- function(base = exp(1)) {
  trans <- function(x) -log(x, base)
  inv <- function(x) base^(-x)
  trans_new(paste0("reverselog-", format(base)), trans, inv, 
            log_breaks(base = base), 
            domain = c(1e-100, Inf))
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

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}


### MORE FUNCTIONS THAT MAY BE USEFUL IN PRIOR VERSIONS OF THIS SCRIPT






##### FIGURE 2: ABS_DIf_COMPRESSION_SEVEN_FEATURES_AND_AUDIOSET OLD: ####

data <- median.index
data <- data[data$frame.size =="2_5min",]

data$compression <- factor(data$compression, levels = c("RAW","VBR0", "CBR320", "CBR256", "CBR128", "CBR64", "CBR32", "CBR16", "CBR8"))
sample <- data[data$index == "ACI",]
plot2 <-ggplot(sample, aes(x=compression, y=median)) + 
  geom_errorbar(aes(ymin=lower, ymax=higher), width=.4, size =0.1) +
  labs(y = "Bias as % of Range") +
  ggtitle("ACI") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust=0.01, vjust =-10)) +
  theme(panel.grid.major.x = element_blank()) +
  #theme(plot.title = element_text(hjust=0.01, vjust =-5)) +
  theme(axis.title.x=element_blank()) +
  theme(axis.text.x = element_blank()) +
  theme(axis.title.y=element_blank())+
  theme(legend.position = "none") +
  theme(axis.line = element_line()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  #ylim(-100,100)+
  geom_point(size=0.9)+
  annotate("rect", xmin = 0, xmax = 9.5, ymin = -5, ymax = 5, 
           alpha = .15,fill='green') +
  geom_hline(yintercept = 0, color = "DarkGrey") 
plot2


sample <- data[data$index == "ADI",]
plot3 <-ggplot(sample, aes(x=compression, y=median)) + 
  geom_errorbar(aes(ymin=lower, ymax=higher), width=.4, size =0.1) +
  labs(y = "Bias as % of Range") +
  ggtitle("ADI") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust=0.01, vjust =-5)) +
  theme(axis.title.x=element_blank()) +
  theme(axis.text.x = element_blank()) +
  theme(axis.title.y=element_blank())+
  theme(legend.position = "none") +
  theme(axis.line = element_line()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  #ylim(-100,100)+
  geom_point(size=0.9)+
  annotate("rect", xmin = 0, xmax = 10, ymin = -5, ymax = 5, 
          alpha = .15,fill='green') +
  geom_hline(yintercept = 0, color = "DarkGrey") 

sample <- data[data$index == "Aeev",]
plot4 <-ggplot(sample, aes(x=compression, y=median)) + 
  geom_errorbar(aes(ymin=lower, ymax=higher), width=.4, size =0.1) +
  labs(y = "Bias as % of Range") +
  ggtitle("AEve") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust=0.01, vjust =-5)) +
  theme(axis.title.x=element_blank()) +
  theme(axis.text.x = element_blank()) +
  theme(axis.title.y=element_blank())+
  theme(legend.position = "none") +
  theme(axis.line = element_line()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  #ylim(-100,100)+
  geom_point(size=0.9)+
  annotate("rect", xmin = 0, xmax = 10, ymin = -5, ymax = 5, 
           alpha = .15,fill='green') +
  geom_hline(yintercept = 0, color = "DarkGrey") 

sample <- data[data$index == "Bio",]
plot5 <-ggplot(sample, aes(x=compression, y=median)) + 
  geom_errorbar(aes(ymin=lower, ymax=higher), width=.4, size =0.1) +
  labs(y = "Bias as % of Range") +
  ggtitle("Bio") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust=0.01, vjust =-10)) +
  theme(axis.title.x=element_blank()) +
  theme(axis.text.x = element_blank()) +
  theme(axis.title.y=element_blank())+
  theme(legend.position = "none") +
  theme(axis.line = element_line()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  #ylim(-100,100)+
  geom_point(size=0.9)+
  annotate("rect", xmin = 0, xmax = 10, ymin = -5, ymax = 5, 
           alpha = .15,fill='green') +
  geom_hline(yintercept = 0, color = "DarkGrey") 

sample <- data[data$index == "H",]
plot6 <-ggplot(sample, aes(x=compression, y=median)) + 
  geom_errorbar(aes(ymin=lower, ymax=higher), width=.4, size =0.1) +
  labs(y = "Bias as % of Range") +
  ggtitle("H") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust=0.01, vjust =-5)) +
  theme(axis.title.x=element_blank()) +
  theme(axis.text.x = element_blank()) +
  theme(axis.title.y=element_blank())+
  theme(legend.position = "none") +
  theme(axis.line = element_line()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  #ylim(-100,100)+
  geom_point(size=0.9)+
  annotate("rect", xmin = 0, xmax = 10, ymin = -5, ymax = 5, 
           alpha = .15,fill='green') +
  geom_hline(yintercept = 0, color = "DarkGrey") 

sample <- data[data$index == "M",]
plot7 <-ggplot(sample, aes(x=compression, y=median)) + 
  geom_errorbar(aes(ymin=lower, ymax=higher), width=.4, size =0.1) +
  labs(y = "Bias as % of Range") +
  ggtitle("M") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust=0.01, vjust =-5)) +
  theme(axis.title.x=element_blank()) +
  theme(axis.text.x = element_text(size = 8, angle = 90)) +
  #theme(axis.text.x = element_blank()) +
  theme(axis.title.y=element_blank())+
  theme(legend.position = "none") +
  theme(axis.line = element_line()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  #ylim(-100,100)+
  geom_point(size=0.9)+
  annotate("rect", xmin = 0, xmax = 10, ymin = -5, ymax = 5, 
           alpha = .15,fill='green') +
  geom_hline(yintercept = 0, color = "DarkGrey") 

sample <- data[data$index == "NDSI",]
plot8 <-ggplot(sample, aes(x=compression, y=median)) + 
  geom_errorbar(aes(ymin=lower, ymax=higher), width=.4, size =0.1) +
  labs(y = "Bias as % of Range") +
  ggtitle("NDSI") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust=0.01, vjust =-5)) +
  theme(axis.title.x=element_blank()) +
  theme(axis.text.x = element_text(size = 8, angle = 90)) +
  theme(axis.title.y=element_blank())+
  theme(legend.position = "none") +
  theme(axis.line = element_line()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  #ylim(-100,100)+
  geom_point(size=0.9)+
  annotate("rect", xmin = 0, xmax = 10, ymin = -5, ymax = 5, 
           alpha = .15,fill='green') +
  geom_hline(yintercept = 0, color = "DarkGrey") 
plot8
###### AUDIOSETS 
data <- median.audiosets
data <- data[data$frame.size =="2_5min",]

data$compression <- factor(data$compression, levels = c("RAW","VBR0", "CBR320", "CBR256", "CBR128", "CBR64", "CBR32", "CBR16", "CBR8"))
sample <- data[data$index == "total.dif.R",]
plot1 <-ggplot(sample, aes(x=compression, y=median)) + 
  geom_errorbar(aes(ymin=lower, ymax=higher), width=.4, size =0.1) +
  labs(y = "Bias as % of Range") +
  ggtitle("AudioSet") + 
  theme_minimal() +
  theme(panel.grid.major.x = element_blank()) +
  theme(plot.title = element_text(hjust=0.01, vjust =-5)) +
  theme(axis.title.x=element_blank()) +
  theme(axis.text.x = element_blank()) +
  theme(axis.title.y=element_blank())+
  theme(legend.position = "none") +
  theme(axis.line = element_line()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  #ylim(-100,100)+
  geom_point(size=0.9)+
  annotate("rect", xmin = 0, xmax = 9.5, ymin = -5, ymax = 5, 
           alpha = .15,fill='green') +
  geom_hline(yintercept = 0, color = "DarkGrey") 
plot1

#PORTRAIT:
patch1 <- plot1 + plot2 + plot3 + plot4 
patch2 <- plot5 + plot6 + plot7 + plot8
patch1/patch2




##### FIGURE 3 COME BACK TO THIS - THE FRAME SIZE FIGURE : ##### 

### Will need to think about this 
### Is it okay to present variance of the AudioSet Fingerprint but absolute values of the Analytical Indices
### Will need to ask


##### Correlation Plots #####
corr.index <- na.omit(dataset.index)
corr.index <- corr.index[,(9:16)]
M.index <- cor(corr.index)

corrplot(M.index, method = "color", "upper")
vals.index <- t(M.index)[lower.tri(t(M.index))]
vals.index <- abs(vals.index)
corr.audioset <- na.omit(dataset.audiosets)
corr.audioset <- corr.audioset[,(9:136)]
M.audioset <- cor(corr.audioset)
corrplot(M.audioset, method = "color", type = "upper", tl.col="white")
vals.audioset <- t(M.audioset)[lower.tri(t(M.audioset))]
vals.audioset <- abs(vals.audioset)

##### FIGURE 4 PCAs (Compressed V. RAW; Analytical Indices vs. AudioSet #####

data1 <- dataset.audiosets

#Set-up Datafram1s
data_raw <- data1[data1$compression == "RAW", ]  
data_8 <- data1[data1$compression == "CBR8", ]  
data25raw <- data_raw[data_raw$frame.size == "5min",]
data258 <- data_8[data_8$frame.size == "5min",]

#Set-up PCAs
data25raw <-data25raw[ , -which(names(data25raw) %in% c("frame.size","compression", "req_freq", "Date", "time", "file.size", "id.no"))]
data25raw_feats <- data25raw[ , -which(names(data25raw) %in% c("site"))]

data258 <-data258[ , -which(names(data258) %in% c("frame.size","compression", "req_freq", "Date", "time", "file.size", "id.no"))]
data258_feats <- data258[ , -which(names(data258) %in% c("site"))]


theme_set(theme_minimal())
plot_1 <- autoplot(prcomp(data25raw_feats), data = data25raw, colour = 'site', size = 0.001, frame = TRUE, frame.type = "norm") +
  scale_color_manual(values=c(col.1, col.2, col.3)) +
  theme(legend.position = c(0.95, 0.05)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "AudioSet Raw")+
  scale_fill_manual(values= c(col.1, col.2, col.3)) + 
  theme(axis.line= element_line())
plot_1


plot_2 <- autoplot(prcomp(data258_feats), data = data258, colour = 'site', size = 0.001,frame = TRUE, frame.type = "norm") +
  scale_color_manual(values=c(col.1, col.2, col.3)) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "AudioSet CBR8")+
  scale_fill_manual(values= c(col.1, col.2, col.3))+ 
  theme(axis.line= element_line())
plot_2


combi <-plot_1/plot_2

data1 <- dataset.index
data1 <- data1[complete.cases(data1),]

#Set-up Datafram1s
data_raw <- data1[data1$compression == "RAW", ]  
data_8 <- data1[data1$compression == "CBR8", ]  
data25raw <- data_raw[data_raw$frame.size == "2_5min",]
data258 <- data_8[data_8$frame.size == "2_5min",]

#Set-up PCAs
data25raw <-data25raw[ , -which(names(data25raw) %in% c("frame.size","compression", "req.freq", "date", "time", "file.size", "id.no","max.freq"))]
data25raw_feats <- data25raw[ , -which(names(data25raw) %in% c("site"))]

data258 <-data258[ , -which(names(data258) %in% c("frame.size","compression", "req.freq", "date", "time", "file.size", "id.no","max.freq"))]
data258_feats <- data258[ , -which(names(data258) %in% c("site"))]


theme_set(theme_minimal())
plot_3 <- autoplot(prcomp(data25raw_feats), data = data25raw, colour = 'site', size = 0.001, frame = TRUE, frame.type = "norm") +
  scale_color_manual(values=c(col.1, col.2, col.3)) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Analytical Raw")+
  scale_fill_manual(values= c(col.1, col.2, col.3)) + 
  theme(axis.line= element_line())
plot_3

plot_4 <- autoplot(prcomp(data258_feats), data = data258, colour = 'site', size = 0.001,frame = TRUE, frame.type = "norm") +
  scale_color_manual(values=c(col.1, col.2, col.3)) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Analytical CBR8")+
  scale_fill_manual(values= c(col.1, col.2, col.3))+ 
  theme(axis.line= element_line())
plot_4

combi2 <-plot_3/plot_4

combi | combi2







##### FIGURE 5 Accuracy/File Size/ Chunking #####

#data.AF <- accuracy.audiosets 
#data.AN <- accuracy.index
#combi.data <- rbind(data.AF, data.AN)

combi.data <- accuracy.both



#PART ONE compression V. file size
data <- combi.data
#data$compression <- factor(data$compression, levels = c("RAW","VBR0", "CBR320", "CBR256", "CBR128", "CBR64", "CBR32", "CBR16", "CBR8"))
#file.size <- data$file.size
#compression <- data$compression
#frame.size <- data$frame.size
#TOD <- data$TOD.chunk
#chunks <- data$chunks
#index <- data$index.type


#SORT OUT INDIVIDUAL DATA SETS
data2 <- data[data$frame.size == "20min", ]
data_none_2 <- data2[data2$chunks == "None", ]  

plot4 <- ggplot(data=data_none_2, aes(x=file.size , y= accuracy, col = index.type, fill =index.type, shape = index.type)) + 
  geom_smooth(method="lm" , formula= (y ~ x), se=TRUE, level = 0.95) +
  geom_point()+
  scale_color_manual(values=c(col.1, col.3)) +
  scale_fill_manual(values= c(col.1, col.3)) +
  scale_shape_manual(values=c(1, 3))+
  ylim(0.65,1) +
  theme_classic() +
  theme(legend.position = "none")+
  scale_x_continuous(trans=reverselog_trans(10)) +
  labs(y = "Accuracy %", x = "Compression")
plot4
data_none_2 <- data2[data2$chunks == "4", ]  

plot5 <- ggplot(data=data_none_2, aes(x=file.size , y= accuracy, col = index.type, fill = index.type, shape = index.type)) + 
  geom_smooth(method="lm" , formula= (y ~ x), se=TRUE, level = 0.95) +
  geom_point()+
  scale_color_manual(values=c(col.1, col.3)) +
  scale_fill_manual(values= c(col.1, col.3)) +
  scale_shape_manual(values=c(1, 3))+
  ylim(0.65,1) +
  theme_classic() +
  theme(legend.position = "none")+
  scale_x_continuous(trans=reverselog_trans(10)) +
  labs(y = "", x = "Compression") 

data_none_2 <- data2[data2$chunks == "12", ]  

plot6 <- ggplot(data=data_none_2, aes(x=file.size , y= accuracy , col = index.type, fill =index.type, shape = index.type)) + 
  geom_smooth(method="lm" , formula= (y ~ x), se=TRUE, level = 0.95) +
  geom_point()+
  scale_color_manual(values=c(col.1, col.3)) +
  scale_fill_manual(values= c(col.1, col.3)) +
  scale_shape_manual(values=c(1, 3))+
  ylim(0.65,1) +
  theme_classic() +
  #theme(legend.position = "none")+
  theme(legend.position = c(0.95, 0.05))+
  scale_x_continuous(trans=reverselog_trans(10)) +
  labs(y = "", x = "Compression")

plot6
accuracy.plot <- plot4 | plot5 | plot6
#precision.plot <- plot4 | plot5 | plot6
#recall.plot <- plot4 | plot5 | plot6
accuracy.plot/precision.plot/recall.plot



##### FIGURE 6 and 7: TIME OF DAY SECTIONING: #####

data <- dataset.audiosets

data <- data[data$compression == "RAW", ]  
data<- data[data$frame.size == "10min",] 

data <-
  data %>%
  mutate(time = str_pad(time, 4, side = 'left', pad = '0'))

data$time <- sapply(data$time, time_chunks_4)
data$time <- unlist(data$time)

data[,1] <- toupper(data[,1])


#Split Dataframe again depending on Time:
data_time <- data %>% group_split(time)

#MAKE SURE YOU CHECK THESE MANUALLY!
data1_dawn <- data_time[[1]]
data2_dusk <- data_time[[2]]
data_mid <- data_time[[3]]
data4_night <- data_time[[4]]

#Do Analysis:
for(i in 1:length(data_time)) {
  data <- data_time[[i]]
  data <-data[ , -which(names(data) %in% c("frame.size","compression", "req_freq", "Date", "time", "id.no", "file.size"))]
  data_feats <- data[ , -which(names(data) %in% c("site"))]
  data_feats <- data_feats[complete.cases(data_feats), ]
  data <- data[complete.cases(data), ]
  theme_set(theme_minimal())
  if(i == 1){
    title = "Dawn"
  }
  if(i == 2){
    title = "Dusk"
  }
  if(i == 3){
    title = "Midday"
  }
  if(i == 4){
    title = "Midnight"
  }
  plot_1 <- autoplot(prcomp(data_feats), data = data, colour = 'site', size = 0.001, frame = TRUE, frame.type = "norm") +
    scale_color_manual(values=c(col.1, col.2, col.3)) +
    theme(legend.position = c(0.95, 0.05)) +
    labs(title = title)+
    scale_fill_manual(values= c(col.1, col.2, col.3))+ 
    theme(axis.line= element_line())+
    theme(legend.position = "none")
  nam <- paste("plot_L_", i, sep = "")
  assign(nam, plot_1)
    }

combipatchRAW <- plot_L_1 | plot_L_3 | plot_L_2 | plot_L_4
combipatchRAW
T_S <- combipatchRAW/ combipatchCBR8
whole <- raw | cbr8

whole/T_S

################################################################### EXTRA: ANALYTICAL INDICES

data <- dataset.index

data <- data[data$compression == "RAW", ]  
data<- data[data$frame.size == "5min",] 

data <-
  data %>%
  mutate(time = str_pad(time, 4, side = 'left', pad = '0'))

data$time <- sapply(data$time, time_chunks_4)
data$time <- unlist(data$time)

data[,1] <- toupper(data[,1])


#Split Dataframe again depending on Time:
data_time <- data %>% group_split(time)

#MAKE SURE YOU CHECK THESE MANUALLY!
data1_dawn <- data_time[[1]]
data2_dusk <- data_time[[2]]
data_mid <- data_time[[3]]
data4_night <- data_time[[4]]

#Do Analysis:
for(i in 1:length(data_time)) {
  data <- data_time[[i]]
  data <-data[ , -which(names(data) %in% c("frame.size","compression", "req.freq", "date", "time", "id.no", "file.size","max.freq"))]
  data_feats <- data[ , -which(names(data) %in% c("site"))]
  data_feats <- data_feats[complete.cases(data_feats), ]
  data <- data[complete.cases(data), ]
  theme_set(theme_minimal())
  if(i == 1){
    title = "Dawn"
  }
  if(i == 2){
    title = "Dusk"
  }
  if(i == 3){
    title = "Midday"
  }
  if(i == 4){
    title = "Midnight"
  }
  plot_1 <- autoplot(prcomp(data_feats), data = data, colour = 'site', size = 0.001, frame = TRUE, frame.type = "norm") +
    scale_color_manual(values=c(col.1, col.5, col.3)) +
    theme(legend.position = c(0.95, 0.05)) +
    labs(title = title)+
    scale_fill_manual(values= c(col.1, col.5, col.3))
  nam <- paste("plot_L_", i, sep = "")
  assign(nam, plot_1)
}

combipatch1 <- plot_L_1 | plot_L_3 | plot_L_2 | plot_L_4
combipatch1









##### Experimental #####

out <- t(corr.indices)[lower.tri(t(corr.indices))]
