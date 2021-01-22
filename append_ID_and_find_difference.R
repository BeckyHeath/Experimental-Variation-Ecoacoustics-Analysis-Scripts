#########################################################
# Script to take Raw AudioSet and Analytical Index Data and
#   1) Append and ID which to group same recordings with
#       which have been compressed to different Levels
#   2) Find the Absolute Difference between the raw and compressed values
#   3) Present Q-Q Plots of the Difference Distributions 
#
# EDIT: Differences were non-normally Distributed so Median and Quartiles found
#       SEE "Median_and_Quartiles_Analytical_and_AudioSet.r" 
#
# Becky Heath Summer 2020 
# r.heath18@imperial.ac.uk
#
#

# Load Libraries and set working directory ####
library("dplyr")
library("car")
library("ggplot2")
library("ggpubr")
library("patchwork")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

##### Append the ID ##########################################################

# Set working directory and load raw dataframe 
audiosets <- read.csv("xx.csv", header = T)

# Set up empty new column 
audiosets$id.no <- NA
audiosets <- audiosets[,c(ncol(audiosets),1:(ncol(audiosets)-1))]

#Append ID to column (this can take hours)
for (i in 1:nrow(audiosets)) {
  row.no <- audiosets[i,]
  req_time <- as.character(row.no$frame.size)
  site <- as.character(row.no$site)
  date <- as.character(row.no$date)
  time <- as.character(row.no$time)
  id.number<- paste(req_time,site,date,time, sep="")
  #print(id.number)
  #print(i)
  audiosets[i,]$id.no <- id.number
} 

#Export to CSV
#write.csv(audiosets, "FEATURES_with_ID_FULL.csv")


##### Find Absolute Differences ##################################

####  THIS ONE FOR ANALYTICAL INDICES

id.audiosets <- read.csv("Dataframes/Data_Analytical_Indices.csv", header = T)

#GET ID NUMBER AS A FACTOR: 
id.audiosets$id.no <- as.factor(id.audiosets$id.no)
id.audiosets <- id.audiosets[complete.cases(id.audiosets),]

#SET UP THE OUTPUT DATAFRAME:
out.file <- id.audiosets[FALSE,]

sort_per_file <- id.audiosets %>% group_split(id.no)


#CREATE A LOOP THAT GOES THROUGH EACH OF THE GENERATED FILES AND COMPARES THE FEATURE TO THAT OF THE RAW
for(i in 1:length(sort_per_file)) {
  one.file <- sort_per_file[[i]]
  #EXTRACT IT: 
  raw.standard <- one.file[(one.file$compression)=="RAW",]
  
  if(nrow(raw.standard) == 0){
    next
  }
  
  for(j in 1:nrow(one.file)){
    
    test.row <- one.file[j,]
  
    #GET META THE ERRORS ARE IN ASSIGNING VARIABLES HERE 
    out.id.no <- as.character(test.row$id.no[1])
    out.frame.size <- as.character(test.row$frame.size[1])
    out.compression <- as.factor(test.row$compression[1])
    out.file.size<- as.character(test.row$file.size[1])
    out.site<- as.character(test.row$site[1])
    
    #DROP NON NUMERICAL COLUMNS 
    drops <- c("id.no","frame.size","file.size", "compression", "site", "date", "time","req.freq")
    raw.standard <- raw.standard[ , !(names(raw.standard) %in% drops)]
    test.row <- test.row[ , !(names(test.row) %in% drops)]
    
    #FIND THE DIFFERENCE 
    difference <- raw.standard - test.row
    x <- data.frame(out.id.no, out.file.size, out.compression, out.frame.size, out.site)
    output.df <- cbind(x, difference)
    out.file = rbind(out.file,output.df)
  } 
}

# Write to CSV
with_dif<-cbind(out.file, total = rowSums(difference))
#write.csv(out.file, "Dataframes/difference_data_Analytical_Indices.csv")


##############  THIS ONE FOR AUDIOSET FINGERPRINT

id.audiosets <- read.csv("Dataframes/Data_AudioSet_Fingerprint.csv", header = T)

#GET ID NUMBER AS A FACTOR: 
id.audiosets$id.no <- as.factor(id.audiosets$id.no)
id.audiosets <- id.audiosets[complete.cases(id.audiosets),]

#SET UP THE OUTPUT DATAFRAME:
out.file <- id.audiosets[FALSE,]

sort_per_file <- id.audiosets %>% group_split(id.no)

# This works but is incredibly long winded
#CREATE A LOOP THAT GOES THROUGH EACH OF THE GENERATED FILES AND COMPARES THE FEATURE TO THAT OF THE RAW
for(i in 1:length(sort_per_file)) {
  one.file <- sort_per_file[[i]]
  #EXTRACT IT: 
  raw.standard <- one.file[(one.file$compression)=="RAW",]
  if(i %% 1000 == 0){
    print(i)
  }
  if(nrow(raw.standard) == 0){
    next
  }
  
  for(j in 1:nrow(one.file)){
    
    test.row <- one.file[j,]
    
    #GET META THE ERRORS ARE IN ASSIGNING VARIABLES HERE 
    out.id.no <- as.character(test.row$id.no[1])
    out.frame.size <- as.character(test.row$frame.size[1])
    out.compression <- as.factor(test.row$compression[1])
    out.file.size<- as.character(test.row$file.size[1])
    out.site<- as.character(test.row$site[1])
    
    #DROP NON NUMERICAL COLUMNS 
    drops <- c("id.no","frame.size","file.size", "compression", "site", "date", "time","req.freq","max.freq")
    raw.standard <- raw.standard[ , !(names(raw.standard) %in% drops)]
    test.row <- test.row[ , !(names(test.row) %in% drops)]
    
    #FIND THE DIFFERENCE 
    difference <- raw.standard - test.row
    total.dif <- rowSums(difference)
    total.abs.dif <- rowSums(abs(difference))
    x <- data.frame(out.id.no, out.file.size, out.compression, out.frame.size, out.site,total.dif, total.abs.dif)
    output.df <- cbind(x, difference)
    out.file = rbind(out.file,output.df)
  } 
}

#Write to CSV
with_dif<-cbind(out.file, total = rowSums(difference))
#write.csv(out.file, "Dataframes/Difference_Data_AudioSet_Fingerprint.csv")












##### Investigate Difference Normality#####



dif.audiosets <- read.csv("Dataframes/Difference_Data_AudioSet_Fingerprint.csv")
dif.analytical <- read.csv("Dataframes/Difference_Data_Analytical_Indices.csv")

# Taking 2.5min as an example: 
dif.AF <- dif.audiosets[(dif.audiosets$frame.size == "2_5min"),]
dif.AI <- dif.analytical[(dif.audiosets$frame.size == "2_5min"),]

# Generate Plots 

# AudioSet
for(i in c("VBR0", "CBR320", "CBR256","CBR128","CBR64", "CBR32","CBR16", "CBR8")){
  dif.AF <- dif.audiosets[(dif.audiosets$compression == i),]
  plot <- ggqqplot(dif.AF, x = "total.dif", title = i)
  var_name <- paste("plt_AF_",i, sep="")
  assign(var_name, plot)
}

top <- plt_AF_VBR0 | plt_AF_CBR320 | plt_AF_CBR256 | plt_AF_CBR128
bottom <- plt_AF_CBR64 | plt_AF_CBR32 | plt_AF_CBR16 | plt_AF_CBR8
top/bottom + plot_annotation(title = "Difference Values QQ Plot AudioSet")

# Analytical Indices 
for(i in c("ACI", "ADI", "Aeev", "Bio", "H","M","NDSI")){
  for(j in c("VBR0", "CBR320", "CBR256","CBR128","CBR64", "CBR32","CBR16", "CBR8")){
    dif.AI <- dif.analytical[(dif.analytical$compression == j),]
    plot <- ggqqplot(dif.AI, x = i, title = j) 
    var_name <- paste("plt_AI_",j, sep="")
    assign(var_name, plot)
  }
  top <- plt_AI_VBR0 | plt_AI_CBR320 | plt_AI_CBR256 | plt_AI_CBR128
  bottom <- plt_AI_CBR64 | plt_AI_CBR32 | plt_AI_CBR16 | plt_AI_CBR8
  plot_title <- paste("Difference Values QQ Plot Analytical ",i,sep="")
  patch_plot <- top/bottom + plot_annotation(title = plot_title)
  out_lab = paste("qq",i, sep="")
  assign(out_lab,patch_plot)
}

# Call Patch Plots Individually
qqACI
qqADI
qqAeev
qqBio
qqH
qqM
qqNDSI
