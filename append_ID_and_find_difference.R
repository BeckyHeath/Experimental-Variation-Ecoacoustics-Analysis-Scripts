#########################################################
# Script to take Raw AudioSet and Analytical Index Data and
#   1) Append and ID which to group same recordings with
#       which have been compressed to different Levels
#   2) Find the Difference between the raw and compressed values
#
# Becky Heath Summer 2020 
# r.heath18@imperial.ac.uk

# Load Libraries and set working directory ####
library("dplyr")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

##### Append the ID ##########################################################

# Set working directory and load raw dataframe 
audiosets <- read.csv("abs_dif_by_avg_SIX_FEATURES_fixed.csv", header = T)

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


##### Compare all Columns with Same ID Number ##################################

####  THIS ONE FOR ANALYTICAL INDICES

id.audiosets <- read.csv("Data_Analytical_Indices.csv", header = T)

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
#write.csv(out.file, "difference_data_Analytical_Indices.csv")


##############  THIS ONE FOR AUDIOSET FINGERPRINT

id.audiosets <- read.csv("Data_AudioSet_Fingerprint.csv", header = T)

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
  print(i)
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
    drops <- c("id.no","frame.size","file.size", "compression", "site", "Date", "time","req_freq")
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
#write.csv(out.file, "Difference_Data_AudioSet_Fingerprint.csv")











