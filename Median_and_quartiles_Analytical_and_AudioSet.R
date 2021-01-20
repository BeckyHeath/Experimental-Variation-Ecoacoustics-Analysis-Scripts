library(tidyr)


##### IMPORT DATA : ####
setwd("C:/Users/becky/Desktop/Research/(in Depth - needs to merge)/Compression Master (In Depth Analysis)/Final_Analysis")
dataset.index <- read.csv("Data_Analytical_Indices.csv")
dataset.audiosets <- read.csv("Data_AudioSet_Fingerprint.csv")
abs.dif.index <- read.csv("Difference_Data_Analytical_Indices.csv")
abs.dif.audiosets <- read.csv("Difference_Data_AudioSet_Fingerprint.csv")


###### FOR FEATURES #####

# Convert data to as a % of range 
# Find range of each column
full.data.og <- dataset.index
dif.data.og <- abs.dif.index

#FOR IF YOU'RE JUST DOING RANGE OF RAW
full.data.og$compression <- as.factor(full.data.og$compression)


#Now find median and upper and lower quartile
#Median
#as.numeric(quantile(dif.data$ACI, na.rm=TRUE)[3])

out.file <- data.frame(index=character(),
                       compression=character(), 
                       frame.size=character(),
                       median=numeric(),
                       higher=numeric(),
                       lower=numeric(),
                       stringsAsFactors=FALSE) 
i = 0
j = 0
k = 0 

for(k in c("20min","10min","5min","2_5min")){
  full.data <- full.data.og[full.data.og$frame.size== k,]
  full.data <- full.data[full.data$compression == "RAW",]
  dif.data <- dif.data.og[dif.data.og$frame.size== k,]
  ACI.range <- as.numeric(range(full.data$Aci, na.rm = TRUE)[2] - range(full.data$Aci, na.rm = TRUE)[1])
  ADI.range <- as.numeric(range(full.data$ADI, na.rm = TRUE)[2] - range(full.data$ADI, na.rm = TRUE)[1])
  Aeev.range <- as.numeric(range(full.data$Aeev, na.rm = TRUE)[2] - range(full.data$Aeev, na.rm = TRUE)[1])
  Bio.range <- as.numeric(range(full.data$Bio, na.rm = TRUE)[2] - range(full.data$Bio, na.rm = TRUE)[1])
  H.range <- as.numeric(range(full.data$H, na.rm = TRUE)[2] - range(full.data$H, na.rm = TRUE)[1])
  M.range <- as.numeric(range(full.data$M, na.rm = TRUE)[2] - range(full.data$M, na.rm = TRUE)[1])
  NDSI.range <- as.numeric(range(full.data$NDSI, na.rm = TRUE)[2] - range(full.data$NDSI, na.rm = TRUE)[1])
  #Find value as % of range:
  dif.data$ACI <- as.numeric(dif.data$Aci)/ACI.range *100
  dif.data$ADI <- as.numeric(dif.data$ADI)/ADI.range *100
  dif.data$Aeev <- as.numeric(dif.data$Aeev)/Aeev.range *100
  dif.data$Bio <- as.numeric(dif.data$Bio)/Bio.range *100
  dif.data$H <- as.numeric(dif.data$H)/H.range *100 
  dif.data$M <- as.numeric(dif.data$M)/M.range *100
  dif.data$NDSI <- as.numeric(dif.data$NDSI)/NDSI.range *100
  for(i in c("Aci", "ADI", "Aeev", "Bio","H","M", "NDSI", "max.freq")){
    index <- dif.data[,c("id.no", i, "compression")]
    wide = index %>%
      spread(compression, i)
    assign(i,wide)
    for(j in c( "CBR320","CBR256", "VBR0", "CBR128", "CBR64", "CBR32", "CBR16", "CBR16","CBR8","RAW")){
      test.comp <- wide[,j]
      out.compression<-as.character(j)
      out.index <- as.character(i)
      out.frame.size <- as.character(k)
      out.median <- as.numeric(quantile(test.comp,0.5, na.rm=TRUE))
      out.lower <- as.numeric(quantile(test.comp,0.25, na.rm=TRUE))
      out.higher <- as.numeric(quantile(test.comp,0.75, na.rm=TRUE))
      out.row <- data.frame(index = out.index, compression= out.compression, frame.size = out.frame.size,
                            median = out.median, lower = out.lower, higher = out.higher)
      out.file <- rbind(out.file, out.row)
      print(paste0(i," : ",j))
    }
  }
}

out.file$compression = as.factor(ifelse(is.na(out.file$compression), "RAW", out.file$compression))
write.csv(out.file, "Median_and_Quartiles_Analytical_Indices.csv")


###### FOR AUDIOSETS ######
full.data.og <- dataset.audiosets
dif.data.og <- abs.dif.audiosets

### GET IT AS A % OF RANGE 
j=0
k=0
data.out <- dif.data.og[0,]
for(k in c("20min","10min","5min","2_5min")){
  full.data <- full.data.og[full.data.og$frame.size== k,]
  full.data <- full.data[full.data$compression == "RAW",]
  dif.data <- dif.data.og[dif.data.og$frame.size== k,]
  data.grp <- dif.data
  for(j in seq(1:128)){
    col.name <- paste0("feat",j)
    full.col <- full.data[,col.name]
    col.range <- as.numeric(range(full.col, na.rm = TRUE)[2] - range(full.col, na.rm = TRUE)[1])
    #print(col.range)
    dif.col <- data.grp[,col.name]
    dif.col <- (dif.col/col.range)*100
    data.grp[,col.name] <- dif.col
    print(j)
    }
  data.out <- rbind(data.out,data.grp)  
  
}
### data.out is now difference as a % of range for each individual index!
data.out <- data.frame(data.out)
data.out[is.na(data.out)] <- 0

###### TO GET JUST ONE VALUE PER FILE: 
data.out$total.dif.R <- rowSums(data.out[8:135])/128
data.out$total.abs.R <- rowSums(abs(data.out[8:135]))/128

dif.stats <- data.out[,-c(8:135)]


out.file <- data.frame(index=character(),
                       compression=character(), 
                       frame.size=character(),
                       median=numeric(),
                       higher=numeric(),
                       lower=numeric(),
                       stringsAsFactors=FALSE) 



i = 0
j = 0
k = 0 
for(k in c("20min","10min","5min","2_5min")){
  dif.stats.x <- dif.stats[dif.stats$frame.size== k,]
  for(i in c("total.dif","total.abs.dif","total.dif.R","total.abs.R")){
    index <- dif.stats.x[,c("id.no", i, "compression")]
    wide = index %>%
      spread(compression, i)
    assign(i,wide)
    for(j in c( "CBR320","CBR256", "VBR0", "CBR128", "CBR64", "CBR32", "CBR16", "CBR16","CBR8","RAW")){
      test.comp <- wide[,j]
      out.compression<-as.character(j)
      out.index <- as.character(i)
      out.frame.size <- as.character(k)
      out.median <- as.numeric(quantile(test.comp,0.5, na.rm=TRUE))
      out.lower <- as.numeric(quantile(test.comp,0.25, na.rm=TRUE))
      out.higher <- as.numeric(quantile(test.comp,0.75, na.rm=TRUE))
      out.row <- data.frame(index = out.index, compression= out.compression, frame.size = out.frame.size,
                            median = out.median, lower = out.lower, higher = out.higher)
      out.file <- rbind(out.file, out.row)
      print(paste0(i," : ",j))
    }
  }
}

out.file$compression = as.factor(ifelse(is.na(out.file$compression), "RAW", out.file$compression))
write.csv(out.file, "Median_and_Quartiles_AudioSet_Fingerprint.csv")
