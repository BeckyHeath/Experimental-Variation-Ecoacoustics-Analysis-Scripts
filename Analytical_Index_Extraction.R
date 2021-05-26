###################################################################################
## Script for Extracting Analytical Indices
## January 2020
##
## Becky Heath 
###################################################################################

library(seewave)
library(soundecology)
library(tuneR)


############ File Names Altered: ONLY USE FOR 20MIN RAW                                                               

# SET UP FILE AND OUTPUT LOCATION:
setwd("/rdsgpfs/general/user/reh2018/home/audiosets")                                  
file.location = "/rdsgpfs/general/user/reh2018/home/audiosets/20min_all_day_RAW" ## Change for each condition!! 
index <- as.integer(Sys.getenv("PBS_ARRAY_INDEX"))
size <- as.integer(Sys.getenv("PBS_ARRAY_LENGTH"))
output.folder = "/rdsgpfs/general/user/reh2018/home/audiosets/20min_all_day_RAW_six_features/"  ## Change for each condition!!


# Set Up Output df
out.file <- data.frame(file.name = character(),
                       max.freq = numeric(),
                       ACI = numeric(),
                       ADI = numeric(),
                       AEev = numeric(),
                       Bio = numeric(),
                       H = numeric (),
                       M = numeric (), 
                       NDSI = numeric())


# Set Up Analysis Function
mp3.do.analysis <- function(file){

    # Read and Load Data 
    file.name = as.character(file)
    path = paste(file.location, "/", file, sep="")
    data = readWave(path)

    max.freq = as.numeric(24000)        # Change these for different compressions
    sample.rate = as.numeric(48000)     # Change these for different compressions


    aci.list = acoustic_complexity(data, min_freq = NA, max_freq = max.freq)
    aci = as.numeric(aci.list$AciTotAll_left_bymin)
  
    adi.list = acoustic_diversity(data, max_freq = max.freq)
    adi = as.numeric(adi.list$adi_left)
  
    aeev.list = acoustic_evenness(data, max_freq = max.freq)
    aeev = as.numeric(aeev.list$aei_left)
  
    bio.list = bioacoustic_index(data, max_freq = max.freq)
    bio = as.numeric(bio.list$left_area)
  
    ndsi.list = ndsi(data, bio_max = max.freq)
    ndsi = as.numeric(ndsi.list$ndsi_left)
  
    H = as.numeric(H(data, sample.rate))
    M = as.numeric(M(data, sample.rate))
  
    x = data.frame(file.name, max.freq, aci, adi, aeev, bio, H, M, ndsi)
    file.name = paste(output.folder, index, "_mp3_six_feature_outputs.csv", sep = "")
    write.table(x, file = file.name, sep = ",", append = TRUE, quote = FALSE,
                col.names = FALSE, row.names = FALSE)
    
  }

f <- as.numeric(0)

for(i in dir(file.location)) {
  f <- f+1
  if(f == index){
      print(paste("processing", i))
      mp3.do.analysis(file = i) 
  }  
}                        
                      
                      
                    
