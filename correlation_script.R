####### Auto-Correlation Script 
#### Becky Heath Aug/Sept 2020
###
###############################

##### Load Packages #####
library(corrr)
library(dplyr)
library(matrixcalc)
library(corrplot)

##### Load Dataframes #####
setwd("C:/Users/becky/Desktop/Research/(in Depth - needs to merge)/Compression Master (In Depth Analysis)/Final_Analysis")
dataset.index <- read.csv("Data_Analytical_Indices.csv")
dataset.audiosets <- read.csv("Data_AudioSet_Fingerprint.csv")

##### Perform Correlation #####

# Analytical Indices
num.index <- dataset.index[,9:16] # Use 9:16 to include max.freq
cor.index <- cor(num.index, use= "complete.obs")
write.table(cor.index, file = "Correlation_Analytical_Indices_throwaway.csv", sep = ",", append = TRUE, quote = FALSE,
            col.names = FALSE, row.names = FALSE)

# AudioSet Fingerprint
dataset.audiosets$max.freq[dataset.audiosets$file.size=='100'] <- '24000'
dataset.audiosets$max.freq[dataset.audiosets$file.size=='41.6'] <- '24000'
dataset.audiosets$max.freq[dataset.audiosets$file.size=='33.35'] <- '24000'
dataset.audiosets$max.freq[dataset.audiosets$file.size=='22.56'] <- '24000'
dataset.audiosets$max.freq[dataset.audiosets$file.size=='16.667'] <- '24000'
dataset.audiosets$max.freq[dataset.audiosets$file.size=='8.33'] <- '24000'
dataset.audiosets$max.freq[dataset.audiosets$file.size=='4.16'] <- '11025'
dataset.audiosets$max.freq[dataset.audiosets$file.size=='2.08'] <- '8000'
dataset.audiosets$max.freq[dataset.audiosets$file.size=='1.04'] <- '4000'
dataset.audiosets$max.freq <- as.numeric(dataset.audiosets$max.freq)
num.audiosets <- dataset.audiosets[,9:137] # Use 9:137 to include max.freq
num.audiosets <-num.audiosets %>%
  select(max.freq, everything())
cor.audiosets <- cor(num.audiosets, use= "complete.obs")
write.table(cor.audiosets, file = "Correlation_AudioSet_Fingerprint_throwaway.csv", sep = ",", append = TRUE, quote = FALSE,
            col.names = FALSE, row.names = FALSE)


# Plotting 

res.index <- cor.mtest(num.index, conf.level = .95)
corrplot(cor.index, p.mat = res.index$p, sig.level = .2, type="upper", method = "color", insig = "blank", tl.col = "black")
res.audiosets <- cor.mtest(num.audiosets, conf.level = .95)
corrplot(cor.audiosets, p.mat = res.audiosets$p, sig.level = .2, type="upper", method = "color", insig = "blank", tl.col = "white")


# Finding vals
abs.index <- abs(cor.index)
abs.index <- upper.triangle(abs.index)
abs.audioset <- abs(cor.audiosets)
abs.audioset <- upper.triangle(abs.audioset)

write.table(abs.index, file = "Correlation_abs_index.csv", sep = ",", append = TRUE, quote = FALSE,
            col.names = TRUE, row.names = FALSE)

write.table(abs.audioset, file = "Correlation_abs_audioset.csv", sep = ",", append = TRUE, quote = FALSE,
            col.names = TRUE, row.names = FALSE)
