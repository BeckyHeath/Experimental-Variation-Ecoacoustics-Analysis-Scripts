########################################################################### .
# Final Stats and Figures Script for Compression Paper
#
# Becky Heath (with considerable contribution from Dr. David Orme)
# r.heath18@imperial.ac.uk
#
# Autumn/Winter 2020/2021
#
#

##### Load Packages and set working directory #####
library(corrplot)
library(matrixcalc)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

##### Import Data #####
dataset.index <- read.csv("Data_Analytical_Indices.csv")
dataset.audiosets <- read.csv("Data_AudioSet_Fingerprint.csv")

##### Impact of Index: Auto Correlation #####

# 1) Correlation Matrix Data and Plots

# Analytical Indices
num.index <- dataset.index[,9:16] # Use 10:16 to exclude max.freq
cor.index <- cor(num.index, use= "complete.obs")

#AudioSet
num.audiosets <- dataset.audiosets[,9:137] # Use 10:137 to exclude max.freq
cor.audiosets <- cor(num.audiosets, use= "complete.obs")

# Plotting 
res.index <- cor.mtest(num.index, conf.level = .95)
corrplot(cor.index, p.mat = res.index$p, sig.level = .2, type="upper", method = "color", insig = "blank", tl.col = "black")
res.audiosets <- cor.mtest(num.audiosets, conf.level = .95)
corrplot(cor.audiosets, p.mat = res.audiosets$p, sig.level = .2, type="upper", method = "color", insig = "blank", tl.col = "white")

# Save Absolute Correlation Values

abs.index <- abs(cor.index)
abs.index <- upper.triangle(abs.index)
abs.audioset <- abs(cor.audiosets)
abs.audioset <- upper.triangle(abs.audioset)

#write.table(abs.index, file = "Correlation_data_abs_index.csv", sep = ",", append = TRUE, quote = FALSE,
#            col.names = TRUE, row.names = FALSE)

#write.table(abs.audioset, file = "Correlation__data_abs_audioset.csv", sep = ",", append = TRUE, quote = FALSE,
#            col.names = TRUE, row.names = FALSE)

##### Impact of Compression: Like for Like Differences #####

# 1) Bland-Altman Style Plots

# 2) Spearman's Rank Correlation (check this)


##### Impact of Recording Schedule: Recording Length ####

# 1) Levene's Test for Homogeneity of Variance

##### Impact of Parameter Alteration on Classification Task #####