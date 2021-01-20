########################################################################### .
### STATS SCRIPT FOR COMPRESSION PAPER
### BECKY HEATH 
### October 2020

##### Load Packages #####
library("dplyr")
library("car")
library("ggplot2")
library(patchwork)
##### Import Data #####
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

##### LIKE-FOR-LIKE DIFFERENCES #####
# SPEARMANS RANK LINEAR REGRESSIONS FOR FEATURES
data <- abs.dif.index
data <- data[data$frame.size == "2_5min",]


cor.test(data$ACI,data$file.size,method = "spearman", exact=F)
cor.test(data$ADI,data$file.size,method = "spearman", exact=F)
cor.test(data$Aeev,data$file.size,method = "spearman", exact=F)
cor.test(data$Bio,data$file.size,method = "spearman", exact=F)
cor.test(data$H,data$file.size,method = "spearman", exact=F)
cor.test(data$M,data$file.size,method = "spearman", exact=F)
cor.test(data$NDSI,data$file.size,method = "spearman", exact=F)

#SPEARMANS RANK LINEAR REGRESSION FOR FEATURES: 
data <- abs.dif.audiosets
data <- data[data$frame.size == "2_5min",]
cor.test(data$total.dif,data$file.size,method = "spearman", exact=F)
cor.test(data$total.abs.dif,data$file.size,method = "spearman", exact=F)

##### Linear Model, just the impact of file size on accuracy #####

combi <- accuracy.both
combi$accuracy <- combi$accuracy*100

combi <- combi[combi$chunks == "None",]

x <- c("20min", "10min", "5min", "2_5min")
y <- c("Analytical_Indices","AudioSet")
z <- c("accuracy", "precision", "recall")

out.file <- data.frame()

# Loop to organise test dataframes and execute analysis
for (i in x) {
  combi.x <- combi[(combi$frame.size == i),]
  for (j in y){
    combi.xy <- combi.x[(combi.x$index.type == j),]
    for (k in z){
      compression <- combi.xy$compression
      frame.size <- combi.xy$frame.size
      index <- combi.xy$index.type
      chunks <- combi.xy$chunks
      file.size <- combi.xy$file.size
      index.type <- combi.xy$index.type
      if (k == "accuracy"){
        accuracy <- combi.xy$accuracy
      } else if (k == "precision") {
        accuracy <- combi.xy$precision
      } else if (k == "recall") {
        accuracy <- combi.xy$recall
      }
      # Compute models!
      print("######################################################################################################")
      print(paste("Frame Size =",i,"   Index Type =",j,"    Accuracy Metric=",k, sep = ""))
      
      # First Find the Interaction Term
      model <- lm(accuracy ~ file.size)
      print(summary(model))
      sum <- summary(model)
      #Define Output Values: 
      out.FS <- as.character(i)   #Frame Size
      out.CN <- as.character(j)   #Index Type
      out.AM <- as.character(k)   #Accuracy Metric
      intercept <- sum[["coefficients"]][1]
      intercept.error <- sum[["coefficients"]][3]
      slope.file.size <- sum[["coefficients"]][2]
      slope.file.size.error <- sum[["coefficients"]][4]
      T.val.file.size <- sum[["coefficients"]][6]
      p.val.file.size <- sum[["coefficients"]][8]
      deg.f <- model[["df.residual"]] 
      out.line <- data.frame(out.FS,out.CN, out.AM, intercept, intercept.error, slope.file.size,
                             slope.file.size.error,T.val.file.size, p.val.file.size,deg.f)
      out.file <- rbind(out.file,out.line)
      
    }
  }
}

write.csv(out.file,"LM_outputs_just_file_size.csv", row.names = FALSE)

## TO TREAT FRAME SIZE AS NUMERIC?
#combi$frame.size <- gsub('min', '', combi$frame.size)
#combi$frame.size <- gsub('_', '.', combi$frame.size)
#combi$frame.size <- as.numeric(combi$frame.size)

##### Bartlett's Test for Homogeneity of Variance #####

analytical <- dataset.index
analytical <- analytical[which(analytical$compression=="RAW"),]
analytical <- analytical[complete.cases(analytical),]

qqPlot(analytical$NDSI)

out.file<- data.frame(index = character(),
                      p.value = numeric())

for(index in c("ACI","ADI","Aeev", "Bio", "H", "M", "NDSI")){
  test.index <- analytical[,index]
  frame.size <- analytical[,"frame.size"]
  output <- leveneTest(test.index ~ frame.size)
  p.val <- output[3]
  p.value <- p.val[1,1]
  out.line <- data.frame(index,p.value)
  out.file <-rbind(out.file,out.line)
}

analytical <- dataset.audiosets

for(j in seq(1:128)){
  index <- paste("feat",j, sep="")
  test.index <- analytical[,index]
  frame.size <- analytical[,"frame.size"]
  output <- leveneTest(test.index ~ frame.size)
  p.val <- output[3]
  p.value <- p.val[1,1]
  out.line <- data.frame(index,p.value)
  out.file <-rbind(out.file,out.line)
}

write.csv(out.file, "Lavernes_test.csv")



##### Stats for big 9 Panel Graph #####
combi <- accuracy.both
combi$accuracy <- combi$accuracy*100

combi$chunks <- gsub('None', '0', combi$chunks)
combi$chunks <- as.numeric(combi$chunks)

x <- c("20min", "10min", "5min", "2_5min")
y <- c(0,4,8,12)
z <- c("accuracy", "precision", "recall")

out.file <- data.frame()

# Loop to organise test dataframes and execute analysis
for (i in x) {
  combi.x <- combi[(combi$frame.size == i),]
  for (j in y){
    combi.xy <- combi.x[(combi.x$chunks == j),]
    for (k in z){
      compression <- combi.xy$compression
      frame.size <- combi.xy$frame.size
      index <- combi.xy$index.type
      chunks <- combi.xy$chunks
      file.size <- combi.xy$file.size
      index.type <- combi.xy$index.type
      if (k == "accuracy"){
        accuracy <- combi.xy$accuracy
      } else if (k == "precision") {
        accuracy <- combi.xy$precision
      } else if (k == "recall") {
        accuracy <- combi.xy$recall
      }
      # Comput models!
      print("######################################################################################################")
      print(paste("Frame Size =",i,"   Chunk Number=",j,"    Accuracy Metric=",k, sep = ""))
      
      # First Find the Interaction Term
      model <- lm(accuracy ~ log(file.size) + index.type + index.type*file.size)
      print(summary(model))
      sum <- summary(model)
      #Define Output Values: 
      out.FS <- as.character(i)   #Frame Size
      out.CN <- as.character(j)   #Chunk Number
      out.AM <- as.character(k)   #Accuracy Metric
      out.model <- as.character("Interaction")
      intercept <- sum[["coefficients"]][1]
      intercept.error <- sum[["coefficients"]][5]
      slope.file.size <- sum[["coefficients"]][2]
      slope.file.size.error <- sum[["coefficients"]][6]
      T.val.file.size <- sum[["coefficients"]][10]
      p.val.file.size <- sum[["coefficients"]][14]
      slope.index <- sum[["coefficients"]][3]
      slope.index.error <- sum[["coefficients"]][7]
      T.val.index <- sum[["coefficients"]][11]
      p.val.index <- sum[["coefficients"]][15]
      slope.interaction <- sum[["coefficients"]][4]
      slope.interaction.error <- sum[["coefficients"]][8]
      T.val.interaction <- sum[["coefficients"]][12]
      p.val.interaction <- sum[["coefficients"]][16]
      deg.f <- model[["df.residual"]] 
      out.line <- data.frame(out.FS,out.CN, out.AM, out.model, intercept, intercept.error, slope.file.size,
                   slope.file.size.error,T.val.file.size, p.val.file.size, slope.index, slope.index.error, 
                   T.val.index, p.val.index,slope.interaction, slope.interaction.error, T.val.interaction,
                   p.val.interaction,deg.f)
      # Add the Interaction model to the output dataframe
      out.file <- rbind(out.file,out.line)
      
      # Then See if the model is addative
      model <- lm(accuracy ~ log(file.size) + index.type)
      print(summary(model))
      sum <- summary(model)
      #Define Output Values: 
      out.FS <- as.character(i)   #Frame Size
      out.CN <- as.character(j)   #Chunk Number
      out.AM <- as.character(k)   #Accuracy Metric
      out.model <- as.character("Additive")
      intercept <- sum[["coefficients"]][1]
      intercept.error <- sum[["coefficients"]][4]
      slope.file.size <- sum[["coefficients"]][2]
      slope.file.size.error <- sum[["coefficients"]][5]
      T.val.file.size <- sum[["coefficients"]][8]
      p.val.file.size <- sum[["coefficients"]][11]
      slope.index <- sum[["coefficients"]][3]
      slope.index.error <- sum[["coefficients"]][6]
      T.val.index <- sum[["coefficients"]][9]
      p.val.index <- sum[["coefficients"]][12]
      slope.interaction <- NA
      slope.interaction.error <- NA
      T.val.interaction <- NA
      p.val.interaction <- NA
      deg.f <- model[["df.residual"]]
      
      out.line <- data.frame(out.FS,out.CN, out.AM, out.model, intercept, intercept.error, slope.file.size,
                             slope.file.size.error,T.val.file.size, p.val.file.size, slope.index, slope.index.error, 
                             T.val.index, p.val.index,slope.interaction, slope.interaction.error, T.val.interaction,
                             p.val.interaction,deg.f)
      # Add the Additive model to the output dataframe
      out.file <- rbind(out.file,out.line)
      
      
    }
  }
}


#write.csv(out.file,"Statistical_Outputs_Multiple_LM_log.csv",row.names = FALSE)


##### Temporal Splitting####

combi <- accuracy.both
combi$accuracy <- combi$accuracy*100

# to treat chunks as Numeric:
combi$chunks <- as.numeric(as.character(combi$chunks))
combi$chunks[is.na(combi$chunks)] <- 0

##### INTERCEPT MODELS############################################################################################
model <- lm(accuracy ~ file.size + chunks + index.type + frame.size, data =combi)
summary(model)


## Checking for interaction terms ACCURACY

model <- lm(accuracy ~ file.size*chunks + index.type + frame.size, data =combi)
summary(model)

model <- lm(accuracy ~ file.size*index.type + chunks  + frame.size, data =combi)
summary(model)

model <- lm(accuracy ~ file.size*frame.size + chunks  + index.type, data =combi)
summary(model)

# chunks 

model <- lm(accuracy ~ file.size + index.type*chunks  + frame.size, data =combi)
summary(model)

model <- lm(accuracy ~ file.size + index.type + chunks*frame.size, data =combi)
summary(model)

# frame size 

model <- lm(accuracy ~ file.size+ index.type*frame.size + chunks, data =combi)
summary(model)

##### Checking for interaction terms PRECISION############################################
## 

#combi <- accuracy.both
combi$precision <- combi$precision*100

model <- lm(precision ~ file.size + chunks + index.type + frame.size, data =combi)
summary(model)


model <- lm(precision ~ file.size*chunks + index.type + frame.size, data =combi)
summary(model)

model <- lm(precision ~ file.size*index.type + chunks  + frame.size, data =combi)
summary(model)

model <- lm(precision ~ file.size*frame.size + chunks  + index.type, data =combi)
summary(model)

# chunks 

model <- lm(precision ~ file.size + index.type*chunks  + frame.size, data =combi)
summary(model)

model <- lm(precision ~ file.size + index.type + chunks*frame.size, data =combi)
summary(model)

# frame size 

model <- lm(precision ~ file.size+ index.type*frame.size + chunks, data =combi)
summary(model)

##### Checking for interaction terms RECALL############################################
## 

#combi <- accuracy.both
combi$recall <- combi$recall*100

model <- lm(recall ~ file.size + chunks + index.type + frame.size, data =combi)
summary(model)


model <- lm(recall ~ file.size*chunks + index.type + frame.size, data =combi)
summary(model)

model <- lm(recall ~ file.size*index.type + chunks  + frame.size, data =combi)
summary(model)

model <- lm(recall ~ file.size*frame.size + chunks  + index.type, data =combi)
summary(model)

# chunks 

model <- lm(recall ~ file.size + index.type*chunks  + frame.size, data =combi)
summary(model)

model <- lm(recall ~ file.size + index.type + chunks*frame.size, data =combi)
summary(model)

# frame size 

model <- lm(recall ~ file.size+ index.type*frame.size + chunks, data =combi)
summary(model)


##### Final Models                                       ################################################################################

#

combi <- accuracy.both

combi$chunks <- as.numeric(as.character(combi$chunks))
combi$chunks[is.na(combi$chunks)] <- 0

combi$accuracy <- combi$accuracy*100
combi$precision <- combi$precision*100
combi$recall <- combi$recall*100

model_a <- lm(accuracy ~ file.size+ index.type*chunks, data =combi)
summary(model)

model_b <- lm(precision~ file.size+ index.type*chunks, data =combi)
summary(model)

model_c <- lm(recall ~ file.size+ index.type*chunks, data =combi)
summary(model)

pred_a <- data.frame(accuracy_pred = predict(model_a, combi), chunks = combi$chunks) 
pred_a$index.type <- combi$index.type

pred_b <- data.frame(precision_pred = predict(model_b, combi), chunks = combi$chunks) 
pred_b$index.type <- combi$index.type

pred_c <- data.frame(recall_pred = predict(model_c, combi), chunks = combi$chunks) 
pred_c$index.type <- combi$index.type

test_plot_a <- ggplot(combi, aes(chunks, accuracy))+
  labs(x = NULL, y= "Accuracy %", colour = "Index Type")+
  geom_jitter(aes(color = index.type), alpha = 0.3)+
  geom_smooth(data = pred_a, aes(x=chunks, y= accuracy_pred, colour = index.type))+
  theme_minimal()+
  theme(legend.position= "none", axis.text.x=element_blank())+
  scale_x_continuous(breaks=seq(0,12,4)) +
  scale_colour_manual(labels = c("Analytical Indices", "AudioSet Fingerprint"), values = c("darkorange1", "darkcyan"))+
  guides(color=guide_legend(override.aes=list(fill=NA)))

test_plot_b <- ggplot(combi, aes(chunks, precision))+
  labs(x = NULL, y= "Precision %", colour = "Index Type")+
  geom_jitter(aes(color = index.type), alpha = 0.3)+
  geom_smooth(data = pred_b, aes(x=chunks, y= precision_pred, colour = index.type))+
  theme_minimal()+
  theme(legend.position= "none", axis.text.x=element_blank())+
  scale_x_continuous(breaks=seq(0,12,4)) +
  scale_colour_manual(labels = c("Analytical Indices", "AudioSet Fingerprint"), values = c("darkorange1", "darkcyan"))+
  guides(color=guide_legend(override.aes=list(fill=NA)))

test_plot_c <- ggplot(combi, aes(chunks, recall))+
  labs(x = "Temporal Split Number", y= "Recall %", colour = "Index Type")+
  geom_jitter(aes(color = index.type), alpha = 0.3)+
  geom_smooth(data = pred_c, aes(x=chunks, y= recall_pred, colour = index.type))+
  theme_minimal()+
  theme(legend.position= c(0.2,0.2), legend.background = element_blank())+
  scale_x_continuous(breaks=seq(0,12,4)) +
  scale_colour_manual(labels = c("Analytical Indices", "AudioSet Fingerprint"), values = c("darkorange1", "darkcyan"))+
  guides(color=guide_legend(override.aes=list(fill=NA)))

test_plot_a/test_plot_b/test_plot_c

##### I want to just see file size quick hold up #######################################################
#### 

combi <- accuracy.both

combi$chunks <- as.numeric(as.character(combi$chunks))
combi$chunks[is.na(combi$chunks)] <- 0

combi$accuracy <- combi$accuracy*100
combi$precision <- combi$precision*100
combi$recall <- combi$recall*100

pred_a <- data.frame(accuracy_pred = predict(model_a, combi), file.size = combi$file.size) 
pred_a$index.type <- combi$index.type


test_plot_a <- ggplot(combi, aes(file.size, accuracy))+
  labs(x = NULL, y= "Accuracy %", colour = "Index Type")+
  geom_jitter(aes(color = index.type), alpha = 0.3)+
  geom_smooth(data = pred_a, aes(x=file.size, y= accuracy_pred, colour = index.type))+
  theme_minimal()+
  theme(legend.position= "none")+
  scale_colour_manual(labels = c("Analytical Indices", "AudioSet Fingerprint"), values = c("darkorange1", "darkcyan"))+
  guides(color=guide_legend(override.aes=list(fill=NA)))
test_plot_a




#### BETA REG STUFF #####

