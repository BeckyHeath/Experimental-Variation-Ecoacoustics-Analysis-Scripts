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
library(ggplot2)
library(ggpubr)
library(patchwork)
library(car)
library(lattice)
library(latticeExtra)
library(betareg)
library(lmtest)
library(knitr)
library(IRdisplay)
library(hexbin)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

options(na.action='na.fail', width=120)

##### Import Data #####
dataset.index <- read.csv("Dataframes/Data_Analytical_Indices.csv")
dataset.audiosets <- read.csv("Dataframes/Data_AudioSet_Fingerprint.csv")
median.index <- read.csv("Dataframes/Median_and_Quartiles_Analytical_Indices.csv")
median.audiosets <- read.csv("Dataframes/Median_and_Quartiles_AudioSet_Fingerprint.csv")
abs.dif.index <- read.csv("Dataframes/Difference_Data_Analytical_Indices.csv")
abs.dif.audiosets <- read.csv("Dataframes/Difference_Data_AudioSet_Fingerprint.csv")
accuracy.both <- read.csv("Dataframes/RF_Accuracy_Both.csv")
cm.all <- read.csv("Dataframes/Complete_Confusion_Matrix_data.csv", fileEncoding = "UTF-8-BOM" )

##### Impact of Index: Auto Correlation (Figure 2) #####

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

write.table(abs.index, file = "Dataframes/Correlation_data_abs_index.csv", sep = ",", append = TRUE, quote = FALSE,
            col.names = TRUE, row.names = FALSE)

write.table(abs.audioset, file = "Correlation__data_abs_audioset.csv", sep = ",", append = TRUE, quote = FALSE,
            col.names = TRUE, row.names = FALSE)

##### Impact of Compression: Like for Like Differences (Figure 3) #####

# 1) Bland-Altman Style Plots

## Analytical Indices: 

# Subset frame size and order data
data <- median.index
data <- data[data$frame.size =="2_5min",]
data$compression <- factor(data$compression, levels = c("RAW","VBR0", "CBR320", "CBR256", "CBR128", "CBR64", "CBR32", "CBR16", "CBR8"))

for(index in c("ACI", "ADI", "Aeev", "Bio", "H", "M", "NDSI")){
  sample <- data[data$index == index,]
  if(index %in% c("M","NDSI")){
    plot8 <-ggplot(sample, aes(x=compression, y=median)) + 
      geom_errorbar(aes(ymin=lower, ymax=higher), width=.4, size =0.1) +
      labs(y = "Bias as % of Range") +
      ggtitle(index) + 
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
    plot_name <- paste("BA_plt_", index, sep="")
    assign(plot_name,plot8)
    
  } else if(index == "Bio"){
    plot8 <-ggplot(sample, aes(x=compression, y=median)) + 
      geom_errorbar(aes(ymin=lower, ymax=higher), width=.4, size =0.1) +
      labs(y = "Bias as % of Range") +
      ggtitle(index) + 
      theme_minimal() +
      theme(plot.title = element_text(hjust=0.01, vjust =-10)) +
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
    plot_name <- paste("BA_plt_", index, sep="")
    assign(plot_name,plot8)
  } else {
    plot <-ggplot(sample, aes(x=compression, y=median)) + 
      geom_errorbar(aes(ymin=lower, ymax=higher), width=.4, size =0.1) +
      ggtitle(index) + 
      theme_minimal() +
      theme(plot.title = element_text(hjust=0.01, vjust =-5)) +
      theme(panel.grid.major.x = element_blank()) +
      theme(axis.title.x=element_blank()) +
      theme(axis.text.x = element_blank()) +
      theme(axis.title.y=element_blank())+
      theme(legend.position = "none") +
      theme(axis.line = element_line()) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      geom_point(size=0.9)+
      annotate("rect", xmin = 0, xmax = 9.5, ymin = -5, ymax = 5, 
               alpha = .15,fill='green') +
      geom_hline(yintercept = 0, color = "DarkGrey") 
    plot_name <- paste("BA_plt_", index, sep="")
    assign(plot_name,plot)
  }
}

## AudioSet: 

# Subset and Order the Data
data <- median.audiosets
data <- data[data$frame.size =="2_5min",]
data$compression <- factor(data$compression, levels = c("RAW","VBR0", "CBR320", "CBR256", "CBR128", "CBR64", "CBR32", "CBR16", "CBR8"))

sample <- data[data$index == "total.dif.R",]
BA_plt_AudioSet <-ggplot(sample, aes(x=compression, y=median)) + 
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


# Align Plots Together: 
p1 <- BA_plt_AudioSet / BA_plt_ADI / BA_plt_Bio / BA_plt_M
p2 <- BA_plt_ACI / BA_plt_Aeev / BA_plt_H / BA_plt_NDSI

# Final Plot
BA_combi_plot <- p1|p2

BA_combi_plot

# 2) Spearman's Rank Correlation 

# Analytical Indices
data <- abs.dif.index
data <- data[data$frame.size == "2_5min",]

cor.test(data$ACI,data$file.size,method = "spearman", exact=F)
cor.test(data$ADI,data$file.size,method = "spearman", exact=F)
cor.test(data$Aeev,data$file.size,method = "spearman", exact=F)
cor.test(data$Bio,data$file.size,method = "spearman", exact=F)
cor.test(data$H,data$file.size,method = "spearman", exact=F)
cor.test(data$M,data$file.size,method = "spearman", exact=F)
cor.test(data$NDSI,data$file.size,method = "spearman", exact=F)

# AudioSet Fingerprint 
data <- abs.dif.audiosets
data <- data[data$frame.size == "2_5min",]
cor.test(data$total.dif,data$file.size,method = "spearman", exact=F)
cor.test(data$total.abs.dif,data$file.size,method = "spearman", exact=F)

##### Impact of Recording Schedule: Recording Length ####

# 1) Levene's Test for Homogeneity of Variance

analytical <- dataset.index
analytical <- analytical[which(analytical$compression=="RAW"),]
analytical <- analytical[complete.cases(analytical),]


# Test for Normality (To use Bartlett's)
for(i in (10:16)){
  index <- colnames(analytical)[i]
  data <- analytical[,i]
  plot <- ggqqplot(data, title = index) 
  name <- paste("qq_",index, sep="")
  assign(name,plot)
}

p1 <- qq_ACI / qq_Aeev / qq_H / qq_NDSI
p2 <- qq_ADI / qq_Bio / qq_M / qq_NDSI 

p1 |p2 # Saved as qq_All_Analytical_Indices


# Run the Lavene's Test (Analytical Indices)
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

# Run Lavene's Test (AudioSet Fingerprint)
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

write.csv(out.file, "Dataframes/Lavenes_test.csv")



##### Impact of Temporal Splitting (Figure 4) ####

## Temporal Splitting Predicted v. True plots 
## contributed by Dr. David Orme (Imperial College London)
## minor contributions by Becky Heath

whole.df <- cm.all
cm <- whole.df[whole.df$frame.size == "20min",]
cm <- subset(cm, (Comp %in% c("CBR8","RAW")) & (chunks %in% c("4", "None")))

# Rename site varibales to be more descriptive

cm$Pred <- as.factor(cm$Pred)
cm$Obs <- as.factor(cm$Obs)
cm$Ind <- as.factor(cm$Ind)

levels(cm$Pred)[match("D",levels(cm$Pred))] <- "Cleared"
levels(cm$Pred)[match("E",levels(cm$Pred))] <- "Logged"
levels(cm$Pred)[match("VJR",levels(cm$Pred))] <- "Primary"

levels(cm$Obs)[match("D",levels(cm$Obs))] <- "Cleared"
levels(cm$Obs)[match("E",levels(cm$Obs))] <- "Logged"
levels(cm$Obs)[match("VJR",levels(cm$Obs))] <- "Primary"

levels(cm$Ind)[match("Analytical",levels(cm$Ind))] <- "AI"
levels(cm$Ind)[match("AudioSet",levels(cm$Ind))] <- "AS"

# Accuracy
accuracy <- aggregate(N ~ Time + Ind + Comp, data=cm, FUN=sum, subset=Obs==Pred)
matrix_ns <- aggregate(N ~ Time + Ind + Comp, data=cm, FUN=sum)
accuracy$accuracy <- accuracy$N / matrix_ns$N

# Precision and Recall by habitat class
# - create an empty data frame with the correct structure to append to
precision <- subset(cm, select=c(Ind, Comp, Time, N), subset=FALSE)
precision$Precision <- numeric(0)
precision$Recall <- numeric(0)
precision$Case <- character(0)

# Loop over combinations
combn <- list(c('Cleared', 'Logged', 'C-L'), 
              c('Cleared', 'Primary', 'C-P'),
              c('Logged', 'Primary', 'L-P'))

for (this_comb in combn){
  
  true_class <- this_comb[1]
  false_class <- this_comb[2]
  
  this_precision <- subset(cm, Obs == true_class & Pred == true_class, 
                           select=c(Ind, Comp, Time, N))
  false_pos <- subset(cm, Obs== false_class & Pred == true_class)
  false_neg <- subset(cm, Obs== true_class & Pred == false_class)
  
  this_precision$Precision <- this_precision$N / (this_precision$N + false_pos$N)
  this_precision$Recall <- this_precision$N / (this_precision$N + false_neg$N)
  this_precision$Case <- this_comb[3]	
  
  precision <- rbind(precision, this_precision)
  
}

# Quick and easy data visualisation 


pdf('Figures/TOD_Time_breakdown_plots.pdf')

  xyplot(accuracy ~ Time | Comp, data=accuracy, groups = Ind, type='o')


  xyplot(Precision ~ Time | Case + Comp , data=precision, groups = Ind, 
       type='o', auto.key=TRUE)

  xyplot(Recall ~ Time | Comp + Case, data=precision, groups = Ind, 
       type='o', auto.key=TRUE)

dev.off()


# Condensed figure for publication

# Panel constants - (ylim, xlim, y axis tick values)
yl <- c(0.35,1.05)
xl <- c(0.7,4.3)
precision$Case <- factor(precision$Case)
yat <- c(0.4,0.6,0.8,1)

# Colours and alpha function
palette(c('turquoise4', 'tan1'))
add.alpha <- function(cols, alpha) rgb(t(col2rgb(cols)/255), alpha = alpha)

# All plots use the same structure so create a plot function to 
# make it easy to generate each panel.

plot_fun <- function(var, d, label='a'){
  
  order <- c("Dawn", "Midday", "Dusk", "Midnight", "Whole Day")
  d <- d %>%
    mutate(Time =  factor(Time, levels = order)) %>%
    arrange(Time) 
  
  
  # Get the plot formula for this variable
  fm <- as.formula(paste0(var, '~ as.numeric(Time)'))
  
  # create a correctly sized empty plot first, to make it easy to add the 
  # all-day means first and then overlay the 6 hour (type='n')
  plot(fm, data=d, subset= Ind == 'AS' & Time !='Whole Day', 
       ylim=yl, xlim=xl, xaxt='n', yaxt='n', type='n')
  
  # Add horizontal lines for the all daily recordings values
  dd <- subset(d, Ind == 'AS' & Time == 'Whole Day', select=var)[,1]
  segments(1,dd,4,dd, col=add.alpha(palette()[1], 0.3), lwd=2)
  dd <- subset(d, Ind == 'AI' & Time == 'Whole Day', select=var)[,1]
  segments(1,dd,4,dd, col=add.alpha(palette()[2], 0.3), lwd=2)
  
  # Add points and lines (type='o) for Audioset and AI for 6 hours
  points(fm, data=d, subset= Ind == 'AS' & Time !='Whole Day', col=1, type='o')
  points(fm, data=d, subset= Ind == 'AI' & Time !='Whole Day', col=2, type='o')
  
  text(1, 0.6, paste0(label, ')'))
}


# Using the layout mechanism, which makes it easy to generate spaces between 
# blocks of graphs for accuracy precision and recall - the matrix defines
# a 2x9 matrix, but the heights make two of the rows really narrow, so we
# can skip over using these for plots and just leave a gap.

par(mar=c(0,0,0,0), oma=c(3,3,1,1), las=0, tcl=-0.3, mgp=c(1.5,0.5,0))
layout(matrix(1:18, ncol=2, byrow=TRUE), heights=c(5,1,5,5,5,1,5,5,5))

# Accuracy
plot_fun('accuracy', subset(accuracy, Comp == 'RAW'), label='a')
mtext('Raw', side=3, cex=0.6)
mtext('Accuracy', side=2, cex=0.6, line=1.5)
axis(2, at=yat)
plot_fun('accuracy', subset(accuracy, Comp == 'CBR8'), label='b')
mtext('All', side=4, cex=0.6, las=3)
mtext('CBR8', side=3, cex=0.6)

# Skip the two narrow plots
plot.new(); plot.new()

# Loop over precision by habitat pair
labels <- list(c('c','d'), c('e','f'), c('g','h'))
names(labels) <- levels(precision$Case)

for (cs in levels(precision$Case)){
  
  these_labels <- labels[[cs]]
  
  plot_fun('Precision', subset(precision, Comp == 'RAW' & Case == cs), 
           label=these_labels[1])
  axis(2, at=yat)
  
  if(cs == 'C-P') mtext('Precision', side=2, cex=0.6, line=1.5)
  
  plot_fun('Precision', subset(precision, Comp == 'CBR8' & Case == cs), 
           label=these_labels[2])
  mtext(cs, side=4, cex=0.6, las=3)
}

# Skip the two narrow plots
plot.new(); plot.new()

# Loop over recall by habitat pair
labels <- list(c('i','j'), c('k','l'), c('m','n'))
names(labels) <- levels(precision$Case)

for (cs in levels(precision$Case)){
  
  these_labels <- labels[[cs]]
  
  plot_fun('Recall', subset(precision, Comp == 'RAW' & Case == cs), 
           label=these_labels[1])
  axis(2, at=yat)
  
  if(cs == 'L-P') axis(1, at=1:4, labels=levels(accuracy$Time)[1:4])
  if(cs == 'G-P') mtext('Recall', side=2, cex=0.6, line=1.5)
  
  plot_fun('Recall', subset(precision, Comp == 'CBR8' & Case == cs), 
           label=these_labels[2])
  mtext(cs, side=4, cex=0.6, las=3)
  
  if(cs == 'L-P') axis(1, at=1:4, labels=levels(accuracy$Time)[1:4])
}



##### Impact of Parameter Alteration on Classification Task (Figure 5) #####

## Beta Regression modelling and plotting 
## contributed by Dr. David Orme (Imperial College London)

dat <- accuracy.both

# Sensible orders for factors
dat$frame.size <- factor(dat$frame.size, levels=c("20min", "10min", "5min", "2_5min"))
dat$chunks <- factor(dat$chunks, levels=c("None", "4", "8", "12"))

# Correction to allow betaregression [0,1] to (0,1)
# Note: this reduces 100% values to 99.94% ((100*1799)+0.5)/1800
n <- nrow(dat)
dat$accuracy_t <- (dat$accuracy * (n - 1) + 0.5) / n
dat$precision_t <- (dat$precision * (n - 1) + 0.5) / n
dat$recall_t <- (dat$recall * (n - 1) + 0.5) / n

dat <- dat[complete.cases(dat),]


# Maximum Models (fully iteracted and substitute)
max_model <- accuracy_t ~ (log10(file.size) + chunks + frame.size) ^ 2 * index.type
max_model <- precision_t ~ (log10(file.size) + chunks + frame.size) ^ 2 * index.type
max_model <- recall_t ~ (log10(file.size) + chunks + frame.size) ^ 2 * index.type

# Linear Model (as in early analysis)
mod_lm <- lm(max_model, data= dat)
summary.aov(mod_lm)

# Betareg Model
mod_br <- betareg(max_model, data=dat) 

# Looking at Variation: 
var_plot <- bwplot(accuracy ~ frame.size | index.type + chunks, data=dat)
var_plot <- useOuterStrips(var_plot)
print(var_plot)


# Including Index Type and Chunks into the precision component of the model 

mod_br_phi <- update(mod_br,  . ~ .  | index.type * chunks)
summary(mod_br_phi)

AIC(mod_lm, mod_br, mod_br_phi) # shows this model is best

aov_table <- Anova(mod_br_phi)
aov_table



## Lattice Plotting with HexBin


dat$index.type <- as.factor(dat$index.type)

# Create a range of values to predict
pred <- expand.grid(file.size = 10 ^ seq(0, 2, length=31),
                    index.type = levels(dat$index.type),
                    chunks = levels(dat$chunks),
                    frame.size = levels(dat$frame.size))

# Create the predictions
pred$fit_acc <- predict(mod_br_phi, newdata=pred)

# Plot them
key <- list(corner=c(0.98, 0.03),
            lines=list(col=trellis.par.get('superpose.line')$col[1:4]),
            text=list(c("20 min", "10 min", "5 min", "2.5 min"), cex=0.7))

pred_plot_acc <- xyplot(fit_acc ~ log10(file.size) | index.type + chunks, group= frame.size, 
                        data=pred, type='l', key=key)
pred_plot_acc <- useOuterStrips(pred_plot_acc)
print(pred_plot_acc)


# Then do this for precision

dat$precision_t <- (dat$precision * (n - 1) + 0.5) / n 
mod_br_phi_prec <- update(mod_br_phi, precision_t ~ ., subset = ! is.na(precision_t))

# Plot them
pred$prec_fit <- predict(mod_br_phi_prec, newdata=pred)

pred_plot_prec <- xyplot(prec_fit ~ log10(file.size) | index.type + chunks, group= frame.size, 
                         data=pred, type='l', key=key, 
                         scales=list(alternating=FALSE, y=list(lim=c(0.65,1.05), at=c(0.7,0.8,0.9,1.0))),
                         ylab='Predicted precision', main='Precision')
pred_plot_prec <- useOuterStrips(pred_plot_prec)
print(pred_plot_prec)


# and Recall

dat$recall_t <- (dat$recall * (n - 1) + 0.5) / n 
mod_br_phi_recall <- update(mod_br_phi, recall_t ~ .)

pred$recall_fit <- predict(mod_br_phi_recall, newdata=pred)

# Plot them
pred_plot_recall <- xyplot(recall_fit ~ log10(file.size) | index.type + chunks, group= frame.size, 
                           data=pred, type='l', key=key, 
                           scales=list(alternating=FALSE, y=list(lim=c(0.65,1.05), at=c(0.7,0.8,0.9,1.0))),
                           ylab='Predicted recall', main='Recall')
pred_plot_recall <- useOuterStrips(pred_plot_recall)
print(pred_plot_recall)

# Assemble Plot
# Refit the plot for accuracy to the full model for comparability

pred$fit_acc <- predict(mod_br_phi, newdata=pred)
pred_plot_acc <- xyplot(fit_acc ~ log10(file.size) | index.type + chunks, group= frame.size, 
                        data=pred, type='l', key=key, 
                        scales=list(alternating=FALSE, y=list(lim=c(0.65,1.05), at=c(0.7,0.8,0.9,1.0))),
                        ylab='Predicted accuracy', main='Accuracy')
pred_plot_acc <- useOuterStrips(pred_plot_acc)

# Combine
options(repr.plot.width = 15, repr.plot.height = 6)
print(pred_plot_acc, split=c(1,1,3,1), more=TRUE)
print(pred_plot_prec, split=c(2,1,3,1), more=TRUE)
print(pred_plot_recall, split=c(3,1,3,1), more=FALSE)

# Using base graphics to plot fits over hexbin

hexplot <- hexbinplot(accuracy_t ~ log10(file.size) | index.type + chunks, 
                      aspect=2/3, data=dat, colorkey=FALSE)
hexplot <- useOuterStrips(hexplot)
print(hexplot)


hexbin_to_poly <- function(hx){
  
  # Function to return a base graphics polygon() representation of
  # the hexbin output
  
  # Get centres
  xy <- hcell2xy(hx)
  
  # Calculate the dx and dy for hexcoords
  sx <- hx@xbins/diff(hx@xbnds)
  sy <- (hx@xbins * hx@shape)/diff(hx@ybnds)
  inner <- 0.5
  outer <- (2 * inner)/sqrt(3)
  dx <- inner/sx
  dy <- outer/(2 * sy)
  
  # generate the polygons and offset to each centre
  hexC <- hexcoords(dx, dy, n=length(xy$x), sep=NA)
  hexC$x <- hexC$x + rep(xy$x, each=7)
  hexC$y <- hexC$y + rep(xy$y, each=7)
  
  return(hexC)
}


options(repr.plot.width = 6, repr.plot.height = 8)

# Create a 4 by 2 layout of edge to edge plots with 
# an outer margin for annotation
par(mfcol=c(4,2), mar=c(0,0,0,0), oma=c(4,4,2.2,2.2), mgp=c(1.8,0.6, 0))

# Loop combinations of index type (columns) and chunking (rows)
for (it in 1:2){
  for (ch in 1:4){
    
    this_it <- levels(dat$index.type)[it]
    this_ch <- levels(dat$chunks)[ch]
    
    # Get a hexbin object for this panel subset
    dd <- subset(dat, index.type == this_it & chunks == this_ch)
    hx <- hexbin(log10(dd$file.size), dd$accuracy_t, xbins=30, 
                 shape=2/3, xbnds=c(0,2), ybnds=c(0.2,1))
    
    # Convert the hexbin of the data to base graphics
    hx_p <- hexbin_to_poly(hx)
    plot(hx_p, type='n', xaxt='n', yaxt='n', xlim=c(0,2), ylim=c(0.2,1))
    # Colour scale for density (relative within plots)
    cols <- (1 - (hx@count / max(hx@count)) * 0.7) - 0.2
    # Add polygons
    polygon(hx_p, col=grey(cols), border=NA)
    
    # Add prediction lines for the four frame size fits
    for (fs in 1:4){
      this_fs <- levels(dat$frame.size)[fs]
      fit <- subset(pred, index.type == this_it & chunks == this_ch & frame.size == this_fs)
      lines(fit_acc ~ log10(file.size), data=fit, col=fs)
    }
    
    # Conditional panel annotation.
    if (it == 1) axis(2, at=seq(0.25, 1, by=0.25))
    if (ch == 4) axis(1)
    if (ch == 1) {
      axis(3, labels=FALSE, at=seq(0.25, 1, by=0.25))
      mtext(side=3, this_it, line=1)
    }
    if (it == 2){
      axis(4, labels=FALSE)
      mtext(side=4, this_ch, line=1)
    }
  }
}

# Final labels. 
legend('bottomright', col=1:4, legend=c("20 min", "10 min", "5 min", "2.5 min"), 
       bty='n', lty=1)
mtext(side=2, 'Accuracy', outer=TRUE, line=2.2)
mtext(side=1, expression(log[10]~file~size), outer=TRUE, line=2.2)


##### For Precision: 

hexplot <- hexbinplot(precision_t ~ log10(file.size) | index.type + chunks, 
                      aspect=2/3, data=dat, colorkey=FALSE)
hexplot <- useOuterStrips(hexplot)
print(hexplot)

options(repr.plot.width = 6, repr.plot.height = 8)

# Create a 4 by 2 layout of edge to edge plots with 
# an outer margin for annotation
par(mfcol=c(4,2), mar=c(0,0,0,0), oma=c(4,4,2.2,2.2), mgp=c(1.8,0.6, 0))

# Loop combinations of index type (columns) and chunking (rows)
for (it in 1:2){
  for (ch in 1:4){
    
    this_it <- levels(dat$index.type)[it]
    this_ch <- levels(dat$chunks)[ch]
    
    # Get a hexbin object for this panel subset
    dd <- subset(dat, index.type == this_it & chunks == this_ch)
    hx <- hexbin(log10(dd$file.size), dd$precision_t, xbins=30, 
                 shape=2/3, xbnds=c(0,2), ybnds=c(0.15,1))
    
    # Convert the hexbin of the data to base graphics
    hx_p <- hexbin_to_poly(hx)
    plot(hx_p, type='n', xaxt='n', yaxt='n', xlim=c(0,2), ylim=c(0.2,1))
    # Colour scale for density (relative within plots)
    cols <- (1 - (hx@count / max(hx@count)) * 0.7) - 0.2
    # Add polygons
    polygon(hx_p, col=grey(cols), border=NA)
    
    # Add prediction lines for the four frame size fits
    for (fs in 1:4){
      this_fs <- levels(dat$frame.size)[fs]
      fit <- subset(pred, index.type == this_it & chunks == this_ch & frame.size == this_fs)
      lines(prec_fit ~ log10(file.size), data=fit, col=fs)
    }
    
    # Conditional panel annotation.
    if (it == 1) axis(2, at=seq(0.25, 1, by=0.25))
    if (ch == 4) axis(1)
    if (ch == 1) {
      axis(3, labels=FALSE, at=seq(0.25, 1, by=0.25))
      mtext(side=3, this_it, line=1)
    }
    if (it == 2){
      axis(4, labels=FALSE)
      mtext(side=4, this_ch, line=1)
    }
  }
}

# Final labels. 
legend('bottomright', col=1:4, legend=c("20 min", "10 min", "5 min", "2.5 min"), 
       bty='n', lty=1)
mtext(side=2, 'Precision', outer=TRUE, line=2.2)
mtext(side=1, expression(log[10]~file~size), outer=TRUE, line=2.2)


##### For Recall: 

hexplot <- hexbinplot(recall_t ~ log10(file.size) | index.type + chunks, 
                      aspect=2/3, data=dat, colorkey=FALSE)
hexplot <- useOuterStrips(hexplot)
print(hexplot)

options(repr.plot.width = 6, repr.plot.height = 8)

# Create a 4 by 2 layout of edge to edge plots with 
# an outer margin for annotation
par(mfcol=c(4,2), mar=c(0,0,0,0), oma=c(4,4,2.2,2.2), mgp=c(1.8,0.6, 0))

# Loop combinations of index type (columns) and chunking (rows)
for (it in 1:2){
  for (ch in 1:4){
    
    this_it <- levels(dat$index.type)[it]
    this_ch <- levels(dat$chunks)[ch]
    
    # Get a hexbin object for this panel subset
    dd <- subset(dat, index.type == this_it & chunks == this_ch)
    hx <- hexbin(log10(dd$file.size), dd$recall_t, xbins=30, 
                 shape=2/3, xbnds=c(0,2), ybnds=c(0.15,1))
    
    # Convert the hexbin of the data to base graphics
    hx_p <- hexbin_to_poly(hx)
    plot(hx_p, type='n', xaxt='n', yaxt='n', xlim=c(0,2), ylim=c(0.2,1))
    # Colour scale for density (relative within plots)
    cols <- (1 - (hx@count / max(hx@count)) * 0.7) - 0.2
    # Add polygons
    polygon(hx_p, col=grey(cols), border=NA)
    
    # Add prediction lines for the four frame size fits
    for (fs in 1:4){
      this_fs <- levels(dat$frame.size)[fs]
      fit <- subset(pred, index.type == this_it & chunks == this_ch & frame.size == this_fs)
      lines(recall_fit ~ log10(file.size), data=fit, col=fs)
    }
    
    # Conditional panel annotation.
    if (it == 1) axis(2, at=seq(0.25, 1, by=0.25))
    if (ch == 4) axis(1)
    if (ch == 1) {
      axis(3, labels=FALSE, at=seq(0.25, 1, by=0.25))
      mtext(side=3, this_it, line=1)
    }
    if (it == 2){
      axis(4, labels=FALSE)
      mtext(side=4, this_ch, line=1)
    }
  }
}

# Final labels. 
legend('bottomright', col=1:4, legend=c("20 min", "10 min", "5 min", "2.5 min"), 
       bty='n', lty=1)
mtext(side=2, 'Recall', outer=TRUE, line=2.2)
mtext(side=1, expression(log[10]~file~size), outer=TRUE, line=2.2)










