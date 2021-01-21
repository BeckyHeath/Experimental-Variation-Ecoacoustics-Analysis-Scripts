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

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

options(na.action='na.fail', width=120)

##### Import Data #####
dataset.index <- read.csv("Data_Analytical_Indices.csv")
dataset.audiosets <- read.csv("Data_AudioSet_Fingerprint.csv")
median.index <- read.csv("Median_and_Quartiles_Analytical_Indices.csv")
median.audiosets <- read.csv("Median_and_Quartiles_AudioSet_Fingerprint.csv")
abs.dif.index <- read.csv("Difference_Data_Analytical_Indices.csv")
abs.dif.audiosets <- read.csv("Difference_Data_AudioSet_Fingerprint.csv")
accuracy.both <- read.csv("RF_Accuracy_Both.csv")

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
    plot_name <- paste("BA_plt_", index, sep="")
    assign(plot_name,plot8)
  }  else {
    plot <-ggplot(sample, aes(x=compression, y=median)) + 
      geom_errorbar(aes(ymin=lower, ymax=higher), width=.4, size =0.1) +
      ggtitle(index) + 
      theme_minimal() +
      theme(plot.title = element_text(hjust=0.01, vjust =-10)) +
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
data <- data[data$frame.size =="5min",]
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

write.csv(out.file, "Lavernes_test.csv")



##### Impact of Parameter Alteration on Classification Task #####

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

# Maximum Models (fully iteracted and substitute)
max_model <- accuracy_t ~ (log10(file.size) + chunks + frame.size) ^ 2 * index.type
max_model_2 <- accuracy_t ~ log10(file.size)*chunks*frame.size*index.type

# Linear Model (as in early analysis)
mod_lm <- lm(max_model_2, data= dat)
summary.aov(mod_lm)

# Betareg Model
mod_br <- betareg(max_model, data=dat) 
summary(mod_br)

# Looking at Variation: 
var_plot <- bwplot(accuracy ~ frame.size | index.type + chunks, data=dat)
var_plot <- useOuterStrips(var_plot)
print(var_plot)


# Including Index Type and Chunks into the precision component of the model 

mod_br_phi <- update(mod_br,  . ~ .  | index.type * chunks)
AIC(mod_lm, mod_br, mod_br_phi) # shows this model is best

aov_table <- Anova(mod_br_phi)
aov_table
display_html(toString(kable(aov_table, format='html')))


## Lattice Plotting with HexBin

# Using base graphics to plot fits over hexbin

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










