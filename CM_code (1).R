library(lattice)
library(dplyr)
setwd("C:/Users/becky/Desktop/Research/(in Depth - needs to merge)/Compression Master (In Depth Analysis)/Final_Analysis/David's")

#David's
cm.o <- read.csv("Confusion_matrix_data.csv", fileEncoding = "UTF-8-BOM" )

#Mine 
whole.df <- read.csv("Complete_Confusion_Matrix_data.csv", fileEncoding = "UTF-8-BOM" )
cm <- whole.df[whole.df$frame.size == "20min",]
cm <- subset(cm, (Comp %in% c("CBR8","RAW")) & (chunks %in% c("4", "None")))

cm$Time <- ordered(cm$Time, levels=c('Dawn','Midday','Dusk','Midnight','Day'))

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
combn <- list(c('Grassland', 'Logged', 'G-L'), 
			  c('Grassland', 'Primary', 'G-P'),
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

pdf('Time_breakdown_plots.pdf')

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
	
  order <- c("Dawn", "Midday", "Dusk", "Midnight", "Day")
  d <- d %>%
    mutate(Time =  factor(Time, levels = order)) %>%
    arrange(Time) 
  
  
	# Get the plot formula for this variable
	fm <- as.formula(paste0(var, '~ as.numeric(Time)'))

	# create a correctly sized empty plot first, to make it easy to add the 
	# all-day means first and then overlay the 6 hour (type='n')
	plot(fm, data=d, subset= Ind == 'AS' & Time !='Day', 
		 ylim=yl, xlim=xl, xaxt='n', yaxt='n', type='n')

	# Add horizontal lines for the all daily recordings values
	dd <- subset(d, Ind == 'AS' & Time == 'Day', select=var)[,1]
	segments(1,dd,4,dd, col=add.alpha(palette()[1], 0.3), lwd=2)
	dd <- subset(d, Ind == 'AI' & Time == 'Day', select=var)[,1]
	segments(1,dd,4,dd, col=add.alpha(palette()[2], 0.3), lwd=2)

	# Add points and lines (type='o) for Audioset and AI for 6 hours
	points(fm, data=d, subset= Ind == 'AS' & Time !='Day', col=1, type='o')
	points(fm, data=d, subset= Ind == 'AI' & Time !='Day', col=2, type='o')
	
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
	
	if(cs == 'G-P') mtext('Precision', side=2, cex=0.6, line=1.5)
		
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


