#######################################################################
# Script that generates bland altman plots from raw data. I think in
# this one everything is compared to the RAW 20 minute recording (not
# frame size specific)
# 
# 1) Seperates RAW 20min as gold standard 
# 2) Generates ID 
# 3) Finds the distance between each sample and corresponding gold standard
# 4) Creates Bland Altman Plots 
#
# David Orme
# Summer 2020 
#
#


# AUDIOSET processing 
	as <- read.csv('Audiosets_Master_fixed.csv')
	as$Site <- toupper(as$Site)
	ft_names <- sprintf('feat%d', 1:128)
	
	# Get the gold standard recording (20 minutes of uncompressed audio)
	raw <- subset(as, compression=='RAW' & frame.size=="20:00")
	
	# create an ID to match to degraded samples - need to format the simple time
	# stamps for 20 min RAWs (e.g. 201) to match the degraded sample (e.g. 0201)
	# but this is not systematic
	raw$time <- as.numeric(as.character(raw$time))
	raw$raw_id <- sprintf('%s_%s_%04d', raw$Site, raw$Date, raw$time)
	
	# maybe use PCA to find independent axes? Using N-D pythagoras assumes
	# each axis has equal importance, which might not be true
	raw_feat <- subset(raw, select=ft_names)
	hist(apply(raw_feat, 2, max)) # A lot of variation in the range
	ppp <- princomp(raw_feat)
	# Not gone any further with this as yet.
	
	# Now look at the degradation effects
	degraded <- droplevels(subset(as, compression!='RAW'))
	
	# Again get the time stamp and make it systematic
	degr_time <- regmatches(degraded$time, regexpr('^[0-9]+', degraded$time))
	degr_time <- as.numeric(degr_time)
	degraded$raw_id <- with(degraded, sprintf('%s_%s_%04d', Site, Date, degr_time))
	
	# Check raw_ids all match - just one mismatch: "E_2019_2_28_2143"
	setdiff(degraded$raw_id, raw$raw_id)
	degraded$raw_index <- match(degraded$raw_id, raw$raw_id)
	
	# Now calculate the feature differences and hence distance
	degraded$raw_value
	degr_diff <- degraded[, ft_names] - raw[degraded$raw_index, ft_names]
	degraded$ft_distance <- sqrt(rowSums(degr_diff ^2))
	
	# Remove raw feature data
	degraded <- subset(degraded, select=! names(degraded) %in% ft_names)
	
# Acoustic indices processing
	
	ft <- read.csv('six_features_file_size_with_ID.csv')
	
	# Get the gold standard recording (20 minutes of uncompressed audio)
	ftraw <- subset(ft, compression=='RAW' & frame.size=="20min")
	
	# create an ID to match to degraded samples - need to format the simple time
	# stamps for 20 min RAWs (e.g. 201) to match the degraded sample (e.g. 0201)
	# but this is not systematic
	ftraw$time <- as.numeric(as.character(ftraw$time))
	ftraw$raw_id <- sprintf('%s_%s_%04d', ftraw$site, ftraw$date, ftraw$time)
	
	# Now look at the degradation effects
	ftdegraded <- droplevels(subset(ft, compression!='RAW'))
	
	# Again get the time stamp and make it systematic
	degr_time <- regmatches(ftdegraded$time, regexpr('^[0-9]+', ftdegraded$time))
	degr_time <- as.numeric(degr_time)
	ftdegraded$raw_id <- with(ftdegraded, sprintf('%s_%s_%04d', site, date, degr_time))
	
	# Check raw_ids all match - three mismatches: "E_2019_3_1_0244"  "E_2019_3_1_0304"  "E_2019_2_28_2143"
	setdiff(ftdegraded$raw_id, ftraw$raw_id)
	ftdegraded$raw_index <- match(ftdegraded$raw_id, raw$raw_id)
	
	# Get raw values and then the mean and difference for Bland Altman
	ft_names <- c("Aci", "ADI", "Aeev", "Bio", "H", "M", "NDSI")
	raw_values <- ftraw[ftdegraded$raw_index, ft_names]
	ft_mean <- (ftdegraded[, ft_names] + raw_values) / 2
	ft_diff <- ftdegraded[, ft_names] - raw_values
	
	# Package all that data up for the Acoustic Indices
	ftdegraded <- cbind(ftdegraded, raw=raw_values, ft_mean=ft_mean, ft_diff=ft_diff)

# Combine:

	# standardise frame size levels
	# > levels(ftdegraded$frame.size)
	# [1] "10min"  "2_5min" "20min"  "5min"  
	# > levels(degraded$frame.size)
	# [1] "02:30" "05:00" "10:00" "20:00"

	levels(ftdegraded$frame.size) <- c('10:00', '02:30', '20:00', '05:00')
	
	# identify temporal subsamples (using the sample_N id to pair up recordings)
	degraded$sample <- regmatches(degraded$time, regexpr('[0-9]$', degraded$time))
	ftdegraded$sample <- regmatches(ftdegraded$id.no, regexpr('[0-9]$', ftdegraded$id.no))
			
	data <- merge(degraded, ftdegraded, 
				  by=c('raw_id', 'sample', 'frame.size', 'compression'),
				  all=TRUE)

	# Add in an artificial x offset to separate site data on scatterplots
	off <- 0.05
	data$x_off <- with(data, ifelse(Site=='VJR', -off, ifelse(Site=='D', 0, off)))
	data$file.size.off <- with(data, log(file.size) + x_off)

	saveRDS(data, file='combined_audioset_indices.rds')
	
# Bland Altman
	library(hexbin)
	library(lattice)
	library(latticeExtra)

	# Too many points for scatterplots to be sensible so use hexbinplots
	# but it is a pain to put many on one page because hexbinplot uses
	# grid graphics, not base graphics so par(mfrow=) doesn't work. Needs
	# to use the split argument to print.trellis.
	
	for(idx in seq_along(ft_names)){
		# Get each variable mean and difference in turn
		df <- data[, paste0('ft_diff.', ft_names[idx])]
		mn <- data[, paste0('ft_mean.', ft_names[idx])]

		# row and column indices
		rw <- ((idx - 1) %/% 2) + 1
		cl <- ((idx - 1) %% 2) + 1
		mr <- if(idx == 7) FALSE else TRUE

		print(hexbinplot(df ~ mn, main=ft_names[idx], , colorkey=FALSE),
			  split=c(rw,cl,4,2), more=mr)
	}

	# Looking at plots of the raw values against the compressed/shorter samples, 
	# with panels for each combination

	# order the compressions by file size
	ord <- unique(subset(data, select=c(compression, file.size), ! is.na(file.size)))
	comp_by_size <- as.character(ord$compression[order(ord$file.size)])
	data$compression <- factor(data$compression, levels=comp_by_size)
	
	pdf('Acoustic_feature_value_plots.pdf', height=11, width=9, paper='a4')	
	
		for(idx in seq_along(ft_names)){
			# Get the feature name
			this_ft <- ft_names[idx]
			
			# build the formula
			fm <- sprintf('%s ~ raw.%s | compression + frame.size', this_ft, this_ft)
			fm <- as.formula(fm)
			
			# Use a log transformation on the counts - the 02:30 obviously has 8 times as
			# much data as the 20:00 and logging the counts makes it easier to see the
			# patterns despite these data differences. useOuterStrips() moves the panel
			# labels to the margins, rather than above each panel.
			p <- hexbinplot(fm, data=data, colorkey=TRUE, trans=log, inv=exp, main=this_ft)
			p <- useOuterStrips(p)
			print(p)
		}
		
	dev.off()	

	# Bland Altman split by compression and frame size
		
	pdf('Acoustic_feature_BA_panes.pdf', height=11, width=9, paper='a4')	
	
		for(idx in seq_along(ft_names)){
			# Get the feature name
			this_ft <- ft_names[idx]
			
			# build the formula
			fm <- sprintf('ft_diff.%s ~ ft_mean.%s | compression + frame.size', this_ft, this_ft)
			fm <- as.formula(fm)
			
			# Use a log transformation on the counts - the 02:30 obviously has 8 times as
			# much data as the 20:00 and logging the counts makes it easier to see the
			# patterns despite these data differences. useOuterStrips() moves the panel
			# labels to the margins, rather than above each panel.
			p <- hexbinplot(fm, data=data, colorkey=TRUE, trans=log, inv=exp, main=this_ft)
			p <- useOuterStrips(p)
			print(p)
		}
		
	dev.off()	



