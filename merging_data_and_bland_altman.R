#######################################################################
# Script that generates bland altman plots from raw data. I think in
# this one everything is compared to the RAW 
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

	audioset <- read.csv('Audiosets_Master_fixed.csv')
	
	# standardise names
	audioset$Site <- toupper(audioset$Site)
	
	# Get and standardize IDs to match up
	# - need to format the simple time stamps for 20 min RAWs (e.g. 201) to 
	#   match the degraded sample (e.g. 0201) but this is not systematic
	# - need to get the sequence number for subsamples
	audioset$starttime <- regmatches(audioset$time, regexpr('^[0-9]+', audioset$time))
	audioset$starttime <- sprintf('%04s', audioset$starttime)
	audioset$sample <- ifelse(grepl('sample', audioset$time), regmatches(audioset$time, regexpr('[0-9]$', audioset$time)), 0)
	audioset$id <- with(as, sprintf('%s_%s_%04s_%s_%s', Site, Date, starttime, frame.size, sample))
	
	# **EXPERIMENTAL** - converting indices to empirical CDF quantiles.
	as_names <- paste0('feat', 1:128)

	for(each_ft in as_names){
		# Odd code - ECDF creates a function based on X, which we then use to find the quantile of each value within it.
		audioset[,each_ft] <- ecdf(audioset[,each_ft])(audioset[,each_ft])
	}
		
	# Split the raw from the degraded recordings to match in gold standard
	raw_as <- subset(as, compression=='RAW')
	
	# Now find both the full gold standard recording (20 minutes of uncompressed 
	# audio) and the specific raw
	audioset$raw_id_full <- with(audioset, sprintf('%s_%s_%04s_20:00_0', Site, Date, starttime))
	audioset$raw_id_fsize <- with(audioset, sprintf('%s_%s_%04s_%s_%s', Site, Date, starttime, frame.size, sample))
	
	# Check matching
	# - just one mismatch on the full id: "E_2019_2_28_2143"
	setdiff(audioset$raw_id_full, raw_as$id)
	# a few more on the frame size specific ones:
	# "E_2019_2_28_2143_20:00_0" "D_2019_2_26_0840_02:30_8" "D_2019_2_26_0840_02:30_9"
	setdiff(audioset$raw_id_fsize, raw_as$id)
	
		
	# Now calculate the feature differences and hence distance
	degr_diff <- audioset[, as_names] - raw_as[match(audioset$raw_id_full, raw_as$id), as_names]
	audioset $ft_distance_full <- sqrt(rowSums(degr_diff ^2))
	degr_diff <- audioset[, as_names] - raw_as[match(audioset$raw_id_fsize, raw_as$id), as_names]
	audioset$ft_distance_fsize <- sqrt(rowSums(degr_diff ^2))
	
	# Remove raw feature data
	audioset <- subset(audioset, select=! names(audioset) %in% as_names)
	
# Acoustic indices processing
	
	ft <- read.csv('six_features_file_size_with_ID.csv')
	
	# > levels(ft$frame.size)
	# [1] "10min"  "2_5min" "20min"  "5min"  
	levels(ft$frame.size) <- c('10:00', '02:30', '20:00', '05:00')

	ft$starttime <- regmatches(ft$time, regexpr('^[0-9]+', ft$time))
	ft$starttime <- sprintf('%04s', ft$starttime)
	ft$sample <- ifelse(grepl('sample', ft$time), regmatches(ft$time, regexpr('[0-9]$', ft$time)), 0)
	ft$id <- with(ft, sprintf('%s_%s_%04s_%s_%s', site, date, starttime, frame.size, sample))
		
	# **EXPERIMENTAL** - converting indices to empirical CDF quantiles.
	ft_names <- c("Aci", "ADI", "Aeev", "Bio", "H", "M", "NDSI")

	for(each_ft in ft_names){
		# Odd code - ECDF creates a function based on X, which we then use to find the quantile of each value within it.
		ft[,each_ft] <- ecdf(ft[,each_ft])(ft[,each_ft])
	}
	
	# Get the raw benchmarks
	raw_ft <- subset(ft, compression=='RAW')
	
	# Now find both the full gold standard recording (20 minutes of uncompressed 
	# audio) and the specific raw
	ft$raw_id_full <- with(ft, sprintf('%s_%s_%04s_20:00_0', site, date, starttime))
	ft$raw_id_fsize <- with(ft, sprintf('%s_%s_%04s_%s_%s', site, date, starttime, frame.size, sample))
	
	# Check matching
	setdiff(ft$raw_id_full, raw_ft$id)
	# - A few mismatches on the full id: 
	# [1] "E_2019_3_1_0244_20:00_0"  "E_2019_3_1_0304_20:00_0"  "E_2019_2_28_2143_20:00_0"
	setdiff(ft$raw_id_fsize, raw_ft$id)
	# a few more on the frame size specific ones:
	# [1]  "E_2019_3_1_0244_20:00_0"    "E_2019_3_1_0304_20:00_0"    "D_2019_2_28_0244_02:30_7" 
	#      "VJR_2019_3_1_0406_02:30_6"  "E_2019_3_2_0626_02:30_1"    "VJR_2019_2_28_0724_02:30_3"
	# [7]  "D_2019_3_1_0947_02:30_2"    "E_2019_2_28_1603_02:30_4"   "D_2019_2_27_1643_02:30_5" 
	#      "D_2019_3_1_1047_05:00_0"    "D_2019_3_1_1047_05:00_1"    "D_2019_3_1_1047_05:00_2"   
	# [13] "D_2019_3_1_1047_05:00_3"    "D_2019_3_1_1107_05:00_0"    "D_2019_3_1_1107_05:00_1"  
	#      "D_2019_3_1_1107_05:00_2"    "D_2019_3_1_1107_05:00_3"    "VJR_2019_2_27_1943_02:30_1"
	# [19] "E_2019_2_28_2143_20:00_0"  
	
	# Get raw values and then the mean and difference for Bland Altman
		
	raw_full <- raw_ft[match(ft$raw_id_full, raw_ft$id), ft_names]
	ft_mean_full <- (ft[, ft_names] + raw_full) / 2
	ft_diff_full <- ft[, ft_names] - raw_full
	raw_fsize <- raw_ft[match(ft$raw_id_fsize, raw_ft$id), ft_names]
	ft_mean_fsize <- (ft[, ft_names] + raw_fsize) / 2
	ft_diff_fsize <- ft[, ft_names] - raw_fsize
	
	# Package all that data up for the Acoustic Indices
	ft <- cbind(ft, raw_full=raw_full, raw_fsize=raw_fsize, 
				ft_mean_full=ft_mean_full, ft_diff_full=ft_diff_full,
				ft_mean_fsize=ft_mean_fsize, ft_diff_fsize=ft_diff_fsize)

# Combine:

	# drop duplicate columns
	ft <- subset(ft, select=! names(ft) %in% c('frame.size', 'time','date', 
				 'start.time', 'sample','id', 'raw_id_full'))	
	data <- merge(audioset, ft, by=c('raw_id_fsize', 'compression'), all=TRUE)

	# Add in an artificial x offset to separate site data on scatterplots
	off <- 0.05
	data$x_off <- with(data, ifelse(Site=='VJR', -off, ifelse(Site=='D', 0, off)))
	data$file.size.off <- with(data, log(file.size) + x_off)

	# order the compressions by file size
	ord <- unique(subset(data, select=c(compression, file.size), ! is.na(file.size)))
	comp_by_size <- as.character(ord$compression[order(ord$file.size)])
	data$compression <- factor(data$compression, levels=comp_by_size)
	
	saveRDS(data, file='combined_audioset_indices_ECDF.rds')

# PLOTS
	
	# Looking at scaling of Audioset features:
	# - TODO think about whether the magnitude of the feature values has meaning or not
	#   because this has huge implications for the distance measure.
	# - The features are all extremely skew, so really hard to figure out what to do here.
	
	audioset_raw_full_features <- subset(raw_as, select=as_names, frame.size=='20:00')
	audioset_feature_span <- sapply(audioset_raw_full_features, function(x) diff(range(x)))
	audioset_feature_sd <- sapply(audioset_raw_full_features, sd)
	plot(audioset_feature_sd ~ audioset_feature_span)	
	
	# Looking at compression and framesize effects
	# Too many points for scatterplots to be sensible so use hexbinplots
	# but it is a pain to put many on one page because hexbinplot uses
	# grid graphics, not base graphics so par(mfrow=) doesn't work. Needs
	# to use the split argument to print.trellis.

	library(hexbin)
	library(lattice)
	library(latticeExtra)
	
	# 1) Looking at effects of comparison to gold standard (full 20 minute raw audio) or the
	# matching sample of that raw for reduced frame size
	
	pdf('full_vs_fsize_raw_as_reference.pdf', width=8, height=11, paper='a4')
	
		parset <- list(layout.widths=list(left.padding=0, right.padding=0), 
		        layout.heights=list(top.padding=0, bottom.padding=0))
		

		# Can't do this easily for audioset, because there is no single value
		# until we figure out what to do with the 128 dimensions
		# p <- hexbinplot(ft_distance_fsize ~ ft_distance_full | Site + frame.size, data=data, 
						 # main='Audioset', trans=log, inv=exp, colorkey=FALSE,
						 # par.settings=parset)
		# print(useOuterStrips(p))

		for(each_ft in ft_names){
			
			# Get each variable mean and difference in turn
			fm <- as.formula(sprintf('raw_fsize.%s ~ raw_full.%s | Site + frame.size', 
									 each_ft, each_ft))
	
			p <- hexbinplot(fm, data=data, 
						    main=each_ft, trans=log, inv=exp, colorkey=FALSE,
						    par.settings=parset)
			print(useOuterStrips(p))

		}
	dev.off()

	# 2) Bland Altman

	for(idx in seq_along(ft_names)){

		fm <- as.formula(sprintf('ft_diff_fsize.%s ~ ft_mean_fsize.%s', 
									 ft_names[idx], ft_names[idx]))

		# row and column indices
		rw <- ((idx - 1) %/% 2) + 1
		cl <- ((idx - 1) %% 2) + 1
		mr <- if(idx == 7) FALSE else TRUE

		print(hexbinplot(fm, main=ft_names[idx], data=data, colorkey=FALSE),
			  split=c(rw,cl,4,2), more=mr)
	}

	# Looking at plots of the raw values against the compressed/shorter samples, 
	# with panels for each combination
	
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
		
	pdf('Acoustic_feature_BA_panes.pdf', height=8, width=11, paper='a4r')	
	
		for(idx in seq_along(ft_names)){
			# Get the feature name
			this_ft <- ft_names[idx]
			
			# build the formula
			fm <- sprintf('ft_diff_fsize.%s ~ ft_mean_fsize.%s | compression + frame.size', this_ft, this_ft)
			fm <- as.formula(fm)
			
			# Use a log transformation on the counts - the 02:30 obviously has 8 times as
			# much data as the 20:00 and logging the counts makes it easier to see the
			# patterns despite these data differences. useOuterStrips() moves the panel
			# labels to the margins, rather than above each panel.
			p <- hexbinplot(fm, data=data, colorkey=TRUE, trans=log, inv=exp, main=this_ft, 
							xlim=c(0,1), ylim=c(-1,1), aspect=2, ybnds=c(-1,1))
			p <- useOuterStrips(p)
			print(p)
		}
		
	dev.off()	


	# Bland Altman split by compression and frame size using the original recording values
	# on the X axis, not the mean. Side by side comparisons to the full 20 minute RAW and
	# to the appropriate RAW subframe.
		
	pdf('Acoustic_feature_BA_panes_using_RAW_standard.pdf', height=8, width=11, paper='a4r')	
	
		for(idx in seq_along(ft_names)){
			# Get the feature name
			this_ft <- ft_names[idx]
			
			# build the formulae
			fmf <- sprintf('ft_diff_full.%s ~ raw_full.%s | compression + frame.size', this_ft, this_ft)
			fmf <- as.formula(fmf)
			
			fms <- sprintf('ft_diff_fsize.%s ~ raw_fsize.%s | compression + frame.size', this_ft, this_ft)
			fms <- as.formula(fms)
			
			fm <- c(fmf, fms)
			mr <- c(TRUE, FALSE)
			
			for(each_st in 1:2){
				# Use a log transformation on the counts - the 02:30 obviously has 8 times as
				# much data as the 20:00 and logging the counts makes it easier to see the
				# patterns despite these data differences. useOuterStrips() moves the panel
				# labels to the margins, rather than above each panel.
				p <- hexbinplot(fm[[each_st]], data=data, colorkey=TRUE, trans=log, inv=exp, main=this_ft, 
							xlim=c(0,1), ylim=c(-1,1), aspect=2, ybnds=c(-1,1))
				p <- useOuterStrips(p)
				print(p, split=c(each_st, 1, 2, 1), more=mr[each_st],)
			}
		}
		
	dev.off()	







