#################################################################
#################################################################
#################################################################
#' @title Find upper/lower thresholds from ROC curves
#'
#' @description Fit a smoothed ROC curve, find bounds for threshold and report 
#'  partial AUC statistics
#' @details
#' See Examples.
#' @param ins A data frame where the first column contains the the observed 
#' binary outcomes (e.g., 0 for absence and 1 for presence) and the second column 
#' contains predictor values (e.g., predicted probabilities/suitability). This 
#' input is used to calculate the ROC curve, derive thresholds, and assess model 
#' performance.
#' @param maxTPQuantile A numeric value representing the quantile of the predicted 
#' values at presence locations used to determine the upper threshold for 
#' presence predictions. For example, a value of `0.3` means the function 
#' uses the 30th percentile of the predicted presence values as the upper 
#' threshold, ensuring that the model maintains at least 70% 
#' sensitivity. Default is `0.3`.
#' @param sdMultiplier A numeric value that controls the adjustment of the lower 
#' threshold for presence predictions. It multiplies the standard deviation of 
#' the predicted values at presence locations to define this lower threshold. 
#' If this threshold results in a negative value, it is adjusted to zero 
#' by default. Default is `2`.
#' @param max.sens value of sensitivity to use if no upper limit is found based 
#'  on derivatives. default is 0.95
#' @param smoothMethod default is 'binormal'
#' @param ... options to be passed to `pROC::smooth`
#' @importFrom stats approx complete.cases lag quantile sd
#' @return a data.frame
#' @author Cory Merow <cory.merow@@gmail.com>
#' @export
trinaryROCRoots <- function(ins,
 												    max.sens = 0.95,
 												    smoothMethod = 'binormal',
 												    sdMultiplier = 2,
 												    maxTPQuantile = .3,
 												    ...) {
  out <- try({
	  print(0)
    #== generate a smooth curve so i can take derivatives
	  a.rough <- pROC::roc(ins[,1], ins[,2], quiet = TRUE)
	  print(1)
	  a <- try(pROC::smooth(a.rough,method = smoothMethod, n = 1024,...), silent = TRUE)
	  print(2)
	  # i used this default because its the pROC package defualt so i assumed it
	  # was the best. if it breaks, try the next one test for 'hooked' curved due 
	  # to a smoothing issue
	  fail <- ifelse(inherits(a, 'try-error'), TRUE,
	                 any(rev(a$sensitivities) - lag(rev(a$sensitivities)) < 0,
	                     na.rm = TRUE))
	  print(3)
	  if (fail) { #second case is a known issue from pROC::smooth
		  smoothMethod = 'density' # ensures its used below too
		  a = pROC::smooth(a.rough,method = smoothMethod, ...)
		  message(paste0("Used method=density for ROC smoothing because your ",
		                 "selected method (set by argument smoothMethod) broke. If ",
		                 "you're unhappy about this, see other options for ",
		                 "methods in ?pROC::smooth."))
	  }
	  print(4)
	  #== youden index 
	  youden <- a$specificities + a$sensitivities - 1
	  best.youden <- which.max(youden)
	  y.youden <- a$sensitivities[best.youden]
	  x.youden <- (1 - a$specificities)[best.youden]
		#== take the smallest value above the threshold. this ensures that you 
	  #== choose an actual threshold (and not -Inf) if the AUC is perfect
	  threshYouden <- rev(a.rough$thresholds)[(findInterval(x.youden, 
	                                                        rev(1 - a.rough$specificities)) + 1)]
	  #== coords for ROC 
	  xx <- rev(1-a$specificities)
	  y <- rev(a$sensitivities)
	  #== catch failed derivatives and use a special case
	  print(5)
	  if (a.rough$auc > .999) {
	    message(paste0("The AUC is too close to 1 to take the derivatives needed ",
		                 "to find   reasonable trinary thresholds. Returning results ",
		                 "using the minimum predicted value at training presence as ", 
		                 "the lower threshold and the 30% training presence quantile",
		                 " as the upper threshold. The max(sensitivity+specificity)) ",
		                 "was still calculated as usual, but note that it may not be ",
		                 "between these upper and lower bounds for very small sample ",
		                 "sizes."))
		#== prep outputs
		#== make the low value the minimum value at a predicted presence - 2sd (since 
	  #== smale sample sizes end up just predicting the presence points are the 
	  #== only places occupied). 2 sd chosen because ...
	  sd1 <- sd(ins$X[ins$Y==1])
		threshLo <- min(ins$X[ins$Y==1]) - sdMultiplier * sd1
		if (threshLo < 0) {
			message(paste0("The value of `sdMultiplier` you used lead to a negative ",
			               "threshold. I'm changing it to zero by default (so your ",
			               "lower threshold is the minimim value at training presences)"))
			threshLo <- min(ins$X[ins$Y == 1])
		}
		lo.thresh.roc.x <- (sum(ins$X[ins$Y == 0] >= threshLo) / length(ins$X[ins$Y == 0]))
		lo.thresh.roc.y <- 1
		#== make the hi value the .3 quantile of predicted presence, cuz you'd never 
		#== want a model with <70% sensitivity
		threshHi <- quantile(ins$X[ins$Y == 1], maxTPQuantile)
		hi.thresh.roc.y <- sum(ins$X[ins$Y == 1] >= threshHi) / length(ins$X[ins$Y == 1])
		hi.thresh.roc.x <- (sum(ins$X[ins$Y==0] > threshHi) / length(ins$X[ins$Y == 0]))
			# I think these shouldnb't be reported cuz the inverse wasn't calculated
		# y.lo.inv=1-x.lo 
		# x.lo.inv=1-y.lo
		out1 <- data.frame(lo.thresh.roc.x = lo.thresh.roc.x,
										   lo.thresh.roc.y = lo.thresh.roc.y, 		
										   threshLo = threshLo,			
										   youden.thresh.roc.x = x.youden,
										   youden.thresh.roc.y = y.youden, 
										   threshYouden = threshYouden, 
										   hi.thresh.roc.x = hi.thresh.roc.x, 
										   hi.thresh.roc.y = hi.thresh.roc.y,
										   threshHi = threshHi, 
										   y.lo.inv = NA,
										   x.lo.inv = NA,
										   trinary.pauc = as.numeric(a.rough$auc))
		plotThings <- list(xx = xx, y = y, y. = NULL, y.. = NULL, xx1 = NULL,
	                     y1 = NULL, y1.. = NULL, xout = NULL, x1out = NULL)
		return(list(trinaryDF = out1, plotThings = plotThings))
	}
	  print(6)
	#== derivatives
	xx. <-  .middle_pts(xx)
	xx.. <-  .middle_pts(.middle_pts(xx))
	xx... <-  .middle_pts(.middle_pts(.middle_pts(xx)))

	y.r <- .deriv(xx, y)
	y..r <- .deriv(xx., y.r)
	y...r <- .deriv(xx.., y..r)
	y....r <- .deriv(xx..., y...r)
	print(7)
	#== make the derivatives real functions so they can be evaluated at the x points (e.g. for curvature)
	xout=seq(0,1,length=200) 
	y.=suppressWarnings(approx(xx.,y.r,xout=xout,method='linear')$y)
	y..=try(approx(xx..,y..r,xout=xout,method='linear')$y,silent=TRUE)
	y...=try(approx(xx...,y...r,xout=xout)$y,silent=TRUE)
	y....=try(approx(.middle_pts(xx...),y....r,xout=xout)$y,silent=TRUE)
	print(8)
	#== remove NAs 
	keep=complete.cases(y..r)
	y..1=y..r[keep]
	xx..1=xx..[keep]
	
	#== if derivatives are possible...
	#== get locations of sign changes of the logmod of the second derivative
	(switches=cumsum(rle(sign(logmod(y..1)))[[1]]))
	#-- remove  nans
	switches[which(switches==length(y..1))]=NA
	switches=stats::na.omit(switches)
	print(9)
		#-- since y is evaluated at the midpoint of the xs get the midpoint...
	if(length(switches)>0 & any(switches>best.youden)){
		x.as=(xx..1[switches]+xx..1[switches+1])/2
		#### x.as=xout[switches]
		keep=which(x.as>x.youden)[1]
		if(all(is.na(keep))) {
			x.as=x.youden
			message(paste0("Couldn't find a better upper limit than the Youden ",
			               "threshold, so defaulting to using that for the upper limit. ",
			               "This can happen if the Youden threshold gives perfect ",
			               "sensitivity (i.e. there are no sensitivity gains possible ",
			               "by lowering the threshold)"))
		} else {
			x.as=x.as[keep[1]] # this finds the midpoint, need to find the left interval
		}
		
		x.ind=findInterval(x.as,xx)
		y.as=y[x.ind]

	} else {
		#-- if no asymptote reached, use max sens
		y.as=max.sens
		not.root.index=findInterval(max.sens,y)
		x.as=xx[not.root.index]
	}
	print(10)
	#== prep for COR (inverse ROC) to find asymptote
	y1=1-xx
	xx1=1-y # plot(xx,y,type='l'); plot(xx1,y1,type='l')
	xx1.=.middle_pts(xx1)
	xx1..=.middle_pts(.middle_pts(xx1))
	xx1...=.middle_pts(.middle_pts(.middle_pts(xx1)))
	x1out=seq(0,1,length=200)
	y1.r= .deriv(xx1, y1) # plot(xx1.,y1.r,type='l')
	y1..r <- .deriv(xx1., y1.r)# plot(xx1..,y1..r,type='l')
		# need to turn NaNs in the middle (leading and trailing don't hurt) 
	  # into +/- Infs so that approx() can proceed below. I'm going to replace NaNs
	  # with the last value that wasn't NaN before them, since this it just an 
	  # issue due to numerical overflow, and the NaN conceptually can just be +/-Inf. 
	  # choosing the last value before the NaNs started ensures that no extra 
	  # witches will be introduced. also doesn't like Infs, so replacing them with
	  # the largest/smallest numbers
	y1..r=ifelse(is.nan(y1..r),stats::lag(y1..r,1),y1..r)
	print(11)
	print(y1..r)
	while(any(is.nan(y1..r))){ y1..r=ifelse(is.nan(y1..r),lag(y1..r,1),y1..r) }
	print(12)
	y1..r[y1..r==-Inf]=.Machine$double.xmin
	y1..r[y1..r==Inf]=.Machine$double.xmax
	# 	check result
	# data.frame(y1..r,,b=ifelse(is.nan(y1..r),lag(y1..r,1),y1..r),a)
	y1...r <- .deriv(xx1.., y1..r) # plot(xx1...,y1...r,type='l')
	y1....r <- .deriv(xx1..., y1...r)
	y1.=suppressWarnings(approx(xx1.,y1.r,xout=xout)$y) # plot(y1.,type='l')
	y1..=suppressWarnings(approx(xx1..,y1..r,xout=xout)$y) # plot(y1..,type='l'); plot(xx1..,y1..r,type='l')
	y1...=suppressWarnings(approx(xx1...,y1...r,xout=xout)$y)
	y1....=suppressWarnings(approx(.middle_pts(xx1...),y1....r,xout=xout)$y)
	####keep=complete.cases(y1..)
	####y1..1=y1..[keep]
	keep=complete.cases(y1..r)
	y1..1=y1..r[keep]
	(switches=cumsum(rle(sign(logmod(y1..1)))[[1]]))
	#-- remove  nans
	switches[which(switches==length(y1..1))]=NA
	switches=stats::na.omit(switches)
	
		#-- since y is evaluated at the midpoint of the xs get the midpoint...
	#if(length(switches)>0 & any(switches>best.youden)){
	x.lo.inv=min((xx1..[switches]+xx1..[switches+1])/2)
	#### x.lo=max(xout[switches]) #- not sure this will generally work 
	#x.lo=x.lo[which(x.lo<y.lo)[1]] # just hoping this is ok
	
	x.ind=findInterval(x.lo.inv,rev(xx1))
	y.lo.inv=rev(y1)[x.ind]

	#} else {
	# 		#-- if no asymptote reached, use max sens
	# commented because hopefully not an issue
	# 		y.as=max.sens
	# 		not.root.index=findInterval(max.sens,y)
	# 		x.as=xx[not.root.index]
		#}
		# plot(xx1,y1,type='l')
	# 	abline(h=y.lo)
	# 	abline(v=x.lo)
	# 	plot(x1out,y1.,type='l',log='y')
	# 	plot(x1out,y1..,type='l',log='y')
	
	x.lo=1-y.lo.inv
	y.lo=1-x.lo.inv
  print(7)
	# this smoothing is just used to get the value of the pAUC, not the actual thresholds
	a.pauc <- try(pROC::roc(ins[,1], ins[,2], auc = TRUE, 
	                        partial.auc = 1 - c(x.lo, x.as), 
	                        partial.auc.focus = 'specificity', 
	                        partial.auc.correct = TRUE, quiet = TRUE,
	                        smoothMethod = smoothMethod), silent = TRUE)
	print(8)
	# try different smooth method if it breaks
	if (inherits(a.pauc, 'try-error')) {
	  a.pauc <- try(pROC::roc(ins[,1], ins[,2], auc = TRUE,
	                          partial.auc = 1 - c(x.lo, x.as), 
	                          partial.auc.focus = 'specificity', 
	                          partial.auc.correct = TRUE, smooth.method = 'density',
	                          quiet = TRUE), silent = TRUE)
	}
	print(9)
	#if(class(a.pauc)=='try-error') a.pauc=list(auc=NA)
	
	#== find thresholds 
	#-- smoothing the auc, which was needed for derivativies, doesn't give you 
	#-- thresholds associated with the prediction (it just smooths the ROC curve, 
	#-- and there are no underlying threshold values associated with this smooth) 
	#-- so i ask for the data threshold associated with the smoothed curve. It can
	#-- happen that when there are few points, the same threshold is associated 
	#-- with different hi, youden, low estimates that came from smoothing. This  
	#-- means that all the maps are the same.
	#-- cm: 9/3/21 i canned this approach and used the smoothed curves because in 
	#-- cases where there are big jumps between data points or AUC ~1, you don't 
	#-- get any differences in the estimated threshold. 
	#-- CM 6/24/22: 9/3 me was stupid. i don't think i ever canned this approach 
	#-- because its not possible to get thresholds from the smoothed curve. at 
	#-- least not without smoothing the thresholds which requires me to do it manually.
	#a.rough=pROC::roc(ins[,1], ins[,2],quiet=T)
	#if(class(a.pauc)=='try-error') a.pauc=a.rough
	threshLo=rev(a.rough$thresholds)[findInterval(x.lo, rev(1-a.rough$specificities))]
	threshYouden=rev(a.rough$thresholds)[findInterval(x.youden, rev(1-a.rough$specificities))]
	threshHi=rev(a.rough$thresholds)[findInterval(x.as, rev(1-a.rough$specificities))]
	
	#== prep outputs
	out1=data.frame(lo.thresh.roc.x=x.lo,lo.thresh.roc.y=y.lo, 
									threshLo=threshLo,			
									youden.thresh.roc.x=x.youden, youden.thresh.roc.y=y.youden,
									threshYouden=threshYouden,
									hi.thresh.roc.x=x.as, hi.thresh.roc.y=y.as,
									threshHi=threshHi,
									y.lo.inv=y.lo.inv,   x.lo.inv=x.lo.inv,
									trinary.pauc=as.numeric(a.pauc$auc))
	plotThings=list(xx=xx,y=y,y.=y.,y..=y..,xx1=xx1,y1=y1,x1out=x1out, y1..=y1..,
	                xout=xout,x1out=x1out)
	
	list(trinaryDF=out1,plotThings=plotThings)
  })
	return(out)
}



#################################################################
#################################################################
#################################################################
#' @title Make trinary maps 
#'
#' @description Use previously calculated thresholds to make trinary maps
#' @param rModel spatRaster representing continuous model predictions
#' @param threshLo lower threshold value; typically determined from the 
#' output of `trinaryROCRoots()`
#' @param threshHi upper threshold value; typically determined from the 
#' output of `trinaryROCRoots()`
#' @param rasterOutputPath optional file name to write out a raster.
#' @param ... optional arguments to pass to `terra::writeRaster`
#' @return a data.frame
#' @author Cory Merow <cory.merow@@gmail.com>
#' @export

# should add the option to use a map in memory
trinaryMap <- function(rModel,
									   	 threshLo,
										   threshHi,
										   rasterOutputPath = NULL,
										   ...
										   ) {
										
	out <- try({	
	  modelNames <- names(rModel)
	  #== make trinary maps
	  trinary.rasters <- terra::rast(lapply(1:(terra::nlyr(rModel)),
												function(x) { 
													 out1 <- rModel[[x]] >= threshLo
													 out2 <- rModel[[x]] >= threshHi
													 out3 <- out1 + out2
													 names(out3) <- paste0(modelNames[x], '_', 
													                       round(threshLo, 2), '_', 
													                       round(threshHi, 2))
													 return(out3)
											})) 
									 
	 #== write results
	if (!is.null(rasterOutputPath)){
		 lapply(1:(terra::nlyr(trinary.rasters)), function(x) {
			 rf <- suppressWarnings(terra::writeRaster(trinary.rasters[[x]], 
												 filename = rasterOutputPath[x], ...))
		 })
	  }
	  return(trinary.rasters)
	})
	return(out)
}

#################################################################
#################################################################
#################################################################
#' @title Calculate upper and lower limits of range size
#' @description Size limits based on trinary thresholds
#' @param trinaryRasters a spatRaster describing a trinary map. It is assumed that 
#'  values of 0 are absent, values of 1 represent the lower bound, and values 
#'  of 2 represent the upper bound (e.g., as determined by `trinaryROCRoots`())
#' @param otherBinaryRaster optional additional binary raster for which range 
#'  size is desired. This can be useful, e.g., if you have an additional 
#'  intermediate threshold of interest (e.g., the max (sensitivity +specificity)).
#' @return a data.frame with the lower, upper, and optimal (Youden threshold) range size
#' @author Cory Merow <cory.merow@@gmail.com>
#' @export
trinaryRangeSize <-  function(trinaryRasters,
                              otherBinaryRaster = NULL) {
  
  cell.size <- prod(terra::res(trinaryRasters) / 1e3)
	
	range.size <-  cell.size * data.frame( 
				range.size.lo.km2 = sum(terra::values(trinaryRasters) > 1, na.rm = TRUE),
	 			range.size.hi.km2 = sum(terra::values(trinaryRasters) > 0, na.rm = TRUE),
	 			range.size.youden.km2 = ifelse(!is.null(otherBinaryRaster), 
	 			                               sum(terra::values(otherBinaryRaster) > 0,
	 			                                   na.rm = TRUE), NA))
	return(range.size)
}
	


