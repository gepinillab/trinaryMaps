#################################################################
#################################################################
#################################################################
#' @title Generate trinary maps from species presence, background and model prediction
#' @description This function takes in the species presence and background data, 
#'  along with a model, and generates trinary maps. It also calculates the 
#'  information for ROC curves.
#' @param pres A data.frame object containing the species presence data
#' @param background A data.frame object containing the background data
#' @param rModel A spatRaster object containing the model data
#' @param NATo0 A logical indicating whether to turn NA values into zeros in the
#'  presence and background data
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
#' @return A list containing the trinary thresholds and trinary rasters
#'
#' @export
trinaryMapWorkflow <- function(pres,
													     background,
													     rModel,
													     NATo0 = TRUE,
													     maxTPQuantile = 0.3,
													     sdMultiplier = 2,
													     max.sens = 0.95,
													     smoothMethod = 'binormal') {
	
	p <- terra::extract(rModel, pres, ID = FALSE)
	a <- terra::extract(rModel, background, ID = FALSE)
	p <- p[, 1]
	a <- a[, 1]
	# optionally turn NA into zeros so they contribute to getting absences right. 
	# i chose to do this because bg from neightboring ecoregions are considered 
	# absences for this evaluation
	if (NATo0) { 
		a[is.na(a)] <- 0
		p[is.na(p)] <- 0
	}
	p <- stats::na.omit(p)
	a <- stats::na.omit(a)
	message(paste0(length(p), ' presences and ', length(a), 
	               ' background points used for building trinary maps'))
	ins <- rbind(data.frame(Y = 1, X = p),
	             data.frame(Y = 0, X = a))
	#== fit auc curves
	threshs <- tryCatch(trinaryROCRoots(ins = ins,
	                                    maxTPQuantile = maxTPQuantile,
	                                    sdMultiplier = sdMultiplier,
	                                    max.sens = max.sens), error = function(e) e)
	if (inherits(threshs, 'try-error')) {
		message(paste0("Couldn't find roots of the ROC curve; this often happens if ",
		               "you have  very few presence or background points. So you're ",
		               "not getting any trinary maps."))
		return(list(threshs = NULL, trinary.rasters = NULL))
	}
	#== make maps
	trinary.rasters <- trinaryMap(rModel,
														    thr.roc.lo = threshs[[1]]$thr.roc.lo,
														    thr.roc.hi = threshs[[1]]$thr.roc.hi,
														    overwrite = TRUE, format = "GTiff",
												 		    datatype = "INT1U", 
														    options = c("COMPRESS=DEFLATE"))
	return(list(threshs = threshs, 
	            trinary.rasters = trinary.rasters))
}
