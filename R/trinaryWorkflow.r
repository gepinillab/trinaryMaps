#################################################################
#################################################################
#################################################################
#' Generate trinary maps from species presence, background and model
#'
#' This function takes in the species presence and background data, along with a model, and generates trinary maps. It also calculates the range size and plots the maps and ROC curves.
#'
#' @param pres A data.frame object containing the species presence data
#' @param background A data.frame object containing the background data
#' @param rModel A spatRaster object containing the model data
#' @param NATo0 A logical indicating whether to turn NA values into zeros in the presence and background data
#' @return A list containing the trinary thresholds and trinary rasters
#'
#' @export
trinaryMapWorkflow <- function(pres,
													     background,
													     rModel,
													     NATo0 = TRUE) {
	
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
	threshs <- tryCatch(trinaryROCRoots(ins = ins), error = function(e) e)
	if (inherits(threshs, 'try-error')) {
		message(paste0("Couldn't find roots of the ROC curve; this often happens if ",
		               "you have  very few presence or background points. So you're ",
		               "not getting any trinary maps."))
		return(list(threshs = NULL, trinary.rasters = NULL))
	}
	#== make maps
	trinary.rasters <- trinaryMap(rModel,
														    threshLo = threshs[[1]]$threshLo,
														    threshHi = threshs[[1]]$threshHi,
														    overwrite = TRUE, format = "GTiff",
												 		    datatype = "INT1U", 
														    options = c("COMPRESS=DEFLATE"))
	return(list(threshs = threshs, 
	            trinary.rasters = trinary.rasters))
}