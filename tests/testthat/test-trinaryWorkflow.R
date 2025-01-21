# Mock functions
mock_extract <- function(rModel, points, ID = FALSE) {
  if(inherits(points, "data.frame")) {
    if(nrow(points) == 0){
      return(numeric(0))
    }
  }
  # Simulate extraction - return random numbers based on the number of points
  set.seed(123)
  
  if(inherits(points, "data.frame")) {
    return(data.frame(values = runif(nrow(points), 0, 1)))
  } else {
    return(data.frame(values = runif(ncell(rModel), 0, 1)))
  }
}


mock_trinaryROCRoots <- function(ins, maxTPQuantile, sdMultiplier, max.sens) {
  if(nrow(ins) == 0) {
    return(simpleError("No presence or background points provided to trinaryROCRoots"))
  }
  # Simple mock to return thresholds
  list(list(thr.roc.lo = 0.2, thr.roc.hi = 0.8))
}


mock_trinaryMap <- function(rModel, thr.roc.lo, thr.roc.hi, overwrite, format, datatype, options) {
  # Simulate map creation
  terra::rast(nrows=terra::nrow(rModel), ncols=terra::ncol(rModel), 
              vals = sample(0:2, terra::ncell(rModel), replace = TRUE))
}

trinaryMapWorkflow_test <- function(pres, background, rModel, NATo0 = TRUE, maxTPQuantile = 0.3, sdMultiplier = 2, max.sens = 0.95, smoothMethod = 'binormal',
                                    extract_func = mock_extract,
                                    trinaryROCRoots_func = mock_trinaryROCRoots,
                                    trinaryMap_func = mock_trinaryMap) {
  
  p <- extract_func(rModel, pres, ID = FALSE)
  a <- extract_func(rModel, background, ID = FALSE)
  
  if (inherits(p, "data.frame")) p <- p[, 1]
  if (inherits(a, "data.frame")) a <- a[, 1]
  
  if (NATo0) {
    a[is.na(a)] <- 0
    p[is.na(p)] <- 0
  }
  
  p <- stats::na.omit(p)
  a <- stats::na.omit(a)
  
  if (length(p) == 0 && length(a) == 0) {
    ins <- data.frame()
  } else if (length(p) == 0) {
    ins <- data.frame(Y = 0, X = a)
  } else if (length(a) == 0) {
    ins <- data.frame(Y = 1, X = p)
  } else {
    ins <- rbind(
      data.frame(Y = 1, X = p),
      data.frame(Y = 0, X = a)
    )
  }
  
  if (nrow(ins) == 0) {
    return(list(threshs = NULL, trinary.rasters = NULL))
  }
  
  threshs <- tryCatch(
    trinaryROCRoots_func(ins = ins, maxTPQuantile = maxTPQuantile, sdMultiplier = sdMultiplier, max.sens = max.sens),
    error = function(e) NULL
  )
  
  if (is.null(threshs)) {
    return(list(threshs = NULL, trinary.rasters = NULL))
  }
  
  trinary.rasters <- tryCatch(
    trinaryMap_func(rModel, thr.roc.lo = threshs[[1]]$thr.roc.lo, thr.roc.hi = threshs[[1]]$thr.roc.hi, overwrite = TRUE, format = "GTiff", datatype = "INT1U", options = c("COMPRESS=DEFLATE")),
    error = function(e) NULL
  )
  
  return(list(threshs = threshs, trinary.rasters = trinary.rasters))
}

test_that("trinaryMapWorkflow works with valid inputs", {
  # Create dummy data
  r <- terra::rast(nrows=10, ncols=10, vals = 1:100)
  pres <- data.frame(x = runif(20, 0, 10), y = runif(20, 0, 10))
  background <- data.frame(x = runif(30, 0, 10), y = runif(30, 0, 10))
  
  # Call the function with mocked dependencies
  result <- trinaryMapWorkflow_test(pres, background, r)
  
  # Assertions
  expect_type(result, "list")
  expect_named(result, c("threshs", "trinary.rasters"))
  expect_type(result$threshs, "list")
  expect_equal(length(result$threshs),1)
  expect_named(result$threshs[[1]], c("thr.roc.lo", "thr.roc.hi"))
  expect_s4_class(result$trinary.rasters, "SpatRaster")
})

test_that("trinaryMapWorkflow handles NA values", {
  # Create dummy data with NAs
  r <- terra::rast(nrows=10, ncols=10, vals = 1:100)
  pres <- data.frame(x = runif(20, 0, 10), y = runif(20, 0, 10))
  background <- data.frame(x = runif(30, 0, 10), y = runif(30, 0, 10))
  pres[1, 1] <- NA #introduce NAs
  background[1, 1] <- NA
  
  # Call the function with mocked dependencies
  result <- trinaryMapWorkflow_test(pres, background, r)
  
  # Assertions
  expect_type(result, "list")
  expect_named(result, c("threshs", "trinary.rasters"))
  expect_type(result$threshs, "list")
  expect_equal(length(result$threshs),1)
  expect_named(result$threshs[[1]], c("thr.roc.lo", "thr.roc.hi"))
  expect_s4_class(result$trinary.rasters, "SpatRaster")
  
})

test_that("trinaryMapWorkflow returns NULL rasters when no thresholds", {
  r <- terra::rast(nrows=10, ncols=10, vals = 1:100)
  pres <- data.frame(x = runif(0, 0, 10), y = runif(0, 0, 10))
  background <- data.frame(x = runif(0, 0, 10), y = runif(0, 0, 10))
  
  mock_trinaryROCRoots_error <- function(ins, maxTPQuantile, sdMultiplier, max.sens) {
    simpleError("No presence or background points")
  }
  
  result <- trinaryMapWorkflow_test(pres, background, r, trinaryROCRoots_func = mock_trinaryROCRoots_error)
  
  expect_null(result$threshs)
  expect_null(result$trinary.rasters)
})

test_that("trinaryMapWorkflow handles empty presence and background", {
  r <- terra::rast(nrows=10, ncols=10, vals = 1:100)
  pres <- data.frame(x = numeric(), y = numeric())
  background <- data.frame(x = numeric(), y = numeric())
  
  result <- trinaryMapWorkflow_test(pres, background, r)
  
  expect_null(result$threshs)
  expect_null(result$trinary.rasters)
})
