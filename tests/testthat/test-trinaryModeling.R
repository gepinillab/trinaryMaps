test_that("trinaryROCRoots: Handles basic cases and different inputs", {
  # Test with a simple, well-behaved dataset
  set.seed(123)
  ins <- data.frame(Y = sample(0:1, 100, replace = TRUE), X = runif(100))
  result <- trinaryROCRoots(ins)
  expect_true(is.list(result))
  expect_equal(length(result), 2)
  expect_true(is.data.frame(result$trinaryDF))
  expect_true(is.list(result$plotThings))
  expect_equal(ncol(result$trinaryDF), 12)
  
  # Test with a data.frame instead of matrix
  ins_df <- data.frame(Y = sample(0:1, 100, replace = TRUE), X = runif(100))
  result_df <- trinaryROCRoots(ins_df)
  expect_true(is.list(result_df))
  expect_equal(length(result_df), 2)
  expect_true(is.data.frame(result_df$trinaryDF))
  expect_true(is.list(result_df$plotThings))
  expect_equal(ncol(result_df$trinaryDF), 12)
  
  # Test with an AUC close to 1
  ins_perfect <- data.frame(Y = c(rep(0, 50), rep(1, 50)), X = c(runif(50, 0, 0.4), runif(50, 0.6, 1)))
  result_perfect <- trinaryROCRoots(ins_perfect)
  expect_true(is.list(result_perfect))
  expect_equal(length(result_perfect), 2)
  expect_true(is.data.frame(result_perfect$trinaryDF))
  expect_true(is.list(result_perfect$plotThings))
  expect_equal(ncol(result_perfect$trinaryDF), 12)
  
  # Test with a smooth method different than the default
  ins_smooth <- data.frame(Y = sample(0:1, 100, replace = TRUE), X = runif(100))
  result_smooth <- trinaryROCRoots(ins_smooth, smoothMethod="density")
  expect_true(is.list(result_smooth))
  expect_equal(length(result_smooth), 2)
  expect_true(is.data.frame(result_smooth$trinaryDF))
  expect_true(is.list(result_smooth$plotThings))
  expect_equal(ncol(result_smooth$trinaryDF), 12)
  
  # Test with different sdMultiplier (shouldn't cause errors)
  ins_sd_mult <- data.frame(Y = sample(0:1, 100, replace = TRUE), X = runif(100))
  result_sd_mult <- trinaryROCRoots(ins_sd_mult, sdMultiplier = 1)
  expect_true(is.list(result_sd_mult))
  expect_equal(length(result_sd_mult), 2)
  expect_true(is.data.frame(result_sd_mult$trinaryDF))
  expect_true(is.list(result_sd_mult$plotThings))
  expect_equal(ncol(result_sd_mult$trinaryDF), 12)
})

test_that("trinaryMap: Handles basic cases and writes rasters", {
  # Create a dummy raster
  r <- terra::rast(matrix(runif(100), nrow = 10, ncol = 10))
  names(r) <- "test_model"
  # Test with valid inputs and no raster output path
  trinary_raster_no_output <- trinaryMap(r, thr.roc.lo = 0.3, thr.roc.hi = 0.7)
  expect_true(inherits(trinary_raster_no_output, "SpatRaster"))
  expect_equal(terra::nlyr(trinary_raster_no_output), 1)
  expect_equal(terra::unique(trinary_raster_no_output)[[1]], c(0, 1, 2))
  
  # Test with valid inputs and a raster output path (check if it writes a file)
  temp_dir <- tempdir()
  output_path <- file.path(temp_dir, "test_raster.tif")
  trinary_raster_output <- trinaryMap(r, thr.roc.lo = 0.3, thr.roc.hi = 0.7, 
                                      rasterOutputPath = output_path)
  expect_true(inherits(trinary_raster_output, "SpatRaster"))
  expect_equal(terra::nlyr(trinary_raster_output), 1)
  expect_equal(terra::unique(trinary_raster_output)[[1]], c(0, 1, 2))
  expect_true(file.exists(output_path))
  file.remove(output_path)  # Clean up the created file
})

test_that("trinaryRangeSize: Handles basic cases and error conditions", {
  # Create a dummy trinary raster
  m <- matrix(sample(c(0, 1, 2), 100, replace = TRUE), nrow = 10, ncol = 10)
  r <- terra::rast(m, crs = "EPSG:4326")
  # Test with valid trinary raster input
  range_size <- trinaryRangeSize(r)
  expect_true(is.data.frame(range_size))
  expect_equal(ncol(range_size), 2)
  expect_true(all(!is.na(range_size)))
  
  # Test when only zeros are provided
  m2 <- matrix(rep(0, 100), nrow = 10, ncol = 10)
  r2 <- terra::rast(m2, crs = "EPSG:4326")
  range_size2 <- trinaryRangeSize(r2)
  expect_true(is.data.frame(range_size2))
  expect_equal(ncol(range_size2), 2)
  expect_true(all(is.na(range_size2)))
  
  # Test with a raster that only contains the value 1
  m3 <- matrix(rep(1, 100), nrow = 10, ncol = 10)
  r3 <- terra::rast(m3, crs = "EPSG:4326")
  range_size3 <- trinaryRangeSize(r3)
  expect_true(is.data.frame(range_size3))
  expect_equal(ncol(range_size3), 2)
  expect_false(is.na(range_size3[1, 1]))
  expect_true(is.na(range_size3[1, 2]))
  
  # Test when values different from 0, 1, and 2, are provided
  m_bad <- matrix(sample(c(0, 1, 2, 3), 100, replace = TRUE), nrow = 10, ncol = 10)
  r_bad <- terra::rast(m_bad)
  expect_error(trinaryRangeSize(r_bad), "Error: trinaryRasters must contain only values of 0, 1, or 2.")
  
  # Test with an incorrect object type
  expect_error(trinaryRangeSize(m), "Error: trinaryRasters must be a SpatRaster object.")
})
