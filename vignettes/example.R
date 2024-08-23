library(maxnet)
library(predicts)
occurence <- system.file("/ex/bradypus.csv", package = "predicts")
occ <- read.csv(occurence)[, -1]
f <- system.file("ex/bio.tif", package="predicts")
envs <- terra::rast(f)
data <- terra::values(envs) |> as.data.frame()
data$ID <- 1:(terra::nrow(envs) * terra::ncol(envs))
occs.p <- terra::extract(envs, occ, cells = TRUE)
data$p <- rep(0, terra::ncell(envs))
data$p[occs.p$cell] <- 1
data$x <- terra::xFromCell(envs, 1:terra::ncell(envs))
data$y <- terra::yFromCell(envs, 1:terra::ncell(envs))
data <- na.omit(data)
mod <- maxnet(data$p, data[, 1:9])
plot(mod, type="cloglog")
p.vals <- predict(mod, terra::values(envs), type = "cloglog")
suitability <- rep(NA, terra::ncell(envs))
suitability[data$ID] <- p.vals
r <- envs[[1]]
names(r) <- "suitability"
terra::values(r) <- suitability
plot(r)

#########
tr <- trinaryMapWorkflow(pres = data[data$p == 1, c("x", "y")], 
                         background = data[data$p == 0, c("x", "y")], 
                         rModel = r)
r2 <- tr$trinary.rasters
plot(r2)
trinaryRangeSize(tr$trinary.rasters)
trinaryROCPlot(tr$threshs$plotThings, trinaryDF = tr$threshs$trinaryDF)
