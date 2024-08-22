library(ENMeval)
library(raster)
library(dplyr)
# Set a random seed in order to be able to reproduce this analysis.
set.seed(48)

# You can search online databases like GBIF using the spocc package (commented below),
# but here we will load in some pre-downloaded data.
bv <- spocc::occ('Bradypus variegatus', 'gbif', limit=300, has_coords=TRUE)
occs <- as.data.frame(bv$gbif$data$Bradypus_variegatus[,2:3])

# Removing occurrences that have the same coordinates is good practice to
# avoid pseudoreplication.
occs <- occs[!duplicated(occs),]

envs.files <- list.files(path=paste(system.file(package='dismo'), '/ex', sep=''), 
                         pattern='grd', full.names=TRUE)
envs <- raster::stack(envs.files)
envs <- raster::mask(envs, envs[[9]]) %>% raster::stack()
envs$biome <- raster::as.factor(envs$biome)
occs.cells <- raster::extract(envs[[1]], occs, cellnumbers = TRUE)
occs.cellDups <- duplicated(occs.cells[,1])
occs <- occs[!occs.cellDups,]
occs.sf <- sf::st_as_sf(occs, coords = c("longitude","latitude"), crs = raster::crs(envs))
eckertIV <- "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
occs.sf <- sf::st_transform(occs.sf, crs = eckertIV)
occs.buf <- sf::st_buffer(occs.sf, dist = 500000) %>% 
  sf::st_union() %>% 
  sf::st_sf() %>%
  sf::st_transform(crs = raster::crs(envs))
envs.bg <- raster::crop(envs, occs.buf)
envs.bg <- raster::mask(envs.bg, occs.buf)
bg <- dismo::randomPoints(envs.bg[[9]], n = 10000) %>% as.data.frame()
colnames(bg) <- colnames(occs)
e.mx.l <- ENMevaluate(occs = occs, envs = envs.bg, bg = bg, 
                      algorithm = 'maxnet', partitions = 'block', 
                      tune.args = list(fc = c("L","LQ","LQH","H"), rm = 1:5))
r <- eval.predictions(e.mx.l)[["fc.LQH_rm.2"]]
plot(terra::rast(r))
###################
###################
tr <- trinaryMapWorkflow(pres = occs, 
                         background = bg, 
                         rModel = terra::rast(r))
r2 <- tr$trinary.rasters
plot(r2)

trinaryRangeSize(tr$trinary.rasters)

trinaryROCPlot(tr$threshs$plotThings, trinaryDF = tr$threshs$trinaryDF)

