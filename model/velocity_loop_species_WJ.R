# Script to calculate velocity for all species
# Edited by Weihan Jia on 10-04-2024

library(raster)

species_dirs <- list.dirs(path = "//smb.isipd.dmawi.de/projects/bioing/data/ArcticSDM/SDM_Predictions_spThin/", full.names = TRUE)
output_dir <- "E:/AWI/glacial_refugia_project/Framework/velocity/Arctic_velo/"


for (species_dir in species_dirs) {
  
  species_name <- basename(species_dir)
  
  wd <- file.path(species_dir, "MaxEnt/")
  mod_file <- file.path(wd, glue::glue("{species_name}_mars_current.tif"))
  lgm_file <- file.path(wd, glue::glue("{species_name}_mars_21ka.tif"))
  
  mod <- raster(mod_file)
  lgm <- raster(lgm_file)
  
  mod[mod < 0.1] <- NA
  lgm[lgm < 0.1] <- NA
  
  # Extract coordinates and values
  x <- coordinates(lgm)[, 1]
  y <- coordinates(lgm)[, 2]
  
  p <- round(values(lgm) * 10) / 10
  f <- round(values(mod) * 10) / 10
  
  complete_cases <- complete.cases(p, f)
  
  p <- p[complete_cases]
  f <- f[complete_cases]
  
  x <- x[complete_cases]
  y <- y[complete_cases]
  
  d <- vector(length=length(p))
  
  u <- unique(p)[order(unique(p))]
  
  m <- sapply(u, function(u){c(which(u==f))})
  
  for(i in 1:length(p)){
    mi   <- m[[which(u==p[i])]]          
    d[i] <- sqrt(min((x[i]-x[mi])^2 + (y[i]-y[mi])^2))
  }
  
  out_mpi <- cbind(y,x,logDist=round(log10(d)*100)) %>% as.data.frame()
  out_mpi$logDist[out_mpi$logDist==-Inf] <- -1000 
  
  out_mpi_select <- out_mpi[out_mpi$logDist<=0,]
  
  distance_mpi <- rasterFromXYZ(out_mpi[, c("x", "y", "logDist")])
  writeRaster(distance_mpi, filename=file.path(output_dir, paste0(species_name, "_velocity.tif")), format="GTiff", overwrite=T)
  
}