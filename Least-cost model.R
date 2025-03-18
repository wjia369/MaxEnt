# define where are the R packages
.libPaths(c("~/R/x86_64-pc-linux-gnu-library/4.0", .libPaths()))

suppressMessages({
  library(tidyverse)
  library(tidyterra)
  library(sf)
  sf_use_s2(FALSE)
  library(stars)
  library(terra)
  library(patchwork)
  library(gdistance)
})

tempdir <- "/netscratch/wjia/tmp"
dir.create(tempdir, showWarnings = FALSE)
Sys.setenv(TMPDIR = tempdir)

wd      <- "N:/bioing/data/ArcticSDM"
fls_wd  <- "N:/netscratch/wjia/prefinal/maxent"
fls_out <- "N:/netscratch/wjia/prefinal/velo_backward_dispersal"

# read asv info
asv <- read.csv2(glue::glue("{wd}/Weihan/sibala/MaxEnt_tas07_range_669ASVs_taxaname_11.03.2025.csv"),
                 header=TRUE, stringsAsFactors = FALSE, sep=',', encoding="UTF-8")

asv_idx <- as.numeric(commandArgs(trailingOnly = TRUE))
a <- asv$ASV[asv_idx]

dist <- asv$dist[asv$ASV == a]

# modern
proj <- "+proj=laea +lon_0=-170 +lat_0=90"

mod <- rast("N:/bioing/data/ArcticSDM/environment/CHELSA_Weihan/MPI3_0ka/sibala/MPI_tas_07_0ka_sibala_cal_laea_18.09.2024.tif")
mod_sf <- mod %>% st_as_stars() %>% st_set_crs(proj) %>% st_as_sf() %>% st_centroid() %>% suppressWarnings()


### change to CESM!!!
asv_info <- read.csv2(glue::glue("{fls_wd}/allmaxentResults_669ASV_16.01.2025.csv"),
                      header=TRUE, stringsAsFactors = FALSE, sep=',', encoding="UTF-8")

## files 
flsTab <- tibble(fls  = list.files(glue::glue("{fls_wd}/ASV{a}"), pattern = ".rda"),
                 asv  = sapply(strsplit(fls, "_"), function(x) x[1]),
                 time = sapply(strsplit(fls, "_"), function(x) x[2])) %>% 
          mutate(time = gsub(".rda", "", time),
                 time = ifelse(grepl("ka$", time),
                               as.numeric(gsub("ka$", "", time)),
                               as.numeric(time))) %>%
          arrange(time) %>% filter(time <= 491) %>% suppressWarnings()

if(nrow(flsTab) == 0) {
  cat("No results for ASV", a, "- stopping the loop. \n")
  quit(status = 1)
}

times <- flsTab %>% pull(time) %>% unique() %>%
  tibble(time = .) %>%
  pull(time)

for(i in seq_along(times[-length(times)])) {
  
  time1 <- times[i]
  time2 <- times[i + 1]
  tt <- (time2 - time1) * 1000
  
  loaded_objects <- load(glue::glue("{fls_wd}/ASV{a}/{flsTab[flsTab$time==time1,] %>% pull(fls)}"))
  assign(glue::glue("maxent1"), get(loaded_objects))
  rm(list = loaded_objects)

  map_maxent1  <- st_rasterize(maxent1 %>% st_as_sf(), st_as_stars(st_bbox(maxent1),
                               nx = 5000, ny = 5000, values = NA_real_, crs=proj)) %>% rast() %>% setNames("time1")
  thd <- as.numeric(asv_info$X10[asv_info$ASV==a])
  
  map_maxent1 <- resample(map_maxent1, mod, method = "near")
  
  map_maxent1_bina <- map_maxent1
  map_maxent1_bina[map_maxent1_bina >= thd]  <- 1
  map_maxent1_bina[map_maxent1_bina != 1] <- NA
  
  loaded_objects <- load(glue::glue("{fls_wd}/ASV{a}/{flsTab[flsTab$time==time2,] %>% pull(fls)}"))
  assign(glue::glue("maxent2"), get(loaded_objects))
  rm(list = loaded_objects)
  
  map_maxent2  <- st_rasterize(maxent2 %>% st_as_sf(), st_as_stars(st_bbox(maxent2),
                               nx = 5000, ny = 5000, values = NA_real_)) %>% rast() %>% setNames("time2")
  
  map_maxent2 <- resample(map_maxent2, mod, method = "near")
  
  map_maxent2_bina <- map_maxent2
  map_maxent2_bina[map_maxent2_bina >= thd]  <- -1
  map_maxent2_bina[map_maxent2_bina != -1] <- NA

  
  diffRast <- tapp(c(map_maxent1_bina, map_maxent2_bina), c(1,1), sum, na.rm = T) %>% suppressWarnings()
  
  naRast   <- ifel(is.na(diffRast), 2, ifel(diffRast>0, NA, 1))
  distRast <- distance(naRast, exclude = 2)
  
  velocity <- ifel(diffRast==0, 0, ifel(diffRast==1, distRast, NA))
  velocity[is.na(map_maxent1_bina)] <- NA
  
  ### dispersal cost layer
  cost    <- mean((1 - map_maxent1), (1 - map_maxent2))

  ########################
  ### least-cost model ###
  ########################

  ## prep
  resistance.mask <- mod
  resistance.mask[resistance.mask < 1] <- 1
  resistance.mask <- distance(resistance.mask)
  resistance.mask[resistance.mask > 10000] <- NA
  resistance.mask[resistance.mask >= 0] <- 1  
  
  cost[is.na(cost)] <- 5000 
  resistance.raster <- mask(cost, resistance.mask)
  
  ## start pts
  xy <- as.data.frame(diffRast, xy = TRUE)
  colnames(xy) <- c('x', 'y', 'diff')
  
  from.xy <- xy %>% filter(diff == 1)
  from.xy <- from.xy[,c('x','y')]
 
  
  ## end pts
  diffRast1 <- diffRast
  diffRast1[velocity > 5000] <- NA
  diffRast1[diffRast1 != 1] <- NA
  
  ### ! now we have the coordinates of the intersection of diffRast = 0 and diffRast = 1 ###
  ### ! but don't forget to add 5000 m back to velcoity later! ###
  
  xy1 <- as.data.frame(diffRast1, xy = TRUE)
  colnames(xy1) <- c('x', 'y', 'diff')
  to.xy <- xy1 %>% filter(diff == 1)
  to.xy <- to.xy[,c('x','y')]
  
  from.xy <- anti_join(from.xy, to.xy, by = c("x", "y"))
  
  ## transfer to maxtrix
  from.xy <- as.matrix(from.xy)
  to.xy   <- as.matrix(to.xy)
  
  ## model
  f <- function(x) 1/mean(x)
  trans <- gdistance::transition(raster::raster(resistance.raster), transitionFunction=f, directions=8) 
  trans <- gdistance::geoCorrection(trans)
  
  cost.distance <- round(gdistance::costDistance(trans, from.xy, to.xy))
  cost.distance1 <- as.data.frame(cost.distance) 
  
  out.lcd <- as.data.frame(apply(cost.distance1, 1, min)) ## select the 'least-cost' of all climate analogs
  names(out.lcd) <- 'cost.dist'
  
  from.x <- from.xy[,1]
  from.y <- from.xy[,2]
  
  ## Set up output data frame. Other data will be added in future steps
  out.df <- as.data.frame(cbind(from.x, from.y))
  out.df$to.x <- 0
  out.df$to.y <- 0
  out.df$MED <- 0
  
  the.function <- function(k) {	
    from.index <- which(out.lcd[k,] == cost.distance1[k,])		
    # if there is more than one "to" with matching minumum distances, choose the first in the list
    if (length(from.index) > 1) {
      from.index[1]
      ID <- out.df[k, 'ID']
      to.x <- to.xy[from.index[1],][1]
      to.y <- to.xy[from.index[1],][2]
      return(c(to.x, to.y))
    } else {
      to.x <- to.xy[from.index,][1]
      to.y <- to.xy[from.index,][2]
      return(c(to.x, to.y))
    }
  }
  
  out <- lapply(1:nrow(out.df), the.function)
  
  out1 <- do.call('cbind', out)

  out.df$to.x <- out1[1,]
  out.df$to.y <- out1[2,]
  
  colnames(out.df)[1:2] <- c('from.x', 'from.y')
  
  the.shortest.path.function <- function(k) {
    shortest.path <- shortestPath(trans, c(out.df[k, 'from.x'], out.df[k, 'from.y']), c(out.df[k, 'to.x'], out.df[k, 'to.y']), output="SpatialLines")
    return(shortest.path)					
  }
  
  sp <- lapply(1:nrow(out.df), the.shortest.path.function)
  
  ## This function is necessary to 'merge' all trajectories into one SpatialLines object
  makeUniqueIDs <- function(x) {
    ids = sapply(x, function(i) slot(i, "ID"))
    if (any(duplicated(ids))) {
      ids <- make.unique(as.character(unlist(ids)), sep = "")
      for (i in seq(along = ids))
        x[[i]]@ID = ids[i]
    }
    x
  }
  
  ll = do.call("c", lapply(sp, function(x) slot(x, "lines")))
  ll = makeUniqueIDs(ll)
  sp <- SpatialLines(ll, proj4string = CRS(proj4string(sp[[1]])))	
  
  
  out.df$MED <- round((SpatialLinesLengths(sp) / 1000), 1) # convert to km
  
  out.df$vel_MED <- round(out.df$MED/90, 2) # 90 is number of years from 1995 to 2085
  
  out.df$MCE <- round(((round((out.lcd$cost.dist / 1000), 1) - out.df$MED) / cost.penalty), 1)

  #......
  # 
  # 
  # 
  # velocity[velocity > dist] <- NA
  # velocity[velocity > 0] <- 1
  # 
  # velo_stars <- st_as_stars(velocity)
  # 
  # ext_velo <- st_extract(velo_stars %>% st_set_crs(proj), st_geometry(mod_sf)) %>%
  #   st_as_sf() %>% setNames(c(glue::glue("ASV{a}"), "geometry")) %>% suppressWarnings()
  # 
  # save(ext_velo, file = glue::glue("{fls_out}/ASV{a}_refugia_dipersal_X10_{time1}_{time2}_NoOverlapHabitat_25.01.2025.rda"))
  
}

unlink(tempdir(), recursive = TRUE)
