### Script to calculate the velocity
weihan <- "//smb.isipd.dmawi.de/projects/bioing/data/ArcticSDM/Weihan/"

library(tidyverse)
library(sf)
sf_use_s2(FALSE)
library(stars)
library(dplyr)
library(snow)

out_wd <- "//smb.isipd.dmawi.de/projects/bioing/data/ArcticSDM/Weihan/maxent_output/"

# load(glue::glue("{weihan}/GBIF/GBIF_Plantea_thinned_sibala.rda"))
# 
# occTab <- gbif_thin %>% filter(species %in% (gbif_thin %>% group_by(species) %>% st_drop_geometry() %>% summarise(count = n()) %>%
#                                                arrange(desc(count)) %>% filter(count>=100) %>% pull(species)))
# 
# spList <- occTab %>% st_drop_geometry() %>% group_by(species) %>% summarise(count = n()) %>%
#   arrange(desc(count))

for (sp in spList$species[1:656]) {
  
  cat("Processing velocity:", species, "\n")

  stars_sp <- read_stars(c(glue::glue("{out_wd}{sp}/MaxEnt/{sp}_MaxEnt_current.tif"),
                           glue::glue("{out_wd}{sp}/MaxEnt/{sp}_MaxEnt_21ka.tif")))

  sf_sp <- stars_sp %>% merge() %>% st_as_sf() %>% st_centroid() %>% setNames(c("current", "past", "geometry")) %>%
    suppressWarnings()


  ### cluster setup ###
  cl <- makeCluster(5)
  invisible(clusterEvalQ(cl, {
    library(sf); sf_use_s2(FALSE)
    library(tidyverse)
  }))
  clusterExport(cl, 'sf_sp')
  #####################

  velocity <- clusterApply(cl, 1:nrow(sf_sp), function(x) {
    tmp <- sf_sp %>% filter(round(sf_sp$past[x], 1) == round(current, 1))
    if(nrow(tmp)>0) min(as.numeric(st_distance(sf_sp[x,], tmp))/1000)
    else NA
  }) %>% Reduce('c',.)

  sf_sp$velocity <- velocity

  velocity_rast <- st_rasterize(sf_sp %>% dplyr::select(velocity), 
                              st_as_stars(st_bbox(stars_sp),
                                          dx = st_res(stars_sp)[1], dy = st_res(stars_sp)[2],
                                          values = NA_real_))


  out <- ggplot() +
    geom_stars(data = velocity_rast %>% setNames('Velocity'), show.legend = F) +
    scale_fill_gradient(low = 'seashell', high = 'tomato4', breaks = seq(0, 10000, length = 10), na.value = "transparent") +
    labs(title=glue::glue("{sp}: velocity, 21 ka to the present"),
         x ="", y = "") +
    theme_void() +
    theme(plot.title = element_text(size=9, face="bold", hjust = 0.5))


  ggsave(glue::glue("{weihan}/velocity/png/{sp}_mpi1_velocity_21ka.png"), out, units = "cm", width = 12, height = 15, bg = "grey50")
  write_stars(velocity_rast, glue::glue("{weihan}/velocity/tif/{sp}_mpi1_velocity_21ka.tif"))


  stopCluster(cl)
  
}