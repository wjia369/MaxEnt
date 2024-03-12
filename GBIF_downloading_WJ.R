# Script to download the occurrence data from GBIF
# Written by Simeon Lisovski
# Make sure all directories exist before running!

library(rgbif)
library(sf)
sf_use_s2(FALSE) # disable the use of s2 library for spatial indexing
library(tidyverse)

# set the GBIF account
options(gbif_user = "wjia", gbif_pwd = "XXX", gbif_email = "weihan.jia@awi.de")

wd <- "E:/AWI/glacial_refugia_project/Framework/ArcticSDM_Simeon/gbif/"

# spatial extent
ecoreg <- st_read(glue::glue("{wd}Ecoregions/tnc_terr_ecoregions.shp")) %>% # read the shapefile of ecoregion
          filter(WWF_MHTNAM %in% c("Boreal Forests/Taiga", "Tundra"), # select only "Boreal Forests/Taiga" or "Tundra".
                 st_coordinates(st_centroid(.))[,2]>0)  # check if the latitude (second column) of the centroid of each ecoregion is greater than 0,
                                                        # which implies it's in the northern hemisphere.

# bbox_grid <- ecoreg %>% st_bbox(crs = 4326) %>% st_as_sfc() %>%
#   st_make_grid(n = c(60, 30)) %>% st_sf() %>% filter(apply(st_intersects(., ecoreg, sparse = FALSE), 1, any)) %>%
#   mutate(grid_id = 1:nrow(.)) %>% dplyr::select(grid_id)
# st_write(bbox_grid, glue::glue("{wd}Data/occurance_grid.shp"))

bbox_grid <- st_read(glue::glue("{wd}Ecoregions/occurance_grid.shp")) # read the shapefile of grids

# plot the map
ggplot() +
  geom_sf(data = ecoreg %>% dplyr::select('ECO_ID_U')) +
  geom_sf(data = bbox_grid, fill = NA)

# where to save the data
save_dir <- glue::glue("{wd}data/") # glue() allow to embed R code within a string

# loop
for(i in 1:nrow(bbox_grid)) { # for each grid in 'bbox_grid'
  
  # check if there is no csv saved in the directory of 'save_dir'
  if(!file.exists(glue::glue("{save_dir}all/{bbox_grid[i,] %>% pull('grid_id')}_gbif_all.csv"))) {
    
    # if it is TRUE, then 
    poly <- ecoreg %>% 
      st_intersection(bbox_grid[i,]) %>% # intersect ecoregions with the current bounding box
      st_geometry() %>% # extract the geometry column
      st_union() %>% # union the geometries
      suppressMessages() %>% # suppress messages
      suppressWarnings() # suppress warnings
    
    bbox <- poly %>% 
      st_bbox(crs = 4326) %>% # show the boundary of the polygon, projection is WGS1984
      st_as_sfc() %>% # convert the bounding box to simple feature geometry
      sf::st_as_text() # convert geometric information to text representation
    
    # initiate a download request/link for GBIF occurrence data
    file_list <- occ_download(pred_within(bbox), format = "SIMPLE_CSV") # download the occurrence data from GBIF within the bounding box in CSV format
    
    repeat{ # repeat this step if
      wait <- tryCatch(occ_download_wait(file_list[1]), error = function(e) NULL) # wait for a download request to be done
                                                                                  # if an error occurs, it assigns NULL to 'wait'
                                                                                  # report the status (tryCatch)
      if(!is.null(wait)) break # if the 'wait' is not NULL (indicating that the download has completed successfully), then breaks the loop
    }
    
    dwnl <- tryCatch(occ_download_get(file_list[1], overwrite = T) %>% # download the occurrence data
                                                                       # report the status (tryCatch)
      occ_download_import() %>% # import a downloaded file from GBIF
        suppressMessages(), error = function(e) NULL) # suppress messages and assign NULL to 'dwnl' if there is any errors occur
    
    if(!is.null(dwnl)) { # if the 'dwnl' is not NULL (indicating that the download has completed successfully), then
      
      inPoly <- dwnl[c(dwnl %>% # selects all rows of 'dwnl'
                         st_as_sf(coords = c('decimalLongitude', 'decimalLatitude'), crs = 4326) %>% # convert the 'dwnl' data into a spatial object, projection is WGS1984
                            st_geometry() %>% # extract the geometric information from 'dwnl'
                              st_intersects(., poly, sparse = FALSE)),] %>% # filter (or extract) the data based on their spatial intersection with the polygon 'poly'
                                                                            # the result of the intersection operation will be a dense logical vector or matrix,
                                                                            # which provides detailed information about the intersection status of each geometry
                                suppressMessages() # suppress messages
      # save 'inPoly' as a csv
      write_csv(inPoly, 
                file = glue::glue("{save_dir}/all/{bbox_grid[i,] %>% pull('grid_id')}_gbif_all.csv")) # save it to the directory
                                                                                                      # named as "<grid_id>_gbif_all.csv"
      # save only plant data in 'inPoly' as a csv
      write_csv(inPoly %>% filter(kingdom == "Plantae"), 
                file = glue::glue("{save_dir}/Plantea/{bbox_grid[i,] %>% pull('grid_id')}_gbif_plantea.csv"))
      
      # removes any ZIP files in the directory
      unlink(list.files(pattern = ".zip", full.names = T))
      
    } else {
      
      inPoly <- tibble(error = TRUE) # still create a dataframe is errors occur
      
      write_csv(inPoly, 
                file = glue::glue("{save_dir}all/{bbox_grid[i,] %>% pull('grid_id')}_gbif_all.csv"))
      
      write_csv(inPoly, 
                file = glue::glue("{save_dir}Plantea/{bbox_grid[i,] %>% pull('grid_id')}_gbif_plantea.csv"))
    }
  }
  
}
