# Script to run SDM
# Written by Simeon Lisovski
# Edited by Weihan Jia


library(sf)
sf_use_s2(FALSE) # disable the use of s2 library for spatial indexing
library(stars)
library(tidyverse)


wd <- "E:/AWI/glacial_refugia_project/Framework/ArcticSDM_Simeon/gbif/"
map_data <- "E:/AWI/glacial_refugia_project/Framework/ArcticSDM_Simeon/gbif/ne_10m_admin_1_states_provinces/"

# set the projection
proj <- "+proj=laea +lon_0=-170 +lat_0=90"
  
# import the map of ecoregions
ecoreg <- st_read(glue::glue("{wd}Ecoregions/tnc_terr_ecoregions.shp")) %>% # read the shapefile of ecoregion
  filter(WWF_MHTNAM %in% c("Boreal Forests/Taiga", "Tundra"), # select only "Boreal Forests/Taiga" or "Tundra".
         st_coordinates(st_centroid(.))[,2]>0)  # check if the latitude (second column) of the centroid of each ecoregion is greater than 0,
                                                # which implies it's in the northern hemisphere.
# check the map
ggplot() +
  geom_sf(data = ecoreg %>% dplyr::select('ECO_ID_U'))

# import the map of provinces
map_wrld <- st_read(glue::glue("{map_data}ne_10m_admin_1_states_provinces.shp")) %>%
  st_intersection(ecoreg %>%        # intersect ecoregions with the current bounding box
                  st_union()) %>%   # union the geometries
  dplyr::select(c('name', 'admin')) # select only the columns named 'name' and 'admin' 

# check the map
ggplot() +
  geom_sf(data = map_wrld, fill = NA)

# check Alaska
map <- map_wrld %>% filter(name=="Alaska") %>% 
  st_transform(proj)

# set the resolution
res <- 5000

# rasterize the map
grd_map <- st_rasterize(map, st_as_stars(st_bbox(map),  # st_as_stars(): convert various spatial objects into stars objects,
                                                        # which are a special type of data structure used for representing raster data
                                                        # st_bbox(): extract the bounding box of the map
                                    dx = res, dy = res, # specify the resolution of the raster grid in the x and y directions
                                    values = NA_real_)) %>% # all cells should be initialized with NA
  setNames("grd_map") # sets the name of the raster as 'grd_map'


# GBIF occurrence data #

# read the shapefile of grids
gbif_grid <- st_read(glue::glue("{wd}Ecoregions/occurance_grid.shp"))

# identify the grids that intersect with the 'map'
ids <- which(c(gbif_grid %>%
                 st_transform(st_crs(map)) %>% # transform the coordinate system of 'gbif_grid' to match that of 'map'
  st_intersects(map, sparse = FALSE)))         # check if there are spatial intersections between 'map' and 'gbif_grid'
                                               # return a simple logical vector instead of a sparse matrix

# plot the 'map' and corresponding grids
ggplot() +
  geom_sf(data = map) +
  geom_sf(data = gbif_grid[ids,] %>%   # select only the grids from 'ids'
            st_transform(st_crs(map)), # transform the coordinate system of 'gbif_grid' to match that of 'map'
          fill = NA)                   # the polygons should not be filled with any color

 
# read the GBIF data files
gbif_list <- tibble(fls = list.files(glue::glue("{wd}data/Plantea"))) %>% # list all files in the directory
  mutate(ids = as.numeric(gsub("_gbif_plantea.csv", "", fls)))            # add a new list named 'ids' to the tibble
                                                                          # replace the name of "_gbif_plantea.csv" with an empty string "" 
                                                                          # in each element of 'fls'
                                                                          # this extracts the number in the file name

# GBIF cleaning ------------------------
# Suggested by Weihan

gbifTab <- lapply(which(gbif_list$ids %in% ids), # lapply() is a function used for parallel processing in R
                                                 # check if 'ids' in 'gbif_list' are present in 'ids' (grids)

  function(x) {

  read_csv(glue::glue("{wd}data/Plantea/{gbif_list$fls[x]}"), # read .csv files in the list
           progress = F,                                      # suppress the progress that is displayed by default while reading the CSV file
           show_col_types = FALSE) %>%                        # suppress the column specification while reading the CSV file
    
    # select the columns we need  
    dplyr::select(kingdom, phylum, class, order, family, genus, species, scientificName, countryCode, 
                  decimalLongitude, decimalLatitude, coordinateUncertaintyInMeters,
                  year, basisOfRecord, institutionCode) %>% 
    
    # cleaning steps
    dplyr::filter(!is.na(decimalLongitude), !is.na(decimalLatitude), !is.na(species), basisOfRecord != "FOSSIL_SPECIMEN",
                  coordinateUncertaintyInMeters / 1000 <= 1 | is.na(coordinateUncertaintyInMeters), year >= 1970) %>%
    
    # remove duplicate coordinates
    dplyr::distinct(decimalLongitude, decimalLatitude)

  }) %>%
  
  # combine the results obtained from parallel processing into a single data frame
  dplyr::bind_rows() %>% 
  
  # convert the combined results into a spatial object, projection is WGS1984
  sf::st_as_sf(coords = c('decimalLongitude', 'decimalLatitude'), crs = 4326) %>% 
  
  # transform the coordinate system of data to match that of 'map'
  sf::st_transform(st_crs(map))
  

