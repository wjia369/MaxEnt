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
                                                                          # replace the name of "_gbif_plantea.csv" with an empty string "" in each element of 'fls'
                                                                          # this extracts the number in the file name

# GBIF cleaning ------------------------
# Suggested by Weihan

gbifTab <- parallel::mclapply(which(gbif_list$ids %in% ids), # lapply() is a function used for parallel processing in R
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
                  coordinateUncertaintyInMeters / 1000 <= 5 | is.na(coordinateUncertaintyInMeters), year >= 1970) %>%

  }, mc.cores = 1) %>%
  
  # combine the results obtained from parallel processing into a single data frame
  Reduce("rbind",.) %>% 
  
  # convert the combined results into a spatial object, projection is WGS1984
  sf::st_as_sf(coords = c('decimalLongitude', 'decimalLatitude'), crs = 4326) %>% 
  
  # transform the coordinate system of data to match that of 'map'
  sf::st_transform(st_crs(map))


# GBIF thinning #
# set the resolution
res <- 15000

# rasterize the map
grd_map <- st_rasterize(map, st_as_stars(st_bbox(map),  # st_as_stars(): convert various spatial objects into stars objects,
                                                        # which are a special type of data structure used for representing raster data
                                                        # st_bbox(): extract the bounding box of the map
                                    dx = res, dy = res, # specify the resolution of the raster grid in the x and y directions
                                    values = NA_real_)) %>% # all cells should be initialized with NA
  setNames("grd_map") # sets the name of the raster as 'grd_map'


# GBIF thinning ------------------------
# Suggested by Simeon
gbif_thin <- (gbifTab %>% group_split(species))[sapply(gbifTab %>% group_split(species), nrow)>10] %>% # split 'gbifTab' by species
                                                            # keeping only species which the number of rows is greater than 50
                                                            # this means that only species with more than 50 occurrences are included.
  parallel::mclapply(
    
    function(x) {
      out <- x %>% 
        
        # selects the column 'species'
        dplyr::select(species) %>% 
        
        # transform the coordinate system of data to match that of 'thin_map'
        st_transform(st_crs(thin_map)) %>%
      
        # mutate() - add a new column called 'cell'
        # st_intersects() - check which features in gbifTab (.) intersect with features in 'thin_map' 
        mutate(cell = unlist(apply(st_intersects(., thin_map %>% # unlist() - convert the output of apply() from a list to a vector
                                                   st_as_sf(), # convert 'thin_map' to a stars object
                                                 sparse = FALSE), 1, # output the result as a dense matrix instead of a sparse one
                                                                     # 1 means the function should be applied over the rows of the input matrix
                                   # for each row, check if there is any intersection with 'thin_map'
                                   # if yes, return the indices (which(y)), if no, return NA
                                   function(y) ifelse(any(y), which(y), NA)))) %>%
      filter(!is.na(cell)) # remove 'cell' is NA
    
    # if the number of rows of 'out' > 1
    if(nrow(out)>1) {out %>% 
        
        # split 'out' by species
        group_split(cell) %>% 
        
        # apply a function
        lapply(function(z) z %>% 
                 # sample() - generate a random number between 1 and the number of rows in 'z'
                 # slice() - choose the row corresponding to this random number
                 slice(sample(1:nrow(z), 1))) %>%
        
        # combine the results into a single data frame
        Reduce("rbind", .) %>% 

        # select cell and species
        dplyr::select(cell, species) %>% 
        
        # suppress the warning messages
        suppressWarnings()

      # or return NULL
    } else NULL
      
  }, mc.cores = 1) %>% 
  
  Reduce("rbind", .)


#-------------------------------------------------------------------
# 04-04-2024
# Create pseudoabsence points #
# Written by Ronja
# Edited by Weihan

# Step 1: Create the buffer #

# create a buffer of 200 km 
presence_buffer <- st_buffer(presence_sf, dist = 20000)

# calculate the intersection of absence_sf and presence_buffer
intersection_200 <- st_intersection(absence_sf, presence_buffer) %>% suppressWarnings()

# only select unique points
buffer_200_abs <- intersection_200[!duplicated(st_coordinates(intersection_200)), ]

# create a new data tab
modTab_buffer_200 <- occID %>% mutate(p = 1) %>% dplyr::select(p) %>% st_drop_geometry() %>%
  bind_cols(subsetList$env[occID$id,]) %>%
  bind_rows(buffer_200_abs %>% mutate(p = 0) %>% dplyr::select(p) %>% st_drop_geometry() %>%
              bind_cols(subsetList$env[buffer_200_abs$id,])) %>% na.omit()

# select presence points
modTab_buffer_200_pres = subset(modTab_buffer_200, modTab_buffer_200$p ==1)


# Step 2: Environmental profiling of background #

library(e1071)

# train a one-class SVM mode with 'modTab_buffer_200'
mod <- svm(modTab_buffer_200[,-1], modTab_buffer_200[,1], type = "one-classification")

# choose the data in modTab_buffer_200
backg <- modTab_buffer_200 %>% 
  
  # if p==0 and the predictions from the SVM model being 0 at the same time
  filter(p==0 & predict(mod)==0) %>% 
  
  # then select all columns except for the column 'p'
  dplyr::select(-p)


# Step 3: K-means clustering #

km <- kmeans(backg, nrow(backg)-1)

# create a new tab for model
modelTab <- km$centers %>% # choose 'centers' in 'km'
  
  # convert it to a dataframe
  as_tibble() %>% 
  
  # create a new column 'p' and place it before all columns
  mutate(p = 0, .before = names(.)[1]) %>% 
  
  # bind all rows of presence points
  bind_rows(modTab_buffer_200_pres) %>% 
  
  # ignore NA
  na.omit()









