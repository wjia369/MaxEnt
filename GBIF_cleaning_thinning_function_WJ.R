# Function to clean and thin the occurrence data (.csv) from GBIF
# Edited by Weihan Jia on 6 March 2024

# Original codes:
# Cleaning: https://cran.r-project.org/web/packages/CoordinateCleaner/vignettes/Cleaning_GBIF_data_with_CoordinateCleaner.html
# Thinning: https://cran.r-project.org/web/packages/spThin/vignettes/spThin_vignette.html

#----------------------------------------------------------------------------

library(devtools)
#install_github("ropensci/CoordinateCleaner")
library(countrycode)
library(CoordinateCleaner)
library(dplyr)
library(ggplot2)
library(rgbif)
library(sf)
library(rnaturalearthdata)
library(spThin)


input.dir <- "E:/AWI/glacial_refugia_project/Framework/GBIF"
output.dir <- "E:/AWI/glacial_refugia_project/Framework/GBIF"

# define taxa name
taxa <- "Betula"

# import the occurrence data downloaded from GBIF
dat <- read.csv(file.path(input.dir, "Betula_0081065-231120084113126.csv"),
                 sep="\t",fill=TRUE,header=TRUE, quote="",encoding="UTF-8")

#----------------------------------------------------------------------------


# PART I Cleaning GBIF data --------------------------------------------------

GBIF_cleaning <- function(x){

  # select columns of interest
  dat <- dat %>%
    dplyr::select(species, decimalLongitude, 
                  decimalLatitude, countryCode, individualCount,
                  gbifID, family, taxonRank, coordinateUncertaintyInMeters,
                  year, basisOfRecord, institutionCode)
  
  # remove records without coordinates
  dat <- dat %>%
    filter(!is.na(decimalLongitude)) %>%
    filter(!is.na(decimalLatitude))
  
  # convert country code from ISO2c to ISO3c
  dat$countryCode <-  countrycode(dat$countryCode, 
                                  origin =  'iso2c',
                                  destination = 'iso3c')
  
  # remove flag errors
  flags <- clean_coordinates(x = dat, 
                             lon = "decimalLongitude", 
                             lat = "decimalLatitude",
                             countries = "countryCode",
                             species = "species",
                             tests = c("capitals", "centroids",
                                       "equal", "zeros", "countries"))
  dat <- dat[flags$.summary,]
  
  # remove coordinates with a precision below 1 km
  dat <- dat %>%
    filter(coordinateUncertaintyInMeters / 1000 <= 1 | is.na(coordinateUncertaintyInMeters))
  
  # function to check if the coordinates after the decimal point are less than two digits (1-km resolution)
  check_pattern <- function(coord) {
    grepl("\\.\\d{2,}$", as.character(coord))
  }
  
  # filter rows based on the pattern
  dat <- subset(dat, check_pattern(dat$decimalLongitude) & check_pattern(dat$decimalLatitude))
  
  # remove data sourced from fossils
  dat <- filter(dat, basisOfRecord != "FOSSIL_SPECIMEN")
  
  # remove individual count = 0
  dat <- dat %>%
    filter(individualCount > 0 | is.na(individualCount))
  
  # remove records older than 1970
  dat <- dat %>% filter(year >= 1970)
  
  # remove duplicates
  dat <- unique(dat[, c("species", "decimalLongitude", "decimalLatitude")])
  
  return(dat)
}

# run the function
output_cleaned <- GBIF_cleaning(dat)


# PART II Spatial thinning -------------------------------------------------

GBIF_thinning <- function(x){
  
  # select the columns we need
  output_cleaned <- output_cleaned %>%
    dplyr::select(decimalLongitude, decimalLatitude)

  # create a new column named by the genus/family name
  output_cleaned$SPEC <- taxa

  # thinning
  thinned <-
    thin( loc.data = output_cleaned, 
          lat.col = "decimalLatitude", long.col = "decimalLongitude", 
          spec.col = "SPEC", 
          thin.par = 1, #the distance (in km) that you want records to be separated by
          reps = 100, 
          locs.thinned.list.return = TRUE, 
          write.files = TRUE, 
          max.files = 1, 
          out.dir = output.dir,
          out.base = paste0(taxa,"_thinned"), 
          write.log.file = TRUE,
          log.file = "thinned_full_log_file.txt" )

  return(thinned)
}

# run the function
output_thinned <- GBIF_thinning(output_cleaned)















