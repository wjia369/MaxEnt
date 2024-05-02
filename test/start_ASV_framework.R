library(stars)
library(dplyr)
library(tidyr)
library(ggplot2)

wd <- "E:/AWI/glacial_refugia_project/"
output <- "E:/AWI/glacial_refugia_project/Framework/ArcticSDM/ASV_MaxEnt_DNA/"

proj <- "+proj=laea +lon_0=-170 +lat_0=90"

# load the data

{
  # load coordinates of 9 lakes
  load(glue::glue("{wd}all_lakes/9_lakes_coor.rda"))
  
  # read the header file
  sibala_header <- read.csv2(glue::glue("{wd}Framework/ArcticSDM/sibala_db/duplication/sibala_db_header_list.csv"),
                             header=TRUE, stringsAsFactors = FALSE, sep=',',encoding="UTF-8")
  
  # read the information of ASVs
  load(glue::glue("{wd}Framework/ArcticSDM/sibala_db/duplication/sibala_db_asv_info.rda"))
  
  # read the duplicate information
  load(glue::glue("{wd}Framework/ArcticSDM/sibala_db/duplication/dup_sibala_full_unique.rda"))
  
  # # read the sequence list of 10 lakes
  # seq_10lakes <- read.csv2("E:/AWI/glacial_refugia_project/all_lakes/10lakes_seq_list_full_lineage.csv", 
  #                          header=TRUE, stringsAsFactors = FALSE, sep=',', encoding="UTF-8")
  
  # list all maxent results
  maxent_files <- "//smb.isipd.dmawi.de/projects/bioing/data/ArcticSDM/Weihan/maxent_10km/"
  
  maxent_current <- list.files(maxent_files, pattern = "MaxEnt_current_10km.tif", full.names = TRUE)
  maxent_21ka <- list.files(maxent_files, pattern = "MaxEnt_21ka_10km.tif", full.names = TRUE)
  
  # list all velocity results
  velo_files <- "//smb.isipd.dmawi.de/projects/bioing/data/ArcticSDM/Weihan/velocity_10km/tif/"
  velocity_files <- list.files(velo_files, pattern = ".tif", full.names = TRUE)
}


#-----------------------------------------------------------------------------------------------
# # THINGS NEED TO BE CHANGED #
# 
# # which lake
# lake_name <- "Ulu"
# 
# # which data
# metabar <- read.csv(glue::glue("{wd}all_lakes/{lake_name}/obi3_output/new/{lake_name}_id1_PCRmerged_c10.csv"), 
#                  header=TRUE, sep=",", check.names=FALSE)
# 
# # which taxa
# target_taxa <- c("Saxifraga", "Micranthes", "Betula")
# pattern <- paste(target_taxa, collapse = "|")
# taxa_df <- metabar %>% filter(grepl(pattern, scientific_name, ignore.case = TRUE))
# 
# # which time
# time <- "current"
# time <- "21ka"

# #-----------------------------------------------------------------------------------------------
# 
# # find species name of these ASVs in sibala database
# matched_species <- sibala_header %>% filter(seq %in% taxa_df$NUC_SEQ)
# unique(matched_species$final_name)
# 
# 
# # check whether we have MaxEnt results of these species
# # do not forget to change the list (maxent_current)
# matched_maxent <- list()
# 
# for(species in unique(matched_species$final_name)) {
#   matching_maxent <- grep(species, maxent_21ka, value = TRUE)
#   matched_maxent <- c(matched_maxent, matching_maxent)
# }
# 
# matched <- matched_maxent
# 
# # check whether we have velocity results of these species
# matched_velo <- list()
# 
# for(species in unique(matched_species$final_name)) {
#   matching_velo <- grep(species, velocity_files, value = TRUE)
#   matched_velo <- c(matched_velo, matching_velo)
# }
# 
# matched <- matched_velo
# 
# #-----------------------------------------------------------------------------------------------
# 
# # Merge raster files and calculate the mean #
# 
# {
#   # merge the raster files of the same ASV
#   stars_sp <- read_stars(sapply(matched, function(file) glue::glue("{file}")))
#   
#   # rename the star object, otherwise the taxa names will be incomplete
#   names(stars_sp) <- sapply(matched, function(file) basename(file))
#   names(stars_sp)
#   
#   # extract the name of matched species for plotting later 
#   matched_species_name <- lapply(names(stars_sp), function(x) unlist(strsplit(x, "_"))[1])
#   
#   
#   
#   # merge the stars
#   stars_sp <- merge(stars_sp)
#   
#   # calculate mean values
#   mean_values <- stars_sp %>%
#     st_apply(MARGIN = 1:2, FUN = function(x) mean(x, na.rm = TRUE)) %>%
#     st_as_sf()
#   
#   mean_values_rast <- st_rasterize(mean_values, st_as_stars(st_bbox(mean_values),
#                                             dx = st_res(stars_sp)[1], dy = st_res(stars_sp)[2],
#                                             values = NA_real_)) %>% st_set_crs(proj) %>% suppressWarnings()
#   
# }
# 
# 
# 
# # plot MaxEnt
# out <- 
#   ggplot() +
#   geom_stars(data = mean_values_rast, downsample = 0.1, show.legend = T) +
#   geom_sf(data = lake_new_coor, color = 'red', size=2) +
#   scale_fill_gradient(low = 'black', high = 'white', breaks = seq(0, 1, length = 5), na.value = "transparent", trans = "sqrt") +
#   labs(fill = "Suitability") +
#   labs(title=paste0("Mean MaxEnt ",time, " - ASV - ", paste(target_taxa, collapse = ", ")),
#          x ="", y = "") +
#   annotate("text", x=0, y=0, label= paste(matched_species_name, collapse = "\n"), size=4, hjust = 0, vjust = 0.4) +
#   coord_sf() +
#   theme_bw()
#   
# out
# 
# ggsave(paste0("Mean MaxEnt ",time, " - ASV - ", paste(target_taxa, collapse = ", "),".png"), out, units = "cm", width = 30, height = 30)
# 
# 
# # plot velocity
# out <- 
#   ggplot() +
#   geom_stars(data = mean_values_rast, downsample = 0.1, show.legend = T) +
#   geom_sf(data = lake_new_coor, color = 'red', size=2) +
#   scale_fill_gradient(low = 'black', high = 'white', breaks = seq(0, 1000, length = 5), na.value = "transparent", trans = "sqrt") +
#   labs(fill = "Distance (km)") +
#   labs(title=paste0("Mean velocity ", " - ASV - ", paste(target_taxa, collapse = ", ")),
#        x ="", y = "") +
#   annotate("text", x=0, y=0, label= paste(matched_species_name, collapse = "\n"), size=4, hjust = 0, vjust = 0.4) +
#   coord_sf() +
#   theme_bw()
# 
# out
# 
# ggsave(paste0("Mean velocity ", " - ASV - ", paste(target_taxa, collapse = ", "),".png"), out, units = "cm", width = 30, height = 30)
# 
# 
# 
# # Check the values around the lake #
# 
# # create a 50-km buffer around the lake
# buffer <- st_buffer(lake_new_coor %>% filter(lake==lake_name), dist = 50000) %>% st_transform(., st_crs(mean_values_rast))
# 
# cropped <- st_crop(mean_values_rast, st_geometry(buffer)) %>% st_as_sf() %>% setNames(c("value", "geometry"))
# 
# summary(cropped$value)


#-----------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------

# Process the results based on ASV level - current - 02.05.2024

for (i in 1:nrow(dup[,-1])) {
  
  cat("Start processing:", i, "\n")
  
  # create the species list of this ASV, e.g., the first ASV
  species_list <- na.omit(unlist(dup[i,-1]))
  
  # check whether we have MaxEnt results of certain species
  matched_maxent <- list()
  
  for(species in unique(species_list)) {
    matching_maxent <- grep(species, maxent_current, value = TRUE)
    matched_maxent <- c(matched_maxent, matching_maxent)
  }
  
  matched <- matched_maxent
  
  if (length(matched)==0) {
    next
  }
  
  # Merge raster files and calculate the mean #

  # merge the raster files of the same ASV
  stars_sp <- read_stars(sapply(matched, function(file) glue::glue("{file}")))
  
  # rename the star object, otherwise the taxa names will be incomplete
  names(stars_sp) <- sapply(matched, function(file) basename(file))
  
  # extract the name of matched species for plotting later 
  matched_species_name <- lapply(names(stars_sp), function(x) unlist(strsplit(x, "_"))[1])
  
  if (length(stars_sp) > 1) {
    # merge the stars
    stars_merge <- merge(stars_sp)
    
    # calculate mean values
    mean_values <- stars_merge %>%
      st_apply(MARGIN = 1:2, FUN = function(x) mean(x, na.rm = TRUE)) %>%
      st_as_sf()
    
    mean_values_rast <- st_rasterize(mean_values, st_as_stars(st_bbox(mean_values),
                                                              dx = st_res(stars_merge)[1], dy = st_res(stars_merge)[2],
                                                              values = NA_real_)) %>% st_set_crs(proj) %>% suppressWarnings()
  } else {
    mean_values_rast <- stars_sp
  }


  out <- 
    ggplot() +
    geom_stars(data = mean_values_rast, downsample = 0.1, show.legend = T) +
    geom_sf(data = lake_new_coor, color = 'red', size=2) +
    scale_fill_gradient(low = 'black', high = 'white', breaks = seq(0, 1, length = 5), na.value = "transparent", trans = "sqrt") +
    labs(fill = "Suitability") +
    labs(title=paste0("Mean MaxEnt current ",i, " - ASV level"),
         x ="", y = "") +
    annotate("text", x=0, y=0, label= paste(matched_species_name, collapse = "\n"), size=4, hjust = 0, vjust = 0.4) +
    coord_sf() +
    theme_bw()

  ggsave(file.path(output, paste0("Mean_MaxEnt current ", "No.", i, " - ASV level.png")),
       plot=out, units = "cm", width = 30, height = 30)

  write_stars(mean_values_rast, glue::glue("{output}/Mean_MaxEnt_current_No.{i}_ASV_level.tif"))

}





# Process the results based on ASV level - 21 ka - 02.05.2024

for (i in 1:nrow(dup[,-1])) {
  
  cat("Start processing:", i, "\n")
  
  # create the species list of this ASV, e.g., the first ASV
  species_list <- na.omit(unlist(dup[i,-1]))
  
  # check whether we have MaxEnt results of certain species
  matched_maxent <- list()
  
  for(species in unique(species_list)) {
    matching_maxent <- grep(species, maxent_21ka, value = TRUE)
    matched_maxent <- c(matched_maxent, matching_maxent)
  }
  
  matched <- matched_maxent
  
  if (length(matched)==0) {
    next
  }
  
  # Merge raster files and calculate the mean #
  
  # merge the raster files of the same ASV
  stars_sp <- read_stars(sapply(matched, function(file) glue::glue("{file}")))
  
  # rename the star object, otherwise the taxa names will be incomplete
  names(stars_sp) <- sapply(matched, function(file) basename(file))
  
  # extract the name of matched species for plotting later 
  matched_species_name <- lapply(names(stars_sp), function(x) unlist(strsplit(x, "_"))[1])
  
  if (length(stars_sp) > 1) {
    # merge the stars
    stars_merge <- merge(stars_sp)
    
    # calculate mean values
    mean_values <- stars_merge %>%
      st_apply(MARGIN = 1:2, FUN = function(x) mean(x, na.rm = TRUE)) %>%
      st_as_sf()
    
    mean_values_rast <- st_rasterize(mean_values, st_as_stars(st_bbox(mean_values),
                                                              dx = st_res(stars_merge)[1], dy = st_res(stars_merge)[2],
                                                              values = NA_real_)) %>% st_set_crs(proj) %>% suppressWarnings()
  } else {
    mean_values_rast <- stars_sp
  }
  
  
  out <- 
    ggplot() +
    geom_stars(data = mean_values_rast, downsample = 0.1, show.legend = T) +
    geom_sf(data = lake_new_coor, color = 'red', size=2) +
    scale_fill_gradient(low = 'black', high = 'white', breaks = seq(0, 1, length = 5), na.value = "transparent", trans = "sqrt") +
    labs(fill = "Suitability") +
    labs(title=paste0("Mean MaxEnt 21 ka ",i, " - ASV level"),
         x ="", y = "") +
    annotate("text", x=0, y=0, label= paste(matched_species_name, collapse = "\n"), size=4, hjust = 0, vjust = 0.4) +
    coord_sf() +
    theme_bw()
  
  ggsave(file.path(output, paste0("Mean_MaxEnt 21 ka ", "No.", i, " - ASV level.png")),
         plot=out, units = "cm", width = 30, height = 30)
  
  write_stars(mean_values_rast, glue::glue("{output}/Mean_MaxEnt_21ka_No.{i}_ASV_level.tif"))
  
}






# dup_info <- read.csv2(glue::glue("{wd}Framework/ArcticSDM/sibala_db/duplication/sibala_db_duplicated_info.csv"),
#                            header=F, stringsAsFactors = FALSE, sep=',',encoding="UTF-8")
# 
# id_list <- dup_info[-1] %>% lapply(function(x) lapply(x, as.list)) %>% na.omit()
# 
# id_list1 <- na.omit(unlist(id_list))
# names(id_list1) <- NULL
# 
# sibala1 <- sibala_header %>% filter(!ID %in% id_list1)
# 
# sibala2 <- sibala1 %>% select(c("ID", "final_name", "seq"))
# sibala2$No <- 1
# sibala2 <- sibala2[, c("No", names(sibala2))]
# unique_info <- sibala2[,-5]
# 
# dup_info1 <- dup_info[,1:2] %>% setNames(c("No", "ID"))
# dup_info1$final_name <- NA
# dup_info1$seq <- NA
# 
# for (i in 1:nrow(dup_info1)) {
#   id <- dup_info1[i, "ID"]
#   matching_row <- sibala_header[sibala_header$ID == id, ]
#   if (nrow(matching_row) > 0) {
#     dup_info1[i, "final_name"] <- matching_row[1, "final_name"]
#     dup_info1[i, "seq"] <- matching_row[1, "seq"]
#   } else {
#     dup_info1[i, "seq"] <- NA
#   }
# }
# 
# asv_info <- rbind(dup_info1, unique_info)
# 
# save(asv_info, file=glue::glue("{wd}Framework/ArcticSDM/sibala_db/duplication/sibala_db_asv_info.rda"))







# # assign species name to dup info
# id_to_species <- setNames(sibala_header$final_name, sibala_header$ID)
# 
# df_species <- dup[,-1]
# 
# for (i in seq_along(df_species)) {
#   for (j in seq_along(df_species[[i]])) {
#     id <- df_species[[i]][j]
#     if (!is.na(id) && id %in% names(id_to_species)) {
#       df_species[[i]][j] <- id_to_species[id]
#     }
#   }
# }
# 
# dup_species <- cbind(dup[,1], df_species)