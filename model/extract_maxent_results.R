out_wd <- "//smb.isipd.dmawi.de/projects/bioing/data/ArcticSDM/Weihan/maxent_output/"

# load(glue::glue("{weihan}/GBIF/GBIF_Plantea_thinned_sibala.rda"))
# 
# occTab <- gbif_thin %>% filter(species %in% (gbif_thin %>% group_by(species) %>% st_drop_geometry() %>% summarise(count = n()) %>%
#                                                arrange(desc(count)) %>% filter(count>=100) %>% pull(species)))
# 
# spList <- occTab %>% st_drop_geometry() %>% group_by(species) %>% summarise(count = n()) %>%
#   arrange(desc(count))
# 
# contrib_list <- list()

for (sp in spList$species[1:2]) {
  
  cat("Processing velocity:", sp, "\n")
  
  result <- read.csv2(glue::glue("{out_wd}{sp}/MaxEnt/ModelOutput/maxentResults.csv"), header=TRUE, stringsAsFactors = FALSE, sep=',',encoding="UTF-8")
  
  contrib <- result %>% select(., c("tasmax.contribution", "tasmin.contribution")) %>% as.data.frame()
  
  contrib$species <- sp
  colnames(contrib)[1:2] <- c('tmax', 'tmin')
  contrib_list[[sp]] <- contrib
}

combined_df <- do.call(rbind, contrib_list)

write.table(combined_df, glue::glue("{out_wd}/allmaxentResults.csv"), sep = ',', row.names = F)
