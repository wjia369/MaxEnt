library(stars)
library(tibble)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(tidypaleo)
library(ggstance)

wd <- "E:/AWI/glacial_refugia_project/"
output <- "E:/AWI/glacial_refugia_project/Framework/ArcticSDM/ASV_MaxEnt_DNA/"
proj <- "+proj=laea +lon_0=-170 +lat_0=90"

# lake <- "Bolshoe Toko"
# 
# metabar <- read.csv2(glue::glue("{wd}{lake}/obi3_output/new/{lake}_id1_PCRsorted.csv"), header=TRUE, stringsAsFactors = FALSE, sep=',',encoding="UTF-8")
# 
# fls <- colnames(metabar[,6:ncol(metabar)])
# 
# sample_list <- tibble(col_names = fls) %>% mutate(PCRID  = as.character(sapply(strsplit(fls, "_"), function(x) x[1])),
#                                           ExtracID  = as.character(sapply(strsplit(fls, "_"), function(x) x[2])),
#                                           depth  = as.numeric(sapply(strsplit(fls, "_"), function(x) x[3])),
#                                           type  = as.character(sapply(strsplit(fls, "_"), function(x) x[4])))
# 
# # PCR replicate counts 
# rep_counts <- table(sample_list$depth)
# 
# # remove samples only have one replicate
# sample_list <- sample_list %>% filter(!(depth %in% names(rep_counts[rep_counts == 1])))
# 
# # select samples with 2, 3, and 4 replicates
# sample_2_rep <- sample_list %>% filter(depth %in% names(rep_counts[rep_counts == 2]))
# sample_3_rep <- sample_list %>% filter(depth %in% names(rep_counts[rep_counts == 3]))
# sample_4_rep <- sample_list %>% filter(depth %in% names(rep_counts[rep_counts == 4]))
# 
# sample_NA <- sample_list %>% filter(depth %in% names(rep_counts[rep_counts = NA]))
# metabar_control <- metabar %>% select(all_of(fls[fls %in% sample_NA$col_names]))
# 
# 
# #-----------------------------------------------------------------------------------------------
# #-----------------------------------------------------------------------------------------------
# 
# # STEP I - Check PCR repeatability #
# 
# {
#   # Check the repeatability of 2 replicates #
#   
#   metabar_2_rep <- metabar %>% select(all_of(fls[fls %in% sample_2_rep$col_names]))
# 
#   x2 <- metabar_2_rep[,1:ncol(metabar_2_rep)]
# 
#   steps2 <- seq(1,ncol(x2),2) # define from, to, step width
# 
#   for (i in 1:length(steps2)){
#     sub2 <- x2[ ,c(steps2[i], steps2[i]+1)]
#     for (row in 1:nrow(sub2)) {
#       if (sub2[row, 1] == 0 && sub2[row, 2] > 0) {
#         sub2[row, 2] <- 0
#       }
#       if (sub2[row, 2] == 0 && sub2[row, 1] > 0) {
#         sub2[row, 1] <- 0
#       }
#     }
#     x2[, c(steps2[i], steps2[i]+1)] <- sub2
#   }
# 
#   metabar_2_rep_filtered <- x2
#   
# 
#   # Check the repeatability of 3 replicates #
#   
#   metabar_3_rep <- metabar %>% select(all_of(fls[fls %in% sample_3_rep$col_names]))
# 
#   x3 <- metabar_3_rep[,1:ncol(metabar_3_rep)]
# 
#   steps3 <- seq(1,ncol(x3),3) # define from, to, step width
# 
#   for (i in 1:length(steps3)){
#     sub3 <- x3[ ,c(steps3[i], steps3[i]+1, steps3[i]+2)]
#     for (row in 1:nrow(sub3)) {
#       if (sum(sub3[row, ] == 0) == 2) {
#         sub3[row, sub3[row, ] > 0] <- 0
#       }
#     }
#     x3[, c(steps3[i], steps3[i]+1, steps3[i]+2)] <- sub3
#   }
# 
#   metabar_3_rep_filtered <- x3
# 
# 
#   # Check the repeatability of 4 replicates #
# 
#   metabar_4_rep <- metabar %>% select(all_of(fls[fls %in% sample_4_rep$col_names]))
# 
#   x4 <- metabar_4_rep[,1:ncol(metabar_4_rep)]
# 
#   steps4 <- seq(1,ncol(x4),4) # define from, to, step width
# 
#   for (i in 1:length(steps4)){
#     sub4 <- x4[ ,c(steps4[i], steps4[i]+1, steps4[i]+2, steps4[i]+3)]
#     for (row in 1:nrow(sub4)) {
#       if (sum(sub4[row, ] == 0) >= 3) { 
#         sub4[row, sub4[row, ] > 0] <- 0
#       }
#     }
#     x4[, c(steps4[i], steps4[i]+1, steps4[i]+2, steps4[i]+3)] <- sub4
#   }
# 
#   metabar_4_rep_filtered <- x4
# 
# }
# 
# 
# # output filtered data
# filtered_data <- cbind(metabar[,1:5], metabar_2_rep_filtered, metabar_3_rep_filtered, metabar_4_rep_filtered, metabar[,1:5], metabar_control)
# 
# write.table(filtered_data, glue::glue("{wd}{lake}/obi3_output/new/{lake}_id1_replicate_filtered.csv"), sep = ',', row.names = F)
# 
# 
# #-----------------------------------------------------------------------------------------------
# #-----------------------------------------------------------------------------------------------
# 
# # STEP II - Check negative controls #
# 
# {
#   controls <- cbind(metabar[,1:5], metabar_control)
# 
#   # convert counts less than 10 to 0 
#   spalten <- seq(6,ncol(controls),1)
# 
#   controls1 <- controls[,spalten]
# 
#   # convert the count <10 to 0
#   for(i in 1:dim(controls1)[1]) {
#     for(j in 1:dim(controls1)[2])
#     {
#       if(controls1[i,j]<10)
#       {
#         controls1[i,j]=0
#       }
#       if(controls1[i,j]>=10)
#       {
#         controls1[i,j]=controls1[i,j]
#       }
#     }
#   }
# 
#   controls2 <- cbind(controls[,1:5], controls1)
# 
#   # remove columns are 0
#   colsums_control <- colSums(controls2[, 6:ncol(controls2)])
#   cleaned_control <- controls2[, !(colnames(controls2) %in% colnames(controls2)[6:ncol(controls2)][which(colsums_control == 0)])]
#   
#   # remove rows are 0
#   rowsums_control <- rowSums(cleaned_control[, 6:ncol(cleaned_control)])
#   cleaned_control1 <- cleaned_control[!(rowsums_control == 0), ]
# 
#   # save the results
#   write.table(cleaned_control1, glue::glue("{wd}{lake}/obi3_output/new/{lake}_id1_cleaned_controls.csv"), sep = ',', row.names = F)
#   
# }
# 
# # Then check this CSV file manually
# 
# 
# #-----------------------------------------------------------------------------------------------
# #-----------------------------------------------------------------------------------------------
# 
# # STEP III - Merge PCR replicates #
# 
# {
#   # two replicates
# 
#   r2 <- metabar_2_rep_filtered
# 
#   steps_r2 <- seq(1,ncol(r2),2)
#   tworowsum <- NULL
# 
#   for (i in 1:length(steps_r2)){
#     sub_r2 <- r2[ ,c(steps_r2[i], steps_r2[i]+1)]
#     tworowsum <- cbind(tworowsum, rowSums(sub_r2))
#   }
# 
#   colnames(tworowsum) <- unique(sample_2_rep$depth)
# 
# 
#   # three replicates
# 
#   r3 <- metabar_3_rep_filtered
# 
#   steps_r3 <- seq(1,ncol(r3),3)
#   threerowsum <- NULL
# 
#   for (i in 1:length(steps_r3)){
#     sub_r3 <- r3[ ,c(steps_r3[i], steps_r3[i]+1, steps_r3[i]+2)]
#     threerowsum <- cbind(threerowsum, rowSums(sub_r3))
#   }
# 
#   colnames(threerowsum) <- unique(sample_3_rep$depth)
# 
#   # four replicates
# 
#   r4 <- metabar_4_rep_filtered
# 
#   steps_r4 <- seq(1,ncol(r4),4) # define from, to, step width
#   fourrowsum <- NULL
# 
#   for (i in 1:length(steps_r4)){
#     sub_r4 <- r4[ ,c(steps_r4[i], steps_r4[i]+1, steps_r4[i]+2, steps_r4[i]+3)]
#     fourrowsum <- cbind(fourrowsum, rowSums(sub_r4))
#   }
# 
#   colnames(fourrowsum) <- unique(sample_4_rep$depth)
# 
# }
# 
# # output the results
# final_merge <- cbind(metabar[,1:5], tworowsum, threerowsum, fourrowsum)
# 
# 
# #-----------------------------------------------------------------------------------------------
# #-----------------------------------------------------------------------------------------------
# 
# # STEP IV - Further filtering steps #
# 
# {
#   # convert counts less than 10 to 0
#   step_merge <- seq(6,ncol(final_merge),1)
# 
#   df <- final_merge[, step_merge]
# 
#   for(i in 1:dim(df)[1]) {
#     for(j in 1:dim(df)[2])
#     {
#       if(df[i,j]<10)
#       {
#         df[i,j]=0
#       }
#       if(df[i,j]>=10)
#       {
#         df[i,j]=df[i,j]
#       }
#     }
#   }
# 
#   final_merge_c10 <- cbind(final_merge[,1:5], df)
# 
# 
#   # remove columns are 0
#   colsums_merge <- colSums(final_merge_c10[, 6:ncol(final_merge_c10)])
#   final_merge_c10_cleaned <- final_merge_c10[, !(colnames(final_merge_c10) %in% colnames(final_merge_c10)[6:ncol(final_merge_c10)][which(colsums_merge == 0)])]
#   
#   # remove rows are 0
#   rowsums_merge <- rowSums(final_merge_c10_cleaned[, 6:ncol(final_merge_c10_cleaned)])
#   final_merge_c10_cleaned1 <- final_merge_c10_cleaned[!(rowsums_merge == 0), ]
#   
#   # reorder the columns
#   col <- final_merge_c10_cleaned1[,6:ncol(final_merge_c10_cleaned1)]
#   col <- col %>% select(order(as.numeric(names(col))))
#   
#   final_merge_c10_cleaned2 <- cbind(final_merge_c10_cleaned1[,1:5], col)
#   
#   # save the results
#   write.table(final_merge_c10_cleaned2, glue::glue("{wd}{lake}/obi3_output/new/{lake}_id1_PCRmerged_c10.csv"), sep = ',', row.names = F)
# 
# }


#-----------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------



# STEP V - Plotting #

# read tables

lake_list <- c("Bolshoe Toko", "Ulu", "Bilyakh", "Emanda", "Ilirney", "Rauchuagytgyn", "E5", "Lama", "Levinson-Lessing")

files <- list.files(glue::glue("{wd}all_lakes/{lake_list}/obi3_output/new"), pattern = "_id1_PCRmerged_c10_plot.csv", full.names = TRUE)
files

# choose the lake
select_lake <- read.csv(files[7], header=TRUE, sep=",", check.names=FALSE)

lake_name <- "Levinson-Lessing" # copy the lake name to here

target_taxa <- c("Carex")
pattern <- paste(target_taxa, collapse = "|")



{
  selected_columns <- grep(pattern, names(select_lake), value = TRUE)
  select_lake1 <- select_lake[, c("age", selected_columns)]
  select_lake2 <- select_lake1 %>% gather(key=param, value=value, -`age`)
  
  # convert to presence/absence
  select_lake3 <- select_lake2 %>% mutate(value = ifelse(value > 0, 1, 0))
  
  # plot as counts
  plot_count <- ggplot(select_lake2, aes(x = value, y = age, group = param)) +
    geom_barh(stat = "identity", color = "gray48") +
    facet_geochem_gridh(vars(param))+
    geom_rect(aes(xmin=-Inf, xmax=Inf, ymin=19000, ymax=25000), fill='cadetblue1', alpha=0.01) + # LGM
    scale_y_reverse(limits = c(30000, 0), breaks = seq(30000, 0, by = -1000),  expand=c(0.01,0.01)) +
    labs(x = NULL, y = "Age (cal. yr BP)", title=paste0(lake_name, " - ", paste(target_taxa, collapse = ", "))) +
    theme_bw()+ 
    theme(strip.text.x = element_text(size = 10, hjust = 0, vjust = 0.5, angle = 90), 
          strip.background = element_blank(), 
          axis.text.x = element_text(size = 10, angle = 90))
  
  
  # plot as presence and absence
  plot_pa <- ggplot(select_lake3, aes(x = value, y = age, group = param)) +
    geom_point(stat = "identity", aes(shape = factor(value), colour = factor(value))) +
    scale_shape_manual(values = c("0" = 1, "1" = 16)) +
    scale_color_manual(values = c("0" = "grey", "1" = "red")) + 
    facet_geochem_gridh(vars(param))+
    geom_rect(aes(xmin=-Inf, xmax=Inf, ymin=19000, ymax=25000), fill='cadetblue1', alpha=0.01) + # LGM
    scale_y_reverse(limits = c(30000, 0), breaks = seq(30000, 0, by = -1000),  expand=c(0.01,0.01)) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 1))+
    labs(x = NULL, y = "Age (cal. yr BP)", title=paste0(lake_name, " - ", paste(target_taxa, collapse = ", "))) +
    theme_bw()+ 
    theme(strip.text.x = element_text(size = 10, hjust = 0, vjust = 0.5, angle = 90), 
          strip.background = element_blank(), 
          axis.text.x = element_text(size = 10, angle = 90))
  
}

plot_count
plot_pa

ggsave(plot_count, file = glue::glue("{output}{lake_name}_{target_taxa}_count.png"), width = 15, height = 10, dpi = 300, limitsize = FALSE) %>% suppressWarnings()
ggsave(plot_pa, file = glue::glue("{output}{lake_name}_{target_taxa}_pre_abs.png"), width = 15, height = 10, dpi = 300, limitsize = FALSE) %>% suppressWarnings()





# check what species are behind these sequences?

# load the data
{
  # load coordinates of 9 lakes
  load(glue::glue("{wd}all_lakes/9_lakes_coor.rda"))
  
  # read the header file
  sibala_header <- read.csv2(glue::glue("{wd}Framework/ArcticSDM/sibala_db/duplication/sibala_db_header_list.csv"),
                             header=TRUE, stringsAsFactors = FALSE, sep=',',encoding="UTF-8")

  # list all maxent results
  maxent_files <- "//smb.isipd.dmawi.de/projects/bioing/data/ArcticSDM/Weihan/maxent_10km/"
  
  maxent_current <- list.files(maxent_files, pattern = "MaxEnt_current_10km.tif", full.names = TRUE)
  maxent_21ka <- list.files(maxent_files, pattern = "MaxEnt_21ka_10km.tif", full.names = TRUE)
  
  # list all velocity results
  velo_files <- "//smb.isipd.dmawi.de/projects/bioing/data/ArcticSDM/Weihan/velocity_10km/tif/"
  velocity_files <- list.files(velo_files, pattern = ".tif", full.names = TRUE)
}


# load the other csvs that contain sequences
files1 <- list.files(glue::glue("{wd}all_lakes/{lake_list}/obi3_output/new"), pattern = "_id1_PCRmerged_c10.csv", full.names = TRUE)
files1

select_lake1 <- read.csv(files1[7], header=TRUE, sep=",", check.names=FALSE)

selected_rows <- grepl(pattern, select_lake1$scientific_name, ignore.case = TRUE)
select_lake2 <- select_lake1[selected_rows,]


# which species are behind these sequences?
sp <- sibala_header %>% filter(seq %in% select_lake2$NUC_SEQ)


# check whether we have MaxEnt results of these species
matched_maxent <- list()

for(species in unique(sp$final_name)) {
  matching_maxent <- grep(species, maxent_21ka, value = TRUE)
  matched_maxent <- c(matched_maxent, matching_maxent)
}

matched_maxent

matched <- matched_maxent


#-----------------------------------------------------------------------------------------------

# Merge raster files and calculate the mean #

{
  # merge the raster files of the same ASV
  stars_sp <- read_stars(sapply(matched, function(file) glue::glue("{file}")))
  
  # rename the star object, otherwise the taxa names will be incomplete
  names(stars_sp) <- sapply(matched, function(file) basename(file))
  names(stars_sp)
  
  # extract the name of matched species for plotting later 
  matched_species_name <- lapply(names(stars_sp), function(x) unlist(strsplit(x, "_"))[1])
  
  
  
  # merge the stars
  stars_sp <- merge(stars_sp)
  
  # calculate mean values
  mean_values <- stars_sp %>%
    st_apply(MARGIN = 1:2, FUN = function(x) mean(x, na.rm = TRUE)) %>%
    st_as_sf()
  
  mean_values_rast <- st_rasterize(mean_values, st_as_stars(st_bbox(mean_values),
                                                            dx = st_res(stars_sp)[1], dy = st_res(stars_sp)[2],
                                                            values = NA_real_)) %>% st_set_crs(proj) %>% suppressWarnings()
  
}



# plot MaxEnt
time <- 'current'
time <- '21ka'

out <- 
  ggplot() +
  geom_stars(data = mean_values_rast, downsample = 0.1, show.legend = T) +
  geom_sf(data = lake_new_coor, color = 'red', size=2) +
  scale_fill_gradient(low = 'black', high = 'white', breaks = seq(0, 1, length = 5), na.value = "transparent", trans = "sqrt") +
  labs(fill = "Suitability") +
  labs(title=paste0("Mean MaxEnt ",time, " - aDNA"),
       x ="", y = "") +
  annotate("text", x=0, y=0, label= paste(matched_species_name, collapse = "\n"), size=4, hjust = 0, vjust = 0.4) +
  coord_sf() +
  theme_bw()

out

ggsave(file.path(output, paste0("Mean_MaxEnt ", time, " - aDNA - ", paste(target_taxa, collapse = ", "), ".png")),
       out, units = "cm", width = 30, height = 30)

write_stars(mean_values_rast, glue::glue("{output}/Mean_MaxEnt_{time}_aDNA.tif"))



# new velocity TO DO!!!







# plotting #
# library(vegan)
# library(rioja)
# 
# wd <- "E:/AWI/glacial_refugia_project/all_lakes/"
# 
# specseq <- read.csv2(glue::glue("{wd}B-Toko/obi3_output/new/Bolshoe Toko_id1_PCRmerged_c10_plot.csv"), header=T, stringsAsFactors = F, sep=',',encoding="UTF-8")
# 
# # Remove taxa where total abundanace is less than 10%
# specseq1 <- specseq[,2:91]
# 
# specseq2 <- specseq1[, !(colnames(specseq1) %in% colnames(specseq1)[which(colSums(specseq1) == 0)])]
# 
# specseq3 <- ifelse(specseq2 > 0, 1, 0) %>% as.data.frame()
# 
# specseq4 <- specseq3[, grep('Saxifraga', names(specseq3), value = TRUE)]
# 
# # Cluster analysis
# ma.dist <- vegdist(specseq4, method="jaccard", binary=T, diag=FALSE, upper=FALSE, na.rm = FALSE) 
# ma.chclust <- chclust(ma.dist, method="coniss")
# 
# ex <- c(rep(TRUE, times=90))
# 
# # Filled plot with cluster analysis
# p.col <- c(rep("gray58"))
# 
# y.scale <- 1:43
# 
# par(mar = c(5, 4, 4, 4))
# 
# #xSpace usually is 0.01, but 0.001 could add more curves
# pol.plot <- strat.plot(specseq4, yvar=y.scale, y.tks=y.scale, y.axis=FALSE, wa.order = "bottomleft", y.rev=TRUE, 
#                        plot.line=F, plot.poly=F, plot.bar=T, col.bar="red", lwd.bar=5,
#                        col.poly=p.col, col.poly.line="black", scale.percent=F, 
#                        xSpace=0.001, x.pc.inc=10, x.pc.lab=TRUE, x.pc.omit0=TRUE, srt.xlabel=45, las=2, 
#                        exag=ex, col.exag="auto", exag.mult=3, exag.alpha=0.5, cex.xlabel=0.8,
#                        #title="Morocco Pollen Surface Samples",
#                        clust=ma.chclust)
# 
# pol.plot <- strat.plot(specseq4, yvar=y.scale, y.tks=y.scale, y.axis=FALSE, wa.order = "bottomleft", y.rev=TRUE, 
#                        plot.line=F, plot.poly=F, plot.bar=T, col.bar="grey", lwd.bar=5, col.poly.line="black", scale.percent=F, 
#                        xSpace=0.001, x.pc.inc=10, x.pc.lab=TRUE, x.pc.omit0=TRUE, srt.xlabel=45, las=2, 
#                        cex.xlabel=0.8,
#                        title="Bolshoe Toko Saxifragaceae")
# 
# 
# 
# par(fig=c(0.04, 1, 0.07, 0.8))
# axis(2, at=y.scale, labels=specseq[,1], las=2)
# 
# addClustZone(pol.plot, ma.chclust, nZone=2, lwd=1.5, lty=2, col="grey25")
# 
# 
# 
# 

