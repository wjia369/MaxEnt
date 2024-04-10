library(ENMeval)

#library(remotes)
#install_version("maptools", "0.9-5")


#=== Data preparation ====
# import environmental data
path <- file.path(system.file(package="dismo"), 'ex')
files <- list.files(path, pattern='grd$', full.names=TRUE )

predictors <- stack(files)
names(predictors)
plot(predictors)

# import occurance data
data(wrld_simpl)
file <- paste(system.file(package="dismo"), "/ex/bradypus.csv", sep="")
bradypus <- read.table(file,  header=TRUE,  sep=',')
bradypus  <- bradypus[,-1]


#=== ENMevaluation ====
bg <- dismo::randomPoints(predictors[[1]], n = 10000) %>% as.data.frame()

colnames(bg) <- colnames(bradypus)

enmeval_results <- ENMeval::ENMevaluate(occs = bradypus, envs = predictors, bg = bg,
                                        tune.args = list(fc = c("L","LQ","H", "LQH", "LQHP", "LQHPT"), rm = 1:5),
                                        partitions = "randomkfold", partition.settings = list(kfolds = 2), 
                                        algorithm = "maxnet")

enmeval <- enmeval_results@results[which.min(enmeval_results@results$delta.AICc), c(1, 2)]

fc <- as.character(enmeval$fc)
rm <- as.numeric(enmeval$rm)


# set up the argument list
# adjust arguments based on fc
if (fc == "L") {
  args_list <- c('linear')
} else if (fc == "LQ") {
  args_list <- c('linear', 'quadratic')
} else if (fc == "H") {
  args_list <- c('hinge')
} else if (fc == "LQH") {
  args_list <- c('linear', 'quadratic', 'hinge')
} else if (fc == "LQHP") {
  args_list <- c('linear', 'quadratic', 'product', 'hinge')
} else if (fc == "LQHPT") {
  args_list <- c('linear', 'quadratic', 'product', 'threshold', 'hinge')
}

# check if it is correct
args_list





# MegaSDM
#devtools::install_github("brshipley/megaSDM", build_vignettes = TRUE)
library(megaSDM)
library(dismo)
library(dplyr)
library(raster)
library(terra)

setwd("E:/AWI/glacial_refugia_project/Framework/MaxEnt/test")

# import environmental data
path <- file.path(system.file(package="dismo"), 'ex')
files <- list.files(path, pattern='grd$', full.names=TRUE )

predictors <- raster::stack(files[1:8])
predictors_spat <- terra::rast(predictors)

# import occurance data
#data(wrld_simpl)
#file <- paste(system.file(package="dismo"), "/ex/bradypus.csv", sep="")
#bradypus <- read.table(file,  header=TRUE,  sep=',')
#colnames(bradypus)[2:3] <- c('decimalLongitude', 'decimalLatitude')



occ_data <- "C:/Users/whjia/AppData/Local/R/win-library/4.3/dismo/ex"
occlist <- list.files(occ_data, "bradypus.csv", full.names = TRUE)

occ_output <- "E:/AWI/glacial_refugia_project/Framework/MaxEnt/test"


OccurrenceManagement(occlist = occlist,
                     output = occ_output,
                     envextract = TRUE,
                     envsample = TRUE,
                     nbins = 9,
                     envdata = predictors_spat)


bg <- dismo::randomPoints(predictors[[1]], n = 10000) %>% as.data.frame()

write.table(bg, "E:/AWI/glacial_refugia_project/Framework/MaxEnt/test/bg.csv", sep=',', row.names = F)

bg_data <- "E:/AWI/glacial_refugia_project/Framework/MaxEnt/test"
bglist <- list.files(bg_data, "bg.csv", full.names = TRUE)

OccurrenceManagement(occlist = bglist,
                     output = occ_output,
                     envextract = TRUE,
                     envsample = TRUE,
                     nbins = 9,
                     envdata = predictors_spat)

model_output <- "E:/AWI/glacial_refugia_project/Framework/MaxEnt/test/model_output"

occlist <- list.files(occ_output, "bradypus.csv", full.names = TRUE)
bglist <- list.files(occ_output, "bg.csv", full.names = TRUE)

MaxEntModel(occlist = occlist,
            bglist = bglist,
            model_output = model_output,
            ncores = 2,
            nrep = 5,
            alloutputs = T,
            regularization =rm,
            features = args_list)

result_dir <- "E:/AWI/glacial_refugia_project/Framework/MaxEnt/test/results"

study_dir <- "E:/AWI/glacial_refugia_project/Framework/MaxEnt/test/study_dir"

time_periods <- 2010

# TO DO!!!!
MaxEntProj(input = model_output,
           time_periods = 2010,
           scenarios = NA,
           study_dir = path,
           predict_dirs = NA,
           output = result_dir,
           aucval = 0.7,
           ncores = 2)

