# Latest version of MaxEnt and velocity
# Modified by Weihan Jia on 02-04-2024
# 'mod' is the RasterStack of environmental variables
# 'occ_coordinates' is a dataframe for the coordinates of presence points

### Step 1: ENMeval ###

predictors <- mod

# create random background points
bg <- dismo::randomPoints(predictors[[1]], n = 10000) %>% as.data.frame()

# rename the columns
colnames(bg)[1] <-'long'
colnames(bg)[2] <-'lat'
colnames(occ_coordinates)[1] <-'long'
colnames(occ_coordinates)[2] <-'lat'

# ENMeval
# If ENMeval cannot run successfully, then output enmeval with NA
enmeval <- NA
tryCatch({
  enmeval_results <- ENMeval::ENMevaluate(occs = occ_coordinates, envs = predictors, bg = bg,
                                          tune.args = list(fc = c("L","LQ","H", "LQH", "LQHP", "LQHPT"), rm = 1:5),
                                          partitions = "randomkfold", partition.settings = list(kfolds = 2), 
                                          algorithm = "maxnet") %>% suppressMessages() %>% suppressWarnings()
  
  enmeval <- enmeval_results@results[which.min(enmeval_results@results$delta.AICc), c(1, 2)]
}, error = function(e) {
  enmeval <- NA
})

# extract fc and rm
fc <- as.character(enmeval$fc)
rm <- as.numeric(enmeval$rm)

# set up the argument list
args_list <- c('responsecurves=TRUE',
               'jackknife=TRUE',
               'pictures=TRUE',
               'autofeature=FALSE',
               'linear=TRUE',
               'quadratic=TRUE',
               'product=TRUE',
               'threshold=TRUE',
               'hinge=TRUE')

# adjust arguments based on fc and rm
if (any(is.na(enmeval))) {
  args_list[5:9] <- c('linear=TRUE', 'quadratic=TRUE', 'product=TRUE', 'threshold=TRUE', 'hinge=TRUE')
  args_list <- c(args_list, 'betamultiplier=1')
} else if (fc == "L") {
  args_list[5:9] <- c('linear=TRUE', 'quadratic=FALSE', 'product=FALSE', 'threshold=FALSE', 'hinge=FALSE')
  args_list <- c(args_list, paste0('betamultiplier=', rm))
} else if (fc == "LQ") {
  args_list[5:9] <- c('linear=TRUE', 'quadratic=TRUE', 'product=FALSE', 'threshold=FALSE', 'hinge=FALSE')
  args_list <- c(args_list, paste0('betamultiplier=', rm))
} else if (fc == "H") {
  args_list[5:9] <- c('linear=FALSE', 'quadratic=FALSE', 'product=FALSE', 'threshold=FALSE', 'hinge=TRUE')
  args_list <- c(args_list, paste0('betamultiplier=', rm))
} else if (fc == "LQH") {
  args_list[5:9] <- c('linear=TRUE', 'quadratic=TRUE', 'product=FALSE', 'threshold=FALSE', 'hinge=TRUE')
  args_list <- c(args_list, paste0('betamultiplier=', rm))
} else if (fc == "LQHP") {
  args_list[5:9] <- c('linear=TRUE', 'quadratic=TRUE', 'product=TRUE', 'threshold=FALSE', 'hinge=TRUE')
  args_list <- c(args_list, paste0('betamultiplier=', rm))
} else if (fc == "LQHPT") {
  args_list[5:9] <- c('linear=TRUE', 'quadratic=TRUE', 'product=TRUE', 'threshold=TRUE', 'hinge=TRUE')
  args_list <- c(args_list, paste0('betamultiplier=', rm))
}

# check if it is correct
args_list


### Step 2: maxent ###
# set the output directory
wd <- 'E:/AWI/glacial_refugia_project/Framework/MaxEnt/test/output_maxent/alnus_alnobetula/tasmax7_tasmin1/'

# maxent
xm <- dismo::maxent(x=predictors, p=occ_coordinates, a=NULL, removeDuplicates=TRUE, nbg=10000,
                    path=wd, args=args_list) %>% suppressMessages() %>% suppressWarnings() %>% suppressPackageStartupMessages()

# modern prediction
modern_pred <- dismo::predict(xm, predictors, progress="text",
                              filename=glue::glue('{wd}/maxent_{sp}_modern_pred.grd'),
                              overwrite=TRUE)

# maxent_model <- dismo::maxent(modTab[,-1], p = modTab$p, a=NULL, removeDuplicates=TRUE, nbg=10000,
#                                     path=wd, args=args_list)
# maxent_modern_pred <- dismo::predict(maxent_model, modTab, type = "response")

# lgm prediction - trace21k
# 'predictors_lgm' is the RasterStack of environmental variables for LGM

names(predictors_lgm) <- names(predictors)

lgm_pred <- dismo::predict(xm, predictors_lgm, progress="text",
                           filename=glue::glue('{wd}/maxent_{sp}_lgm_pred.grd'),
                           overwrite=TRUE)

# maxent_lgm_pred <- dismo::predict(maxent_model, modTab_lgm, type = "response")


