library(raster)

# test velocity
mod  <- raster("E:/AWI/glacial_refugia_project/Framework/MaxEnt/output_maxent_software/Alnus/Alnus_avg.asc")
lgm <- raster("E:/AWI/glacial_refugia_project/Framework/MaxEnt/output_maxent_software/Alnus/Alnus_projection_avg.asc")

x <- coordinates(lgm)[, 1]
y <- coordinates(lgm)[, 2]

p <- round(values(lgm)*10)/10
f <- round(values(mod)*10)/10

# Check for complete cases
complete_cases <- complete.cases(p, f)

# Subset p and f based on complete cases
p <- p[complete_cases]
f <- f[complete_cases]

# Subset x and y coordinates based on complete cases
x <- x[complete_cases]
y <- y[complete_cases]


d <- vector(length=length(p))

u <- unique(p)[order(unique(p))]
print(u)

p <- p[p>=0.5]
f <- f[f>=0.5]

match <- function(u){c(which(u<=f))}   
m <- sapply(u, match)             

for(i in 1:length(p)){
  if (p[i]>=0.5) {
  mi   <- m[[which(u==p[i])]]          
  d[i] <- sqrt(min((x[i]-x[mi])^2 + (y[i]-y[mi])^2))
  }
  print(i)
}

d[d=="Inf"] <- 100000 

out <- cbind(y,x,logDist=round(log10(d)*100),logSpeed=round(log10(d/20000)*100))
#out <- cbind(y,x,logDist=round(log10(d+0.001)*100),logSpeed=round(log10(d+0.001)/20000)*100)

distance_alnus <- rasterFromXYZ(out[, c("x", "y", "logDist")])
#speed_alnus <- rasterFromXYZ(out[, c("x", "y", "logSpeed")])

# conversion: -300=0.001km, -200=0.01km, -100=0.1km, 0=1km, 100=10km, 200=100km etc.
plot(distance_alnus)
#plot(speed_alnus)

writeRaster(distance_alnus, "E:/AWI/glacial_refugia_project/Framework/velocity/distance_alnus_31.01.2024.asc")
#writeRaster(speed_alnus, "speed_alnus_2_26.01.2024.asc")
