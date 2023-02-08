library(raster)
library(geodist)
library(NLMR)


set.seed(100)

#####
###
#simulate a raster to mimic environmental data
r <- NLMR::nlm_fbm(ncol=50, nrow=50, resolution=1, fract_dim=1)

#replace 20% of cells with NA values
r[sample(x=1:2500, size=2500/5, replace=FALSE)] <- NA

#view simulated raster with NA values ("holes" in the raster)
spplot(r)


#####
###
#simulate having 100 locations of interest
latlong <- as.data.frame(x=r, xy=TRUE, na.rm=FALSE)[,c("x", "y")][sample(x=1:2500, size=100, replace=FALSE),]
colnames(latlong) <- c("long", "lat")

#by chance, ~20 should have NA values
##let's check this just to be sure the rest of the script demonstration works
env_extract <- raster::extract(x=r, y=latlong, fun="mean", na.rm=TRUE, layer=1, nl=1, sp=FALSE, df=TRUE)
sum(is.na(env_extract))

#get coordinates of locations that yield NA values
latlong_na <- latlong[which(is.na(env_extract$layer)),]


#####
###
#define function that will find the nearest non-NA raster cell to each location yielding an NA value

nearest_nonNA <- function(raster, xy_na){
  
  #convert raster to df, omitting NA values
  r_df <- raster::as.data.frame(x=raster, xy=TRUE, na.rm=TRUE)
  
  #calculate distance between all non-NA cells and the locations yields NA values
  na_dist <- geodist::geodist(x=xy_na, y=r_df[,1:2], measure="geodesic")
  
  #establish vector to save index of non-NA cells nearest each location yielding an NA value
  na_dist_min_index <- c(rep(NA, nrow(xy_na)))
  na_dist_min <- c(rep(NA, nrow(xy_na)))
  
  #loop through locations yielding NA values and identify nearest non-NA cell
  for (i in 1:nrow(xy_na)){
    
    #store r_df index of nearest non-NA cell
    na_dist_vec<-as.vector(na_dist[i,])
    index_temp<-which(na_dist_vec==min(na_dist_vec))
    
    #if multiple non-NA raster cells are of equal distance from location of interest, choose one randomly
    na_dist_min_index[i] <- index_temp[sample(1:length(index_temp), 1)]
    
    #store the distance
    na_dist_min[i] <- min(na_dist_vec)
  }
  
  #extract xy coordinates of nearest non-NA raster cells (i.e., the replacements of the locations yielding NA values)
  new_xy <- r_df[na_dist_min_index,1:2]
  
  #bind the old and new xy coordinates together
  old_new_xy <- cbind(xy_na, new_xy, na_dist_min)
  colnames(old_new_xy) <- c("x_old", "y_old", "x_new", "y_new", "distance")
  
  #extract environmental data at the new xy coordinates
  env_extract_new <- raster::extract(x=raster, y=new_xy, fun="mean", na.rm=TRUE, layer=1, nl=1, sp=FALSE, df=TRUE)
  
  #prepare function output
  out <- list(env_extract_new=env_extract_new, old_new_points=old_new_xy)
  return(out)
}


#####
###
#run the function on the simulated data
test <- nearest_nonNA(raster=r, xy_na=latlong_na)

#env_extract_new contains the environmental data extracted at the new (i.e., replacement) xy coordinates
test$env_extract_new

#old_new_points contains the old xy coordinates (those yielding NA values) and the new xy coordinates (those of the nearest raster cells with non-NA values)
##note that with the simulated raster data, the distance values are do not correspond to any real unit of measurement
test$old_new_points
