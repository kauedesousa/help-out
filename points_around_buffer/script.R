########################################################
# CALCULATE THE SHARE OF NEIGHBORS
########################################################

library("tidyverse")
library("sf")
library("sp")

proj <- "+proj=longlat +datum=WGS84"
newproj <- "+proj=utm +zone=30N +datum=WGS84 +units=km"
radius <- 5

#read files
df1 <- read_csv("data/sampled_coops.csv")

names(df1)[(ncol(df1)-1):ncol(df1)] <- c("x","y")

#missing data in certified since is inputed as 2019
df1 <- mutate(df1, 
              ST_ft_since = ifelse(is.na(ST_ft_since), 2019, ST_ft_since))

sum(is.na(df1$ST_ft_since))

df2 <- read_csv("data/all_coops.csv")

names(df2)[(ncol(df2)-1):ncol(df2)] <- c("x","y")

plot(df1[c("x","y")])
points(df2[c("x","y")], col="red", pch = 21)


#split df1 into a list so we can work in each entry independently
df1 <- split(df1 , f = df1$GI_coop_label)


# transform dataframes into spatial points then set the original projection
df1 <- lapply(df1, function(x) {
  # df into spatial points object
  x <- st_as_sf(x, coords = c("x","y"))
  # set the projection
  x <- st_set_crs(x, proj)
  # return the result
  x
})

df1[[1]]

#convert the projection to UTM in km
df1 <- lapply(df1, function(x) st_transform(x, newproj))
df1[[1]]

#same for the other dataset
df2 <-  st_as_sf(df2, coords=c("x","y"))
df2 <- st_set_crs(df2, proj)
#then to UTM
df2 <- st_transform(df2, newproj)
df2

#now create a 5 km buffer aroud the points in df1
buffer <- lapply(df1, function(x) st_buffer(x, radius))
buffer[[1]]

#then make the intersection using the points in df2
intersect <- lapply(buffer, function(x){

  x <- st_intersection(x, df2)
  x <- st_set_geometry(x, NULL)
  #take the name of the coop
  coop <- unique(x$GI_coop_label)
  #keep only the coop that was certified before the surveyed coop
  keep <- x$ST_ft_since - x$ST_ft_since.1 >= 1
  x <- x[keep,]
  #take the name of the coop and the number of certified neighbours 
  x <- as_tibble(cbind(coop = coop, n_neighbours = nrow(x)))
  
  x
  
} )

result <- bind_rows(intersect)

# some missing coop labels because the entry has no certified neighbour 
# impute this
coops <- lapply(df1, function(x) x$GI_coop_label)
coops <- do.call(rbind, coops)

# combine 
result <- mutate(result, 
                 coop = ifelse(is.na(coop), coops, coop))

length(unique(result$coop))




