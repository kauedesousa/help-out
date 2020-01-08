# number of observations
n <- 20

# vector with 8-day dates
# run over these years
# for modis it should always start in Jan-01
dates <- NULL
for(i in c(2003:2004)){
  dates <- c(dates, c(seq(as.Date(paste0(i, "-01-01"), "%Y-%m-%d"), 
                          as.Date(paste0(i, "-12-31"), "%Y-%m-%d"), 
                          8)))
}

dates <- as.Date(dates, origin = "1970-01-01")

# sequence of 1-day dates
full_dates <- seq(dates[1], dates[length(dates)], 1)

# generate some random numbers
nv <- n*length(dates)

set.seed(1234)
day <- rnorm(nv, 33, 3)

set.seed(4321)
day <- rnorm(nv, 25, 3)

# 2 layers array
# fill it with the generated data
modis <- array(c(day, night), 
               dim = c(n, length(dates), 2), 
               dimnames = list(1:n, as.character(dates), c(1:2)))


# apply linear interpolation to the 8-day temperature
modis_approx <- array(NA, 
                      dim = c(n, length(full_dates), 2), 
                      dimnames = list(1:n,  as.character(full_dates), c(1:2)))

for(i in 1:dim(modis)[1]){
  
  modis_approx[i,,1] <- approx(modis[i,,1], n = length(full_dates))[[2]]
  
  modis_approx[i,,2] <- approx(modis[i,,2], n = length(full_dates))[[2]]
  
}


# check how the interpolation was done
x <- !dimnames(modis_approx)[[2]] %in% dimnames(modis)[[2]]
y <- modis_approx[1, ,1] 
y[x] <- NA

plot(modis_approx[1, ,1])
points(y, col = "red")



