library("bestNormalize")

data("airquality")

X <- airquality[,c(2:4)]

X <- na.omit(X)

# check those that are not normal
nonnorm <- apply(X, 2, function(x) shapiro.test(x)$p)
# keep only non normal cases
nonnorm <- as.vector(which(nonnorm < 0.05))

# apply to X
to_norm <- X[,nonnorm]

?bestNormalize

# apply best normalize function
for (i in seq_along(nonnorm)){
  # normalize
  x <- bestNormalize(to_norm[,i], allow_lambert_s = TRUE)
  x <- predict(x)
  
  # add it to the main dataset as a new colunm 
  to_norm[,i] <- x
}

names(to_norm) <- paste0(names(to_norm), "_norm")

X <- cbind(X, to_norm)

hist(X$Solar.R)
hist(X$Solar.R_norm)

hist(X$Temp)
hist(X$Temp_norm)
