# Calculate environmental indices using MODIS, CHIRPS or NASA POWER


#devtools::install_github("agrobioinfoservices/ClimMobTools", upgrade = "never")
library("ClimMobTools")

load("data/data.rda")

# when calculating indices using MODIS or CHIRPS, follow the steps
# 1. download the daily data from its sources
# 2. extract the data for the target area (hint, using raster::extract)
# 3. put it into a named matrix (CHIRPS) or array (MODIS)
#    where rows are the observations for each lonlat
#    and columns are the days in the format "1970-01-01"
#    MODIS is an array with two dimensions, first is the day temperature
#    and the second is the night temperature
#    this data was linearly interpolated from an 8-day data into and daily data
#    using function stats::approx
dimnames(modis)
dimnames(chirps)

# calculate heat indices from the first 50 days after planting date

temperature(modis, 
            day.one = df["planting_date"],
            span = 50)

# calculate rainfall indices 
rainfall(chirps, 
         day.one = df["planting_date"],
         span = 50)

# There is also the option of calculating these indices using NASA POWER
# an internal call to nasapower::get_power is made
# in that case, the lonlat info is provided instead of the matrix/array

temperature(df[c("lon","lat")],
            day.one = df["planting_date"],
            span = 50)

# same for rainfall
rainfall(df[c("lon","lat")],
         day.one = rep("2018-08-01", 50),
         span = 150)

# These functions can be used to calculate indices over a growing season
# let imagine Meher 2018 in Ethiopia
do <- as.Date(rep("2018-08-01", 50), format = "%Y-%m-%d")

temperature(df[c("lon","lat")],
            day.one = do,
            span = 150)


# other indices are GDD and ETo
ETo(modis, 
    day.one = df["planting_date"],
    span = 50,
    lat = df$lat)

GDD(modis,
    day.one = df$planting_date,
    degree.days = 500,
    base = 5, 
    span = 100)




