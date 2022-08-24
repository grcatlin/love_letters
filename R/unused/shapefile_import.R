library(sf)
library(data.table)

# import shape file
shape = as.data.table(st_read('shapefile/tl_2020_56_tabblock10.shp'))
shape = shape[, .(GEOID10, INTPTLAT10, INTPTLON10)]
setkey(shape, GEOID10)

# import census data
cens = as.data.table(readRDS('R_Objects/cens_import.rds'))
setkey(cens, GEOCODE)

# Merge by GEOCODE and filter for obs that have lat & lon
dat = shape[cens]
dat = dat[!(is.na(INTPTLAT10) & is.na(INTPTLON10))]

# lmao the og already had lat & lon :(
