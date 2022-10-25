library(data.table)
library(stringr)

cens = as.data.table(readRDS("R_Objects/cens_import.rds"))
cens = cens[, .(GEOCODE, POP100, INTPTLAT, INTPTLON, PLACE)]
cens[, INTPTLAT := gsub("\\+", "", INTPTLAT)] # get rid of "+" in latitude coordinates
cens = cens[str_length(GEOCODE) == 15] # filter to just census blocks
setkey(cens, PLACE)

# import names (https://www.census.gov/geographies/reference-files/time-series/geo/name-lookup-tables.html)
CDP_names = fread("cens_files/names/NAMES_ST56_WY_CDP.txt")
CITY_names = fread("cens_files/names/NAMES_ST56_WY_INCPLACE.txt")
names = rbind(CDP_names, CITY_names) # combine
names[, c("STATEFP", "NAMELSAD") := NULL] # remove more detailed name & state code
names[, PLACEFP := as.character(PLACEFP)]
setkey(names, PLACEFP)

# merge
cens = cens[names]
cens = cens[!(is.na(NAME))]

# filter to Casper Area
cens = cens[NAME %in% c("Casper", "Mills", "Evansville", "Casper Mountain")]
cens = cens[POP100 != 0]
length(cens$GEOCODE); length(unique(cens$GEOCODE)) # check to make sure each census block is unique
cens[, ID := 1:.N] # create ID variable (to replace GEOCODE)
cens[, c("NAME", "PLACE", "GEOCODE") := NULL] # remove unused cols after reduction
setnames(cens, c("Population", "Lat", "Lon", "ID")) # clean colnames
setcolorder(cens, "ID") # ID first
cens[, Population := as.numeric(Population)][, Lon := as.numeric(Lon)][, Lat := as.numeric(Lat)]

library(leaflet)

# map
leaflet(cens) %>% 
  addProviderTiles(providers$CartoDB.DarkMatter) %>% 
  addCircleMarkers(lng = ~Lon, lat = ~Lat, radius = 3,
                   stroke = F, fillOpacity = .4,
                   color = "#d0587e")

# generate response variable
monthly = data.table()
for (month in 1:12) {
  monthdat = copy(cens)
  monthdat = monthdat[, MonthNo := month]
  monthly = rbind(monthly, monthdat)
}

monthly[!(MonthNo %in% c(1:2, 11:12)), Response := rpois(.N, Population/125)]
monthly[MonthNo == 2, Response := rpois(.N, 3 * Population/125)]
monthly[(MonthNo %in% c(1, 11:12)), Response := rpois(.N, 2 * Population/125)]

# check
monthly[, mean(Response), by = MonthNo]

# map average calls
cens = cbind(cens, monthly[, mean(Response), by =  ID]$V1)
setnames(cens, "V2", "Avg_Call")

leaflet(cens) %>% 
  addProviderTiles(providers$CartoDB) %>% 
  addCircleMarkers(~Avg_Call, lng = ~Lon, lat = ~Lat,
                   stroke = F, fillOpacity = .7,
                   color = "#d0587e")

# save
saveRDS(monthly, "R_Objects/monthly.rds")
saveRDS(cens, "R_Objects/cens_cleaned.rds")
