library(data.table)
library(stringr)
library(osrm)
library(geodist)
library(WeightedCluster)
library(leaflet)

# import data
cens = readRDS("R_Objects/cens_cleaned.rds")

# osrm address setup
options(osrm.server = "http://127.0.0.1:5000/", osrm.profile = "car")

# euclidean distance matrix
euc_dist = as.dist(geodist(cens[, .(Lon, Lat)], measure = "geodesic"))

# travel time matrix
time_mat = osrmTable(cens[, .(ID, Lon, Lat)]) # this takes a while to run
time_mat = as.data.table(time_mat$durations)

# travel time distance matrix
time_dist = as.dist(time_mat)

# check dimension of distance matrices
all.equal(dim(euc_dist), dim(time_dist))

# wcKMedoids test
test_clust = wcKMedoids(
  diss = euc_dist,
  # diss = time_dist,
  k = 3, 
  weights = cens$Avg_Call, 
  method = "PAMonce")
test_assignment = test_clust$clustering

# leaflet test data
test_cens = copy(cens)
test_cens[, Clust := test_assignment]
test_cens[, Medoid := ifelse(Clust == ID, 1, 0)]

# leaflet test palette 
test_colors = c("#FFEEB2FF", "#FFCA9EFF", "#FF8C89FF", "#756585FF", "#09A7B4FF",
           "#43D99AFF", "#8FFA85FF")
structure((grDevices::colorRampPalette(test_colors))(length(test_colors)), 
          class = "palette",
          name = "leaflet palette")
test_pal = colorFactor(palette = test_colors, domain = test_cens$Clust)
test_cens[, Palette := test_pal(Clust)]
test_cens[Medoid == 1, Palette := "#214559"]

# leaflet test radius & opacity
test_radius_function = function(pop, q) {
  if (pop %between% c(q[1], q[2])) {3
  } else if (pop %between% c(q[2], q[3])) {
    4.5
  } else if (pop %between% c(q[3], q[4])) {
    6
  } else {
    7.5
  }
}
test_Q = quantile(test_cens$Population)
test_cens[, Radius := test_radius_function(Population, test_Q), by = ID]
test_cens[Medoid == 1, Radius := 10]
test_cens[, Opacity := ifelse(Medoid == 1, 1, .7)]

# leaflet test labels
test_label_function = function(dt) {
  if (dt$Medoid == 1) {arg = "Medoid: "} else {arg ="Cluster Assignment: "}
  htmltools::HTML(paste0(arg, dt$Clust, "<br/>",
              "Population: ", dt$Population, "<br/>",
              "Lat, Lon: (", round(dt$Lat, 3), ", ", round(dt$Lon, 3), ")"))
}
test_cens[, Label := test_label_function(.SD), by = ID]

# leaflet test 
leaflet() %>% 
  addProviderTiles(providers$CartoDB) %>% 
  setView(lat=mean(test_cens$Lat), 
          lng=mean(test_cens$Lon), 
          zoom = 12) %>%
  addCircleMarkers(data = test_cens[Medoid == 0],
                   ~Lon, ~Lat, 
                   fillColor = ~Palette, 
                   fillOpacity = ~Opacity, 
                   color="white", 
                   radius = ~Radius, 
                   stroke=F,
                   label = ~Label,
                   labelOptions = labelOptions(textsize = "13px")) %>%
  addCircleMarkers(data = test_cens[Medoid == 1],
                   ~Lon, ~Lat, 
                   fillColor = ~Palette, 
                   fillOpacity = ~Opacity, 
                   color="white", 
                   radius = ~Radius, 
                   stroke=F,
                   label = ~Label,
                   labelOptions = labelOptions(textsize = "13px"))

# remove test objects, flush memory
rm(list = ls(pattern = "test_")); gc(full = T)

# save distance matrices
saveRDS(euc_dist, "R_Objects/euc_dist.rds")
saveRDS(time_dist, "R_Objects/time_dist.rds")
time_mat[, ID := cens$ID]
saveRDS(time_mat, "R_Objects/time_mat.rds")
