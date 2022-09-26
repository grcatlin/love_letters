library(data.table)
library(stringr)
library(WeightedCluster)
library(leaflet)
library(future.apply)

plan(multisession)

# imports
cens = readRDS("R_Objects/cens_cleaned.rds")
euc_dist = readRDS("R_Objects/euc_dist.rds")
time_dist = readRDS("R_Objects/time_dist.rds")
time_mat = readRDS("R_Objects/time_mat.rds")

# generalized cluster assignment
cluster = function(census_data, distance_matrix, n_vans, time_mat) {
  # copy data
  data = copy(census_data)
  setkey(data, ID)
  
  # wcKMedoids
  clust = wcKMedoids(
    diss = distance_matrix,
    k = n_vans, 
    weights = data$Avg_Call, 
    method = "PAMonce")
  assignment = data.table(clust = clust$clustering)
  assignment[, ID := 1:.N]
  setkey(assignment, ID)
  
  # get travel times to from assigned medoid
  time_cols = c(unique(assignment$clust), "ID")
  time = time_mat[ , ..time_cols]
  setkey(time, ID)
  time = time[assignment]
  
  which_time = function(dt) {
    col = as.character(dt$clust)
    return(as.numeric(dt[, ..col]))
  }
  
  time[, Travel_Time := which_time(.SD), by = ID]
  time = time[, .(ID, Travel_Time)]
  setkey(time, ID)
  
  # cluster assignments as N's instead of index
  clustkey = data.table(Clust = 1:n_vans, 
                        clust = sort(unique(assignment$clust)))
  setkey(clustkey, clust)
  setkey(assignment, clust)
  assignment = clustkey[assignment]
  assignment[, clust := NULL]
  setkey(assignment, ID)

  # append cluster assignment data
  data = data[assignment]
  setkey(data, ID)
  data = data[time]
  data[, Medoid := ifelse(ID %in% clustkey$clust, 1, 0)]
  data[, N_Clust := n_vans]
  
  # average metrics per cluster
  average = data[, .(Travel_Time_Average = mean(Travel_Time),
                     Total_Population = sum(Population)), by = Clust]
  setkey(average, Clust)
  setkey(data, Clust)
  data = data[average]
  setkey(data, ID)
}

# euclidean clustering
euc_cluster = future_Map(cluster, 
                         n_vans = 2:10, 
                         MoreArgs = list(census_data = cens, 
                                         distance_matrix = euc_dist,
                                         time_mat = time_mat),
                         future.seed = T)
euc_cluster = rbindlist(euc_cluster)
euc_cluster[, Method := "Euc"]

# travel time clustering
time_cluster = future_Map(cluster, 
                          n_vans = 2:10, 
                          MoreArgs = list(census_data = cens, 
                                          distance_matrix = time_dist,
                                          time_mat = time_mat),
                          future.seed = T)

time_cluster = rbindlist(time_cluster)
time_cluster[, Method := "Time"]

# combine for full data and save
cluster_dat = rbind(time_cluster, euc_cluster)
saveRDS(cluster_dat, "R_Objects/cluster_dat.rds")

# generalized leaflet function
cluster_map = function(cluster_data, n_vans, method) {
  data = cluster_data[N_Clust == n_vans & Method == method]
  
  # leaflet palette 
  colors = c("#FFEEB2FF", "#FFCA9EFF", "#FF8C89FF", "#756585FF", "#09A7B4FF",
             "#43D99AFF", "#8FFA85FF")
  pal = colorFactor(palette = colors, domain = data$Clust)
  data[, Palette := pal(Clust)]
  data[Medoid == 1, Palette := "#214559"]
  
  # leaflet radius & opacity
  radius_function = function(pop, q) {
    if (pop %between% c(q[1], q[2])) {
      2
    } else if (pop %between% c(q[2], q[3])) {
      4
    } else if (pop %between% c(q[3], q[4])) {
      6
    } else {
      8
    }
  }
  Q = quantile(data$Population)
  data[, Radius := radius_function(Population, Q), by = ID]
  data[Medoid == 1, Radius := 10]
  data[, Opacity := ifelse(Medoid == 1, 1, .7)]
  
  # leaflet labels
  label_function = function(dt) {
    if (dt$Medoid == 1) {
      arg1 = "Medoid: "
      arg2 = "Average Travel Time: "
      travel_metric = round(dt$Travel_Time_Average, 2)
      arg3 = "Total Population: "
      pop_metric = format(dt$Total_Population, big.mark = ",", trim = T)
    } else {
        arg1 ="Cluster Assignment: "
        arg2 = "Travel Time: "
        travel_metric = dt$Travel_Time
        arg3 = "Population: "
        pop_metric = dt$Population
        }
    htmltools::HTML(paste0(
      arg1, dt$Clust, "<br/>",
      arg2, travel_metric, "<br/>",
      arg3, pop_metric, "<br/>",
      "Lat, Lon: (", round(dt$Lat, 3), ", ", round(dt$Lon, 3), ")"))
  }
  data[, Label := label_function(.SD), by = ID]
  
  print(data[Medoid == 1, c(6, 10:11)])
  
  # leaflet  
  leaflet() %>% 
    addProviderTiles(providers$CartoDB) %>% 
    setView(lat=mean(data$Lat), 
            lng=mean(data$Lon), 
            zoom = 12) %>%
    addCircleMarkers(data = data[Medoid == 0],
                     ~Lon, ~Lat, 
                     fillColor = ~Palette, 
                     fillOpacity = ~Opacity, 
                     color="white", 
                     radius = ~Radius, 
                     stroke=F,
                     label = ~Label,
                     labelOptions = labelOptions(textsize = "13px")) %>%
    addCircleMarkers(data = data[Medoid == 1],
                     ~Lon, ~Lat, 
                     fillColor = ~Palette, 
                     fillOpacity = ~Opacity, 
                     color="white", 
                     radius = ~Radius, 
                     stroke=F,
                     label = ~Label,
                     labelOptions = labelOptions(textsize = "13px"))
}

cluster_map(cluster_dat, n_vans = 3, method = "Euc")
cluster_map(cluster_dat, n_vans = 3, method = "Time")

cluster_map(cluster_dat, n_vans = 6, method = "Euc")
cluster_map(cluster_dat, n_vans = 6, method = "Time")
