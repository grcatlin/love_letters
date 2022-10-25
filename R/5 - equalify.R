library(data.table)
library(stringr)
library(leaflet)
library(future.apply)
library(pushoverr)

plan(multisession)
set_pushover_user(user = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")
set_pushover_app(token = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")

# pushoverr test
# pushover("Testing!")

# imports
cens = readRDS("R_Objects/cens_cleaned.rds")
time_dist = readRDS("R_Objects/time_dist.rds")
time_mat = readRDS("R_Objects/time_mat.rds")
cluster_dat = readRDS("R_Objects/cluster_dat.rds")

# debugging
# census_data = cens
# distance_matrix = time_dist
# n_vans = 3
# threshold = .9
# iters = 50
# subiters = 100

# equalify algorithm
equalify = function(census_data, distance_matrix, n_vans, time_mat,
                    threshold = .9, iters = 1000, subiters = 1000) {
  # copy data
  data = copy(census_data)
  setkey(data, ID)
  
  # select candidates
  cand = data[Avg_Call >= quantile(data$Avg_Call, threshold)]
  cand = as.character(cand$ID)
  
  # goal population per van
  goal = sum(data$Avg_Call)/n_vans
  
  # custom min functions used
  which.min.name = function(x) {
    which = which.min(x)
    names(which)
  }
  which.pmin = function(...) {
    apply(cbind(...), 1, which.min.name)
  }
  
  # overall loop (this eases starting bias concerns)
  results = data.table()
  for (iter in 1:iters) {
    # pick starting blocks
    valid = copy(cand)
    selected = sample(valid, n_vans, replace = F)
    selected = c(selected, "ID")
    
    # find travel times from time matrix
    time_subset = time_mat[, ..selected]
    time_subset[, Avg_Call := data$Avg_Call]
    
    # find which selected minimizes travel time
    time_subset[, min_time := do.call(pmin, .SD), 
                .SDcols = selected[selected != "ID"]]
    time_subset[, Clust := do.call(which.pmin, .SD), 
                .SDcols = selected[selected != "ID"]]
    
    # fringe case - ensure selected select themselves
    time_subset[ID %in% selected, Clust := ID]
    
    # reallocation process
    satisfy = F
    it = 1
    assign_table = data.table()
    while (satisfy != T) {
      # how far from goal
      goal_eval = time_subset[, .(Avg_Call = sum(Avg_Call)), by = Clust]
      goal_eval[, Off := Avg_Call - goal]
      goal_eval[, COMP := fifelse(abs(Off) <= goal*(1-threshold), 1, 0)]
      
      # reduce possible
      done = goal_eval[COMP == 1]$Clust
      id_done = time_subset[(Clust %in% done)]$ID
      
      # save completed
      if (length(done) >= 1) {
        assign_table = rbind(
          assign_table, 
          time_subset[ID %in% id_done, .(ID, Clust, min_time)]
          ) 
  
        # if completed stop
        if (length(unique(assign_table$Clust)) == n_vans) {
          satisfy = T
          break
        }
        
        # remove completed ID's from valid candidates
        valid = valid[!(valid %in% assign_table$ID)]
      }
      
      # re-sample from candidate list
      selected = sample(valid, 
                        n_vans - length(unique(assign_table$Clust)), 
                        replace = F)
      selected = c(selected, "ID")
      
      # find travel times from time matrix
      time_subset = time_mat[, ..selected]
      time_subset[, Avg_Call := data$Avg_Call]
      
      # make sure ID's done aren't re-assigned
      time_subset = time_subset[!(ID %in% assign_table$ID)]
      
      # find which selected minimizes travel time
      time_subset[, min_time := do.call(pmin, .SD), 
                  .SDcols = selected[selected != "ID"]]
      time_subset[, Clust := do.call(which.pmin, .SD), 
                  .SDcols = selected[selected != "ID"]]
      
      # fringe case - ensure selected select themselves
      time_subset[ID %in% selected, Clust := ID]
      
      # break while loop if it can't find solution
      it = it +1
      if (it == subiters) {
        assign_table = rbind(assign_table, 
                             time_subset[, .(ID, Clust, min_time)]) 
        break
      }
    }
    
    # create evaluation criteria
    assign_table$Avg_Call = data$Avg_Call
    assign_table[, EVAL := min_time * Avg_Call]
    OV_EVAL = sum(assign_table$EVAL)
    
    # turn ID's into 1:n_vans numbers
    clustkey = data.table(assignID = as.character(
      sort(unique(as.integer(assign_table$Clust)))), 
      clust = as.character(1:n_vans))
    setkey(clustkey, assignID)
    setkey(assign_table, Clust)
    assign_table = assign_table[clustkey]
    assign_table[, crit := OV_EVAL][, it := iter]
    results = rbind(results, assign_table)
  }
  
  # pick which iteration 1:iters did best
  minOV_EVAL = min(results$crit)
  results = results[crit == minOV_EVAL]
  
  # if more than 1 optimal, sample
  if (nrow(results) != nrow(data)) {
    it_samp = sample(results$it, 1, replace = F)
    results = results[it == it_samp]
  }
  
  # stamp placements, bind to original data
  results[, Medoid := fifelse(ID == Clust, 1, 0)]
  results = results[, .(ID, Clust = clust, Medoid, Clust_ID = Clust)]
  setkey(results, ID)
  setkey(data, ID)
  data = data[results]
  setkey(data, ID)
  
  # get travel times to from assigned medoid
  time_cols = c(unique(data$Clust_ID), "ID")
  time = time_mat[ , ..time_cols]
  setkey(time, ID)
  time = time[data]

  which_time = function(dt) {
    col = as.character(dt$Clust_ID)
    return(as.numeric(dt[, ..col]))
  }

  time[, Travel_Time := which_time(.SD), by = ID]
  time = time[, .(ID, Travel_Time)]
  setkey(time, ID)
  data = data[time]
  
  # Average Travel Time & Population by Cluster
  summary = data[, .(Travel_Time_Average = mean(Travel_Time), 
                     Total_Population = sum(Population)),
                 by = Clust]
  setkey(summary, Clust)
  setkey(data, Clust)
  data = data[summary]
  setkey(data, ID)
  
  # Number of Vans, setting column order
  data[, N_Clust := n_vans]
  data[, Clust_ID := NULL]
  setcolorder(data, colnames(cluster_dat)[-12])
  
  # Message when finished
  message(n_vans)
  return(data)
}

# clustering
equal_cluster = future_Map(equalify,
                           n_vans = 1:10,
                           MoreArgs = list(census_data = cens,
                                           distance_matrix = time_dist,
                                           time_mat = time_mat,
                                           subiters = 250,
                                           iters = 250,
                                           threshold = .9),
                           future.seed = T)
equal_cluster = rbindlist(equal_cluster)
equal_cluster[, Method := "Equal"]

# optional - push notification when finished
# pushover("Equalify has finished running!")

# bind clustering solutions and save
cluster_dat = rbind(cluster_dat, equal_cluster)
saveRDS(cluster_dat, "R_Objects/cluster_dat.rds")

# investigate
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

cluster_map(cluster_dat, n_vans = 2, method = "Time")
cluster_map(cluster_dat, n_vans = 2, method = "Equal")
