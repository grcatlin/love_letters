library(data.table)
library(stringr)
library(future.apply)
library(pushoverr)
library(lubridate)
library(simmer)

plan(multisession)
set_pushover_user(user = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")
set_pushover_app(token = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")

# imports
time_mat = readRDS("R_Objects/time_mat.rds")
cluster_dat = readRDS("R_Objects/cluster_dat.rds")
monthly = readRDS("R_Objects/monthly.rds")

# debugging options
# monthly_data = monthly
# n_vans = 3
# method = "Equal"
# cluster_solutions = cluster_dat

# function
letter_sim = function(monthly_data, cluster_solutions, n_vans, 
                      method, time_mat) {
  
  # get placements
  clust_assign = cluster_solutions[N_Clust == n_vans & Method == method]
  placements = clust_assign[Medoid == 1]
  clust_assign = clust_assign[, .(ID, Clust)]

  # subset travel times for placements
  placement_ids = c(sort(placements$ID), "ID")
  time_mat = time_mat[, ..placement_ids]
  setnames(time_mat, c(paste0("van_",1:n_vans,"_time"), "ID"))
  setkey(time_mat, ID)
  
  # date key
  datekey = data.table(Date = seq(as.Date("2023-01-01"), 
                                  as.Date("2023-12-31"), 
                                  by = "1 day"), 
                       Elapsed_Day = 1:365)
  setkey(datekey, Date)
  
  # generate simmer data function
  simmer_gen = function(dt) {
    if (dt$MonthNo %in% 1:9) {
      month = paste0(dt$MonthNo)
    } else {month = as.character(dt$MonthNo)}
    
    if (dt$Response != 0) {
    month_full = datekey[month(Date) == month]
    day_request = sample(month_full$Elapsed_Day, dt$Response, replace = T)
    hour_request = sample(1:23, dt$Response, replace = T)/24
    minute_request = sample(1:59, dt$Response, replace = T)/1440
    simmer_time = day_request + hour_request + minute_request
    table = data.table(ID = dt$ID, simmer_time)
    } else (table = NULL)
    
    return(table)
  }
  
  # generate simmer data
  simmer_data = data.table()
  for (row in 1:nrow(monthly_data)) {
    table = simmer_gen(monthly_data[row])
    simmer_data = rbind(simmer_data, table)
  }
  setkey(simmer_data, ID)
  setkey(clust_assign, ID)
  simmer_data = clust_assign[simmer_data]
  simmer_data[, Clust := as.numeric(Clust)]
  setkey(simmer_data, ID)
  simmer_data = time_mat[simmer_data]
  setkey(simmer_data, simmer_time) # orders by time for simmer
  
  # date key overwrite (so sim can extend past generated year)
  datekey = data.table(Date = seq(as.Date("2023-01-01"), 
                                  as.Date("2025-12-31"), 
                                  by = "1 day"))
  datekey[,Elapsed_Day := 1:.N]
  setkey(datekey, Date)
  
  # simulation trajectory
  letter = trajectory() %>% 
    
    # initialize - records when the letter was requested
    set_attribute("letter_request", 
                  values = function() {simmer::now(sim)}) %>%
    
    # are our staff working? work 9a-5p Mon-Fri
    timeout(function() {
      time = simmer::now(sim)
      base_day = as.numeric(gsub("\\..*","",simmer::now(sim)))
      time_request = time - base_day
      
      # if before 9a - kick to 9a
      if (time_request < 9/24) {
        timeout = 9/24 - time_request
      } else {
          timeout = 0
      }
      
      # update
      time = time + timeout
      base_day = as.numeric(gsub("\\..*","",time))
      time_request = time - base_day
      
      # if after 5p - kick to 9a next day
      if (time_request > 17/24) {
        timeout = (1 - time_request) + 9/24
      } else {
          timeout = 0
      }
      
      # update
      time = time + timeout
      base_day = as.numeric(gsub("\\..*","",time))
      time_request = time - base_day
      
      # do our writers have enough time (i.e., request at 4:59p)?
      # letters take between 30 minutes and 2 hours to write,
      # skip to next day if less than an hour remains
      if (time_request + 1/24 >= 17/24) {
        timeout = (1 - time_request) + 9/24
      } else{
        timeout = 0
      }
      
      # update
      time = time + timeout
      base_day = as.numeric(gsub("\\..*","",time))
      time_request = time - base_day
      
      # is it a weekend? 
      # if so, skip to Monday
      what_day = weekdays(datekey[Elapsed_Day == base_day]$Date)
      if (what_day == "Saturday") {
        timeout = (1 - time_request) + 1 + 9/24
      } else if (what_day == "Sunday") {
        timeout = (1 - time_request) + 9/24
      } else {
        timeout = 0
      }
      
      # update
      time = time + timeout
      base_day = as.numeric(gsub("\\..*","",time))
      time_request = time - base_day
      
      # is it a holiday? 
      # if so, skip to next available day
      what_day = datekey[Elapsed_Day == base_day]$Date
      if (what_day == as.Date("2023-01-01")) { # new years 
        timeout = (1 - time_request) + 1 + 9/24
      } else if (what_day == as.Date("2023-01-02")) { # new years (in lieu)
        timeout = (1 - time_request) + 9/24
      } else if (what_day == as.Date("2023-01-16")) { # MLK day
        timeout = (1 - time_request) + 9/24
      } else if (what_day == as.Date("2023-02-20")) { # Washington birthday
        timeout = (1 - time_request) + 9/24
      } else if (what_day == as.Date("2023-05-29")) { # Memorial day
        timeout = (1 - time_request) + 9/24
      } else if (what_day == as.Date("2023-06-19")) { # Juneteenth
        timeout = (1 - time_request) + 9/24
      } else if (what_day == as.Date("2023-07-04")) { # Independence day
        timeout = (1 - time_request) + 9/24
      } else if (what_day == as.Date("2023-09-04")) { # Labor day
        timeout = (1 - time_request) + 9/24
      } else if (what_day == as.Date("2023-10-09")) { # Columbus day
        timeout = (1 - time_request) + 9/24
      } else if (what_day == as.Date("2023-11-10")) { # Veterans' day
        timeout = (1 - time_request) + 2 + 9/24
      } else if (what_day == as.Date("2023-11-23")) { # Thanksgiving
        timeout = (1 - time_request) + 9/24
      } else if (what_day == as.Date("2023-12-25")) { # Christmas
        timeout = (1 - time_request) + 9/24
      } else if (what_day == as.Date("2024-01-01")) { # New Years
        timeout = (1 - time_request) + 9/24
      } else {
        timeout = 0
      }
      
      # update
      time = time + timeout
      base_day = as.numeric(gsub("\\..*","",time))
      time_request = time - base_day
      
      # process all possible delays
      time - simmer::now(sim)
    }) %>% 
    set_attribute(c("initial_delay_end", "initial_delay_time"), function() {
      c(simmer::now(sim), 
        simmer::now(sim) - get_attribute(sim, "letter_request"))
    }) %>% 
    
    # time for writing (anywhere from 30 minutes to 2 hours)
    # assuming infinite writers here but could set up writer "resources"
    # like vans below
    timeout(function() {
      sample(30:120, 1)/60/24
    }) %>% 
    set_attribute(c("write_end", "write_time"), function() {
      c(simmer::now(sim), 
        simmer::now(sim) - get_attribute(sim, "initial_delay_end"))
    }) %>% 
    
    # select which van to choose
    select(function() {
      
      # get assigned van
      assigned = get_attribute(sim, "Clust")
      
      # get van travel times
      vans = paste0("van_",1:n_vans)
      time_cols = paste0(vans,"_time")
      times = get_attribute(sim, time_cols)
      select_table = data.table(vans, times)
      select_table[, number := gsub("van_","",vans)]
      
      # get queue
      select_table[, queue := get_queue_count(sim, vans)]
      
      # if assigned has lowest queue, select it
      # otherwise, select the closest van with lowest queue
      if (select_table[number == assigned]$queue == min(select_table$queue)) {
        select_table[number == assigned]$vans
      } else {
        # select which minimizes Q & TT
        sample(select_table[queue == min(queue), 
                            .SD[times == min(times)]]$vans, 1)
      }
    }) %>% 
    seize_selected(1) %>% 
    set_attribute("van_selected", values = function() {
      van = get_selected(sim)
      as.numeric(gsub("\\D","",van))
    }) %>% 
    
    # time to drive (assuming printing time is nominal here)
    # take travel time and manipulate with uncertainty, adding time for
    # winter travel conditions
    timeout(function() {
      van = get_selected(sim)
      travel_time = get_attribute(sim, paste0(van, "_time"))
      
      # can our driver deliver and reset by 5p? If not skip to 9a next day
      time = simmer::now(sim)
      base_day = as.numeric(gsub("\\..*","",simmer::now(sim)))
      time_request = time - base_day
      if (time_request + 2*travel_time/60/24 >= 19/24) {
        timeout = 1-time_request + 9/24
      } else {
        timeout = 0
      }
      
      # update
      time = time + timeout
      base_day = as.numeric(gsub("\\..*","",time))
      time_request = time - base_day
      
      # is it a weekend? 
      # if so, skip to Monday
      what_day = weekdays(datekey[Elapsed_Day == base_day]$Date)
      if (what_day == "Saturday") {
        timeout = (1 - time_request) + 1 + 9/24
      } else if (what_day == "Sunday") {
        timeout = (1 - time_request) + 9/24
      } else {
        timeout = 0
      }
      
      # update
      time = time + timeout
      base_day = as.numeric(gsub("\\..*","",time))
      time_request = time - base_day
      
      # is it a holiday? 
      # if so, skip to next available day
      what_day = datekey[Elapsed_Day == base_day]$Date
      if (what_day == as.Date("2023-01-01")) { # new years 
        timeout = (1 - time_request) + 1 + 9/24
      } else if (what_day == as.Date("2023-01-02")) { # new years (in lieu)
        timeout = (1 - time_request) + 9/24
      } else if (what_day == as.Date("2023-01-16")) { # MLK day
        timeout = (1 - time_request) + 9/24
      } else if (what_day == as.Date("2023-02-20")) { # Washington birthday
        timeout = (1 - time_request) + 9/24
      } else if (what_day == as.Date("2023-05-29")) { # Memorial day
        timeout = (1 - time_request) + 9/24
      } else if (what_day == as.Date("2023-06-19")) { # Juneteenth
        timeout = (1 - time_request) + 9/24
      } else if (what_day == as.Date("2023-07-04")) { # Independence day
        timeout = (1 - time_request) + 9/24
      } else if (what_day == as.Date("2023-09-04")) { # Labor day
        timeout = (1 - time_request) + 9/24
      } else if (what_day == as.Date("2023-10-09")) { # Columbus day
        timeout = (1 - time_request) + 9/24
      } else if (what_day == as.Date("2023-11-10")) { # Veterans' day
        timeout = (1 - time_request) + 2 + 9/24
      } else if (what_day == as.Date("2023-11-23")) { # Thanksgiving
        timeout = (1 - time_request) + 9/24
      } else if (what_day == as.Date("2023-12-25")) { # Christmas
        timeout = (1 - time_request) + 9/24
      } else if (what_day == as.Date("2024-01-01")) { # New Years
        timeout = (1 - time_request) + 9/24
      } else {
        timeout = 0
      }
      
      # update
      time = time + timeout
      base_day = as.numeric(gsub("\\..*","",time))
      time_request = time - base_day
      
      # if in winter travel_time will always be expected, up to twice as long
      # if not, want rough bound of +/- 10%
      what_month = month(datekey[Elapsed_Day == base_day]$Date)
      if (what_month %in% c(12,1:3)) {
        timeout = travel_time * runif(1, 1, 2)
      } else {
        timeout = travel_time * rnorm(1, 1, 0.05)
      }
      time = time + timeout/60/24
      
      # process delays
      time - simmer::now(sim)
    }) %>% 
    set_attribute(c("travel_end", "travel_time"), function() {
      c(simmer::now(sim), 
        simmer::now(sim) - get_attribute(sim, "write_end"))
    }) %>% 
    
    # delivery time, 0.5 - 5 minutes
    timeout(function() {
      delivery_time = runif(1,.5,5)
      delivery_time/60/24
    }) %>% 
    set_attribute(c("delivery_end", "delivery_time"), function() {
      c(simmer::now(sim), 
        simmer::now(sim) - get_attribute(sim, "travel_end"))
    }) %>% 
    
    # reset time, drive back to base placement
    # not realistic but this code is already long for a toy example
    timeout(function() {
      van = get_selected(sim)
      travel_time = get_attribute(sim, paste0(van, "_time"))
      
      base_day = as.numeric(gsub("\\..*","",simmer::now(sim)))
      what_month = month(datekey[Elapsed_Day == base_day]$Date)
      
      # if in winter travel_time will always be expected, up to twice as long
      # if not, want rough bound of +/- 10%
      if (what_month %in% c(12,1:3)) {
        travel_time = travel_time * runif(1, 1, 2)
      } else {
        travel_time = travel_time * rnorm(1, 1, 0.05)
      }
      
      travel_time/60/24
    }) %>% 
    set_attribute(c("travelback_end", "travelback_time"), function() {
      c(simmer::now(sim), 
        simmer::now(sim) - get_attribute(sim, "delivery_end"))
    }) %>% 
    release_selected(1)
    
  # sim setup and run
  sim = simmer() %>% 
    add_resource(paste0("van_", 1:n_vans), 1) %>% 
    add_dataframe("letter ", 
                  letter, 
                  simmer_data, 
                  col_time = "simmer_time", 
                  time = "absolute", 
                  mon = 2)
  sim %>% simmer::run()
  
  # sim summary
  simulation = sim %>% get_mon_attributes() 
  simulation = as.data.table(simulation)
  simulation = dcast(simulation, name~key, value.var = "value")
  
  setcolorder(simulation, 
              c("name", "ID", "letter_request", "initial_delay_end",
                "write_end", "travel_end", "delivery_end",
                "travelback_end", "initial_delay_time", "travel_time",
                "Clust", "van_selected"))
  
  simulation = simulation[order(letter_request)]
  simulation[, N_Clust := n_vans]
  simulation[, Method := method]
  
  # quick sanity check
  # simulation[, Time_finished := travelback_end - floor(travelback_end)]
  # View(simulation[Time_finished <= 9/24 | Time_finished >= 19/24])
  
  message(n_vans)
  return(simulation)
}

# equalify simulation
equal_sim = future_Map(letter_sim,
                       n_vans = 2:10,
                       MoreArgs = list(
                         monthly_data = monthly,
                         method = "Equal",
                         cluster_solutions = cluster_dat,
                         time_mat = time_mat),
                       future.seed = T)
equal_sim = rbindlist(equal_sim, fill = T)

# pam with time simulation
time_sim = future_Map(letter_sim,
                       n_vans = 2:10,
                       MoreArgs = list(
                         monthly_data = monthly,
                         method = "Time",
                         cluster_solutions = cluster_dat,
                         time_mat = time_mat),
                       future.seed = T)
time_sim = rbindlist(time_sim, fill = T)

# pam with euclidean simulation
euc_sim = future_Map(letter_sim,
                       n_vans = 2:10,
                       MoreArgs = list(
                         monthly_data = monthly,
                         method = "Euc",
                         cluster_solutions = cluster_dat,
                         time_mat = time_mat),
                       future.seed = T)
euc_sim = rbindlist(euc_sim, fill = T)

# save simulations
simulation_dat = rbind(equal_sim, time_sim, euc_sim)
saveRDS(simulation_dat, "R_Objects/simulation_dat.rds")
pushover("All simulations completed!")
