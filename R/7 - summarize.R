library(data.table)
library(stringr)
library(lubridate)
library(highcharter)

# imports
simulation_results = readRDS("R_Objects/simulation_dat.rds")
cluster_dat = readRDS("R_Objects/cluster_dat.rds")

# subset setup
method = "Equal"; vans = 5
sim_subset = simulation_results[Method == method & N_Clust == vans]

# get distribution of wait times
sim_subset[, wait_total := travelback_end - letter_request]
sim_subset[, wait_adjusted := travel_end - initial_delay_end]
sim_subset[, wait_deliver := delivery_end - write_end]
dist_wait_minutes = sim_subset$wait_deliver * 60 * 24
dist_wait_hours = sim_subset$wait_deliver * 60
dist_wait_days = sim_subset$wait_deliver

# workload pie chart data
work = sim_subset[, .(.N, Wait =mean(delivery_end - write_end) * 60 * 24), 
                  by = van_selected]
work[, van_selected := paste0("Van ", van_selected)]
work = work[order(van_selected)]

# palette
colors = c("#FFEEB2FF", "#FFCA9EFF", "#FF8C89FF", "#756585FF", "#09A7B4FF",
           "#43D99AFF", "#8FFA85FF")

# hchart theme (modified monokai)
love_theme = function (main = "black", back = NA) 
{
  theme <- hc_theme(colors = colors, 
                    chart = list(
                      backgroundColor = back,
                      style = list(
                        fontFamily = "Inconsolata", 
                        color = main
                      )
                    ), 
                    title = list(
                      style = list(color = main), 
                      align = "left"), 
                    subtitle = list(
                      style = list(color = main), 
                      align = "left"), 
                    legend = list(
                      align = "right", 
                      verticalAlign = "bottom", 
                      itemStyle = list(
                        fontWeight = "normal", 
                        color = main)
                    ), 
                    xAxis = list(
                      gridLineDashStyle = "Dot", 
                      gridLineWidth = 1, 
                      gridLineColor = main, 
                      lineColor = main, 
                      minorGridLineColor = main, 
                      tickColor = main, 
                      tickWidth = 1,
                      labels = list(
                        style = list(
                          color = main
                        )
                      ),
                      title = list(
                        style = list(
                          color = main
                        )
                      )
                    ), 
                    yAxis = list(
                      gridLineDashStyle = "Dot", 
                      gridLineColor = main, 
                      lineColor = main, 
                      minorGridLineColor = main, 
                      tickColor = main, 
                      tickWidth = 1,
                      labels = list(
                        style = list(
                          color = main
                        )
                      ),
                      title = list(
                        style = list(
                          color = main
                        )
                      )
                    )
  )
  
  theme <- structure(theme, class = "hc_theme")
  theme
}

# distribution plot
hchart(dist_wait_hours, color = colors[3]) %>% 
  hc_add_theme(love_theme()) %>% 
  hc_yAxis(title = list(text = "Density")) %>%
  hc_title(text = "Wait Time Density") %>%
  hc_subtitle(text = "Equalify for 5 Vans") %>%
  hc_xAxis(title = list(text = "Time in Hours")) %>%
  hc_legend(enabled = F) %>% 
  hc_tooltip(enabled = F)

# pie chart
hchart(work,
       type = "pie",
       hcaes(x = van_selected, y = N)) %>% 
  hc_add_theme(love_theme()) %>% 
  hc_tooltip(pointFormat = paste0(
    "# Cards Delivered: <b> {point.N} </b> <br>",
    "Average Wait: <b> {point.Wait: .2f} Minutes </b>")) %>% 
  hc_title(text = "Workload") %>%
  hc_subtitle(text = "Equalify for 5 Vans")

# subset 2 setup
method = "Time"; vans = 5
sim_subset2 = simulation_results[Method == method & N_Clust == vans]

# get distribution of wait times
sim_subset2[, wait_total := travelback_end - letter_request]
sim_subset2[, wait_adjusted := travel_end - initial_delay_end]
sim_subset2[, wait_deliver := delivery_end - write_end]
dist_wait_hours2 = sim_subset2$wait_deliver * 60

# distribution compare setup
h1 = hist(dist_wait_hours, breaks = 100)$density
h2 = -hist(dist_wait_hours2, breaks = 100)$density
b1 = max(abs(h1))
b2 = max(abs(h2))
bound = fifelse(b1 > b2, b1, b2)

# distribution compare plot
highchart() %>% 
  hc_chart(type = "bar") %>% 
  hc_plotOptions(series = list(stacking='normal'),
                 column = list(dataLabels = list(enabled = FALSE), 
                               enableMouseTracking = TRUE)) %>% 
  hc_add_series(data = h1, name = "Equalify", color = colors[5]) %>% 
  hc_add_series(data = h2, name = "Time", color = colors[7]) %>% 
  hc_xAxis(reversed=F,
           title = list(text = "Time in Hours"),
           max = 20) %>%
  hc_yAxis(title = list(text = "Density"),
           labels = list(enabled = F),
           tickAmount = 0,
           min = -bound,
           max = bound) %>%
  hc_legend(enabled = T)  %>% 
  hc_add_theme(love_theme()) %>% 
  hc_title(text = "Density Comparison") %>%
  hc_subtitle(text = "Equalify vs. Time for 5 Vans") %>% 
  hc_tooltip(pointFormat = paste0(
    "Density: <b> {point.y: .2f} </b> <br>",
    "Hour: <b> {point.x} </b>"))

# now summarize for all combinations
summarize = function(method, van, dat) {
  subset = dat[Method == method & N_Clust == van]
  
  # get distribution of wait times
  subset[, wait_total := travelback_end - letter_request]
  subset[, wait_adjusted := travel_end - initial_delay_end]
  subset[, wait_deliver := delivery_end - write_end]
  dist_wait_minutes = subset$wait_deliver * 60 * 24
  dist_wait_hours = subset$wait_deliver * 60
  dist_wait_days = subset$wait_deliver
  density = list(minutes = dist_wait_minutes,
                 hours = dist_wait_hours,
                 days = dist_wait_days)
  
  # get workload
  work = subset[, .(.N, Wait = mean(delivery_end - write_end) * 60 * 24), 
                by = van_selected]
  work[, van_selected := paste0("Van ", van_selected)]
  work = work[order(van_selected)]
  
  # combine as one object and save
  summary = list(density = density, work = work)
  assign(paste0("sum_",method,"_",van), summary, envir = parent.frame())
}

# summarize
for (van in 2:10) {
  summarize("Time", van, simulation_results)
  summarize("Euc", van, simulation_results)
  summarize("Equal", van, simulation_results)
}

# combine results & save
summarized = mget(ls(pattern = "sum_"))
dir.create("shiny")
dir.create("shiny/data")
saveRDS(summarized, "shiny/data/summarized.rds", compress = F)
saveRDS(cluster_dat, "shiny/data/cluster.rds", compress = F)
