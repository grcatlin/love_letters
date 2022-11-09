library(data.table)
library(leaflet)
library(highcharter)
library(shiny)

# import data 
cluster = readRDS("data/cluster.rds")
summarized = readRDS("data/summarized.rds")

# dropdown choices
mts = unique(cluster$Method)

# palette
colors = c("#FFEEB2FF", "#FFCA9EFF", "#FF8C89FF", "#756585FF", "#09A7B4FF",
           "#43D99AFF", "#8FFA85FF")

# leaflet function
cluster_map = function(cluster_data) {
  cluster_data[Medoid == 1, Palette := "black"]
  
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
  Q = quantile(cluster_data$Population)
  cluster_data[, Radius := radius_function(Population, Q), by = ID]
  cluster_data[Medoid == 1, Radius := 10]
  cluster_data[, Opacity := ifelse(Medoid == 1, 1, .7)]
  
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
  cluster_data[, Label := label_function(.SD), by = ID]
  
  # leaflet  
  leaflet() %>% 
    addProviderTiles(providers$CartoDB.DarkMatter) %>% 
    setView(lat=mean(cluster_data$Lat), 
            lng=mean(cluster_data$Lon), 
            zoom = 12) %>%
    addCircleMarkers(data = cluster_data[Medoid == 0],
                     ~Lon, ~Lat, 
                     fillColor = ~Palette, 
                     fillOpacity = ~Opacity, 
                     color="white", 
                     radius = ~Radius, 
                     stroke=F,
                     label = ~Label,
                     labelOptions = labelOptions(textsize = "13px")) %>%
    addCircleMarkers(data = cluster_data[Medoid == 1],
                     ~Lon, ~Lat, 
                     fillColor = ~Palette, 
                     fillOpacity = ~Opacity, 
                     color="white", 
                     radius = ~Radius, 
                     stroke=T,
                     label = ~Label,
                     labelOptions = labelOptions(textsize = "13px"))
}

# highchart theme
love_theme = function (main = "white", back = NA, pal) 
{
  theme <- hc_theme(colors = pal, 
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