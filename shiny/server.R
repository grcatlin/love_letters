library(data.table)
library(leaflet)
library(ggplot2)
library(stringr)

function(input, output, session) {
  
  #######################
  # palette consistency #
  #######################
  
  palette = reactive({
    fn_cols <- grDevices::colorRamp(colors, space = "Lab", interpolate = "spline")
    cols <- fn_cols(seq(0, 1, length.out = input$nclust))/255
    pal = grDevices::rgb(cols[, 1], cols[, 2], cols[, 3], alpha = 1)
    pal = data.table(Palette = pal)
    pal[, clust := as.character(.I)]
    setkey(pal, clust)
    return(pal)
  })

  ###########
  # leaflet #
  ###########
  
  map_dat = reactive({
    dat = cluster[Method == input$method & N_Clust == input$nclust]
    setkey(dat, Clust)
    dat = dat[palette()]
    return(dat)
  })

  output$map <- renderLeaflet({
    dat = map_dat()
    cluster_map(dat)
  })

  
  #####################
  # side panel graphs #
  #####################
  
  summarized_dat = reactive({
    str = paste0("sum_",input$method,"_",input$nclust)
    summarized[[str]]
  })
  
  output$density = renderHighchart({
    
    dat = summarized_dat()
    dat = dat$density
    dat = dat[[str_to_lower(input$scale)]]
    
    hchart(dat, color = colors[3]) %>%
      hc_add_theme(love_theme(pal = palette()$Palette)) %>%
      hc_yAxis(title = list(text = "Density")) %>%
      hc_title(text = "Wait Time Density") %>%
      hc_subtitle(text = paste0(input$method, " Cluster for ", input$nclust, 
                                " Vans")) %>%
      hc_xAxis(title = list(text = paste0("Time in ", input$scale))) %>%
      hc_legend(enabled = F) %>%
      hc_tooltip(enabled = F)
  })
  
  output$workload = renderHighchart({
    dat = summarized_dat()
    dat = dat$work
    
    hchart(dat,
           type = "pie",
           hcaes(x = van_selected, y = N)) %>%
      hc_add_theme(love_theme(pal = palette()$Palette)) %>%
      hc_tooltip(pointFormat = paste0(
        "# Cards Delivered: <b> {point.N} </b> <br>",
        "Average Wait: <b> {point.Wait: .2f} Minutes </b>")) %>%
      hc_title(text = "Workload") %>%
      hc_subtitle(text = paste0(input$method, " Cluster for ", input$nclust, 
                                " Vans"))
  })
  

  ########################
  # observers - map view #
  ########################

  observeEvent(input$reset, {
    map <- leafletProxy("map")
    map %>% fitBounds(min(map_dat()$Lon), min(map_dat()$Lat), 
                      max(map_dat()$Lon), max(map_dat()$Lat))
  })
  
  ####################
  # comparison plots #
  ####################
  
  palette_comp = reactive({
    fn_cols <- grDevices::colorRamp(colors, space = "Lab", interpolate = "spline")
    cols <- fn_cols(seq(0, 1, length.out = input$nclustcomp))/255
    pal = grDevices::rgb(cols[, 1], cols[, 2], cols[, 3], alpha = 1)
    pal = data.table(Palette = pal)
    pal[, clust := as.character(.I)]
    setkey(pal, clust)
    return(pal)
  })
  
  m1_dat = reactive({
    str = paste0("sum_",input$m1,"_",input$nclustcomp)
    summarized[[str]]
  })
  
  m2_dat = reactive({
    str = paste0("sum_",input$m2,"_",input$nclustcomp)
    summarized[[str]]
  })
  
  output$w1 = renderHighchart({
    dat = m1_dat()
    dat = dat$work
    
    hchart(dat,
           type = "pie",
           hcaes(x = van_selected, y = N)) %>%
      hc_add_theme(love_theme(pal = palette_comp()$Palette)) %>%
      hc_tooltip(pointFormat = paste0(
        "# Cards Delivered: <b> {point.N} </b> <br>",
        "Average Wait: <b> {point.Wait: .2f} Minutes </b>")) %>%
      hc_title(text = "Workload") %>%
      hc_subtitle(text = paste0(input$m1, " Cluster for ", input$nclust, 
                                " Vans"))
  })
  
  output$w2 = renderHighchart({
    dat = m2_dat()
    dat = dat$work
    
    hchart(dat,
           type = "pie",
           hcaes(x = van_selected, y = N)) %>%
      hc_add_theme(love_theme(pal = palette_comp()$Palette)) %>%
      hc_tooltip(pointFormat = paste0(
        "# Cards Delivered: <b> {point.N} </b> <br>",
        "Average Wait: <b> {point.Wait: .2f} Minutes </b>")) %>%
      hc_title(text = "Workload") %>%
      hc_subtitle(text = paste0(input$m2, " Cluster for ", input$nclust, 
                                " Vans"))
  })
  
  output$density_compare = renderHighchart({
    h1 = m1_dat()$density$hours
    h2 = m2_dat()$density$hours
    h1 = -hist(h1, breaks = 100)$density
    h2 = hist(h2, breaks = 100)$density
    b1 = max(abs(h1))
    b2 = max(abs(h2))
    bound = fifelse(b1 > b2, b1, b2)
    
    # distribution compare plot
    highchart() %>% 
      hc_chart(type = "bar") %>% 
      hc_plotOptions(series = list(stacking='normal'),
                     column = list(dataLabels = list(enabled = FALSE), 
                                   enableMouseTracking = TRUE)) %>% 
      hc_add_series(data = h1, name = input$m1, color = colors[5]) %>% 
      hc_add_series(data = h2, name = input$m2, color = colors[7]) %>% 
      hc_xAxis(reversed=F,
               title = list(text = "Time in Hours")) %>%
      hc_yAxis(title = list(text = "Density"),
               labels = list(enabled = F),
               tickAmount = 0,
               min = -bound,
               max = bound) %>%
      hc_legend(enabled = T)  %>% 
      hc_add_theme(love_theme(pal = palette_comp()$Palette)) %>% 
      hc_title(text = "Density Comparison") %>%
      hc_subtitle(text = paste0(input$m1," vs. ", input$m2, " for ",
                                input$nclustcomp, " Vans")) %>% 
      hc_tooltip(pointFormat = paste0(
        "Density: <b> {point.y: .2f} </b> <br>",
        "Hour: <b> {point.x} </b>"))
  })
  


  ##########################
  # if method1 == method 2 #
  ##########################

  observe({
    if (input$m1 == input$m2) {
      via_mts = mts[mts != input$m1]
      updateSelectInput(session,"m2", selected = sample(via_mts))
    }
  })

  ###############################################
  # swamp # amb and county between comp and map #
  ###############################################

  observe({
    if (input$nav == "Cluster Comparison") {
      updateSelectInput(session, "nclustcomp", selected = input$nclust)
    }

    if (input$nav == "Interactive Map") {
      updateSelectInput(session, "nclust", selected = input$nclustcomp)
    }
  })
  
}
