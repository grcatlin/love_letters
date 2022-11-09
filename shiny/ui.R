navbarPage(
  "love_letters app",
  id = "nav",
  
  tags$head(tags$link(
    rel = "icon", type = "image/png", href = "heart.png"
  )),
  
  tabPanel(
    "Interactive Map",
    div(
      class = "outer",
      
      tags$head(
        # Include custom CSS
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
      ),
      
      leafletOutput("map", width = "100%", height = "100%"),
      
      
      absolutePanel(
        id = "controls",
        class = "panel panel-default",
        fixed = TRUE,
        draggable = F,
        top = 60,
        right = 20,
        bottom = "auto",
        width = "350px",
        height = "auto",
        
        tags$p(strong("Selection"), style = "font-size: 2.25vh; padding-top: 2vh"),
        
        # inputs
        selectInput("method", "Clustering Method", mts),
        sliderInput(
          "nclust",
          "Number of Ambulances",
          2,
          10,
          value = 7,
          ticks = F,
          step = 1
        ),
        actionButton("reset", "Reset View"),
        
        hr(style = "border-top: 1px solid #ff4e4e; margin-top: 3vh; margin-bottom: 2vh"),
        
        tags$p(strong("Results"),
               style = "font-size: 2.25vh; margin-top:0px; padding-top: 0px"),
        
        selectInput(
          "res",
          label = NULL,
          c("Distribution of Times", "Workload"),
          selected = "Workload"
        ),
        
        conditionalPanel(
          condition = "input.res == 'Distribution of Times'",
          selectInput(
            "scale",
            "Time Measurement",
            c("Minutes", "Hours", "Days"),
            selected = "Minutes"
          ),
          highchartOutput("density", height = "225px")
        ),
        
        conditionalPanel(condition = "input.res == 'Workload'",
                         highchartOutput("workload", height = "300px"))
      )
    )
  ),
  
  tabPanel(
    "Cluster Comparison",
    fluidPage(
      align = "center",
      fluidRow(
        column(
          4,
          selectInput("m1", "Method 1:", choices = mts, selected = 'Equal')
        ),
        
        column(
          4,
          sliderInput(
            "nclustcomp",
            "Number of Ambulances",
            2,
            10,
            value = 7,
            ticks = F,
            step = 1
          )
        ),
        
        column(4,
               selectInput(
                 "m2", "Method 2:", choices = mts, selected = "Time"
               ))
      ),
      
      # border
      fluidRow(column(4),
               column(
                 4,
                 hr(style = "border-top: 1px solid #ff4e4e")
               ),
               column(4)),
      
      # workload plots
      fluidRow(
        # all else
        column(3, align = "center",
               highchartOutput("w1", height = "500px")),
        column(6,
               # density comparison
               highchartOutput("density_compare", height = "500px", width = "80%")
               ),
        column(3, align = "center",
               highchartOutput("w2", height = "500px"))
      ),
      

    )
  )
)
