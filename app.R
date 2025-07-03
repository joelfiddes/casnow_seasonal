library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(viridisLite)

# Load all variable datasets into a named list
data_list <- list(
  "albedo"     = read_csv("./data/processed_alb.csv"),
  "ROF"        = read_csv("./data/processed_Rof.csv"),
  "snow depth" = read_csv("./data/processed_snd.csv"),
  "SWE"        = read_csv("./data/processed_SWE.csv"),
  "T_surf"     = read_csv("./data/processed_Tsf.csv")
)

# Optional: map variable names to units for labeling
variable_units <- list(
  "albedo" = "[-]",
  "ROF" = "[mm/day]",
  "snow depth" = "[cm]",
  "SWE" = "[mm]",
  "T_surf" = "[°C]"
)

# UI
ui <- fluidPage(

    # Logos with links
  fluidRow(
    style = "position: fixed; bottom: 10px; right: 10px; z-index: 1000; display: flex; gap: 15px; align-items: center;",
    tags$a(href = "https://www.unesco.org", target = "_blank",
           tags$img(src = "unesco.png", height = "50px")),
    tags$a(href = "https://mountainfutures.ch/", target = "_blank",
           tags$img(src = "mf.png", height = "100px")),
    tags$a(href = "https://www.thegef.org/", target = "_blank",
           tags$img(src = "gef.png", height = "110px"))
  ),

  titlePanel("21st Century Snow Climate Scenarios for Central Asia "),
    # Explanatory text paragraph

  sidebarLayout(
    sidebarPanel(
      selectInput("variable", "Variable", 
                  choices = names(data_list), 
                  selected = "SWE"),
      selectInput("region", "Region", choices = NULL),
      selectInput("scenario", "Scenario", choices = NULL),

        # Explanation box under selectors
  wellPanel(
    tags$p(tags$b("Information:")),
tags$p("This app presents 21st-century snow climate scenarios for Central Asia using the TopoCLIM model chain (Fiddes et al., 2022). 
It integrates terrain clustering, high-resolution climate downscaling, and bias-corrected future climate projections (CMIP6 SSP2-4.5 & SSP5-8.5).
 The FSM snow model then simulates key snow variables—such as snow water equivalent, depth, and runoff—across different time periods (2000-2020, 2040-2060, 2080-2100). 
 This approach captures complex mountain influences to assess climate change impacts on snow conditions in the region.")

  )

    ),
    mainPanel(
      plotOutput("swePlot", height = "600px")
    )
  )
)

# Server
server <- function(input, output, session) {

  # Reactive data reshaping
  df_long <- reactive({
    req(input$variable)
    
    data_list[[input$variable]] %>%
      pivot_longer(cols = c("historical_mean", "future_mean"), 
                   names_to = "period", values_to = "value") %>%
      mutate(elev_band = paste0(elev_low, "-", elev_high))
  })

  # Update region choices based on selected variable
  observe({
    req(df_long())
    updateSelectInput(session, "region", 
                      choices = unique(df_long()$region),
                      selected = unique(df_long()$region)[1])
  })

  # Update scenario choices based on selected region
  observe({
    req(df_long(), input$region)
    scenarios <- df_long() %>%
      filter(region == input$region) %>%
      pull(scenario) %>%
      unique()
    updateSelectInput(session, "scenario", 
                      choices = scenarios,
                      selected = scenarios[1])
  })

  # Render plot
  output$swePlot <- renderPlot({
    req(df_long(), input$region, input$scenario)

    df_filtered <- df_long() %>%
      filter(region == input$region, scenario == input$scenario)

      df_filtered <- df_long() %>%
    filter(region == input$region, scenario == input$scenario) %>%
    mutate(
      # Reference hydrological year start: Sept 1, 2000
      hydro_date = as.Date("2000-09-01") + (hydro_day - 1)
    )


    if (nrow(df_filtered) == 0) {
      return(ggplot() +
               annotate("text", x = 0.5, y = 0.5, label = "No data available", size = 8) +
               theme_void())
    }

    unit_label <- variable_units[[input$variable]]

    ggplot(df_filtered, aes(x = hydro_date, y = value,
                            color = elev_band, linetype = period,
                            group = interaction(elev_band, period))) +
      geom_line(size = 1) +
      scale_color_viridis_d(name = "Elevation Band") +
      scale_linetype_manual(values = c("historical_mean" = "solid", "future_mean" = "twodash"),
  guide = guide_legend(
    override.aes = list(size = 1.5)  # Make lines thicker in legend

  ))+ 

    scale_x_date(
    date_labels = "%m-%d",     # Format x-axis labels as MM-DD
    date_breaks = "1 month",   # Tick marks every month (adjust as needed)
    expand = expansion(add = c(0, 0))
  ) +

  

    
      labs(
        title = paste("Variable:", input$variable,
                      "| Region:", input$region,
                      "| Scenario:", input$scenario),
        x = "Hydrological Day",
        y = paste0(input$variable, " ", unit_label),
        linetype = "Period"
      ) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom", legend.box = "vertical",
    legend.title = element_text(size = 16, face = "bold"),  # Increase legend title size
    legend.text = element_text(size = 14),


    )
  })
}

# Run the app
shinyApp(ui, server)
