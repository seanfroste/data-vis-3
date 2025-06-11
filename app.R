library(shiny)
library(shinydashboard)
library(tidyverse)
library(tidytuesdayR)
library(plotly)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# fetch data from the tidyTuesday github repo
tuesday_data <- tidytuesdayR::tt_load("2025-05-20")

# splitting datasets
# Historical weather data for Sydney provided by https://open-meteo.com/ API.
weather <- tuesday_data$weather
# Water quality data for Sydney beaches provided by https://www.beachwatch.nsw.gov.au/waterMonitoring/waterQualityData
water_quality <- tuesday_data$water_quality

# making water_quality date resolution the same as weather (daily)
daily_wq <- water_quality %>%
  group_by(region, council, swim_site, date) %>%
  summarize(
    enterococci_cfu_100ml = mean(enterococci_cfu_100ml),
    water_temperature_c = mean(water_temperature_c),
    conductivity_ms_cm = mean(conductivity_ms_cm),
    latitude = first(latitude),
    longitude = first(longitude),
    .groups = "drop",
  )

# filter datasets by common dates
water_date_range <- daily_wq$date %>% range.Date()
weather_date_range <- weather$date %>% range.Date()

daily_wq <- daily_wq %>%
  filter(date > max(water_date_range[1], weather_date_range[1])) %>%
  filter(date < min(water_date_range[2], weather_date_range[2]))

weather <- weather %>%
  filter(date > max(water_date_range[1], weather_date_range[1])) %>%
  filter(date < min(water_date_range[2], weather_date_range[2]))

# weather %>% Hmisc::describe()
# daily_wq %>% Hmisc::describe()

# impute the NA values
daily_wq <- daily_wq %>%
  mutate(month = month(date))

# For E-coli (Enterococci) stats
# I am going to group it by site and impute by median values over time.
daily_wq <- daily_wq %>%
  group_by(swim_site) %>%
  mutate(
    ecoli_site_median = median(enterococci_cfu_100ml, na.rm = TRUE),
    ecoli_imputed = ifelse(
      is.na(enterococci_cfu_100ml) & sum(!is.na(enterococci_cfu_100ml)) > 30,
      ecoli_site_median,
      enterococci_cfu_100ml
    )
  ) %>%
  ungroup() %>%
  mutate(enterococci_cfu_100ml = ecoli_imputed) %>%
  # deselect created columns
  select(-ecoli_site_median, -ecoli_imputed)

# For Water Temp and Conductivity, there are quite a few missing values, so
daily_wq <- daily_wq %>%
  group_by(swim_site, month) %>%
  mutate(
    water_temperature_c = ifelse(
      is.na(water_temperature_c),
      median(water_temperature_c, na.rm = TRUE),
      water_temperature_c
    ),
    conductivity_ms_cm = ifelse(
      is.na(water_temperature_c),
      median(conductivity_ms_cm, na.rm = TRUE),
      conductivity_ms_cm
    )
  ) %>%
  ungroup()

# outlier values
# set lower and upper quantiles for outliers
q <- quantile(daily_wq$enterococci_cfu_100ml, probs = c(0.01, 0.99), na.rm = TRUE)

# filtering
daily_wq <- daily_wq %>%
  filter(enterococci_cfu_100ml >= q[1], enterococci_cfu_100ml <= q[2])

ecoli_range <- daily_wq$enterococci_cfu_100ml %>% range()

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Swimming in the Sea: Sydney"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Water Quality: Time Series", tabName = "trends", icon = icon("line-chart")),
      menuItem("Rainfall and Water", tabName = "rain", icon = icon("cloud")),
      menuItem("Water Safety Map", tabName = "map", icon = icon("map"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "trends",
        fluidRow(plotlyOutput("trend_plot", height = "700px"))
      ),
      tabItem(
        tabName = "rain",
        fluidRow(plotlyOutput("rain_plot", height = "700px"))
      ),
      tabItem(
        tabName = "map",
        fluidRow(
          column(
            width = 8,
            plotlyOutput("map_plot", height = "600px")
          ),
          column(
            width = 8,
            sliderInput("threshold", "Safety Threshold (Enterococci per 100ml):",
              min = ecoli_range[1], max = ecoli_range[2], value = 650, step = ecoli_range[2] / 200
            ),
            actionButton("reset", "Reset Threshold", icon = icon("undo")),
            br(), br(),
            valueBoxOutput("unsafe_pct"),
          )
        )
      )
    )
  )
)

# SERVER
server <- function(input, output, session) {
  default_threshold <- 650

  observeEvent(input$reset, {
    updateSliderInput(session, "threshold", value = default_threshold)
  })

  # classifying swim sites as safe or unsafe
  water_data <- reactive({
    daily_wq %>%
      mutate(status = ifelse(enterococci_cfu_100ml > input$threshold, "Unsafe", "Safe"))
  })

  output$trend_plot <- renderPlotly({
    data <- water_data() %>%
      mutate(month_date = lubridate::floor_date(date, "month")) %>%
      group_by(month_date, region) %>%
      summarise(mean_ec = mean(enterococci_cfu_100ml, na.rm = TRUE), .groups = "drop")

    p <- ggplot(data, aes(x = month_date, y = mean_ec, colour = region)) +
      geom_line() +
      labs(
        title = "Monthly Average Enterococci Levels by Region",
        y = "Mean Enterococci (cfu/100ml)",
        x = "Month"
      ) +
      theme_minimal()

    ggplotly(p)
  })


  output$rain_plot <- renderPlotly({
    merged <- water_data() %>%
      left_join(weather, by = "date") %>%
      filter(!is.na(precipitation_mm), !is.na(enterococci_cfu_100ml))

    p <- ggplot(merged, aes(
      x = precipitation_mm, y = enterococci_cfu_100ml, color = status,
      text = paste("Site:", swim_site)
    )) +
      geom_point(alpha = 0.6) +
      scale_color_manual(values = c("Safe" = "blue", "Unsafe" = "red")) +
      labs(
        title = "Rainfall vs Enterococci Levels",
        x = "Rainfall (mm)", y = "Enterococci (cfu/100ml)"
      ) +
      theme_minimal()

    ggplotly(p, tooltip = "text")
  })

  output$map_plot <- renderPlotly({
    mapdata <- water_data()

    # Load Australia's Eastern coastline from rnaturalearth package
    land <- rnaturalearth::ne_countries(
      scale = "medium", returnclass = "sf", country = "Australia"
    )

    p <- ggplot() +
      # Add landmass silhouette
      geom_sf(data = land, fill = "grey80", alpha = 0.4, colour = NA) +
      # Swim site points
      geom_point(
        data = mapdata,
        aes(
          x = longitude, y = latitude, color = status,
          # hover tooltip for the point under the mouse
          text = paste(
            "Site:", swim_site,
            "<br>Enterococci:", enterococci_cfu_100ml
          )
        ),
        size = 3, alpha = 0.7
      ) +
      scale_color_manual(values = c("Safe" = "blue", "Unsafe" = "red")) +
      labs(title = "Swim Site Safety Map", x = "Longitude", y = "Latitude") +
      theme_minimal() +
      coord_sf(xlim = c(150.5, 151.5), ylim = c(-34.1, -33.5), expand = FALSE) +
      annotate("text", x = 150.9, y = -33.6, label = "Northern Beaches", size = 3, colour = "black") +
      annotate("text", x = 151.15, y = -33.9, label = "Southern Sydney", size = 3, colour = "black") +
      annotate("text", x = 151.1, y = -33.85, label = "Harbour", size = 3, colour = "black")

    ggplotly(p, tooltip = "text")
  })


  output$unsafe_pct <- renderValueBox({
    data <- water_data()
    unsafe_pct <- mean(data$status == "Unsafe", na.rm = TRUE) * 100
    valueBox(sprintf("%.1f%% Unsafe", unsafe_pct), "(E-coli conc. > NSW Threshold)", icon = icon(
      "biohazard"
    ), color = "yellow")
  })
}

shinyApp(ui, server)
