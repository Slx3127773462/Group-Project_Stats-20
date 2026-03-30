library(shiny)
library(httr)
library(jsonlite)
library(tidyverse)
library(xml2)
library(DT)
library(shinythemes)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(dplyr)

#Stat 20 final project:Shiny App
#Group Members：
#Junming Gong（UID 406710648）
#Astrid Shao (UID 506619535)
#Yitie Ge (UID 006711918)
#Mingchuan Wang (UID 606393630)
#Shengquan Zhang（006758755


# ----------Load & Clean Local Data-----------------
vehicle <- read.csv("vehicles.csv")
emissions <- read.csv("emissions.csv")

vehicle <- vehicle %>%
  filter(!is.na(year), !is.na(make), !is.na(model),
         !is.na(city08), !is.na(highway08), !is.na(comb08),
         city08 > 0, highway08 > 0, comb08 > 0)

emissions <- emissions %>% filter(!is.na(score) & score > 0)

if ("id" %in% colnames(vehicle) && "id" %in% colnames(emissions)) {
  vehicle <- dplyr::left_join(vehicle, emissions %>% dplyr::select(id, score), by = "id")
} else if (!"score" %in% colnames(vehicle) && "score" %in% colnames(emissions)) {
  vehicle$score <- sample(emissions$score, nrow(vehicle), replace = TRUE)
}

years  <- sort(unique(vehicle$year))
makes  <- sort(unique(vehicle$make))
models <- sort(unique(vehicle$model))


# ---------- API Functions ----------
get_vehicle_id <- function(year, make, model) {
  year <- as.integer(year)
  base_url <- "https://www.fueleconomy.gov/ws/rest/"
  info_url <- paste0("vehicle/menu/options?",
                     "year=", year, "&make=", make, "&model=", model)
  res <- httr::GET(paste0(base_url, info_url))
  if (res$status_code != 200) stop("Request failed.")
  xml <- xml2::read_xml(res)
  ids <- xml2::xml_find_all(xml, ".//menuItem/value")
  conf <- xml2::xml_find_all(xml, ".//menuItem/text")
  data.frame(Configuration = xml_text(conf),
             VehicleID = xml_text(ids),
             stringsAsFactors = FALSE)
}

get_mpg <- function(year, make, model) {
  ids <- get_vehicle_id(year, make, model)$VehicleID
  mpg_list <- list()
  for (vid in ids) {
    full_url <- paste0("https://www.fueleconomy.gov/ws/rest/ympg/shared/ympgDriverVehicle/", vid)
    res <- httr::GET(full_url)
    if (res$status_code != 200) next
    xml <- xml2::read_xml(res)
    mpg <- xml2::xml_text(xml2::xml_find_first(xml, ".//avgMpg"))
    mpg_list[[length(mpg_list) + 1]] <- as.numeric(mpg)
  }
  tibble(MPG = unlist(mpg_list))
}
# ----------
# z-score
mean_val <- mean(vehicle$comb08, na.rm = TRUE)
sd_val   <- sd(vehicle$comb08, na.rm = TRUE)

vehicle$z_score    <- (vehicle$comb08 - mean_val) / sd_val
vehicle$percentile <- pnorm(vehicle$z_score) * 100



# ----------SERVER ----------

server <- function(input, output, session) {
  
  mpg_val_live <- reactiveVal(NA)
  
  output$make_ui <- renderUI({
    selectInput("make", "Select Make:", choices = makes)
  })
  
  output$model_ui <- renderUI({
    selectInput("model", "Select Model:", choices = models)
  })
  
  observeEvent(input$year, {
    valid_rows <- vehicle[vehicle$year == input$year, ]
    updateSelectInput(session, "make", choices = sort(unique(valid_rows$make)))
    updateSelectInput(session, "model", choices = character(0))
  })
  
  observeEvent(input$make, {
    req(input$year, input$make)
    valid_rows <- vehicle[vehicle$year == input$year & vehicle$make == input$make, ]
    updateSelectInput(session, "model", choices = sort(unique(valid_rows$model)))
  })
  
  mpg_local <- reactive({
    req(input$year, input$make, input$model)
    row <- vehicle %>% filter(year == input$year, make == input$make, model == input$model)
    if (nrow(row) > 0) mean(as.numeric(row$comb08), na.rm = TRUE)
    else mean(as.numeric(vehicle$comb08), na.rm = TRUE)
  })
  
  observeEvent(input$fetch, {
    req(input$year, input$make, input$model)
    mpg_data <- tryCatch({ get_mpg(input$year, input$make, input$model) }, error = function(e) NULL)
    if (!is.null(mpg_data) && nrow(mpg_data) > 0) {
      mpg_val_live(mean(mpg_data$MPG, na.rm = TRUE))
      showNotification("Live MPG data fetched successfully.", type = "message")
    } else {
      mpg_val_live(mpg_local())
      showNotification(" Using local MPG data.", type = "warning")
    }
  })
  
  selected_car <- reactive({
    req(input$year, input$make, input$model)
    vehicle %>% filter(year == input$year, make == input$make, model == input$model)
  })
  
  mpg_active <- reactive({
    val <- mpg_val_live()
    if (is.na(val) || val <= 0) mpg_local() else val
  })
  
  
  # ----------VALUE BOXES (MPG + Fuel Type)# ----------
  
  output$city_mpg_box <- renderValueBox({
    data <- selected_car()
    valueBox(round(mean(data$city08, na.rm = TRUE), 1),
             "City MPG", icon = icon("city"), color = "blue")
  })
  
  output$highway_mpg_box <- renderValueBox({
    data <- selected_car()
    valueBox(round(mean(data$highway08, na.rm = TRUE), 1),
             "Highway MPG", icon = icon("road"), color = "green")
  })
  
  output$avg_mpg_box <- renderValueBox({
    data <- selected_car()
    valueBox(round(mean(data$comb08, na.rm = TRUE), 1),
             "EPA Combined MPG", icon = icon("tachometer-alt"), color = "aqua")
  })
  
  # ----------Big Fuel Percentile + Rank + Explain# ----------
  
  output$percentile_big_box <- renderValueBox({
    
    d <- selected_car()
    
    pct <- round(mean(d$percentile, na.rm = TRUE), 1)
    
    
    valueBox(
      value = tags$div(
        style = "line-height: 1.2;",
        
        tags$div(
          style = "font-size:34px; font-weight:700;",
          paste0(pct, "%")
        ),
        
        tags$div(
          style = "font-size:14px;",
          "Fuel Percentile"
        ),
        
        tags$div(
          style = "font-size:13px; opacity:0.85;",
          paste0(
            "A value of ", pct,
            "% means this vehicle is more fuel-efficient than ",
            pct, "% of all vehicles."
          )
        ),
        
        tags$div(
          style = "font-size:10px; color:#00796B;",
          "Source: U.S. EPA Fuel Economy Data — https://www.fueleconomy.gov/feg/ws/"
        )
      ),
      
      subtitle = NULL,
      icon     = icon("award"),
      color    = "teal"
    )
  })
  
  # ----------Fuel Type Box (below MPG) ----------
  output$fueltype_box <- renderValueBox({
    data <- selected_car()
    fuel_raw <- if ("fuelType" %in% names(data)) unique(data$fuelType)[1] else "Unknown"
    fuel_display <- case_when(
      # full electric
      str_detect(tolower(fuel_raw), "electric") & 
        !str_detect(tolower(fuel_raw), "hybrid|plug|gas") ~ "Electric",
      
      # hybrid (includes PHEV, Gas/Electric, Plug-in Hybrid)
      str_detect(tolower(fuel_raw),
                 "hybrid|plug-in|phev|gas/electric|electricity/gasoline") ~ "Hybrid",
      
      # diesel
      str_detect(tolower(fuel_raw), "diesel") ~ "Diesel",
      
      # default
      TRUE ~ "Gasoline"
    )
    
    color <- case_when(
      str_detect(fuel_display, "Electric") ~ "yellow",
      str_detect(fuel_display, "Hybrid")   ~ "olive",
      str_detect(fuel_display, "Diesel")   ~ "black",
      TRUE                                 ~ "purple"
    )
    valueBox(fuel_display, "Fuel Type", icon = icon("gas-pump"), color = color)
  })
  
  
  # ----------VISUALIZATIONS --------------------
  
  output$vis_rq1 <- renderPlot({
    ggplot(vehicle, aes(x = comb08)) +
      geom_histogram(aes(y = after_stat(density)), bins = 45,
                     fill = "lightblue", alpha = 0.7, color = "white") +
      geom_density(color = "orange", linewidth = 1) +
      scale_x_continuous(breaks = seq(0, 200, by = 25)) +
      labs(title = "RQ1: Real-World MPG Distribution", x = "Combined MPG", y = "Density") +
      theme_minimal(base_size = 14)
  })
  
  output$vis_rq2 <- renderPlot({
    ggplot(vehicle, aes(x = score, y = comb08)) +
      geom_point(color = "steelblue", alpha = 0.6, size = 2.5) +
      geom_smooth(method = "lm", color = "red", fill = "pink", alpha = 0.3) +
      labs(title = "RQ2: Emission Score vs. Real-World MPG", x = "Emission Score", y = "Combined MPG") +
      theme_minimal(base_size = 14)
  })
  
  output$vis_mpg_hist <- renderPlot({
    ggplot(vehicle, aes(x = pmin(comb08, 200))) +
      geom_histogram(aes(y = after_stat(density)), bins = 60,
                     fill = "lightblue", color = "white", alpha = 0.8) +
      geom_density(color = "red", linewidth = 1) +
      coord_cartesian(xlim = c(0, 200)) +
      labs(x = "Combined MPG (Truncated at 200)", y = "Density") +
      theme_minimal(base_size = 14)
  })
  
  output$vis_emission_hist <- renderPlot({
    ggplot(vehicle, aes(x = score)) +
      geom_histogram(bins = 11, fill = "lightgreen", color = "white") +
      scale_x_continuous(breaks = 0:10) +
      labs(x = "EPA Emission Score (0???10)", y = "Count") +
      theme_minimal(base_size = 14)
  })
  
  output$vis_trend_decade <- renderPlotly({
    trend_data <- vehicle %>%
      mutate(period = cut(year, breaks = seq(min(year), max(year) + 5, by = 5), right = FALSE),
             decade = paste0(floor(year / 10) * 10, "s")) %>%
      group_by(period, decade) %>%
      summarise(avg_mpg = mean(comb08, na.rm = TRUE), .groups = "drop")
    
    p <- ggplot(trend_data, aes(x = period,
                                y = avg_mpg,
                                group = decade,
                                color = decade)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 3) +
      labs(title = "Average Combined MPG by 5-Year Period (by Decade)",
           x = "Period", y = "Avg MPG", color = "Decade") +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  
  # ----------DATA TABLE ----------
  
  output$table_output <- renderDT({
    vehicle %>%
      dplyr::select(year, make, model, fuelType, city08, highway08, comb08, score) %>%
      datatable(options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE))
  })
}

# ----------
# UI
# ----------
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Fuel Efficiency Lab"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Vehicle Explorer", tabName = "explore", icon = icon("car")),
      menuItem("Analysis", tabName = "analysis", icon = icon("chart-line")),
      menuItem("Data Table", tabName = "table", icon = icon("table"))
    )
  ),
  dashboardBody(
    tabItems(
      # Vehicle Explorer
      tabItem(tabName = "explore",
              fluidRow(
                box(width = 3, title = "Vehicle Selector", status = "primary", solidHeader = TRUE,
                    selectInput("year", "Select Year:", choices = years, selected = max(years)),
                    uiOutput("make_ui"),
                    uiOutput("model_ui"),
                    actionButton("fetch", "Fetch Live Data ", icon = icon("bolt")),
                    hr(),
                    h4("Vehicle Summary"),
                    textOutput("selected_vehicle")
                ),
                valueBoxOutput("city_mpg_box", width = 3),
                valueBoxOutput("highway_mpg_box", width = 3),
                valueBoxOutput("avg_mpg_box", width = 3),
                valueBoxOutput("percentile_big_box", width = 9),
                # Second line: fuel type
                fluidRow(valueBoxOutput("fueltype_box", width = 3))
              ),
              
      ),
      
      # ----------# Analysis tab (all four + trend)
      tabItem(tabName = "analysis",
              fluidRow(
                box(width = 12, title = "RQ1: Real MPG Distribution",
                    status = "primary", solidHeader = TRUE, plotOutput("vis_rq1")),
                box(width = 12, title = "RQ2: Emission vs. MPG",
                    status = "success", solidHeader = TRUE, plotOutput("vis_rq2"))
              ),
              fluidRow(
                box(width = 6, title = "MPG Distribution (???200)",
                    status = "info", solidHeader = TRUE, plotOutput("vis_mpg_hist")),
                box(width = 6, title = "Emission Score Distribution",
                    status = "warning", solidHeader = TRUE, plotOutput("vis_emission_hist"))
              ),
              fluidRow(
                box(width = 12, title = "5-Year Average MPG Trend",
                    status = "success", solidHeader = TRUE, plotlyOutput("vis_trend_decade"))
              ),
              
              
      ),
      
      # ----------# Data Table
      tabItem(tabName = "table",
              box(width = 12, title = "Filter and Explore Vehicles",
                  status = "warning", solidHeader = TRUE, DTOutput("table_output")))
    )
  )
)

# ----------
# RUN APP

shinyApp(ui = ui, server = server)