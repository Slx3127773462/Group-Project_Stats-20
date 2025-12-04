# ======================================================
# 💸 Emissions Game (Updated + Warning-Free)
# ======================================================

library(shiny)
library(httr)
library(jsonlite)
library(dplyr)
library(tidyverse)
library(xml2)
library(DT)
library(shinythemes)
library(shinydashboard)

# -----------------------------
# Load data
# -----------------------------
mpg <- read.csv("mpg.csv")
emissions <- read.csv("emissions.csv")
mpg       <- mpg[sample(1:nrow(mpg), 1000), ]
emissions <- emissions[sample(1:nrow(emissions), 1000), ]
vehicle_mpg <- as.numeric(mpg$MPG)

# -----------------------------
# Get vehicle id function
# -----------------------------
get_vehicle_id <- function(year, make, model){
  year <- as.integer(year)
  if(!is.integer(year)){
    stop("'year' must be integer.")
  }
  if(!is.character(make) || !is.character(model)){
    stop("'make' and 'model' must be character.")
  }
  base_url <- "https://www.fueleconomy.gov/ws/rest/"
  info_url <- paste0("vehicle/menu/options?",
                     "year=",year,
                     "&make=",make,
                     "&model=",model)
  full_url <- paste0(base_url, info_url)
  api_id <- httr::GET(full_url)
  api_char_id <- base::rawToChar(api_id$content)
  if(api_id$status_code != 200){
    stop("Request failed, please check your input : year, make, and model.")
  } else {
    api_id <- jsonlite::fromJSON(api_char_id, flatten = TRUE)
    api_id <- as.data.frame(api_id)
    colnames(api_id) <- c("Configuration", "Vehicle ID")
    return(api_id)
  }  
}

# -----------------------------
# get_mpg function
# -----------------------------
get_mpg <- function(year, make, model){
  id <- get_vehicle_id(year, make, model)
  id_list <- id$`Vehicle ID`
  if(length(id_list) == 0){
    stop("No Vehicle ID found.")
  }
  mpg_list <- list()
  for(i in seq_along(id_list)){
    vid <- id_list[i]
    base_url <- "https://www.fueleconomy.gov/ws/rest/"
    info_url <- paste0("ympg/shared/ympgDriverVehicle/", vid)
    full_url <- paste0(base_url, info_url)
    res <- httr::GET(full_url)
    if(res$status_code != 200){
      next
    }
    api_char_mpg <- rawToChar(res$content)
    api_mpg <- jsonlite::fromJSON(api_char_mpg, flatten = TRUE)
    api_mpg <- as.data.frame(api_mpg)
    mpg_list[[i]] <- api_mpg
  }
  final_mpg <- do.call(rbind, mpg_list)
  colnames(final_mpg) <- c("City Percent", "Highway Percent","Last Recorded Day","MPG","State","Vehicle ID")
  return(final_mpg)
}

# -----------------------------
# Get emission function
# -----------------------------
get_emission <- function(year, make, model){
  id <- get_vehicle_id(year, make, model)
  id_list <- id$`Vehicle ID`
  if(length(id_list) == 0){
    stop("No Vehicle ID found.")
  }
  emi_list <- list()
  for(i in seq_along(id_list)){
    vid <- id_list[i]
    base_url <- "https://www.fueleconomy.gov/ws/rest/"
    info_url <- paste0("vehicle/emissions/", vid)
    full_url <- paste0(base_url, info_url)
    res <- httr::GET(full_url)
    if(res$status_code != 200){
      next
    }
    api_char_emi <- rawToChar(res$content)
    api_emi <- jsonlite::fromJSON(api_char_emi, flatten = TRUE)
    api_emi <- as.data.frame(api_emi)
    emi_list[[i]] <- api_emi
  }
  final_emi <- do.call(rbind, emi_list)
  final_emi <- final_emi[final_emi$emissionsInfo.salesArea == "7", ]
  return(final_emi)
}

# ======================================================
# SERVER
# ======================================================
server <- function(input, output, session) {
  
  # single vehicle MPG reactive
  mpg_single <- reactive({
    req(input$year, input$make, input$model)
    as.numeric(get_mpg(
      year = as.integer(input$year),
      make = trimws(input$make),
      model = trimws(input$model)
    )$MPG[1])
  })
  
  # -----------------------------
  # Home tab: Vehicle summary
  # -----------------------------
  output$selected_vehicle <- renderText({
    paste(input$year, input$make, input$model)
  })
  
  output$selected_fuel <- renderText({
    "Gasoline / Hybrid / Electric"
  })
  
  output$vehicle_score <- renderText({
    mpg_val  <- mpg_single()
    mean_val <- mean(vehicle_mpg, na.rm = TRUE)
    # normalize to 0–100 scale around average MPG
    score <- round(pmin(100, pmax(0, 50 + (mpg_val - mean_val) * 2)), 1)
    paste0(score)
  })
  
  # -----------------------------
  # Plots
  # -----------------------------
  output$mpgPlot <- renderPlot({
    ggplot() +
      geom_histogram(
        aes(x = vehicle_mpg, y = after_stat(density)),
        bins = 45, fill = "lightblue", alpha = 0.6, color = "white"
      ) +
      geom_density(aes(x = vehicle_mpg), size = 1, color = "orange") +
      geom_vline(
        xintercept = mean(vehicle_mpg, na.rm = TRUE),
        linetype = "dashed", color = "grey", size = 1
      ) +
      geom_vline(xintercept = mpg_single(), color = "red", size = 2) +
      coord_cartesian(xlim = c(0, 100)) +
      labs(
        title = "RQ1: Real MPG Distribution",
        subtitle = "Gasoline vehicle vs Electronic vehicle",
        x = "Actual MPG",
        y = "Density",
        caption = "Source: fueleconomy.gov"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 8, face = "bold", color = "darkblue"),
        plot.subtitle = element_text(size = 8),
        panel.grid.minor = element_blank()
      )
  })
  
  output$emissionPlot <- renderPlot({
    vehicle_score <- as.numeric(emissions$score)
    tibble(mpg = vehicle_mpg, score = vehicle_score) %>%
      ggplot(aes(x = score, y = mpg)) +
      geom_point(color = "lightblue", alpha = 0.7, size = 2.5) +
      geom_smooth(method = "lm", color = "red", linewidth = 1, 
                  fill = "pink", alpha = 0.3) +
      coord_cartesian(ylim = c(0, 200)) +
      geom_rug(alpha = 0.3, color = "grey") + 
      labs(
        title = "RQ2: Is Higher EPA Emission Score = Much Better Real-World MPG ?",
        x = "EPA Emission Score (0–10, higher = cleaner)",
        y = "Fuel Economy",
        caption = "Linear regression"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(size = 18, face = "bold", color = "darkgreen"),
        plot.subtitle = element_text(size = 13, color = "darkgreen")
      )
  })
  
  output$mpgDistribution <- renderPlot({
    ggplot(tibble(mpg = vehicle_mpg)) +
      geom_histogram(
        aes(x = pmin(mpg, 200), y = after_stat(density)),
        bins = 60, fill = "lightblue", color = "white", alpha = 0.9
      ) +
      geom_density(aes(x = pmin(mpg, 200)), color = "red", linewidth = 1.3) +
      coord_cartesian(xlim = c(0, 200)) +
      theme_minimal(base_size = 15) +
      theme(panel.grid.minor = element_blank())
  })
  
  output$scoreDistribution <- renderPlot({
    vehicle_score <- as.numeric(emissions$score)
    ggplot(tibble(score = vehicle_score)) +
      geom_histogram(aes(x = score), bins = 11, fill = "lightblue", color = "white") +
      scale_x_continuous(breaks = 0:10) +
      theme_minimal(base_size = 15) +
      theme(panel.grid.minor = element_blank())
  })
  
  output$result <- renderText({
    mpg_val  <- mpg_single()
    mean_val <- mean(vehicle_mpg, na.rm = TRUE)
    paste0(
      "Selected vehicle MPG = ", round(mpg_val, 1), "\n",
      "Overall average MPG = ", round(mean_val, 1), "\n",
      ifelse(
        mpg_val > mean_val,
        "The vehicle has above average fuel efficiency.",
        "The vehicle has below average fuel efficiency."
      )
    )
  })
}

# ======================================================
# UI (shinydashboard)
# ======================================================
ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(title = "💸 Emissions Game"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("🏠 Home", tabName = "home", icon = icon("car")),
      menuItem("📊 Analysis", tabName = "analysis", icon = icon("chart-line")),
      menuItem("ℹ️ About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # ---------------------------
      # Home Tab
      # ---------------------------
      tabItem(tabName = "home",
              fluidRow(
                box(width = 4, status = "primary", solidHeader = TRUE,
                    title = HTML("<h3>Input Vehicle Information</h3>"),
                    sliderInput("year", "Model Year:",
                                min = 2014, max = 2026, value = 2026, step = 1),
                    textInput("make", "Make", "Honda"),
                    textInput("model", "Model", "Fit"),
                    actionButton("fetch", "Fetch Data 🚀", icon = icon("bolt")),
                    br(), br(),
                    helpText("Enter a vehicle and compute its Eco-Score (0–100).")
                ),
                
                box(width = 8, status = "success", solidHeader = TRUE,
                    title = HTML("<h3>🚗 Vehicle Eco-Score Summary</h3>"),
                    fluidRow(
                      column(6,
                             tags$h4("Selected Vehicle:"),
                             textOutput("selected_vehicle"),
                             br(),
                             tags$h4("Fuel Type:"),
                             textOutput("selected_fuel")
                      ),
                      column(6, align = "center",
                             tags$h4("Overall Eco-Score (out of 100):"),
                             tags$style(".scoreText{font-size:50px;font-weight:bold;color:#1E90FF;}"),
                             div(class = "scoreText", textOutput("vehicle_score"))
                      )
                    ),
                    br(),
                    tags$hr(),
                    p("Eco-Score is calculated based on MPG, CO₂ emissions, and fuel type.")
                )
              )
      ),
      
      # ---------------------------
      # Analysis Tab
      # ---------------------------
      tabItem(tabName = "analysis",
              fluidRow(
                box(width = 6, status = "info", solidHeader = TRUE,
                    title = "MPG Distribution",
                    plotOutput("mpgDistribution", height = "300px")),
                box(width = 6, status = "warning", solidHeader = TRUE,
                    title = "Eco-Score Distribution",
                    plotOutput("scoreDistribution", height = "300px"))
              ),
              fluidRow(
                box(width = 6, status = "primary", solidHeader = TRUE,
                    title = "Average MPG by Vehicle",
                    plotOutput("mpgPlot", height = "300px")),
                box(width = 6, status = "danger", solidHeader = TRUE,
                    title = "CO₂ Emissions by Vehicle",
                    plotOutput("emissionPlot", height = "300px"))
              )
      ),
      
      # ---------------------------
      # About Tab
      # ---------------------------
      tabItem(tabName = "about",
              box(width = 12, status = "primary", solidHeader = TRUE,
                  h3("About This Project"),
                  p("The Emissions Game evaluates vehicle efficiency and emissions to assign an Eco-Score (0–100)."),
                  p("This dashboard allows you to explore MPG, CO₂ data, and compare performance across models."),
                  tags$hr(),
                  p("Created by Astrid Shao using R Shiny & shinydashboard.")
              )
      )
    )
  )
)

# ======================================================
# Run App
# ======================================================
shinyApp(ui = ui, server = server)
