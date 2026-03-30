# Fuel Efficiency Lab — Interactive Shiny Dashboard

An interactive R Shiny dashboard for exploring vehicle fuel efficiency 
and emissions data across 40,000+ vehicles using official EPA data.

Live App: https://shaoisdeveloper.shinyapps.io/Fuel_Efficiency_Lab/

---

## Project Overview

This app allows users to explore and compare vehicle fuel efficiency 
and emission trends through an interactive dashboard. Built as a team 
class project at UCLA (Fall 2025).

**Research Questions:**
- How is real-world MPG distributed across vehicle types?
- What is the relationship between emission scores and fuel efficiency?
- How has average fuel efficiency trended over time?

---

## Features

- **Vehicle Explorer** — Select any year/make/model to view city, 
  highway, and combined MPG alongside a fuel efficiency percentile 
  ranking vs. all vehicles
- **Live API Integration** — Fetches real-time MPG data from the 
  official EPA Fuel Economy API, with automatic fallback to local CSV 
  if the API is unavailable
- **Analysis Tab** — Visualizes MPG distributions, emission score vs. 
  MPG relationships, and 5-year average MPG trends using ggplot2 and Plotly
- **Data Table** — Searchable and filterable table of all vehicle records

---

## Tech Stack

| Tool | Usage |
|------|-------|
| R + Shiny | App framework |
| shinydashboard | UI layout |
| ggplot2 + Plotly | Visualizations |
| httr + xml2 | EPA API calls |
| tidyverse | Data cleaning |
| DT | Interactive data table |

---

## Data Sources

- EPA Fuel Economy API — https://www.fueleconomy.gov/feg/ws/
- Local CSV backup — vehicles.csv and emissions.csv (EPA open data)

---

## How to Run Locally

install.packages(c("shiny", "shinydashboard", "httr", "jsonlite",
                   "tidyverse", "xml2", "DT", "plotly", "shinythemes"))

shiny::runApp("app.R")

---

## Team

Developed by a 5-member UCLA Statistics team.
My contributions: UI design, EPA API integration, 
and local CSV fallback strategy.

---

## License

For educational purposes only.
