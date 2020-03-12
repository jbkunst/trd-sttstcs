# packages ----------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(highcharter)
library(tradestatistics)
library(purrr)
library(dplyr)
library(countrycode)
library(shinyWidgets)
library(scales)

source("R/99-shiny-helpers.R")

# data --------------------------------------------------------------------
data_yrpc <- readRDS("data/yrpc.rds")
data_yr   <- readRDS("data/yr.rds")

colors <- tradestatistics::ots_communities %>% 
  distinct(community_name, community_color) %>% 
  add_row(community_name = "Others", community_color = "#d3d3d3")

countries <- tradestatistics::ots_countries %>% 
  mutate(iso2 = countrycode(country_iso, origin = "iso3c", destination = "iso2c")) %>% 
  filter(!is.na(iso2)) %>% 
  semi_join(data_yr, by = c("country_iso" = "reporter_iso"))
  
mapwrdl <- hcmap(showInLegend = FALSE) %>%
  hc_size(height = "90vh") %>%
  hc_elementId("worldmap")

cntnt <- countries %>% 
  select(country = country_name_english, iso2) %>% 
  pmap_chr(function(country = "Australia", iso2 = "au") {
    
    urlflag <- paste0("https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/", tolower(iso2), ".svg")

    as.character(HTML(paste(tags$img(src = urlflag, width=20, height=15), country)))
    
  })

PARS <- list(
  sparkline_color = "lightgray"
)

