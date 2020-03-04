library(shiny)
library(shinydashboard)
library(highcharter)
library(tradestatistics)
library(purrr)
library(dplyr)
library(countrycode)
library(shinyWidgets)

mapwrdl <- hcmap(showInLegend = FALSE) %>%
  hc_size(height = "90vh") %>%
  hc_elementId("worldmap")

countries <- tradestatistics::ots_countries %>% 
  mutate(
    iso2 = countrycode(country_iso, origin = "iso3c", destination = "iso2c")    
  ) %>% 
  filter(!is.na(iso2))

cntnt <- countries %>% 
  select(country = country_name_english, iso2) %>% 
  pmap_chr(function(country = "Australia", iso2 = "au") {
    
    urlflag <- paste0("https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/", tolower(iso2), ".svg")

    as.character(HTML(paste(tags$img(src = urlflag, width=20, height=15), country)))
    
  })

str(cntnt)
