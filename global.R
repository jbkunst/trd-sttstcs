library(shiny)
library(shinydashboard)
library(highcharter)

mapwrdl <- hcmap(showInLegend = FALSE) %>%
  hc_size(height = "90vh") %>%
  hc_elementId("worldmap")
