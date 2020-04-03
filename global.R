# packages ----------------------------------------------------------------
# library(tradestatistics)
library(purrr)
library(shiny)
library(shinydashboard)
library(highcharter)
library(dplyr)
library(shinyWidgets)
library(scales)

source("R/99-shiny-helpers.R")

PARS <- list(
  debug = FALSE,
  classcol = "col-lg-offset-1 col-lg-10 col-md-offset-0 col-md-12 col-sm-offset-0 col-sm-12",
  sparkline_color = "#333333",
  font = '-apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Helvetica, Arial, sans-serif, "Apple Color Emoji", "Segoe UI Emoji", "Segoe UI Symbol"'
)

options(
  highcharter.google_fonts = FALSE,
  highcharter.debug = PARS$debug,
  shiny.launch.browser = PARS$debug,
  highcharter.theme = 
    hc_theme_smpl(
      title = list(style = list(fontSize = "1.2em", fontFamily = PARS$font)),
      subtitle = list(style = list(fontFamily = PARS$font, fontSize = "0.95em")),
      chart = list(
        backgroundColor = "transparent",
        style = list(fontFamily = PARS$font, fontSize = "1.0em")
      ),
      plotOptions = list(
        series = list(
          dataLabels = list(color = "#222d32", style = list(fontWeight = "normal", textShadow = FALSE, textOutline = FALSE)),
          animation = list(duration = 3000)
        )
      ),
      legend = list(
        itemStyle =  list(
          fontWeight = "normal"
        )
      )
    )
)

dropdownButtonp <- purrr::partial(
  dropdownButton,
  status = "customstatus",
  size = "sm",
  right = TRUE,
  status = "info",
  width = "400px",
  inline = TRUE,
)


# data --------------------------------------------------------------------
if(PARS$debug) {
  data_yrpc <- readRDS("data/yrpc_chl.rds")
} else {
  message("reading long data")
  data_yrpc <- readRDS("data/yrpc.rds")
  message("long data ready!")
} 

data_yr   <- readRDS("data/yr.rds")
data_yrc  <- readRDS("data/yrc2018.rds")

communities <- readRDS("data/communities.rds")
countries   <- readRDS("data/countries.rds")

mapwrdl <- hcmap(showInLegend = FALSE, nullColor = "#f3f6f9", borderColor = "#edf1f6") %>%
  hc_size(height = "90vh") %>%
  hc_elementId("worldmap") %>% 
  hc_credits(enabled = FALSE)

dfsrt <- data_yr %>%
  filter(year == max(year)) %>% 
  arrange(desc(export_value_usd)) %>% 
  select(reporter_iso, export_value_usd) %>% 
  mutate(
    aux = export_value_usd/1e6/sum(export_value_usd/1e6),
    auxcum = cumsum(aux),
  ) %>% 
  left_join(countries %>% select(country_name_english, reporter_iso = country_iso), by = "reporter_iso") %>% 
  filter(auxcum <= .95) %>% 
  mutate(p = exp(exp((1 + aux) * 1.75))) %>% 
  sample_frac(1, weight = p) %>% 
  filter(complete.cases(.)) %>% 
  mutate(cntnt = iso3_to_flag_n_name(reporter_iso))


