library(tidyverse)
library(colorfindr)

# community colors with Others --------------------------------------------
communities <- tradestatistics::ots_communities %>%
  distinct(community_name, community_color) %>%
  add_row(community_name = "Others", community_color = "#d3d3d3", .before = 1)


# countries data ----------------------------------------------------------
data_yr   <- readRDS("data/yr.rds")

countries <- tradestatistics::ots_countries %>% 
  mutate(iso2 = countrycode::countrycode(country_iso, origin = "iso3c", destination = "iso2c")) %>% 
  filter(!is.na(iso2)) %>% 
  semi_join(data_yr, by = c("country_iso" = "reporter_iso"))

countries_cols <- countries %>% 
  pull(iso2) %>% 
  map_df(function(x = "US"){
    
    message(x)
    
    url <- stringr::str_glue("https://www.countryflags.io/{ x }/flat/24.png")
    
    cols <- colorfindr::get_colors(url)
    
    colorfindr::plot_colors(cols, sort = "size")
    
    cols <- cols %>% 
      filter(col_hex != "#FFFFFF")
    
    col <- cols %>% 
      pull(col_hex) %>% 
      first()
    
    data_frame(
      iso2 = x,
      url_countryflags = url,
      main_color_flag = col
    )
    
  })

countries <- countries %>% 
  left_join(countries_cols, by = "iso2")


glimpse(countries)



# export ------------------------------------------------------------------
saveRDS(communities, "data/communities.rds")
saveRDS(countries, "data/countries.rds")

