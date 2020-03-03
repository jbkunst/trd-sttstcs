library(tradestatistics)
library(tidyverse)

country <- "chl"

system.time({
  d <- tradestatistics::ots_create_tidy_data(years = 1962:2018, reporters = country, table = "yrc", include_communities = TRUE)  
})


glimpse(d)

d <- ots_inflation_adjustment(d, reference_year = 2017)

dr <- d %>% 
  group_by(community_name, community_color) %>% 
  summarise(export_value_usd = sum(export_value_usd, na.rm = TRUE)) %>% 
  ungroup() %>% 
  arrange(desc(export_value_usd)) %>% 
  mutate(
    pc = cumsum(export_value_usd) / sum(export_value_usd),
    community_name2 = ifelse(pc <= 0.8, community_name, "Other"),
    community_color2 = ifelse(pc <= 0.8, community_color, "gray")
  ) 

ds <- d %>% 
  left_join(dr %>% select(community_name, community_name2, community_color2)) %>% 
  group_by(year, community_name2, community_color2) %>% 
  summarise(export_value_usd = sum(export_value_usd)) %>% 
  ungroup()

ds


library(highcharter)

ds %>% 
  filter(year >= 1980) %>% 
  hchart("streamgraph", hcaes(year, export_value_usd/10000000, group = community_name2)) %>% 
  hc_yAxis(visible = FALSE) %>% 
  hc_colors(ds %>% filter(year == min(year)) %>% pull(community_color2)) %>% 
  hc_tooltip(table = TRUE, sort = TRUE) %>% 
  hc_plotOptions(
    series = list(animation = list(duration = 5000)) 
  )

