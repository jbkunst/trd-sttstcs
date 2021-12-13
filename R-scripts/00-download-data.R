library(tradestatistics)
library(tidyverse)
library(furrr)

plan(multiprocess(workers = 3))

tradestatistics::ots_countries$country_iso %>% 
  str_subset("-", negate = TRUE) %>% 
  setdiff(c("all", "arb", "tmp")) %>% 
  sample() %>% 
  walk(function(ciso = "arb"){
  # future_map_dfr(function(ciso = "arb"){
    
    message(ciso)
    
    fout <- paste0("data/country/", ciso, ".rds")
    
    if(file.exists(fout)) return(TRUE)
    
    d <- tradestatistics::ots_create_tidy_data(years = 1962:2018, reporters = ciso, partners = "all", table = "yrpc", include_communities = TRUE)
    
    d <- d %>% 
      group_by(year, community_name, reporter_iso, partner_iso) %>% 
      summarise_at(vars(export_value_usd, import_value_usd), sum, na.rm = TRUE) %>% 
      ungroup()
    
    d
    
    saveRDS(d, fout, compress = "xz")
    
  })


# data for treemaps -------------------------------------------------------
dyrc2018 <- ots_create_tidy_data(
  years = 2018,
  reporters = "all",
  partners = "all",
  include_shortnames = TRUE,
  include_communities = TRUE,
  table = "yrc"
)

glimpse(dyrc2018)

dyrc2018 <- dyrc2018 %>% 
  select(
    community_name, community_color,
    product_shortname_english,
    reporter_iso, reporter_fullname_english,
    export_value_usd, import_value_usd) 

distinct(dyrc2018)

saveRDS(dyrc2018, "data/yrc2018.rds")




