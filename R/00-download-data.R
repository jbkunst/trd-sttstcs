library(tradestatistics)
library(tidyverse)
library(furrr)
library(economiccomplexity)

plan(multiprocess(workers = 6))

tradestatistics::ots_countries$country_iso %>% 
  str_subset("-", negate = TRUE) %>% 
  setdiff(c("all", "arb", "tmp")) %>% 
  map(function(ciso = "arb"){
    
    message(ciso)
    
    fout <- paste0("data/country/", ciso, ".rds")
    
    if(file.exists(fout)) return(TRUE)
    
    d <- tradestatistics::ots_create_tidy_data(years = 1962:2018, reporters = ciso, table = "yrc", include_communities = TRUE)  
    
    d <- d %>% 
      group_by(year, community_name, reporter_iso) %>% 
      summarise_at(vars(export_value_usd, import_value_usd), sum) %>% 
      ungroup()
    
    d
    
    saveRDS(d, fout, compress = "xz")
    
  })

data_yrc <- dir("data/country/", full.names = TRUE) %>% 
  map_df(readRDS)

data_yrc <- data_yrc %>% 
  mutate_if(is.numeric, replace_na, 0)

data_yr <- data_yrc %>% 
  distinct(year) %>% 
  pull() %>% 
  map_df(function(y = 2000){
    
    message(y)
    
    dux <- data_yrc %>% 
      filter(year == y)
    
    rca <- balassa_index(dux, country = "reporter_iso", product = "community_name", value = "export_value_usd", discrete = TRUE)
    
    com <- complexity_measures(rca)
    
    dot <- tibble(
      reporter_iso = names(com$complexity_index_country),
      complexity_index_country = com$complexity_index_country
      )
    
    dux %>% 
      group_by(reporter_iso) %>% 
      summarise_at(vars(export_value_usd, import_value_usd), sum) %>% 
      mutate(year = y) %>% 
      left_join(dot, by = "reporter_iso") %>% 
      select(year, everything())
    
  })


data_yrc
data_yr
