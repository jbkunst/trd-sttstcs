library(tradestatistics)
library(tidyverse)
library(furrr)
library(economiccomplexity)

plan(multiprocess(workers = 6))

tradestatistics::ots_countries$country_iso %>% 
  str_subset("-", negate = TRUE) %>% 
  setdiff("all") %>% 
  future_map(function(ciso = "chl"){
    
    fout <- paste0("data/country/", ciso, ".rds")
    
    if(file.exists(fout)) return(TRUE)
    
    d <- tradestatistics::ots_create_tidy_data(years = 1962:2018, reporters = ciso, table = "yrc", include_communities = TRUE)  
    
    d <- d %>% 
      group_by(year, community_name, reporter_iso) %>% 
      summarise_at(vars(export_value_usd, import_value_usd), sum) %>% 
      ungroup()
    
    d
    
    saveRDS(d, fout, compress = "xz")
    
  }, .progress = TRUE)

data <- dir("data/country/", full.names = TRUE) %>% 
  map_df(readRDS)

data_2000 <- data %>% 
  filter(year == 2000) %>% 
  mutate(export_value_usd = replace_na(export_value_usd, 0))

rca_2000 <- balassa_index(data_2000, country = "reporter_iso", product = "community_name", value = "export_value_usd", discrete = TRUE)

com_2000 <- complexity_measures(rca_2000)

com_2000


data_2000

data %>% 
  group_by(year) %>% 
  economiccomplexity::complexity_measures()

data %>% 
  group_by(year, reporter_iso) %>% 
  summarise_at(vars(export_value_usd, import_value_usd), sum, na.rm = TRUE) %>% 
  ungroup()


economiccomplexity::
