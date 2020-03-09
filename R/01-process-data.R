library(tidyverse)
library(economiccomplexity)

data_yrpc <- dir("data/country/", full.names = TRUE) %>%
  map_df(readRDS)

# data_yrpc <- data_yrpc %>%
#   mutate_if(is.numeric, replace_na, 0)
data_yrpc %>% 
  filter(is.na(community_name))

data_yrpc <- data_yrpc %>% 
  filter(!is.na(community_name))

data_yrpc %>% 
  filter(partner_iso == "all")

data_yrpc %>% 
  filter(export_value_usd + import_value_usd == 0)

data_yr <- data_yrpc %>%
  distinct(year) %>%
  pull() %>%
  map_df(function(y = 2000){

    message(y)

    dux <- data_yrpc %>%
      filter(year == y) %>% 
      group_by(year, reporter_iso, community_name) %>%
      summarise(
        export_value_usd = sum(export_value_usd, na.rm = TRUE),
        import_value_usd = sum(import_value_usd, na.rm = TRUE)
        ) %>%
      filter(TRUE)

    rca <- balassa_index(dux, country = "reporter_iso", product = "community_name", value = "export_value_usd", discrete = TRUE)

    com <- complexity_measures(rca)

    dot <- tibble(
      reporter_iso = names(com$complexity_index_country),
      complexity_index_country = com$complexity_index_country
      )
    
    dot

    dux %>%
      group_by(year, reporter_iso) %>%
      summarise_at(vars(export_value_usd, import_value_usd), sum) %>%
      ungroup() %>% 
      left_join(dot, by = "reporter_iso") %>% 
      mutate_if(is.numeric, replace_na, 0)

  })


# data_yrpc %>% 
#   group_split(reporter_iso)
# 
# data_yr %>% 
#   group_split(reporter_iso)

saveRDS(data_yrpc, "data/yrpc.rds", compress = "xz")
saveRDS(data_yr, "data/yr.rds", compress = "xz")
