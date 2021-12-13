daux <- dyrpc %>% 
  filter(year == max(year)) %>% 
  select(-year, -reporter_iso)

# main communities via sum(export_value_usd)
# then regroup the new communities in the yr data
daux1 <- daux %>% 
  group_by(community_name) %>% 
  summarise(export_value_usd = sum(export_value_usd)) %>% 
  ungroup() %>% 
  arrange(desc(export_value_usd)) %>% 
  mutate(
    aux = cumsum(export_value_usd)/sum(export_value_usd),
    community_name2 = ifelse(aux <= input$percent, community_name, "Others")
  ) %>% 
  select(community_name, community_name2) %>% 
  left_join(colors, by = c("community_name2" = "community_name"))

daux2 <- daux %>% 
  mutate(total_trade_usd = export_value_usd + import_value_usd) %>% 
  group_by(partner_iso) %>% 
  summarise(total_trade_usd = sum(total_trade_usd)) %>% 
  arrange(desc(total_trade_usd)) %>% 
  mutate(
    aux = cumsum(total_trade_usd)/sum(total_trade_usd),
    # partner_iso2 = ifelse(aux <= input$percent, partner_iso, "rest of the World")
    partner_iso2 = ifelse(row_number() <= 5, partner_iso, "rest of the World")
  )

cols <- daux1 %>%
  filter(row_number() <= min(which(community_name2 == "Others"))) %>% 
  pull(community_color)

prts <- daux2 %>%
  filter(row_number() <= min(which(partner_iso2 == "rest of the World"))) %>% 
  pull(partner_iso2)

daux <- daux %>% 
  left_join(daux1, by = "community_name") %>% 
  mutate(partner_iso2 = ifelse(partner_iso %in% prts, partner_iso, "rest of the World")) %>% 
  group_by(community_name2, partner_iso2) %>% 
  summarise(
    export_value_usd = sum(export_value_usd),
    import_value_usd = sum(import_value_usd)
  ) %>% 
  ungroup() %>% 
  mutate(
    community_name2 = factor(community_name2, levels = unique(pull(daux1, community_name2))),
    xpos = as.numeric(factor(partner_iso2, levels = prts)) - 1
  )


highchart() %>% 
  hc_yAxis_multiples(
    create_yaxis(naxis = 2, lineWidth = 2, title = list(text = NULL), turnopposite = FALSE, sep = 0.05)
  ) %>% 
  hc_add_series(
    data = daux,
    type = "column",
    hcaes(xpos, export_value_usd, group = community_name2), color = cols
  ) %>%
  hc_add_series(
    data = daux,
    type = "column",
    hcaes(xpos, import_value_usd, group = community_name2), color = cols,
    linkedTo = 0:(length(cols)-1),
    yAxis = 1
  ) %>%
  hc_tooltip(shared = TRUE, table = TRUE) %>% 
  hc_xAxis(categories = prts, min = 0) %>% 
  hc_plotOptions(
    series = list(
      stacking = "normal",
      borderWidth  = 0
    )
  )
