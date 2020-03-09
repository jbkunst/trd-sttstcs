# input <- list(country = "Chile", percent = 0.8, since_year = 1990)

shinyServer(function(input, output) {

  dyr <- reactive({
    
    print(input$country)
    
    dyr <- countries %>% 
      filter(country_name_english == input$country) %>% 
      select(reporter_iso = country_iso) %>% 
      inner_join(data_yr, by = "reporter_iso") %>% 
      filter(year >= input$since_year)
    
    dyr
    
  })
  
  dyrpc <- reactive({
    
    print(input$country)
    
    dyrpc <- countries %>% 
      filter(country_name_english == input$country) %>% 
      select(reporter_iso = country_iso) %>% 
      inner_join(data_yrpc, by = "reporter_iso") %>% 
      filter(year >= input$since_year)
    
    dyrpc
    
  })
  
  output$stream <- renderHighchart({
    
    daux <- dyrpc()
    
    # main communities via sum(export_value_usd)
    # then regroup the new communities in the yr data
    # 
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
    
    cols <- daux1 %>%
      filter(row_number() <= min(which(community_name2 == "Others"))) %>% 
      pull(community_color)
    
    daux <- daux %>% 
      left_join(daux1, by = "community_name") %>% 
      group_by(year, community_name2) %>% 
      summarise(export_value_usd = sum(export_value_usd)) %>% 
      ungroup() %>% 
      mutate(community_name2 = factor(community_name2, levels = unique(pull(daux1, community_name2))))
        
    hchart(daux, "streamgraph", hcaes(year, export_value_usd, group = community_name2)) %>% 
      hc_colors(cols) %>% 
      hc_yAxis(visible = FALSE) %>% 
      hc_tooltip(table = TRUE, sort = TRUE) %>% 
      hc_plotOptions(series = list(animation = list(duration = 5000))) %>% 
      hc_size(height = "100vh")
    
  })
  
  output$vb_export <- renderValueBox({
    
    dyr <- dyr()
    
    d <- dyr %>% 
      select(year, export_value_usd) %>% 
      mutate(export_value_usd = round(export_value_usd/1e9, 2)) %>% 
      select(x = year, y = export_value_usd)
    
    lbl <- d %>% pull(y) %>% last() %>% comma() %>% paste0("USD $", .," B") 
    
    hc <- hc_spark(d, color = "white", prefix = "USD $ ", suffix = " B", type = "area")
    
    valueBoxSpark(
      value = lbl,
      subtitle = "Exports",
      color = "black",
      spark = hc,
      minititle = "Exports value in 2018"
    )
    
  })
  
  output$vb_import <- renderValueBox({
    
    dyr <- dyr()
    
    d <- dyr %>% 
      select(year, import_value_usd) %>% 
      mutate(import_value_usd = round(import_value_usd/1e9, 2)) %>% 
      select(x = year, y = import_value_usd)
    
    lbl <- d %>% pull(y) %>% last() %>% comma() %>% paste0("USD $", .," B") 
    
    hc <- hc_spark(d, color = "white", prefix = "USD $ ", suffix = " B", type = "area")
    
    valueBoxSpark(
      value = lbl,
      subtitle = "Imports",
      color = "black",
      spark = hc,
      minititle = "Import value in 2018"
    )
    
  })
  
  output$vb_pci <- renderValueBox({
    
    dyr <- dyr()
    
    d <- dyr %>% 
      select(year, complexity_index_country) %>% 
      mutate(complexity_index_country = round(complexity_index_country, 2)) %>%
      select(x = year, y = complexity_index_country)
    
    lbl <- d %>% pull(y) %>% last() %>% round(2)
    
    hc <- hc_spark(d, color = "white", prefix = "CI ", suffix = "", type = "area")
    
    valueBoxSpark(
      value = lbl,
      color = "black",
      subtitle = "Complexity Index",
      spark = hc,
      minititle = "CCI in 2018"
    )
    
  })
  
})
