# input <- list(country = "Chile", percent = 0.8)

shinyServer(function(input, output) {

  dyr <- reactive({
    
    print(input$country)
    
    dyr <- countries %>% 
      filter(country_name_english == input$country) %>% 
      select(reporter_iso = country_iso) %>% 
      inner_join(data_yr, by = "reporter_iso")
    
    dyr
    
  })
  
  dyrpc <- reactive({
    
    print(input$country)
    
    dyrpc <- countries %>% 
      filter(country_name_english == input$country) %>% 
      select(reporter_iso = country_iso) %>% 
      inner_join(data_yrpc, by = "reporter_iso")
    
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
      hc_plotOptions(series = list(animation = list(duration = 5000)))
    
  })
  
  
  
})
