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
    
    subtitle <- daux %>% 
      filter(year == max(year)) %>% 
      mutate(perc = percent(export_value_usd/sum(export_value_usd))) %>% 
      arrange(desc(export_value_usd)) %>% 
      filter(community_name2 != "Others", community_name2 != "Unspecified") %>% 
      head(3) %>% 
      left_join(colors, by = c("community_name2" = "community_name")) %>% 
      mutate(
        sep = case_when(
          row_number() == 1 ~ "",
          row_number() == dplyr::n() ~ " and ",
          TRUE ~ ", "
        ),
        txt = stringr::str_glue("{sep}<strong><span style='color: {community_color}'>{community_name2}</span></strong> ({perc})")
      ) %>% 
      summarise(paste0(txt, collapse = "")) %>% 
      pull() %>% 
      stringr::str_c("Most exported products in 2018 are ", .)
    
        
    hchart(daux, "streamgraph", hcaes(year, export_value_usd, group = community_name2)) %>% 
      hc_colors(cols) %>% 
      hc_title(text = "Most Exported products by year") %>% 
      hc_subtitle(text = subtitle, useHTML = TRUE) %>% 
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
    
    hc <- hchart(d, "area", color = PARS$sparkline_color) %>% 
      hc_add_theme(hc_theme_sparkline2()) %>% 
      hc_tooltip(valuePrefix = "USD $ ", valueSuffix = " B") %>% 
      hc_plotOptions(
        series = list(
          color = PARS$sparkline_color,
          fillColor = list(
            linearGradient = list(x1 = 0, y1 = 1, x2 = 0, y2 = 0),
            stops = list(
              list(0.0, "transparent"),
              list(1.0, PARS$sparkline_color)
            )
          )
        )
      )
    
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
    
    hc <- hchart(d, "area", color = PARS$sparkline_color) %>% 
      hc_add_theme(hc_theme_sparkline2()) %>% 
      hc_tooltip(valuePrefix = "USD $ ", valueSuffix = " B") %>% 
      hc_plotOptions(
        series = list(
          color = PARS$sparkline_color,
          fillColor = list(
            linearGradient = list(x1 = 0, y1 = 1, x2 = 0, y2 = 0),
            stops = list(
              list(0.0, "transparent"),
              list(1.0, PARS$sparkline_color)
            )
          )
        )
      )
    
    valueBoxSpark(
      value = lbl,
      subtitle = "Imports",
      color = "black",
      spark = hc,
      minititle = "Import value in 2018"
    )
    
  })
  
  output$vb_expdiv <- renderValueBox({
    
    dyr <- dyr()
    
    d <- dyr %>% 
      select(year, export_value_usd_diversity) %>% 
      mutate(export_value_usd_diversity = round(export_value_usd_diversity, 3)) %>% 
      select(x = year, y = export_value_usd_diversity)
    
    lbl <- d %>% pull(y) %>% last() %>% round(3)
    
    hc <- hchart(d, "line", color = PARS$sparkline_color) %>% 
      hc_add_theme(hc_theme_sparkline2()) 
    
    valueBoxSpark(
      value = lbl,
      subtitle = "Export Diversity",
      color = "black",
      spark = hc,
      minititle = "Simpson's Diversity Index in 2018"
    )
    
  })
  
  output$vb_pcompx <- renderValueBox({
    
    dyr <- dyr()
    
    d <- dyr %>% 
      select(year, complexity_index_country) %>% 
      mutate(complexity_index_country = round(complexity_index_country, 2)) %>%
      select(x = year, y = complexity_index_country)
    
    lbl <- d %>% pull(y) %>% last() %>% round(2)
    
    hc <- hchart(d, "line", color = PARS$sparkline_color) %>% 
      hc_add_theme(hc_theme_sparkline2())
    
    valueBoxSpark(
      value = lbl,
      color = "black",
      subtitle = "Complexity",
      spark = hc,
      minititle = "Country Complity Index in 2018"
    )
    
  })
  
})
