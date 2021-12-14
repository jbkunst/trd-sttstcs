# input <- list(country = "Chile", percent = 80, since_year = 2000, since_to_year = c("2000", "2008"))

shinyServer(function(input, output, session) {
  
  # updatePickerInput(
  #   session,
  #   "country",
  #   selected = ifelse(PARS$debug, "Chile", first(pull(dfsrt, country_name_english))),
  #   choices = pull(dfsrt, country_name_english),
  #   choicesOpt = list(content =  pull(dfsrt, cntnt))  
  # )
  
  reac_iso <- reactive({
    
    validate(need(input$country != "", ""))
    
    reac_iso <- countries %>% 
      filter(country_name_english == input$country) %>% 
      select(reporter_iso = country_iso) %>% 
      pull(reporter_iso)
    
    reac_iso
    
    
  })
  
  dyr <- reactive({
    
    reac_iso <- reac_iso()
    
    dyr <- data_yr %>% 
      filter(reporter_iso == reac_iso) %>% 
      collect() %>% 
      filter(as.numeric(input$since_to_year[1]) <= year, year <=  as.numeric(input$since_to_year[2]))
    
    dyr
    
  })
  
  dyrpc <- reactive({
    
    reac_iso <- reac_iso()
    
    dyrpc <-  data_yrpc %>%
      filter(reporter_iso == reac_iso) %>% 
      collect() %>% 
      filter(as.numeric(input$since_to_year[1]) <= year, year <=  as.numeric(input$since_to_year[2]))
    
    dyrpc
    
  })
  
  drc <- reactive({
    
    reac_iso <- reac_iso()
    
    drc <- data_yrc %>%
      filter(reporter_iso == reac_iso) %>% 
      collect()
    
    drc
    
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
        community_name2 = ifelse(aux <= input$percent/100, community_name, "Others")
        ) %>% 
      select(community_name, community_name2) %>% 
      left_join(communities, by = c("community_name2" = "community_name"))
    
    cols <- daux1 %>%
      filter(row_number() <= min(which(community_name2 == "Others"))) %>% 
      pull(community_color)
    
    daux <- daux %>% 
      left_join(daux1, by = "community_name") %>% 
      group_by(year, community_name2) %>% 
      summarise(export_value_usd = sum(export_value_usd), .groups = "drop") %>% 
      mutate(community_name2 = factor(community_name2, levels = unique(pull(daux1, community_name2))))
    
    subtitle <- daux %>% 
      group_by(community_name2) %>% 
      summarise(export_value_usd = sum(export_value_usd/1e6)) %>% 
      mutate(perc = percent(export_value_usd/sum(export_value_usd))) %>% 
      arrange(desc(export_value_usd)) %>% 
      filter(community_name2 != "Others", community_name2 != "Unspecified") %>% 
      head(3) %>% 
      mutate_if(is.factor, as.character) %>% 
      left_join(communities, by = c("community_name2" = "community_name")) %>% 
      mutate(
        sep = case_when(
          row_number() == 1 ~ "",
          row_number() == dplyr::n() ~ " and ",
          TRUE ~ ", "
        ),
        txt = stringr::str_glue("{sep}<strong><span style='background-color: {community_color};color:white'>&nbsp;{community_name2}&nbsp;</span></strong> ({perc})")
      ) %>% 
      summarise(paste0(txt, collapse = "")) %>% 
      pull() %>% 
      stringr::str_c("Most exported historically products are ", .)
    
    hchart(daux, "streamgraph", hcaes(year, export_value_usd, group = community_name2)) %>% 
      hc_colors(cols) %>% 
      hc_title(text = "Most Exported products by year") %>% 
      hc_subtitle(text = subtitle, useHTML = TRUE) %>% 
      hc_yAxis(visible = FALSE) %>% 
      hc_xAxis(crosshair = list(label = list(enabled = TRUE))) %>% 
      hc_tooltip(table = TRUE, sort = TRUE) %>% 
      hc_plotOptions(
        series = list(
          borderWidth = 0,
          marker = list(states = list(hover = list(enabled = FALSE)))
          )
        ) 
      
    
  })
  
  output$treemp <- renderHighchart({

    daux <- drc()
   
    daux1 <- daux %>% 
      group_by(community_name) %>% 
      summarise(export_value_usd = sum(export_value_usd)) %>% 
      ungroup() %>% 
      arrange(desc(export_value_usd)) %>% 
      mutate(
        aux = cumsum(export_value_usd)/sum(export_value_usd),
        community_name2 = ifelse(aux <= input$percent/100 | row_number() <= 1, community_name, "Others")
      ) %>% 
      select(community_name, community_name2) %>% 
      left_join(communities, by = c("community_name2" = "community_name"))
    
    daux1
    
    cols <- daux1 %>%
      filter(row_number() <= min(which(community_name2 == "Others"))) %>% 
      pull(community_color)
    
    daux <- daux %>% 
      select(-community_color) %>% 
      left_join(daux1, by = "community_name") %>% 
      mutate(product_shortname_english2 = ifelse(community_name2 == "Others", "Other products", product_shortname_english)) %>% 
      group_by(community_name2, community_color, product_shortname_english2) %>% 
      summarise(export_value_usd = sum(export_value_usd), .groups = "drop") %>% 
      arrange(community_name2, desc(export_value_usd)) %>% 
      group_by(community_name2) %>% 
      mutate(
        cumshare = cumsum(export_value_usd)/sum(export_value_usd),
        product_shortname_english2 = ifelse(cumshare <= input$percent/100 | row_number() <= 1, product_shortname_english2, "Other products")
      ) %>% 
      ungroup() %>% 
      group_by(community_name2, community_color, product_shortname_english2) %>% 
      summarise(export_value_usd = sum(export_value_usd), .groups = "drop") %>% 
      group_by(community_name2) %>% 
      mutate(
        share = round(export_value_usd/sum(export_value_usd), 2),
        # product_color = community_color
        product_color = hex_to_rgba(community_color, scales::rescale(share, to = c(0.75, 1)))
      ) %>% 
      ungroup() %>% 
      mutate(community_name2 = factor(community_name2, levels = unique(pull(daux1, community_name2)))) %>% 
      arrange(community_name2, desc(share))
    
    # tail(daux)
    # View(daux)
    
    subtitle <- daux %>% 
      group_by(community_name2) %>% 
      summarise(export_value_usd = sum(export_value_usd/1e6)) %>% 
      mutate(perc = percent(export_value_usd/sum(export_value_usd))) %>% 
      arrange(desc(export_value_usd)) %>% 
      filter(community_name2 != "Others", community_name2 != "Unspecified") %>% 
      head(3) %>% 
      mutate_if(is.factor, as.character) %>% 
      left_join(communities, by = c("community_name2" = "community_name")) %>% 
      mutate(
        sep = case_when(
          row_number() == 1 ~ "",
          row_number() == dplyr::n() ~ " and ",
          TRUE ~ ", "
        ),
        txt = stringr::str_glue("{sep}<strong><span style='background-color: {community_color};color:white'>&nbsp;{community_name2}&nbsp;</span></strong> ({perc})")
      ) %>% 
      summarise(paste0(txt, collapse = "")) %>% 
      pull() %>% 
      stringr::str_c("Most exported products in 2018 are ", .)
    
    d1 <- daux %>% 
      select(name = community_name2, color = community_color) %>% 
      distinct() %>% 
      mutate(id = str_to_id(name)) 
    
    d2 <- daux %>% 
      transmute(
        name =  product_shortname_english2, 
        parent = str_to_id(community_name2),
        color = product_color,
        value = export_value_usd
      ) %>% 
      mutate(id = as.character(row_number()))
    
    dd <- list(d1, d2) %>% 
      map(mutate_if, is.factor, as.character) %>% 
      bind_rows() %>% 
      list_parse() %>% 
      map(function(x) x[!is.na(x)])
    
    highchart() %>% 
      hc_chart(type = "treemap") %>% 
      hc_title(text = "Most Exported products in 2018") %>% 
      hc_subtitle(text = subtitle, useHTML = TRUE) %>% 
      hc_add_series(
        data = dd,
        allowDrillToNode = TRUE,
        levelIsConstant = FALSE,
        textOverflow = "clip",
        dataLabels = list(color = "white"),
        levels = list(
          list(
            level = 1,
            borderWidth = 1,
            dataLabels = list(
              enabled = TRUE,
              verticalAlign = "top",
              align = "left",
              style = list(fontSize = "12px", textOutline = FALSE)
            )
          ),
          list(
            level = 2,
            borderWidth = 0,
            dataLabels = list(enabled = FALSE)
          )
        )
      ) %>% 
      hc_colors("trasnparent")
   
  })
  
  output$prntrs <- renderHighchart({
    
    dyrpc <- dyrpc()
  
    daux <- dyrpc
    
    # main partners via sum(export_value_usd)
    # then regroup the new communities in the yr data
    daux1 <- daux %>% 
      group_by(partner_iso) %>% 
      summarise(export_value_usd = sum(export_value_usd)) %>% 
      left_join(countries %>% select(partner_iso = country_iso, country_name_english, iso2), by = "partner_iso") %>% 
      ungroup() %>% 
      arrange(desc(export_value_usd)) %>% 
      mutate(
        share = export_value_usd/sum(export_value_usd),
        aux = cumsum(share),
        partner_iso2 = ifelse(aux <= input$percent/100, partner_iso, "rest of the World"),
        country_name_english2 = ifelse(aux <= input$percent/100, country_name_english, "rest of the World"),
        iso22 = ifelse(aux <= input$percent/100, iso2, "rest of the World"),
      ) %>% 
      select(-export_value_usd)
    
    tail(daux1)
    
    # partners <- daux1 %>%
    #   filter(row_number() <= min(which(partner_iso2 == "rest of the World"))) %>% 
    #   pull(partner_iso2)
    
    daux <- daux %>% 
      left_join(daux1, by = "partner_iso") %>% 
      group_by(year, partner_iso2, country_name_english2, iso22) %>% 
      summarise(export_value_usd = sum(export_value_usd), .groups = "drop") %>% 
      mutate(
        partner_iso2 = factor(partner_iso2, levels = unique(pull(daux1, partner_iso2))),
        country_name_english2 = factor(country_name_english2, levels = unique(pull(daux1, country_name_english2))),
        iso22 = factor(iso22, levels = unique(pull(daux1, iso22))),
       )
    
    subtitle <- daux %>% 
      group_by(partner_iso2) %>% 
      summarise(export_value_usd = sum(export_value_usd/1e6)) %>% 
      mutate(perc = percent(export_value_usd/sum(export_value_usd), accuracy = 0.1)) %>% 
      arrange(desc(export_value_usd)) %>% 
      filter(partner_iso2 != "rest of the World") %>% 
      head(3) %>% 
      mutate_if(is.factor, as.character) %>% 
      mutate(
        sep = case_when(
          row_number() == 1 ~ "",
          row_number() == dplyr::n() ~ " and ",
          TRUE ~ ", "
        ),
        txt = iso3_to_flag_n_name(partner_iso2),
        txt = stringr::str_glue("{sep} {txt} ({perc})")
      ) %>% 
      summarise(paste0(txt, collapse = "")) %>% 
      pull() %>% 
      stringr::str_c("Most important partners historically in terms of exported value are ", .)
    
    dpars <- daux1 %>% 
      filter(row_number() <= min(which(partner_iso2 == "rest of the World"))) %>% 
      left_join(countries %>% select(iso2, url_countryflags, main_color_flag), by = c("iso22" = "iso2"))
    
    dpars <- dpars %>% 
      # select(ends_with("2"), share) %>% 
      transmute(
        share = ifelse(country_name_english2 == "rest of the World", 1 - lag(aux), share),
        # aux2 = cumsum(share),
        lineWidth = round(scales::rescale(share, to = c(0.5, 3)), 2),
        visible = case_when(
          row_number() <= 3 ~ TRUE,
          country_name_english2 == "rest of the World" ~ TRUE,
          TRUE ~ FALSE
        ),
        symbol = stringr::str_glue("url({url_countryflags})"),
        symbol = ifelse(country_name_english2 == "rest of the World", NA, symbol),
        color = main_color_flag,
        color = coalesce(color, "gray")
      )
    
    dpars
    
    hc <- hchart(
      daux %>% select(x = year, group = country_name_english2, y = export_value_usd), "line",
      hcaes(x, y, group = group),
      lineWidth = pull(dpars, lineWidth),
      visible   = pull(dpars, visible),
      symbol    = pull(dpars,  symbol),
      color     = pull(dpars,  color),
      ) %>% 
      hc_title(text = "Most important partners in term of exported value") %>% 
      hc_subtitle(text = subtitle, useHTML = TRUE) %>% 
      hc_tooltip(table = TRUE, sort = TRUE) %>% 
      hc_yAxis(
        # type = "logarithmic"
        title = list(text = "Exported USD")
        ) %>% 
      hc_xAxis(
        crosshair = list(label = list(enabled = TRUE)),
        title = list(text = "Year")
        ) %>% 
      hc_plotOptions(
        series = list(
          marker = list(enabled = FALSE)
          )
        ) %>% 
      hc_legend(layout = "proximate", align = "right")
    
    hc$x$hc_opts$series <- hc$x$hc_opts$series %>% 
      map(function(x){
        
        # x <- hc$x$hc_opts$series %>% sample(1) %>% first()
        x$marker <- list(symbol = x$symbol)
        
        x
        
      })
    
    hc
    

  })
  
  output$vb_export <- renderValueBox({
    
    dyr <- dyr()
    
    d <- dyr %>% 
      select(year, export_value_usd) %>% 
      mutate(export_value_usd = round(export_value_usd/1e9, 2)) %>% 
      select(x = year, y = export_value_usd)
    
    lbl <- d %>% pull(y) %>% last() %>% comma() %>% paste0("USD $", .," B") 
    
    hc <- hchart(d, "areaspline", color = PARS$sparkline_color) %>% 
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
    
    hc <- hchart(d, "areaspline", color = PARS$sparkline_color) %>% 
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
    
    hc <- hchart(d, "spline", color = PARS$sparkline_color) %>% 
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
    
    hc <- hchart(d, "spline", color = PARS$sparkline_color) %>% 
      hc_add_theme(hc_theme_sparkline2())
    
    valueBoxSpark(
      value = lbl,
      color = "black",
      subtitle = "Complexity",
      spark = hc,
      minititle = "Country Complexity Index in 2018"
    )
    
  })
  
})
