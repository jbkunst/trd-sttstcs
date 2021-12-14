dashboardPage(
    dashboardHeader(disable = TRUE),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "css/custom.css")
        ),
        tags$div(
            style = "position: absolute; width: 100%; left: 0; z-index: 0; height: 90vh; position:fixed",
            mapwrdl
        ),
        
        fluidRow(
            column(
                width = 12,
                class = PARS$classcol,
                tags$h2(class = "specialfont", "XporterXplorer"),
                tags$em("A shiny app to explore international trade data. Just pick a country...")
            ),
        ),
        
        fluidRow(
            class = "top-buffer",
            column(
                width  = 10,
                class = "col-lg-offset-4 col-lg-4 col-md-offset-2 col-md-8 col-sm-offset-1 col-sm-10",
                pickerInput(
                    "country", NULL,
                    multiple = FALSE,
                    width = "100%",
                    selected = ifelse(PARS$debug, "Chile", first(pull(dfsrt, country_name_english))),
                    choices = pull(dfsrt, country_name_english),
                    choicesOpt = list(content =  pull(dfsrt, cntnt))
                    )
                ),
            column(
                width = 2,
                dropdownButtonp(
                    tags$h4("Control Outputs"),
                    tags$hr(),
                    sliderInput(
                        "percent", 
                        "Detail of the data",
                        min = 50, 
                        max = 90,
                        step = 5,
                        round = TRUE, 
                        value = 80, 
                        post  = "%",
                        width = "100%"
                        ),
                    tags$small(
                        tags$em(
                            "This parameter is used to set the categories as 'Others'
                            when the sort cumulative percentage reach this value.
                            Default value is 80%, you know: Pareto."
                        )
                    ),
                    tags$hr(),
                    # sliderInput("since_year", NULL, min = 1962, max = 1990, value = 1990),
                    sliderTextInput(
                        "since_to_year",
                        label = "Amount of historical data",
                        choices = 1962:2018,
                        selected = c(1990, 2018),
                        from_min = 1962,
                        from_max = 2010,
                        to_min = 2018,
                        to_max = 2018,
                        width = "100%"
                        ),
                    tags$small(
                        tags$em(
                            "With this input you can control how much historical data consider in temporal charts."
                        )
                    ),
                    icon = icon("cog"),
                    tooltip = tooltipOptions(title = "Click to configure options")
                    ),
                dropdownButtonp(
                    tags$h4("About this app"),
                    "This is an app developed by Joshua Kunst (@jbkunst) using R",
                    "The package used are shiny, shiny dashboard, tradestatiscits, highcharter among others",
                    "URL repo: https://github.com/jbkunst/trd-sttstcs",
                    "Licence CC0",
                    icon = icon("info"),
                    tooltip = tooltipOptions(title = "About this app")
                    )
                )
            ),

        fluidRow(
            class = "top-buffer",
            column(
                # offset = 2,
                width = 12,
                class = PARS$classcol,
                valueBoxOutput("vb_export", 3),
                valueBoxOutput("vb_import", 3),
                valueBoxOutput("vb_expdiv", 3),
                valueBoxOutput("vb_pcompx", 3)
                )
            
            ),

        fluidRow(
            class = "top-buffer",
            column(
                width = 12,
                class = PARS$classcol,
                column(width = 07, highchartOutput("stream", height = "600px")),
                column(width = 05, highchartOutput("treemp", height = "600px"))
                )
            ),
        

        fluidRow(
            class = "top-buffer",
            column(
                width = 12,
                class = PARS$classcol,
                highchartOutput("prntrs", height = "600px")
                )
            )
        )
    )
