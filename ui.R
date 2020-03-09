dashboardPage(
    dashboardHeader(disable = TRUE),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "css/custom.css")
        ),
        tags$div(
            style = "position: absolute; width: 100%; left: 0; z-index: 0; height: 90vh;", 
            mapwrdl
        ),
        
        fluidRow(
            column(
                width = 4,
                pickerInput(
                    "country", NULL, multiple = FALSE,
                    choices = pull(countries, country_name_english),
                    choicesOpt = list(content =  cntnt)
                    )
                ),
            column(
                width = 4,
                sliderInput("percent", NULL, min = .5, max = 0.99, round = TRUE, value = 0.8)
            ),
            column(
                width = 4,
                sliderInput("since_year", NULL, min = 1962, max = 1990, value = 1990)
            )
        ),
        
        fluidRow(
            valueBoxOutput("vb_export", 4),
            valueBoxOutput("vb_import", 4),
            valueBoxOutput("vb_pci", 4)
        ),
        
        fluidRow(
            column(width = 6, highchartOutput("stream")),
            fluidRow(
                column(
                    width = 6,
                    highcharts_demo(),
                    highcharts_demo()
                    )
                )
            # box(title = "Title 1", width = 4, solidHeader = TRUE, status = "primary", "Box content"),
            # box(title = "Title 1", width = 4, solidHeader = TRUE, status = "primary", "Box content"),
            # box(title = "Title 1", width = 4, solidHeader = TRUE, status = "primary", "Box content")
        ),
        
    )
)