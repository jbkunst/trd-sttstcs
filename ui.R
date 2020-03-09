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
                width = 6,
                pickerInput(
                    "country", NULL, multiple = FALSE,
                    choices = pull(countries, country_name_english),
                    choicesOpt = list(content =  cntnt)
                    )
                ),
            column(
                width = 6,
                sliderInput("percent", NULL, min = .5, max = 0.99, round = TRUE, value = 0.8)
            )
        ),
        
        fluidRow(
            valueBox(width = 4, 10 * 2, "New Orders", icon = icon("credit-card")),
            valueBox(width = 4, 10 * 2, "New Orders", icon = icon("credit-card")),
            valueBox(width = 4, 10 * 2, "New Orders", icon = icon("credit-card"))
        ),
        
        fluidRow(
            column(width = 4, highchartOutput("stream")),
            column(width = 4, highcharts_demo()),
            column(width = 4, highcharts_demo())
            # box(title = "Title 1", width = 4, solidHeader = TRUE, status = "primary", "Box content"),
            # box(title = "Title 1", width = 4, solidHeader = TRUE, status = "primary", "Box content"),
            # box(title = "Title 1", width = 4, solidHeader = TRUE, status = "primary", "Box content")
        ),
        
    )
)