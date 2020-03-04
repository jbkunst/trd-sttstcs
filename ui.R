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
        # selectInput("country", NULL, c("Chile", "Argentina")),
        pickerInput(
            "country", NULL, multiple = FALSE,
            choices = pull(countries, country_name_english),
            choicesOpt = list(content =  cntnt)
            ),
        
        fluidRow(
            valueBox(width = 3, 10 * 2, "New Orders", icon = icon("credit-card")),
            valueBox(width = 3, 10 * 2, "New Orders", icon = icon("credit-card")),
            valueBox(width = 3, 10 * 2, "New Orders", icon = icon("credit-card")),
            valueBox(width = 3, 10 * 2, "New Orders", icon = icon("credit-card"))
        ),
        
        fluidRow(
            column(width = 4, highcharts_demo()),
            column(width = 4, highcharts_demo()),
            column(width = 4, highcharts_demo())
            # box(title = "Title 1", width = 4, solidHeader = TRUE, status = "primary", "Box content"),
            # box(title = "Title 1", width = 4, solidHeader = TRUE, status = "primary", "Box content"),
            # box(title = "Title 1", width = 4, solidHeader = TRUE, status = "primary", "Box content")
        ),
        
    )
)