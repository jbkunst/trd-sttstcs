dashboardPage(
    dashboardHeader(disable = TRUE),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
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
            valueBox(10 * 2, "New Orders", icon = icon("credit-card")),
            valueBox(10 * 2, "New Orders", icon = icon("credit-card")),
            valueBox(10 * 2, "New Orders", icon = icon("credit-card"))
        ),
        fluidRow(
            box(title = "Title 1", width = 4, solidHeader = TRUE, status = "primary", "Box content"),
            box(title = "Title 1", width = 4, solidHeader = TRUE, status = "primary", "Box content"),
            box(title = "Title 1", width = 4, solidHeader = TRUE, status = "primary", "Box content")
        ),
        
    )
)