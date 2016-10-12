library(shiny)

shinyUI(fluidPage(
  titlePanel("How much time will you save with an automation?"),
  h4("(minus the time you spend)"),
  includeMarkdown("introduction.Rmd"), hr(),
  sidebarLayout(
    sidebarPanel(
      p("How long do you plan to run the automation?"),
      fluidRow(
        column(5,
               numericInput("time_value", "", value = 1, min = 0, max = 1e3)
        ),
        column(7,
               selectInput("time_value_units", label = "",
                           choices = c("week(s)", "month(s)", "year(s)"),
                           selected = "year(s)")
        )
      ), width = 3
    ),
    mainPanel(
      plotOutput("time_saved_table", height = "700px"),
      width = 9
    )
  ),
  tags$head(tags$script(src = "disqus.js"))
))
