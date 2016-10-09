library(shiny)

shinyUI(fluidPage(
  titlePanel("How much time will you save with an automation?"),
  plotOutput("time_saved_table")
))
