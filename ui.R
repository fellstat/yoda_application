
library(shiny)
library(shinyWidgets)
library(shinyhelper)
library(magrittr)

srhelp <- function(x, ...){
    helper(x, ..., colour="lightgrey")
}
source("ui-globals.R")


shinyUI(
    uiOutput("ui")
)
