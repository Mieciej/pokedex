library(shiny)
library(bslib)

pokemon_selector <- function(num){
    selectizeInput(paste("pokeName",num, sep=""),label = "Choose your pokemon:",choices = NULL)
}

ui <- fluidPage(
  titlePanel("PokeDex"),
  fluidRow(
    column(2, pokemon_selector('01')),
    column(2, pokemon_selector('02')),
    column(2, pokemon_selector('03')),
    column(2, pokemon_selector('04')),
    column(2, pokemon_selector('05')),
    column(2, pokemon_selector('06'))
  ),
  plotOutput("avg_stat"),
  card(
    card_header(
      class = "bg-dark",
      "A header"
    ),
    markdown("Some text with a [link](https://github.com).")
  )
)
