library(shiny)
library(bslib)

pokemon_selector <- function(num) {
  selectizeInput(paste("pokeName", num, sep = ""), label = "Choose your pokemon:", choices = NULL)
}

ui <- fluidPage(
  tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
  titlePanel("PokeDex"),
  fluidRow(class = "poke-row",
           column(4, class = "poke-col",
                  card(
                    class = "poke-card",
                    pokemon_selector('01'),
                    uiOutput("pokeOutput01")
                  )
           ),
           column(4, class = "poke-col",
                  card(
                    class = "poke-card",
                    pokemon_selector('02'),
                    uiOutput("pokeOutput02")
                  )
           ),
           column(4, class = "poke-col",
                  card(
                    class = "poke-card",
                    pokemon_selector('03'),
                    uiOutput("pokeOutput03")
                  )
           ),
           column(4, class = "poke-col",
                  card(
                    class = "poke-card",
                    pokemon_selector('04'),
                    uiOutput("pokeOutput04")
                  )
           ),
           column(4, class = "poke-col",
                  card(
                    class = "poke-card",
                    pokemon_selector('05'),
                    uiOutput("pokeOutput05")
                  )
           ),
           column(4, class = "poke-col",
                  card(
                    class = "poke-card",
                    pokemon_selector('06'),
                    uiOutput("pokeOutput06")
                  )
           )
  ),
  plotOutput("avg_stat"),
)
