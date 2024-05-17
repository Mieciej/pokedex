library(shiny)

pokemon_selector <- function(num){
    selectizeInput(paste("pokeName",num, sep=""),label = "Choose your pokemon:",choices = NULL)
}

fluidPage(
    titlePanel("PokeDex"),
    pokemon_selector('01'),
    pokemon_selector('02'),
    pokemon_selector('03'),
    pokemon_selector('04'),
    pokemon_selector('05'),
    pokemon_selector('06'),
    plotOutput("avg_stat")
)

