#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
data <- read.csv("pokedex.csv", header = TRUE)

# Define server logic required to draw a histogram
function(input, output, session) {
  nums = c("01", "02", "03", "04", "05", "06")
  for (num in nums) {
    name <- paste("pokeName", num, sep = "")
    updateSelectizeInput(session, name, choices = data$name, server = TRUE)
  }
  
  selected_pokemons =  reactiveValues(
    pokeName01 = NULL,
    pokeName02 = NULL,
    pokeName03 = NULL,
    pokeName04 = NULL,
    pokeName05 = NULL,
    pokeName06 = NULL
  )
  observe({
    for (num in nums) {
      name <- paste("pokeName", num, sep = "")
      selected_pokemons[[name]] = input[[name]]
    }
    poke_stats = data %>% filter(
      name == selected_pokemons$pokeName01 |
        name == selected_pokemons$pokeName02 |
        name == selected_pokemons$pokeName03 |
        name == selected_pokemons$pokeName04 |
        name == selected_pokemons$pokeName05 |
        name == selected_pokemons$pokeName06)
    team_stats = poke_stats %>% select(name, hp, attack, defense, sp_attack,sp_defense, speed) %>%  gather("stat","value", -name)
    output$avg_stat <- renderPlot(
      {
        ggplot(team_stats,aes(x=stat, y=value)) +
          geom_point(aes(color=name))
      }
    )
    
  })
  
  
  
  
}
