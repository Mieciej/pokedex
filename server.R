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
library(httr)
library(jsonlite)
library(shinycssloaders)

data <- read.csv("pokedex.csv", header = TRUE)

get_pokemon_image_url <- function(pokemon_name) {
  url <- paste0("https://pokeapi.co/api/v2/pokemon/", pokemon_name)
  response <- httr::GET(url)
  if (httr::http_error(response)) {
    return(NULL)
  }
  pokemon_data <- jsonlite::fromJSON(httr::content(response, "text"))
  return(pokemon_data$sprites$front_default)
}

# use the data from csv
fetch_pokemon_index_from_name <- function(pokemon_name) {
  index <- data %>% filter(name == pokemon_name) %>% select(pokedex_number)
  return(index)
}

# Define server logic required to draw a histogram
function(input, output, session) {
  nums = c("01", "02", "03", "04", "05", "06")
  for (num in nums) {
    name <- paste("pokeName", num, sep = "")
    updateSelectizeInput(session, name, choices = data$name, server = TRUE)
  }
  
  lapply(1:6, function(i) {
    output[[paste0("pokeOutput", sprintf("%02d", i))]] <- renderUI({
      pokemon_name <- input[[paste0("pokeName", sprintf("%02d", i))]]
      if (is.null(pokemon_name)) {
        return(NULL)
      }
      pokemon_image_url <- get_pokemon_image_url(fetch_pokemon_index_from_name(pokemon_name))
      if (is.null(pokemon_image_url)) {
        return(NULL)
      }
      tags$div(
        class = "poke-image-container",
        style = "padding-top: 100%; position: relative;", # Position card inside container
        tags$div(
          style = "position: absolute; top: 0; left: 0; right: 0; bottom: 0;", # Position card inside container
          tags$img(src = pokemon_image_url, class = "poke-image")
        )
      )
    })
  })
  
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
