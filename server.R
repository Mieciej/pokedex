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
library(fmsb)
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

max_hp = max(data$hp)
max_attack = max(data$attack)
max_defense = max(data$defense)
max_sp_attack = max(data$sp_attack)
max_sp_defense = max(data$sp_defense)
max_speed = max(data$speed)

min_hp = min(data$hp)
min_attack = min(data$attack)
min_defense = min(data$defense)
min_sp_attack = min(data$sp_attack)
min_sp_defense = min(data$sp_defense)
min_speed = min(data$speed)
poke_colors = c("#cd5241", "#084152", "#833118", "#eede7b","#207394","#eeb45a")
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
      print(pokemon_image_url)
      if (is.null(pokemon_image_url)) {
        return(NULL)
      }
      tags$img(src = pokemon_image_url, class = "poke-image")
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
    name = c(
      selected_pokemons$pokeName01,
      selected_pokemons$pokeName02,
      selected_pokemons$pokeName03,
      selected_pokemons$pokeName04,
      selected_pokemons$pokeName05,
      selected_pokemons$pokeName06
    ) 
    poke_counter <- data.frame(name)
    poke_counter <- poke_counter %>% 
      count(name)
    poke_stats = data %>% filter(
      name == selected_pokemons$pokeName01 |
        name == selected_pokemons$pokeName02 |
        name == selected_pokemons$pokeName03 |
        name == selected_pokemons$pokeName04 |
        name == selected_pokemons$pokeName05 |
        name == selected_pokemons$pokeName06)
    team_stats = poke_stats %>%
      select(name, hp, attack, defense, sp_attack,sp_defense, speed) %>%
      mutate(hp = (hp-min_hp)/(max_hp-min_hp)) %>%
      mutate(attack = (attack-min_attack)/(max_attack-min_attack)) %>%
      mutate(defense = (defense-min_defense)/(max_defense-min_defense)) %>%
      mutate(sp_attack = (sp_attack-min_sp_attack)/(max_sp_attack-min_sp_attack)) %>%
      mutate(sp_defense = (sp_defense-min_sp_defense)/(max_sp_defense-min_sp_defense)) %>%
      mutate(speed = (speed-min_speed)/(max_speed-min_speed)) %>%
      left_join(poke_counter, by="name") %>%
      uncount(n)  %>% 
      gather("stat","value", -name) %>%
      group_by(stat) %>%
      mutate(value = mean(value)) %>% 
      ungroup() %>%
      select(-name) %>% 
      distinct()
      
    
    output$avg_stat <- renderPlot(
      {
        
        ggplot(team_stats, aes(x =stat, y=value )) +
          geom_bar(stat="identity",aes(fill = stat)) + 
          scale_colour_manual(values = poke_colors) +
          ylim(0,1) +
          coord_polar()
        
      }
    )
  })
}
