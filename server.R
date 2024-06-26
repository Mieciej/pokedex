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
library(ggtext)
library(DT)
library(plotly)
library(stringr)

data <- read.csv("pokedex.csv", header = TRUE)
data <- data %>%
  distinct(pokedex_number, .keep_all = TRUE)
 # filter(!grepl('Mega|Alolan|Galarian|Partner|Primal', name) | name %in% c('Meganium', 'Yanmega'))

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
poke_colors <- c(hp = "#FF3333", attack = "#3366FF", sp_attack = "#9933FF",
                  defense = "#339933", sp_defense = "#CCCC33",
                  speed = "#FF9933")
 
type_colors <- c(
  normal = "#A8A77A",
  fire = "#EE8130",
  water = "#6390F0",
  electric = "#F7D02C",
  grass = "#7AC74C",
  ice = "#96D9D6",
  fight = "#C22E28",
  poison = "#A33EA1",
  ground = "#E2BF65",
  flying = "#A98FF3",
  psychic = "#F95587",
  bug = "#A6B91A",
  rock = "#B6A136",
  ghost = "#735797",
  dragon = "#6F35FC",
  dark = "#705746",
  steel = "#B7B7CE",
  fairy = "#D685AD"
)
rank_colors = c(rocks = "#339933", sucks = "#3366FF")

type_labels <- c(
  normal = "<img src = 'icons/normal.png' width = '45' />",
  fire = "<img src = 'icons/fire.png' width = '45' />",
  water = "<img src = 'icons/water.png' width = '45' />",
  electric = "<img src = 'icons/electric.png' width = '45' />",
  grass = "<img src = 'icons/grass.png' width = '45' />",
  ice = "<img src = 'icons/ice.png' width = '45' />",
  fight = "<img src = 'icons/fighting.png' width = '45' />",
  poison = "<img src = 'icons/poison.png' width = '45' />",
  ground = "<img src = 'icons/ground.png' width = '45' />",
  flying = "<img src = 'icons/flying.png' width = '45' />",
  psychic = "<img src = 'icons/psychic.png' width = '45' />",
  bug = "<img src = 'icons/bug.png' width = '45' />",
  rock = "<img src = 'icons/rock.png' width = '45' />",
  ghost = "<img src = 'icons/ghost.png' width = '45' />",
  dragon = "<img src = 'icons/dragon.png' width = '45' />",
  dark = "<img src = 'icons/dark.png' width = '45' />",
  steel = "<img src = 'icons/steel.png' width = '45' />",
  fairy = "<img src = 'icons/fairy.png' width = '45' />"
)

stat_labels <- c(
  attack = "<img src = 'icons/attack.png' width = '30' />",
  hp = "<img src = 'icons/hp.png' width = '30' />",
  defense = "<img src = 'icons/defense.png' width = '30' />",
  sp_attack = "<img src = 'icons/sp_attack.png' width = '30' />",
  sp_defense = "<img src = 'icons/sp_defense.png' width = '30' />",
  speed = "<img src = 'icons/speed.png' width = '30' />"
)

stat_labels_comp <- c(
  attack = "icons/attack.png",
  hp = "icons/hp.png",
  defense = "icons/defense.png",
  sp_attack = "icons/sp_attack.png",
  sp_defense = "icons/sp_defense.png",
  speed = "icons/speed.png"
)

scaleFUN <- function(x) sprintf("%.2f", x)
humanReadify <- function(d) {
  d %>%
    
    mutate(
      img = paste(
        "<img src = 'https://raw.githubusercontent.com/pokeapi/sprites/master/sprites/pokemon/",
        pokedex_number,
        ".png'",
        " width = '75' />",
        sep = ""
      )
    ) %>%
    select(
      img,
      pokedex_number,
      name,
      type_1,
      type_2,
      total_points,
      hp,
      attack,
      sp_attack,
      defense,
      sp_defense,
      speed
    )
}
human_readable_data <- humanReadify(data)

gens =  c(
  "Red/Green" = 1,
  "Gold/Silver" = 2,
  "Ruby/Sapphire" = 3,
  "Diamond/Pearl" = 4,
  "Black/White" = 5,
  "X/Y" = 6,
  "Sun/Moon" = 7,
  "Sword/Shield" = 8,
  "Scarlet/Violet" = 9
)
# Define server logic required to draw a histogram
function(input, output, session) {
  
  observeEvent(input$openAbout, {
    showModal(
      modalDialog(
        title = "About the Project",
        tags$img(src = "icons/PP_logotyp_RGB.png", width = 300),
        tags$div(
          style = "text-align: justify;",
          tags$p(
            "Welcome to our Interactive Dashboards project! This dashboard have been created as a fourth project for Data Visualization course.",
            "Our idea was to create visually appeling dashboard that would allow users to explore the world of Pokémon in a fun and interactive way.",
            "The dashboard comprises three distinct sections, each offering a unique set of features:",
            
              tags$li(
                "On the first page, users can construct their own Pokemon teams by selecting six Pokémon from a list of all available creatures. The dashboard then provides a visual representation of the team's average statistics, allowing users to quickly assess the strengths and weaknesses of their team."
              ),
              tags$li(
                "The second page hosts an encyclopedia of Pokémon, allowing user to go through the generations, look for their favorite Pokémon and see their stats. The table is interactive and allows for sorting."
              ),
              tags$li(
                "The third page introduces a Pokemon comparison functionality, allowing users to compare various Pokémon based on their statistics."
              ),
          
          )
        ),
      )
    )
  })
  
  nums = c("01", "02", "03", "04", "05", "06")
  nums_comp = c("31", "32")
  for (num in nums) {
    name <- paste("pokeName", num, sep = "")
    updateSelectizeInput(session, name, choices = data$name, server = TRUE,selected = sample_n(data,1) %>% select(name))
  }
  
  for (num in nums_comp) {
    name <- paste("pokeName", num, sep = "")
    updateSelectizeInput(session, name, choices = data$name, server = TRUE,selected = sample_n(data,1) %>% select(name))
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
        style = "padding-top: 100%; position: relative;",
        tags$div(
          id = paste0("spinner", i),
          class = "spinner",
        ),
        tags$div(
          style = "position: absolute; top: 0; left: 0; right: 0; bottom: 0;",
          tags$img(src = pokemon_image_url, class = "poke-image", id = paste0("pokeImage", i), onload = "imageLoaded(this.id)")
        ),
      )
    })
  })
  
  lapply(nums_comp, function(num) {
    output[[paste0("pokeOutput", num)]] <- renderUI({
      pokemon_name <- input[[paste0("pokeName", num)]]
      if (is.null(pokemon_name)) {
        return(NULL)
      }
      pokemon_image_url <- get_pokemon_image_url(fetch_pokemon_index_from_name(pokemon_name))
      if (is.null(pokemon_image_url)) {
        return(NULL)
      }
      tags$div(
        class = "poke-image-container",
        style = "padding-top: 100%; position: relative;",
        tags$div(
          id = paste0("spinner", num),
          class = "spinner",
        ),
        tags$div(
          style = "position: absolute; top: 0; left: 0; right: 0; bottom: 0;",
          tags$img(src = pokemon_image_url, class = "poke-image", id = paste0("pokeImage", num), onload = "imageLoadedComparisonDasboard(this.id)")
        ),
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
  
  selected_pokemons_comp =  reactiveValues(
    pokeName31 = NULL,
    pokeName32 = NULL
  )
  
  # Dashboard observe
  observe({
    for (num in nums) {
      name <- paste("pokeName", num, sep = "")
      selected_pokemons[[name]] = input[[name]]
    }
    
    for (num in nums_comp) {
      name <- paste("pokeName", num, sep = "")
      selected_pokemons_comp[[name]] = input[[name]]
    }
    name = c(
      selected_pokemons$pokeName01,
      selected_pokemons$pokeName02,
      selected_pokemons$pokeName03,
      selected_pokemons$pokeName04,
      selected_pokemons$pokeName05,
      selected_pokemons$pokeName06
    ) 
    
    names_comp = c(
      selected_pokemons_comp$pokeName31,
      selected_pokemons_comp$pokeName32
    )

    comparison_stats = data %>% filter(
      name == selected_pokemons_comp$pokeName31 |
      name == selected_pokemons_comp$pokeName32)
    
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
        g <- ggplot(team_stats, aes(x =stat, y=value )) +
          geom_bar(stat="identity",aes(fill = stat)) + 
          ylab("") +
          xlab("") +
          scale_fill_manual(values = poke_colors, labels=stat_labels ) +
          coord_polar() +
          scale_y_continuous(breaks = c(0,1),labels = NULL) +
          theme_minimal() +
          theme(axis.title.y = element_blank(),
                axis.text = element_text(size = 12),
                axis.text.x =  element_blank(),
                legend.text = element_markdown(),
                legend.title = element_blank(),
                panel.background = element_rect(fill = "#ECF0F5", color = "#ECF0F5"),
                plot.background = element_rect(fill = "#ECF0F5", color = "#ECF0F5"),
                panel.border = element_blank(),
                panel.grid.major = element_line(color = "#404040"),
                panel.grid.minor = element_line(color = "#404040"),
                plot.margin = margin(0, 0, 0, 0)) 
        library(grid)
        grob <- ggplotGrob(g)
        grid.newpage()
        grid.draw(grob)
      }
    )
    team_damage <- poke_stats %>%
      select(name, against_normal:against_fairy) %>%
      left_join(poke_counter, by="name") %>%
      uncount(n)  %>% 
      gather("type","value", -name) %>%
      group_by(type)%>%
      mutate(value = mean(value)) %>%
      ungroup() %>%
      mutate(type = gsub("against_",'',type)) %>%
      select(-name) %>%
      distinct()
    output$avg_damage_taken <- renderPlot({
      ggplot(team_damage, aes(x = type, y = value)) +
        geom_bar(stat = "identity", aes(fill = type)) +
        scale_fill_manual(values = type_colors) +
        geom_hline(yintercept = 1.0) + 
        scale_x_discrete(name = NULL, labels =type_labels ) +
        scale_y_continuous(labels = scaleFUN,breaks =c(0,1,max(team_damage$value)))+
        theme_minimal() +
        theme(axis.text.x = element_markdown(),
              axis.title.y = element_blank(),
              axis.text.y = element_text(size=20),
              panel.background = element_rect(fill = "#ECF0F5", color = "#ECF0F5"),
              plot.background = element_rect(fill = "#ECF0F5", color = "#ECF0F5"),
              panel.border = element_blank(),) +
        guides(fill ="none") 
        
    })
      
    
  })# Dashboard observe end
  data_for_datatable <- reactive({
    input_buttons = c(input$gen_radio_buttons, input$gen_radio_buttonsB, input$gen_radio_buttonsC, input$gen_radio_buttonsD, input$gen_radio_buttonsE)
    humanReadify(data %>% filter(generation %in% gens[input_buttons]))
  })
  
  output$national_table <- renderDataTable({
    datatable(data = data_for_datatable(),escape = FALSE,rownames = FALSE)
  }) 
  
  comparison_data = reactive({
    comp_stats = data %>% filter(name %in% c(input$pokeName31, input$pokeName32))
    x = comp_stats %>%
      select(name, hp, attack, defense, sp_attack, sp_defense, speed) %>%
      mutate(hp = (hp-min_hp)/(max_hp-min_hp)) %>%
      mutate(attack = (attack-min_attack)/(max_attack-min_attack)) %>%
      mutate(defense = (defense-min_defense)/(max_defense-min_defense)) %>%
      mutate(sp_attack = (sp_attack-min_sp_attack)/(max_sp_attack-min_sp_attack)) %>%
      mutate(sp_defense = (sp_defense-min_sp_defense)/(max_sp_defense-min_sp_defense)) %>%
      mutate(speed = (speed-min_speed)/(max_speed-min_speed))
    x
  })
  
  output$comparison_plot <- renderPlotly({
    thetas = c("HP", "Attack", "Defense", "Sp. Attack", "Sp. Defense", "Speed")
    data1 <- comparison_data() %>% 
      select(hp, attack, defense, sp_attack, sp_defense, speed) %>% 
      slice(1) %>% 
      unlist() %>% 
      round(2)
    data2 <- comparison_data() %>% 
      select(hp, attack, defense, sp_attack, sp_defense, speed) %>% 
      slice(2) %>% 
      unlist() %>% 
      round(2)
    
    hovertext1 <- paste0("<b>", input$pokeName31, "</b><br>", thetas, ": ", data1)
    hovertext2 <- paste0("<b>", input$pokeName32, "</b><br>", thetas, ": ", data2)
    
    fig <- plot_ly(
      type = 'scatterpolar',
      fill = 'toself'
    )
    fig <- fig %>%
      add_trace(
        r = data1,
        theta = c(" ", "  ", "   ", "    ", "     ", "      "),
        hovertext = hovertext1,
        name = input$pokeName31,
        hoverinfo = 'text'
      )
    fig <- fig %>%
      add_trace(
        r = data2,
        theta = c(" ", "  ", "   ", "    ", "     ", "      "),
        hovertext = hovertext2,
        name = input$pokeName32,
        hoverinfo = 'text'
      )
    fig <- fig %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = TRUE,
            range = c(0, 1)
          )
        )
      )
    
    
    fig <- fig %>%
      layout(
        paper_bgcolor = "#ECF0F5",
        plot_bgcolor = "#ECF0F5",
        showlegend = FALSE,
        images = list(
          list(
            source = base64enc::dataURI(file = "icons/hp.png"),
            x = 0.80,
            y = 0.5,
            sizex = 0.1,
            sizey = 0.1,
            xanchor = "left",
            yanchor = "middle"
          ),
          list(
            source = base64enc::dataURI(file = "icons/attack.png"),
            x = 0.67,
            y = 0.95,
            sizex = 0.1,
            sizey = 0.1,
            xanchor = "center",
            yanchor = "bottom"
          ),
          list(
            source = base64enc::dataURI(file = "icons/defense.png"),
            x = 0.33,
            y = 0.95,
            sizex = 0.1,
            sizey = 0.1,
            xanchor = "center",
            yanchor = "bottom"
          ),
          list(
            source = base64enc::dataURI(file = "icons/sp_attack.png"),
            x = 0.20,
            y = 0.5,
            sizex = 0.1,
            sizey = 0.1,
            xanchor = "right",
            yanchor = "middle"
          ),
          list(
            source = base64enc::dataURI(file = "icons/sp_defense.png"),
            x = 0.33,
            y = 0.05,
            sizex = 0.1,
            sizey = 0.1,
            xanchor = "center",
            yanchor = "top"
          ),
          list(
            source = base64enc::dataURI(file = "icons/speed.png"),
            x = 0.67,
            y = 0.05,
            sizex = 0.1,
            sizey = 0.1,
            xanchor = "center",
            yanchor = "top"
          )
        )
      )
    
    fig
  })
  
  data_for_type_histogram <- reactive({
    input_buttons = c(input$gen_radio_buttons, input$gen_radio_buttonsB, input$gen_radio_buttonsC, input$gen_radio_buttonsD, input$gen_radio_buttonsE)
    data %>% filter(generation %in% gens[input_buttons]) %>%
      select(type_1,type_2) %>%
      pivot_longer(everything(),names_to = "garbage", values_to = "type") %>%
      filter(type !="") %>%
      mutate(type = tolower(type)) %>%
      mutate(type = ifelse(type =="fighting","fight",type)) %>%
      select(-garbage)
  })
  
  output$type_histogram <- renderPlot({
    ggplot(data_for_type_histogram(), aes(x = type)) +
      geom_bar(aes(fill = type)) +
      scale_x_discrete(name = NULL, labels = type_labels) +
      scale_fill_manual(values = type_colors) +
      theme_minimal() +
      theme(
        axis.text.x = element_markdown(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 20),
        panel.background = element_rect(fill = "#ECF0F5", color = "#ECF0F5"),
        plot.background = element_rect(fill = "#ECF0F5", color = "#ECF0F5"),
        panel.border = element_blank(),
      ) +
      guides(fill = "none")
    
  })
  data_for_best_pokemon <- reactive({
    input_buttons = c(input$gen_radio_buttons, input$gen_radio_buttonsB, input$gen_radio_buttonsC, input$gen_radio_buttonsD, input$gen_radio_buttonsE)
    temp <- data %>% filter(generation %in% gens[input_buttons]) %>%
      mutate(
        name = paste(
          "<img src = 'https://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/pokemon/",
          pokedex_number,
          ".png'",
          " width = '75' />",
          sep = ""
        )
      ) %>%
      select(pokedex_number,name,total_points)
    
    best_pokemon <- top_n(temp, 3, total_points) %>%
      mutate(rank = "rocks")
    worst_pokemon <- top_n(temp, 3, -total_points) %>%
      mutate(rank = "sucks")
    union(best_pokemon, worst_pokemon)
  })
  output$best_pokemon_histogram <- renderPlot({
    ggplot(data_for_best_pokemon(),aes(x=reorder(name,total_points), y=total_points)) +
      geom_bar(stat = "identity",aes(fill = rank)) +
      scale_fill_manual(values = rank_colors) +
      coord_flip() +
      theme_minimal() +
      theme(
        axis.text.y = element_markdown(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 20),
        axis.title.x = element_blank(),
        panel.background = element_rect(fill = "#ECF0F5", color = "#ECF0F5"),
        plot.background = element_rect(fill = "#ECF0F5", color = "#ECF0F5"),
        panel.border = element_blank(),
      ) + 
      guides(fill = "none")
  })
  data_for_best_poke_types <- reactive({
    # join info from gen_radio_buttons, gen_radio_buttonsB, gen_radio_buttonsC
    input_buttons = c(input$gen_radio_buttons, input$gen_radio_buttonsB, input$gen_radio_buttonsC, input$gen_radio_buttonsD, input$gen_radio_buttonsE)
    tmp = data %>% filter(generation %in% gens[input_buttons]) %>%
      pivot_longer(type_1:type_2,names_to = "garbage", values_to = "type") %>%
      filter(type !="") %>%
      mutate(type = tolower(type)) %>%
      mutate(type = ifelse(type =="fighting","fight",type)) %>%
      select(pokedex_number,name,total_points,type) %>%
      mutate(
        name = paste(
          "<img src = 'https://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/pokemon/",
          pokedex_number,
          ".png'",
          " width = '50' />",
          sep = ""
        )
      )
    best_pokemon = tmp %>%
      group_by(type) %>%
      top_n(1,total_points) %>%
      slice(1:1) %>%
      ungroup()
    worst_pokemon = tmp %>%
      group_by(type) %>%
      top_n(1,-total_points) %>% 
      slice(1:1) %>%
      ungroup()
    union(best_pokemon,worst_pokemon)
  })
  output$best_pokemon_by_type_histogram <- renderPlot({
    ggplot(data_for_best_poke_types(),aes(x=reorder(name,-total_points), y=total_points)) +
      geom_bar(stat="identity",aes(fill=type)) +
      scale_fill_manual(values = type_colors) +
      facet_grid(.~type, scales = "free_x",labeller = labeller(type = type_labels)) +
      theme_minimal() +
      theme(
        axis.text.x = element_markdown(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 20),
        strip.text = element_markdown(),
        axis.title.x = element_blank(), 
        panel.background = element_rect(fill = "#ECF0F5", color = "#ECF0F5"),
        plot.background = element_rect(fill = "#ECF0F5", color = "#ECF0F5"),
        panel.border = element_blank(),
      ) +  
      guides(fill = "none")
  })
  comparison_data_data_table_01 <- reactive({
    comp_stats = data %>% filter(name == input$pokeName31) %>%
      select(type_1, type_2, hp:speed,total_points) %>%
      mutate(type_1 = ifelse(type_1 =="fighting","fight",type_1)) %>%
      mutate(type_2 = ifelse(type_2 =="fighting","fight",type_2)) %>%
      mutate(type_2 = ifelse(is.na(type_2), " ", type_2)) %>%
      mutate(type_1 = type_labels[tolower(type_1)]) %>%
      mutate(type_2 = type_labels[tolower(type_2)]) %>%
      unite("types", type_1, type_2, sep = " ") %>%
      mutate(types = ifelse(str_sub(types, 1, 2) == "NA", str_sub(types, start = 3), types)) %>%
      mutate(types = ifelse(str_sub(types, -2) == "NA", str_sub(types, end = -3), types)) %>%
      gather("Statistic","Value")
  })
  output$pokemon_comparision_table_01 <- renderDataTable(
    datatable(data = comparison_data_data_table_01(),rownames = FALSE, escape = FALSE, options = list(
      searching = FALSE,  # Removes the search bar
      paging = FALSE,     # Removes pagination controls
      info = FALSE,       # Removes the info text
      lengthChange = FALSE  # Removes the length change dropdown
    ))
  )
   
  comparison_data_data_table_02 <- reactive({
    comp_stats = data %>% filter(name == input$pokeName32) %>%
      select(type_1, type_2, hp:speed,total_points) %>%
      mutate(type_1 = ifelse(type_1 =="fighting","fight",type_1)) %>%
      mutate(type_2 = ifelse(type_2 =="fighting","fight",type_2)) %>%
      mutate(type_2 = ifelse(is.na(type_2), " ", type_2)) %>%
      mutate(type_1 = type_labels[tolower(type_1)]) %>%
      mutate(type_2 = type_labels[tolower(type_2)]) %>%
      unite("types", type_1, type_2, sep = " ") %>%
      mutate(types = ifelse(str_sub(types, 1, 2) == "NA", str_sub(types, start = 3), types)) %>%
      mutate(types = ifelse(str_sub(types, -2) == "NA", str_sub(types, end = -3), types)) %>%
      gather("Statistic","Value")
  })
  output$pokemon_comparision_table_02 <- renderDataTable(
    datatable(data = comparison_data_data_table_02(),rownames = FALSE, escape = FALSE, options = list(
      searching = FALSE,  # Removes the search bar
      paging = FALSE,     # Removes pagination controls
      info = FALSE,       # Removes the info text
      lengthChange = FALSE  # Removes the length change dropdown
    ))
  )
}
