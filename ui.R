library(shiny)
library(bslib)
library(shinycssloaders)
library(shinydashboard)
library(DT)
library(plotly)

pokemon_selector <- function(num) {
  selectizeInput(paste("pokeName", num, sep = ""),
                 label = "Choose your pokemon:",
                 choices = NULL)
}

sidebar <- dashboardSidebar(sidebarMenu(
  menuItem(
    "Dashboard",
    tabName = "dashboard",
    icon = icon("dashboard")
  ),
  menuItem("National Pokédex", tabName = "widgets", icon = icon("th")),
  menuItem(
    "Pokemon Comparison",
    tabName = "comparison",
    icon = icon("th")
  )
))

# Pierwsza strona
## To kółko: Team Stats (Average)
## Barchart: Average Damage Received

# Druga strona:
## Ten szeroki na górze: Best Pokęmons by Type
## Ten na południowy zachód: Pokemon Type Frequency
## Ten na południowy wschod: 3 Best & Worst Pokemons

first_page <-       tabItem(tabName = "dashboard",
                            fluidPage(
                              tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
                              tags$script(src = "script.js"),
                              mainPanel(
                                width = 12,
                                fluidRow(
                                  class = "poke-row",
                                  column(
                                    4,
                                    class = "poke-col",
                                    card(class = "poke-card",
                                         pokemon_selector('01'),
                                         uiOutput("pokeOutput01"), )
                                  ),
                                  column(
                                    4,
                                    class = "poke-col",
                                    card(class = "poke-card",
                                         pokemon_selector('02'),
                                         uiOutput("pokeOutput02"))
                                  ),
                                  column(
                                    4,
                                    class = "poke-col",
                                    card(class = "poke-card",
                                         pokemon_selector('03'),
                                         uiOutput("pokeOutput03"))
                                  ),
                                  column(
                                    4,
                                    class = "poke-col",
                                    card(class = "poke-card",
                                         pokemon_selector('04'),
                                         uiOutput("pokeOutput04"))
                                  ),
                                  column(
                                    4,
                                    class = "poke-col",
                                    card(class = "poke-card",
                                         pokemon_selector('05'),
                                         uiOutput("pokeOutput05"))
                                  ),
                                  column(
                                    4,
                                    class = "poke-col",
                                    card(class = "poke-card",
                                         pokemon_selector('06'),
                                         uiOutput("pokeOutput06"))
                                  )
                                ),
                                fluidRow(
                                  column(
                                    width = 4,
                                    class = "plot-title",
                                    tags$text("Team Stats (Average)")
                                  ),
                                  column(
                                    width = 8,
                                    class = "plot-title",
                                    tags$text("Average Damage Received")
                                  )
                                ),
                                fluidRow(
                                  column(
                                    width = 4,
                                    plotOutput("avg_stat", width = "461px", height = "400px"),
                                    class = "avg-stat-plot"
                                  ),
                                  column(
                                    width = 8,
                                    plotOutput("avg_damage_taken"),
                                    class = "avg-damage-dealt-plot"
                                  )
                                )
                              )
                            ))
gens =  c(
  "Red/Green",
  "Gold/Silver",
  "Ruby/Sapphire",
  "Diamond/Pearl",
  "Black/White",
  "X/Y",
  "Sun/Moon",
  "Sword/Shield",
  "Scarlet/Violet"
)

second_page <- tabItem(tabName = "widgets",
                       fluidPage(
                         fluidRow(
                           class = "gen-choice-row",
                           column(
                             width = 2,
                             checkboxGroupInput(
                               "gen_radio_buttons",
                               "Generations:",
                               choices = gens[0:2],
                               selected = gens
                             )
                           ),
                           column(
                             width = 2,
                             checkboxGroupInput(
                               "gen_radio_buttonsB",
                               label = "",
                               choices = gens[3:4],
                               selected = gens
                             )
                           ),
                           column(
                             width = 2,
                             checkboxGroupInput(
                               "gen_radio_buttonsC",
                               label = "",
                               choices = gens[5:6],
                               selected = gens
                             )
                           ),
                           column(
                             width = 2,
                             checkboxGroupInput(
                               "gen_radio_buttonsD",
                               label = "",
                               choices = gens[7:8],
                               selected = gens
                             )
                           ),
                           column(
                             width = 2,
                             checkboxGroupInput(
                               "gen_radio_buttonsE",
                               label = "",
                               choices = gens[9:9],
                               selected = gens
                             )
                           ),
                         ),
                         
                         
                         dataTableOutput("national_table"),
                         fluidRow(
                           column(
                             width=12,
                             class = "plot-title",
                             tags$text("Best Pokémons by Type"),
                           )
                         ),
                         plotOutput("best_pokemon_by_type_histogram"),
                         fluidRow(
                           column(
                             width = 7,
                             class = "plot-title",
                             tags$text("Pokemon Type Frequency")
                           ),
                           column(
                             width = 5,
                             class = "plot-title",
                             tags$text("3 Best & Worst Pokémons")
                           )
                         ),
                         fluidRow(
                           column(
                             width = 7,
                             plotOutput("type_histogram"),
                             class = "best-pokemon-plot"
                           ),
                           column(
                             width = 5,
                             plotOutput("best_pokemon_histogram"),
                             class = "type-plot"
                           )
                         )
                       ))

third_page <- tabItem(tabName = "comparison",
                      fluidPage(
                        fluidRow(
                          column(
                            width = 4,
                            offset = 2,
                            card(class = "poke-card second_comp",
                                 pokemon_selector('31'),
                                 uiOutput("pokeOutput31"))
                          ),
                          column(
                            width = 4,
                            offset = 2,
                            card(class = "poke-card first_comp",
                                 pokemon_selector('32'),
                                 uiOutput("pokeOutput32"))
                          )
                        ),
                        fluidRow(
                          column(width = 4,
                                 dataTableOutput("pokemon_comparision_table_01")),
                          column(
                            width = 4,
                            plotlyOutput("comparison_plot", width = 670, height = 400),
                          ),
                          column(width = 4,
                                 dataTableOutput("pokemon_comparision_table_02"))
                        )
                      ))

dashboardPage(dashboardHeader(title = "Pokédex", tags$li(
  actionLink("openAbout", label = "", icon = icon("info")),
  class = "dropdown"
)),
sidebar,
dashboardBody(tabItems(first_page,
                       second_page,
                       third_page)))
