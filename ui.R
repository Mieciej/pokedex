library(shiny)
library(bslib)
library(shinycssloaders)
library(shinydashboard)

pokemon_selector <- function(num) {
  selectizeInput(paste("pokeName", num, sep = ""), label = "Choose your pokemon:", choices = NULL)
}

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Widgets", tabName = "widgets", icon = icon("th"))
  )
)

first_page <-       tabItem(tabName = "dashboard",
                            fluidPage(
                              tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
                              tags$script(src = "script.js"),
                              mainPanel(width=12,
                                        fluidRow(class = "poke-row",
                                                 column(4, class = "poke-col",
                                                        card(
                                                          class = "poke-card",
                                                          pokemon_selector('01'),
                                                          uiOutput("pokeOutput01"),
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
                            )
)

second_page <- tabItem(tabName = "widgets",
                       fluidPage(
                         plotOutput("avg_damage_taken")
                       )
)

dashboardPage(
  dashboardHeader(title = "Pokédex"),
  sidebar,
  dashboardBody(
    tabItems(
      first_page,
      second_page
    )
  )
)

