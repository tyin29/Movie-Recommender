## ui.R

# install.packages("devtools")
# devtools::install_github("stefanwilhelm/ShinyRatingInput")

library(shiny)
library(shinydashboard)
library(recommenderlab)
library(data.table)
library(ShinyRatingInput)
library(shinyjs)

source('functions/helpers.R')

shinyUI(
    dashboardPage(
          skin = "blue",
          dashboardHeader(title = "Movie Recommender"),
          
          dashboardSidebar(
            sidebarMenu(
              # Setting id makes input$tabs give the tabName of currently-selected tab
              id = "tabs",
              menuItem("Recommendation by Genre", tabName = "genre", icon = icon("dashboard")),
              menuItem("Recommendation by Rating", icon = icon("dashboard"), tabName = "rating")
            )
          ),
          
          dashboardBody(includeCSS("css/movies.css"),
              tabItems(
                tabItem(
                  tabName = "genre",
                  fluidRow(
                    box(width = 12, title = "Step 1: Select your favorite genre", status = "info", solidHeader = TRUE, collapsible = TRUE,
                        div(class = "genreitems",
                            uiOutput('genres_dropdown')
                        )
                    )
                  ),
                  
                  fluidRow(
                    useShinyjs(),
                    box(
                      width = 12, status = "info", solidHeader = TRUE,
                      title = "Step 2: Movies you may like",
                      br(),
                      withBusyIndicatorUI(
                        actionButton("button_Genre", "Click here to get your recommendations", class = "btn-warning")
                      ),
                      br(),
                      tableOutput("results_by_genre")
                    )
                  )
                ),
                
                tabItem(
                  tabName = "rating",
                  fluidRow(
                    box(width = 12, title = "Step 1: Rate the popular movies as many as possible", status = "info", solidHeader = TRUE, collapsible = TRUE,
                        div(class = "rateitems",
                            uiOutput('ratings')
                        )
                    )
                  ),
                  fluidRow(
                    useShinyjs(),
                    box(
                      width = 12, status = "info", solidHeader = TRUE,
                      title = "Step 2: Discover movies you might like",
                      br(),
                      withBusyIndicatorUI(
                        actionButton("btnRating", "Click here to get your recommendations", class = "btn-warning")
                      ),
                      br(),
                      tableOutput("results")
                    )
                  )
                )
              )
          )
    )
) 
