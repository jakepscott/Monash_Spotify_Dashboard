
# Loading Libs ------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(shinyalert)
library(shinyjs)
library(shinyWidgets)
library(shinycssloaders)
library(stringr)
library(Rspotify)
library(shinybusy)
library(tidyverse)
library(here)
library(lubridate)
library(zoo)
library(ggtext)
library(glue)
library(ggiraph)
windowsFonts(`Roboto Condensed`=windowsFont("Roboto Condensed"))
theme_set(theme_minimal(base_family = "Roboto Condensed",
                        base_size=12))

# Loading Necessary Data and Functions ------------------------------------
source(here("key.R"))
data <- read_rds(here("data/full_data.rds"))
source(here("helper-functions/all_songs_analysis/Bar-Plot_Function.R"))
source(here("helper-functions/all_songs_analysis/Scatter-Plot_Function.R"))
#Mandatory Fields
fieldsMandatory <- c("username", "playlists","features")

#Reactive values storage
RV <- reactiveValues()

# UI ----------------------------------------------------------------------


ui <- dashboardPage(skin = "green",
                    # Dashboard Header --------------------------------------------------------
                    dashboardHeader(title = "Spotify Song Analysis"),
                    
                    # Dashboard Sidebar -------------------------------------------------------  
                    dashboardSidebar(
                      useShinyalert(),  # Set up shinyalert
                      useShinyjs(), #set up shinyjs
                      # Username Input ----------------------------------------------------------
                      textInput("username",label="Enter your Spotify Username or URI",placeholder = "Username or URI...",
                                value = "jakerocksalot"),
                      actionBttn("usernameclick","Search",style='minimal',size = "sm")
                    ),
                    
                    # Dashboard Body ----------------------------------------------------------
                    dashboardBody(
                      tabsetPanel(
                        # Overview tab -------------------------------------------------------------
                        tabPanel(title = "Liked Songs",
                                 
                                 # Fixing the header color -------------------------------------------------
                                 ##This fixes this the background color of the header to be spotify green. To be honest it is css and idk how it works. 
                                 #Need to learn. Got it from https://stackoverflow.com/questions/31711307/how-to-change-color-in-shiny-dashboard
                                 tags$head(tags$style(HTML('
                                                           /*logo*/
                                                           .skin-blue .main-header .logo {
                                                           background-color: #1DB954;
                                                           }
                                                           
                                                           /* navbar (rest of the header) */
                                                           .skin-blue .main-header .navbar {
                                                           background-color: #1DB954;
                                                           }        
                                                           
                                                           /* main sidebar */
                                                           .skin-grey .main-sidebar {
                                                           background-color: #191414;
                                                           }
                                                           
                                                           '))),
                                 
                                 # Mainbody format ---------------------------------------------------------
                                 fluidRow(),
                                 fluidRow(
                                   setSliderColor("#1DB954", 1),
                                   #Input Selection
                                   column(width = 4, style='padding-left:0px',
                                          box(width = 12,
                                              selectizeInput("barplot_variable_allsongs",label="Which feature would you see highest and lowest tracks for?",
                                                             choices = c("Danceability"="danceability", "Duration"="minutes",
                                                                         "Energy"="energy", "Loudness"="loudness", "Tempo"="tempo", 
                                                                         "Track Popularity"="track_popularity", "Valence"="valence")),
                                              sliderInput(inputId = "num_tracks_barplot_allsongs", label = "Show top and bottom X values:",
                                                          max = 10, min=3, value = 10, 
                                                          step = 1, round = T) 
                                          )),
                                   #Figure
                                   column(width = 8, style='padding-left:0px', 
                                          box(width=12,
                                              withSpinner(girafeOutput("All_Songs_Barplot"),type = 6,color = "#1DB954")
                                          ))
                                 ),
                                 
                                 fluidRow(
                                   #Input Selection
                                   column(width = 4, style='padding-left:0px',
                                          box(width = 12,
                                              selectizeInput("scatterplot_xaxis_allsongs",label="X Axis:",
                                                             choices = c("Danceability"="danceability", "Duration"="minutes",
                                                                         "Energy"="energy", "Loudness"="loudness", "Tempo"="tempo", 
                                                                         "Track Popularity"="track_popularity", "Valence"="valence"),
                                                             selected="danceability"),
                                              selectizeInput("scatterplot_yaxis_allsongs",label="Y Axis:",
                                                             choices = c("Danceability"="danceability", "Duration"="minutes",
                                                                         "Energy"="energy", "Loudness"="loudness", "Tempo"="tempo", 
                                                                         "Track Popularity"="track_popularity", "Valence"="valence"),
                                                             selected="valence")
                                          )),
                                   #Figure
                                   column(width = 8, style='padding-left:0px', 
                                          box(width=12,
                                              withSpinner(girafeOutput("All_Songs_Scatterplot"),type = 6,color = "#1DB954")
                                          ))
                                 )
                        ),
                        
                        # Compare Playlists Tab ---------------------------------------------------
                        tabPanel(title = "Analyze a Playlist")
                      )
                    )
)


# SERVER ------------------------------------------------------------------


server <- function(input, output, session) {
  ## initialize reactive values
  obj <- reactiveValues()
  
  #Initially disable all functions until a username has been found
  shinyjs::disable("playlists")
  
  #Disable exploration UI until data is collected
  shinyjs::disable("barplot_variable_allsongs")
  shinyjs::disable("num_tracks_barplot_allsongs")
  shinyjs::disable("scatterplot_xaxis_allsongs")
  shinyjs::disable("scatterplot_yaxis_allsongs")
  
  #generating playlist selection function -------------------------------------------------------
  observeEvent(input$usernameclick, {
    #This makes it so that we just have the username of the user
    if (str_detect(input$username,"spotify")==T) {
      user <- str_remove(input$username,"spotify:user:")
    } else {
      user <- input$username
    }
    #This tells us whether the search was a success or not
    if(input$username!="jakerocksalot"){
      shinyalert("Oops!", "Username not found, please try again.", type = "error")
      updateSelectizeInput(session, "playlists",
                           label = "Choose which playlists to analyze",
                           choices = "")
    } else {
      shinyalert("Success!", "Username found", type = "success")
      #Enabling playlist and feature selection if and only if a username is found
      #Now that data is loaded, enable the exploration UI
      shinyjs::enable("barplot_variable_allsongs")
      shinyjs::enable("num_tracks_barplot_allsongs")
      shinyjs::enable("scatterplot_xaxis_allsongs")
      shinyjs::enable("scatterplot_yaxis_allsongs")
      
      #Saving the data into a reactive object housed in obj
      obj$full_data <- data
    }
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # All Songs Bar Plot ---------------------------------------------------------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$All_Songs_Scatterplot <- renderGirafe({
    if(!is.null(obj$full_data)==T){
      req(input$scatterplot_xaxis_allsongs)
      song_scatter_plot_function(input$scatterplot_xaxis_allsongs, input$scatterplot_yaxis_allsongs, obj$full_data)
    }
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # All Songs Scatter Plot ---------------------------------------------------------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$All_Songs_Barplot <- renderGirafe({
    if(!is.null(obj$full_data)==T){
      req(input$barplot_variable_allsongs)
      song_bar_plot_function(input$barplot_variable_allsongs, input$num_tracks_barplot_allsongs, obj$full_data)
    }
  })
}

shinyApp(ui, server)
