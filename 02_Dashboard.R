
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
library(patchwork)
windowsFonts(`Roboto Condensed`=windowsFont("Roboto Condensed"))
theme_set(theme_minimal(base_family = "Roboto Condensed",
                        base_size=12))

# Loading Necessary Data and Functions ------------------------------------
#Spotify API key
source(here("key.R"))
#Load my data
data <- read_rds(here("data/full_data.rds"))
#Load all songs plot function
source(here("helper-functions/all_songs_analysis/Combined_Bar_Scatter_Plot_Function.R"))
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
                                              selectizeInput("main_variable_allsongs",label="Main Feature (for bar plot and y-axis of scatter)",
                                                             choices = c("Danceability"="danceability", "Duration"="minutes",
                                                                         "Energy"="energy", "Loudness"="loudness", "Tempo"="tempo", 
                                                                         "Track Popularity"="track_popularity", "Valence"="valence"),
                                                             selected="danceability"),
                                              selectizeInput("comp_variable_allsongs",label="Comparison Feature (for x-axis of scatter)",
                                                             choices = c("Danceability"="danceability", "Duration"="minutes",
                                                                         "Energy"="energy", "Loudness"="loudness", "Tempo"="tempo", 
                                                                         "Track Popularity"="track_popularity", "Valence"="valence"),
                                                             selected="valence"),
                                              sliderInput(inputId = "num_tracks_barplot_allsongs", label = "How many tracks to show in bar plot (top and bottom):",
                                                          max = 10, min=3, value = 10, 
                                                          step = 1, round = T),
                                              actionButton("all_songs_plot_go","View")
                                              
                                          )),
                                   #Figure
                                   column(width = 8, style='padding-left:0px', 
                                          box(width=12,
                                              withSpinner(girafeOutput("All_Songs_Plot"),type = 6,color = "#1DB954")
                                          ))
                                 ),
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
  shinyjs::disable("main_variable_allsongs")
  shinyjs::disable("comp_variable_allsongs")
  shinyjs::disable("num_tracks_barplot_allsongs")
  
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
      shinyjs::enable("main_variable_allsongs")
      shinyjs::enable("comp_variable_allsongs")
      shinyjs::enable("num_tracks_barplot_allsongs")
      
      #Saving the data into a reactive object housed in obj
      obj$full_data <- data
    }
  })
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # All Songs Bar Plot ---------------------------------------------------------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  all_songs_plot_inputs<- reactiveValues(main_variable=NULL,
                                         comparison_variable=NULL,
                                         num_tracks=NULL,
                                         data=NULL)
  observeEvent(input$all_songs_plot_go, {
    all_songs_plot_inputs$main_variable <- input$main_variable_allsongs
    all_songs_plot_inputs$comparison_variable <- input$comp_variable_allsongs
    all_songs_plot_inputs$num_tracks <- input$num_tracks_barplot_allsongs
    all_songs_plot_inputs$data <- obj$full_data
  })
  
  output$All_Songs_Plot <- renderGirafe({
    req(!is.null(all_songs_plot_inputs$data))
      all_songs_function_plot(main_variable = all_songs_plot_inputs$main_variable, 
                              comparison_variable = all_songs_plot_inputs$comparison_variable,
                              how_many = all_songs_plot_inputs$num_tracks, 
                              data = all_songs_plot_inputs$data)
    #}  
    })
}

shinyApp(ui, server)
