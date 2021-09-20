
# Loading Libs ------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(shinyalert)
library(shinyjs)
library(shinyWidgets)
library(shinycssloaders)
library(stringr)
library(shinybusy)
library(tidyverse)
library(here)
library(lubridate)
library(zoo)
library(ggtext)
library(glue)
library(ggiraph)
library(patchwork)
library(Rspotify)
library(broom)
library(spotifyr)
library(tidymodels)
library(janitor)
#windowsFonts(`Roboto Condensed`=windowsFont("Roboto Condensed"))
theme_set(theme_minimal(base_family = "Roboto Condensed",
                        base_size=12))

# Loading Necessary Data and Functions ------------------------------------

#Spotify API key
source(here("key.R"))
load(here("keys"))
#access_token <- get_spotify_access_token()

#Mandatory Fields
fieldsMandatory <- c("username", "playlists")

#Load liked songs data
liked_songs <- read_rds(here("data_publish/full_data.rds"))

#Load playlist data
playlists <- read_rds(here("data_publish/playlist_tracks.rds"))

#Load model
model_for_pred <- read_rds(here("model_publish/Model_Predict.rds"))


#Load helper functions
source(here("helper-functions_publish/all_songs_analysis/AllSongs_Plot_Function.R"))#Load all songs plot function
source(here("helper-functions_publish/playlists_analysis/Playlist_Plot.R")) #Load the playlists plot function
source(here("helper-functions_publish/Barplot_Function.R"))#Load barplot function
source(here("helper-functions_publish/Scatterplot_Function.R")) #Load scatterplot function
source(here("helper-functions_publish/Aggregate_Function.R")) #Load playlist aggregation function
source(here("helper-functions_publish/Input_Toggle.R")) # Toggle inputs
source(here("helper-functions_publish/Get_Artist_Songs.R"))
source(here("helper-functions_publish/Prediction_Function.R"))
source(here("helper-functions_publish/obtain-all-playlists.r")) # Get the list of playlists
source(here("helper-functions_publish/obtain-track-features.R")) # Get the track features



#Reactive values storage
RV <- reactiveValues()

# UI ----------------------------------------------------------------------


ui <- dashboardPage(skin = "green",
                    
                    # Dashboard Header --------------------------------------------------------
                    dashboardHeader(title = "Spotify Analysis"),
                    
                    # Dashboard Sidebar -------------------------------------------------------  
                    dashboardSidebar(
                      useShinyalert(),  # Set up shinyalert
                      useShinyjs(), #set up shinyjs
                      # Username Input ----------------------------------------------------------
                      textInput("username",label="Enter your Spotify Username or URI",placeholder = "Username or URI...",
                                value = "jakerocksalot"),
                      actionBttn("usernameclick","Search",style='minimal',size = "sm"),
                      selectizeInput("playlists",label="Choose which playlists to analyze",
                                     choices = "", multiple = T),
                      #Feature selection is the way to get the values, features is the id in the server side, relevant for disabling and enabling
                      checkboxInput("include_radios_mixes",label = "Include artist and song radio and mix playlists?",
                                    value = F),
                      actionBttn("analyze","Analyze!",style = "minimal")
                    ),
                    
                    # Dashboard Body ----------------------------------------------------------
                    dashboardBody(
                      setSliderColor("#1DB954", 2),
                      tabsetPanel(
                        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                        # All Songs Tab ---------------------------------------------------------
                        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                        tabPanel(title = "Explore Liked Songs",
                                 
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
                                   #Input Selection
                                   column(width = 4, style='padding-left:0px',
                                          box(width = 12,
                                              selectizeInput("main_variable_allsongs",label="Main Feature (for bar plot and y-axis of scatter)",
                                                             choices = c("Danceability"="danceability", "Date Song Saved"="Date_Song_Saved",
                                                                         "Duration"="minutes", "Energy"="energy", "Loudness"="loudness", 
                                                                         "Tempo"="tempo", "Track Popularity"="track_popularity", 
                                                                         "Track Release Date"="Track_Release_Date",  "Valence"="valence"),
                                                             selected="valence"),
                                              selectizeInput("comp_variable_allsongs",label="Comparison Feature (for x-axis of scatter)",
                                                             choices = c("Danceability"="danceability", "Date Song Saved"="Date_Song_Saved",
                                                                         "Duration"="minutes", "Energy"="energy", "Loudness"="loudness", 
                                                                         "Tempo"="tempo", "Track Popularity"="track_popularity", 
                                                                         "Track Release Date"="Track_Release_Date",  "Valence"="valence"),
                                                             selected="Date_Song_Saved"),
                                              sliderInput(inputId = "num_bars_allsongs", label = "How many tracks to show in bar plot (top and bottom):",
                                                          max = 7, min=3, value = 5, 
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
                        
                        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                        # Playlist Tab ---------------------------------------------------------
                        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                        # Compare Playlists Tab ---------------------------------------------------
                        tabPanel(title = "Explore Playlists",
                                 # Mainbody format ---------------------------------------------------------
                                 fluidRow(),
                                 fluidRow(
                                   #setSliderColor("#1DB954", sliderId = 1:10),
                                   #Input Selection
                                   column(width = 4, style='padding-left:0px',
                                          box(width = 12,
                                              selectizeInput("main_variable_playlist",label="Main Feature (for bar plot and y-axis of scatter)",
                                                             choices = c("Danceability"="danceability", "Date Song Saved"="Date_Song_Saved",
                                                                         "Duration"="minutes", "Energy"="energy", "Loudness"="loudness", 
                                                                         "Tempo"="tempo", "Track Popularity"="track_popularity", 
                                                                         "Track Release Date"="Track_Release_Date",  "Valence"="valence"),
                                                             selected="valence"),
                                              selectizeInput("comp_variable_playlist",label="Comparison Feature (for x-axis of scatter)",
                                                             choices = c("Danceability"="danceability", "Date Song Saved"="Date_Song_Saved",
                                                                         "Duration"="minutes", "Energy"="energy", "Loudness"="loudness", 
                                                                         "Tempo"="tempo", "Track Popularity"="track_popularity", 
                                                                         "Track Release Date"="Track_Release_Date",  "Valence"="valence"),
                                                             selected="energy"),
                                              selectizeInput("method_playlist",label="Method:",
                                                             choices = c("Compare Playlists"="compare_playlists",
                                                                         "Compare Songs Within Selected Playlist"="songs_within_playlist",
                                                                         "Compare Songs Across Selected Playlists"="songs_across_playlists"),
                                                             selected="compare_playlists"),
                                              selectizeInput(inputId = "playlist_of_interest",
                                                             label="Which playlist do you want to analyze the songs of:",
                                                             choices = ""),
                                              selectizeInput(inputId = "across_playlists_playlists", label= "Which playlist do you want to analyze the songs of? (Max is 5)", 
                                                             choices = "",
                                                             options = list(maxItems = 4)),
                                              sliderInput(inputId = "num_bars_playlist", label = "How many bars to show in bar plot:",
                                                          max = 7, min=3, value = 5, 
                                                          step = 1, round = T),
                                              actionButton("playlists_plot_go","View")
                                              
                                          )),
                                   #Figure
                                   column(width = 8, style='padding-left:0px',
                                          box(width=12,
                                              withSpinner(girafeOutput("Playlists_Plot"),type = 6,color = "#1DB954")
                                          ))
                                 ))
                      )
                    )
)


# SERVER ------------------------------------------------------------------


server <- function(input, output, session) {
  shinyalert(title = "Welcome!",
             text = "Enter your Spotify Username or URI and click search to get started",
             type = "info")
  ## initialize reactive values, which is a named list that will hold my data objects
  data <- reactiveValues(user_playlists = NULL)
  
  #Disable exploration UI until data is collected
  input_toggle(enable_or_disable = "disable")
  
  #Initially disable playlist selection
  shinyjs::disable("playlists")
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Show and hide playlist to analyze input ------------------------------------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  shinyjs::hide("playlist_of_interest")
  
  #For comparing songs within playlist
  observe({
    if (input$method_playlist=="songs_within_playlist") {
      shinyjs::show("playlist_of_interest")
      #update the playlist selection in the playlist tab itself,
      # this one for if the method is within playlist analysis of songs
      updateSelectizeInput(session, "playlist_of_interest",
                           label = "Choose which playlists to analyze",
                           choices = sort(data$user_playlists %>%
                                            filter(playlist_name %in% input$playlists) %>%
                                            pull(playlist_name)))
    } else{
      shinyjs::hide("playlist_of_interest")
      #Don't give any playlists in the playlist analysis tab either, for either
      # within or across playlist comparisons
      updateSelectizeInput(session, "playlist_of_interest",
                           label = "Choose which playlists to analyze",
                           choices = "")
    }
  })
  
  #For comparing songs across playlists
  observe({
    if (input$method_playlist=="songs_across_playlists") {
      shinyjs::show("across_playlists_playlists")
      #update the playlist selection in the playlist tab itself 
      # this one for if the method is across playist analysis of songs
      updateSelectizeInput(session, "across_playlists_playlists",
                           label = "Choose which playlists to analyze",
                           choices = sort(data$user_playlists %>%
                                            filter(playlist_name %in% input$playlists) %>%
                                            pull(playlist_name)),
                           options = list(maxItems = 4))
    } else{
      shinyjs::hide("across_playlists_playlists")
      #Don't give any playlists in the playlist analysis tab either, for either
      # within or across playlist comparisons
      updateSelectizeInput(session, "across_playlists_playlists",
                           label = "Choose which playlists to analyze",
                           choices = "")
    }
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Obtain playlists from username ---------------------------------------------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #Finding username, collecting data, enabling inputs  -------------------------
  observeEvent(input$usernameclick, {
    #This makes it so that we just have the username of the user
    if (str_detect(input$username,"spotify")==T) {
      user <- str_remove(input$username,"spotify:user:")
    } else {
      user <- input$username
    }
    
    #This brings a popup telling the user we are looking for the username
    showModal(modalDialog("Searching for username...", 
                          footer=NULL))
    #This gets the playlist
    data$user_playlists <- get_playlist_data(user_id = user,
                                        number_of_playlists = 200,
                                        include_radios_mixes = input$include_radios_mixes)
    #This removes the loading popup
    removeModal()
    #This tells us whether the search was a success or not
    if(nrow(data$user_playlists)==0){
      shinyalert("Oops!", "Username not found, please try again.", type = "error")
      #If username not found, don't give any options for playlists in sidebar
      updateSelectizeInput(session, "playlists",
                           label = "Choose which playlists to analyze",
                           choices = "")
      
      #Disable all UI
      input_toggle(enable_or_disable = "disable")
      #If there are no playlists, don't enable to buttons
      shinyjs::disable("playlists")
      
    } else {
      shinyalert("Success!", "Username found", type = "success")
      
      #Update the playlist selections in the side bar
      updateSelectizeInput(session, "playlists",
                           label = "Choose which playlists to analyze",
                           choices = sort(data$user_playlists$playlist_name))
      
      #Enabling playlist selection and analyze button if and only if 
      # a username is found
      shinyjs::enable("playlists")
    }
  })
  
  #Checking if mandatory fields are filled in. Got this code from Dean Attali, need to figure out how it works
  observe({
    # check if all mandatory fields have a value
    mandatoryFilled <-
      vapply(fieldsMandatory,
             function(x) {
               !is.null(input[[x]]) && input[[x]] != ""
             },
             logical(1))
    mandatoryFilled <- all(mandatoryFilled)
    
    # enable/disable the submit button
    shinyjs::toggleState(id = "analyze", condition = mandatoryFilled)
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Obtain track data from playlists -------------------------------------------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  observeEvent(input$analyze, {
    #This brings up a popup telling the user we are loading the track feature
    showModal(modalDialog("Loading track features.... this can take a few minutes for every 100 songs", 
                          footer=NULL))
    data$liked_songs <- obtain_track_features(playlists_of_int = input$playlists, 
                                              data = data$user_playlists)
    
    data$playlists <- data$liked_songs %>% 
      left_join(data$user_playlists, by = c("playlist_id" = "id"))
    
    if (nrow(data$liked_songs) >0) {
      shinyalert("Success!", "Song data collected", type = "success")
      #Enabling exploration UI if and only if a username is found and data is found
      #Now that data is loaded, enable the exploration UI
      input_toggle(enable_or_disable = "enable")
    } else{
      shinyalert("Failure", "Data could not be collected", type = "error")
    }
    
    #This gets rid of the loading popup
    removeModal()
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # All Songs Plot ---------------------------------------------------------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #Initialize a reactive values list, which each entry being one of the inputs for the all songs 
  # function, set to NUL
  all_songs_plot_inputs<- reactiveValues(main_variable=NULL,
                                         comparison_variable=NULL,
                                         num_bars=NULL,
                                         data=NULL)
  
  #Once the "View" button is clicked, set the value of each of those entries within all_songs_plot_inputs to the 
  # user defined value
  observeEvent(input$all_songs_plot_go, {
    all_songs_plot_inputs$main_variable <- input$main_variable_allsongs
    all_songs_plot_inputs$comparison_variable <- input$comp_variable_allsongs
    all_songs_plot_inputs$num_bars <- input$num_bars_allsongs
    all_songs_plot_inputs$liked_songs <- data$liked_songs
  })
  
  #Plug the inputs from all_songs_plot_inputs into the All_Songs_Plot function
  output$All_Songs_Plot <- renderGirafe({
    req(!is.null(all_songs_plot_inputs$liked_songs))
    AllSongs_Plot(main_variable = all_songs_plot_inputs$main_variable, 
                  comparison_variable = all_songs_plot_inputs$comparison_variable,
                  how_many = all_songs_plot_inputs$num_bars, 
                  data = all_songs_plot_inputs$liked_songs)
    #}  
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Playlists Plot ---------------------------------------------------------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #Initialize a reactive values list, which each entry being one of the inputs for the all songs 
  # function, set to NUL
  playlists_plot_inputs<- reactiveValues(main_variable=NULL,
                                         comparison_variable=NULL,
                                         num_bars=NULL,
                                         method=NULL,
                                         playlist_of_interest=NULL,
                                         data=NULL,
                                         bar_playlist_or_track=NULL,
                                         scatter_playlist_or_track=NULL,
                                         playlists_to_compare=NULL
  )
  
  #Once the "View" button is clicked, set the value of each of those entries within playlists_plot_inputs to the 
  # user defined value
  observeEvent(input$playlists_plot_go, {
    #All these are just whatever the user decides
    playlists_plot_inputs$main_variable <- input$main_variable_playlist
    playlists_plot_inputs$comparison_variable <- input$comp_variable_playlist
    playlists_plot_inputs$num_bars <- input$num_bars_playlist
    playlists_plot_inputs$method <- input$method_playlist
    playlists_plot_inputs$data <- data$playlists
    
    #These depend on what the user chooses or method
    #If comparing playlists
    if (input$method_playlist=="compare_playlists") {
      playlists_plot_inputs$bar_playlist_or_track <- "playlist"
      playlists_plot_inputs$scatter_playlist_or_track <- "playlist"
      
      #If comparing songs within playlist
    } else if (input$method_playlist=="songs_within_playlist") {
      playlists_plot_inputs$bar_playlist_or_track <- "track"
      playlists_plot_inputs$scatter_playlist_or_track <- "track"
      playlists_plot_inputs$playlist_of_interest <- input$playlist_of_interest
      
      #if comparing songs across playlists
    } else if (input$method_playlist=="songs_across_playlists") {
      playlists_plot_inputs$bar_playlist_or_track <- "track"
      playlists_plot_inputs$scatter_playlist_or_track <- "track"
      playlists_plot_inputs$playlists_to_compare <- input$across_playlists_playlists
      
    } 
  })
  
  #Plug the inputs from playlists_plot_inputs into the Playlist_Plot function
  output$Playlists_Plot <- renderGirafe({
    req(!is.null(playlists_plot_inputs$data))
    Playlist_Plot(main_variable = playlists_plot_inputs$main_variable, 
                  comparison_variable = playlists_plot_inputs$comparison_variable,
                  method = playlists_plot_inputs$method,
                  playlist_of_interest = playlists_plot_inputs$playlist_of_interest,
                  how_many = playlists_plot_inputs$num_bars, 
                  
                  bar_playlist_or_track = playlists_plot_inputs$bar_playlist_or_track,
                  scatter_playlist_or_track = playlists_plot_inputs$scatter_playlist_or_track,
                  
                  playlists_to_compare = playlists_plot_inputs$playlists_to_compare,
                  
                  data = playlists_plot_inputs$data)
  })
}

shinyApp(ui, server)
