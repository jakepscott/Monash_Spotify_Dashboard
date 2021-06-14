
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
windowsFonts(`Roboto Condensed`=windowsFont("Roboto Condensed"))
theme_set(theme_minimal(base_family = "Roboto Condensed",
                        base_size=12))

# Loading Necessary Data and Functions ------------------------------------

#Spotify API key
source(here("key.R"))
load(here("data/keys"))

#Load liked songs data
liked_songs <- read_rds(here("data/full_data.rds"))

#Load playlist data
playlists <- read_rds(here("data/playlist_tracks.rds"))

#Load model
# pop_songs_data <- read_rds(here("data/Full_Popular_Songs.rds"))
# pop_songs_data <- pop_songs_data %>% 
#   distinct(track_id,.keep_all = T) %>% 
#   na.omit()
# 
# #Taking all but the hold out data
# full_data <- pop_songs_data %>% 
#   #Remove identified columns
#   select(-contains("id"), -contains('name')) %>%
#   #Make logical outcome into character
#   mutate(liked=case_when(liked==T~"Liked",
#                          liked==F~"Not Liked")) %>% 
#   #make character columns factors
#   mutate(across(where(is.character), as.factor)) %>% 
#   mutate(liked=as.factor(liked)) %>% 
#   select(-type) %>% 
#   mutate(track_release_date=as.numeric(track_release_date)) %>% 
#   mutate(key=as.factor(key),
#          mode=as.factor(mode),
#          time_signature=as.factor(time_signature)) %>% 
#   mutate(across(where(is.numeric),as.numeric)) %>% 
#   select(-genres)

model_for_pred <- read_rds(here("model/model_for_app_prediction.rds"))


#Load keys
load(here("data/keys"))

#Load helper functions
source(here("helper-functions/all_songs_analysis/AllSongs_Plot_Function.R"))#Load all songs plot function
source(here("helper-functions/playlists_analysis/Playlist_Plot.R")) #Load the playlists plot function
source(here("helper-functions/Barplot_Function.R"))#Load barplot function
source(here("helper-functions/Scatterplot_Function.R")) #Load scatterplot function
source(here("helper-functions/Aggregate_Function.R")) #Load playlist aggregation function
source(here("helper-functions/Input_Toggle.R")) # Toggle inputs
source(here("helper-functions/Get_Artist_Songs.R"))
source(here("helper-functions/Prediction_Function.R"))

#Mandatory Fields
fieldsMandatory <- c("username", "playlists","features")

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
                      actionBttn("usernameclick","Search",style='minimal',size = "sm")
                    ),
                    
                    # Dashboard Body ----------------------------------------------------------
                    dashboardBody(
                      setSliderColor("#1DB954", 2),
                      tabsetPanel(
                        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                        # All Songs Tab ---------------------------------------------------------
                        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                        # Overview tab -------------------------------------------------------------
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
                                                             selected="danceability"),
                                              selectizeInput("comp_variable_allsongs",label="Comparison Feature (for x-axis of scatter)",
                                                             choices = c("Danceability"="danceability", "Date Song Saved"="Date_Song_Saved",
                                                                         "Duration"="minutes", "Energy"="energy", "Loudness"="loudness", 
                                                                         "Tempo"="tempo", "Track Popularity"="track_popularity", 
                                                                         "Track Release Date"="Track_Release_Date",  "Valence"="valence"),
                                                             selected="valence"),
                                              sliderInput(inputId = "num_bars_allsongs", label = "How many tracks to show in bar plot (top and bottom):",
                                                          max = 10, min=3, value = 5, 
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
                                                             selected="danceability"),
                                              selectizeInput("comp_variable_playlist",label="Comparison Feature (for x-axis of scatter)",
                                                             choices = c("Danceability"="danceability", "Date Song Saved"="Date_Song_Saved",
                                                                         "Duration"="minutes", "Energy"="energy", "Loudness"="loudness", 
                                                                         "Tempo"="tempo", "Track Popularity"="track_popularity", 
                                                                         "Track Release Date"="Track_Release_Date",  "Valence"="valence"),
                                                             selected="valence"),
                                              selectizeInput("method_playlist",label="Method:",
                                                             choices = c("Compare Playlists"="compare_playlists",
                                                                         "Compare Songs Within Selected Playlist"="songs_within_playlist",
                                                                         "Compare Songs Across Selected Playlists"="songs_across_playlists"),
                                                             selected="compare_playlists"),
                                              selectizeInput(inputId = "playlist_of_interest",
                                                             label="Which playlist do you want to analyze the songs of:",
                                                             choices=(playlists %>% 
                                                                        group_by(playlist_name) %>% 
                                                                        mutate(tracks=n()) %>% 
                                                                        ungroup() %>% 
                                                                        filter(tracks>10) %>% 
                                                                        distinct(playlist_name) %>%
                                                                        arrange(playlist_name) %>% 
                                                                        pull(playlist_name)
                                                             )),
                                              selectizeInput(inputId = "across_playlists_playlists", label= "Which playlist do you want to analyze the songs of? (Max is 5)", 
                                                             (playlists %>% 
                                                                group_by(playlist_name) %>% 
                                                                mutate(tracks=n()) %>% 
                                                                ungroup() %>% 
                                                                filter(tracks>10) %>% 
                                                                distinct(playlist_name) %>%
                                                                arrange(playlist_name) %>% 
                                                                pull(playlist_name)), options = list(maxItems = 4)),
                                              sliderInput(inputId = "num_bars_playlist", label = "How many bars to show in bar plot:",
                                                          max = 10, min=3, value = 5, 
                                                          step = 1, round = T),
                                              actionButton("playlists_plot_go","View")
                                              
                                          )),
                                   #Figure
                                   column(width = 8, style='padding-left:0px',
                                          box(width=12,
                                              withSpinner(girafeOutput("Playlists_Plot"),type = 6,color = "#1DB954")
                                          ))
                                 )),
                        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                        # Model Tab ---------------------------------------------------------
                        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                        tabPanel(title = "Recommendation Model",
                                 infoBox("Model Accuracy", "77.1%", subtitle = "Proportion of the model's predictions that are correct",
                                         icon = icon("search"), fill = TRUE, color = "green"),
                                 infoBox("Model Sensitiviy",subtitle = "Proportion of liked songs the model correctly identifies", 
                                         "68.5%", icon = icon("thumbs-up"), fill = TRUE,  color = "green"),
                                 infoBox("Model Specificity",subtitle = "Proportion of non-liked songs the model correctly identifies", 
                                         "77.4%", icon = icon("thumbs-down"), fill = TRUE,  color = "green"),
                                 fluidRow(),
                                 fluidRow(
                                   #setSliderColor("#1DB954", sliderId = 1:10),
                                   #Input Selection
                                   column(width = 4, style='padding-left:0px',
                                          box(width = 12,
                                              textInput("artist_for_song_pred",label="Enter an artist to pick a song from",placeholder = "Artist"),
                                              actionButton("artist_for_song_pred_go","Search for Artist Songs"),
                                              selectizeInput(inputId = "songs_to_pred_choice", label="Select song to predict whether I'd like:",
                                                             multiple=FALSE, choices=c("")),
                                              actionButton("song_pred_go","Predict")
                                              ),
                                          ),
                                   column(width = 8, style='padding-left:0px',
                                          box(width=12,
                                              withSpinner(htmlOutput("prediction"),type = 6,color = "#1DB954")#,
                                              # tags$head(tags$style("#prediction{color: #1DB954;
                                              #                      font-size: 30px;
                                              #                      font-style: italic;
                                              #                      }"
                                              # )
                                              # ),
                                              )
                                          )
                                   ),
                                 fluidRow(#Figure
                                   column(width = 12, style='padding-left:0px',
                                          box(width=12,
                                              withSpinner(girafeOutput("Model_Plot"),type = 6,color = "#1DB954")
                                          )))
                                )
                      )
                    )
)


# SERVER ------------------------------------------------------------------


server <- function(input, output, session) {
  shinyalert(title = "Welcome!",
             text = "Enter your Spotify Username or URI and click search to get started. For now, \"jakerocksalot\" is only the option.",
             type = "info")
  ## initialize reactive values, which is a named list that will hold my data objects
  data <- reactiveValues()
  
  #Disable exploration UI until data is collected
  input_toggle(enable_or_disable = "disable")
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Show and hide playlist to analyze input ------------------------------------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  shinyjs::hide("playlist_of_interest")
  
  #For comparing songs within playlist
  observe({
    if (input$method_playlist=="songs_within_playlist") {
      shinyjs::show("playlist_of_interest")
    } else{
      shinyjs::hide("playlist_of_interest")
    }
  })
  
  #For comparing songs across playlists
  observe({
    if (input$method_playlist=="songs_across_playlists") {
      shinyjs::show("across_playlists_playlists")
    } else{
      shinyjs::hide("across_playlists_playlists")
    }
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Code for Song Prediction ------------------------------------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  shinyjs::hide("songs_to_pred_choice")
  shinyjs::hide("song_pred_go")
  
  prediction_inputs <- reactiveValues(artist_data=NULL,
                                      track_name_choice=NULL,
                                      model=model_for_pred)
  prediction_output <- reactiveValues(prediction=NULL,
                                      song=NULL,
                                      artist=NULL)
  
  #Update songs select input
  observeEvent(input$artist_for_song_pred_go, {
    prediction_inputs$artist_data <- get_artist_songs(input$artist_for_song_pred)
    
    if (nrow(prediction_inputs$artist_data)==0) {
      shinyalert("Oops!", "Artist not found, please try again.", type = "error")
      shinyjs::hide("songs_to_pred_choice")
      shinyjs::hide("song_pred_go")
    } else {
      shinyalert("Success!", "Artist found.", type = "success")
      shinyjs::hide("songs_to_pred_choice")
      shinyjs::hide("song_pred_go")
      songs <- prediction_inputs$artist_data %>% arrange(track_name) %>% pull(track_name)
      shinyjs::show("songs_to_pred_choice")
      shinyjs::show("song_pred_go")
      
      
      updateSelectizeInput(session, "songs_to_pred_choice", label = "Select song to predict whether I'd like:", choices = songs)
    }
  })
  
  #Predict song
  observeEvent(input$song_pred_go, {
    prediction_inputs$track_name <- input$songs_to_pred_choice
    prediction_output$prediction <- predict_like(track_name_choice = prediction_inputs$track_name,
                        data = prediction_inputs$artist_data,
                        model = prediction_inputs$model)
    prediction_output$song <- input$songs_to_pred_choice
    prediction_output$artist <- input$artist_for_song_pred
    })
  
  output$prediction <- renderText({
    Sys.sleep(0.5)
    result <- ifelse(prediction_output$prediction=="Liked",
                     "like",
                     "not like")
    song <- prediction_output$song
    artist <- prediction_output$artist
    HTML(glue("<font size='5' face='arial' color='#1DB954'>The model predicts I would <b><i>{result}</i></b> {song} by {artist} </font>"))
  }
  )

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Obtain data from username ---------------------------------------------------------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #Finding username, collecting data, enabling inputs  -------------------------
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
      input_toggle(enable_or_disable = "disable")
    } else {
      shinyalert("Success!", "Username found", type = "success")
      
      #Enabling exploration UI if and only if a username is found
      #Now that data is loaded, enable the exploration UI
      input_toggle(enable_or_disable = "enable")
      
      #Saving the data into a reactive object housed in obj
      data$liked_songs <- liked_songs
      data$playlists <- playlists 
    }
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
  
  #Plug the inputs from all_songs_plot_inputs into the AllSongs_Plot function
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
    playlists_plot_inputs$playlist_of_interest <- input$playlist_of_interest
    playlists_plot_inputs$playlists_to_compare <- input$across_playlists_playlists
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
    #if comparing songs across playlists
    } else if (input$method_playlist=="songs_across_playlists") {
      playlists_plot_inputs$bar_playlist_or_track <- "track"
      playlists_plot_inputs$scatter_playlist_or_track <- "track"
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
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Model Plot ---------------------------------------------------------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$Model_Plot <- renderGirafe({
    data_final <- readRDS(here("model/logistic_model.rds"))
    plot <- data_final %>% 
        pull(.workflow) %>% 
        pluck(1) %>% 
        tidy() %>% 
        filter(!str_detect(term,"genre"), 
               term!="(Intercept)") %>% 
        mutate(term=str_replace_all(term,"_"," "),
               term=str_to_title(term)) %>% 
        rowwise() %>% 
        mutate(up_or_down = case_when(p.value>0.1 ~ "is not statistically significantly \nassociated with me liking a song",
                                      estimate>0 & p.value<0.1 ~ "is positively associated \nwith me liking a song",
                                      estimate<0 & p.value<0.1 ~ "is negatively associated \nwith me liking a song"),
               label=glue("{term} {up_or_down} \n Estimate: {round(estimate,2)} \n p-Value: {round(p.value,4)}")) %>% 
        ungroup() %>% 
        ggplot(aes(estimate, fct_reorder(term,estimate))) +
        geom_vline(xintercept = 0, color="grey70", linetype="dashed", size=1) +
        geom_errorbar(aes(xmin=estimate-std.error,
                          xmax=estimate+std.error),
                      width=0.2, alpha=0.7) +
        geom_point_interactive(aes(tooltip=label)) +
        labs(y=NULL,
             x="Coefficient",
             title="I am more likely to like a speechy and energetic song, less likely to \nlike a long song from a popular artist",
             subtitle = "Estimates from logistic classification model, thus one should focus on direction and \nmagnitude. Positive means more likely, negative means less likely") +
        theme_minimal() +
        theme(plot.title.position = "plot",
              plot.title = element_text(size=rel(1.25)),
              plot.subtitle = element_text(size=rel(1)),
              axis.text.y = element_text(size=rel(1)))
    
    girafe(ggobj = plot)
  })
}

shinyApp(ui, server)
