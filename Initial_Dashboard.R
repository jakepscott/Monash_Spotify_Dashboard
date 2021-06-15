
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
#windowsFonts(`Roboto Condensed`=windowsFont("Roboto Condensed"))
theme_set(theme_minimal(base_family = "Roboto Condensed",
                        base_size=12))

# Loading Necessary Data and Functions ------------------------------------

#Spotify API key
source(here("key.R"))
load(here("keys"))

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
                        # Welcome Page ---------------------------------------------------------
                        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                        # Overview tab -------------------------------------------------------------
                        tabPanel(title = "Welcome!",
                                 box(width=12,
                                     htmlOutput("welcome_page"))
                                 
                        ),
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
                                                          max = 7, min=3, value = 5, 
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
                                 infoBox("Model Sensitiviy",subtitle = "Proportion of liked songs the model correctly identifies as liked", 
                                         "68.5%", icon = icon("thumbs-up"), fill = TRUE,  color = "green"),
                                 infoBox("Model Specificity",subtitle = "Proportion of non-liked songs the model correctly identifies as not liked", 
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
                        ),
                        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                        # Methodology Tab ---------------------------------------------------------
                        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                        # Overview tab -------------------------------------------------------------
                        tabPanel(title = "Methodology",
                                 box(width=12,
                                     htmlOutput("method_tab"))
                                 
                        ),
                        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                        # Reference Tab ---------------------------------------------------------
                        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                        # Reference tab -------------------------------------------------------------
                        tabPanel(title = "References",
                                 box(width=12,
                                     htmlOutput("reference_tab"))
                                 
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
  # Set Up Welcome Page ------------------------------------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$welcome_page <- renderText({
    HTML(glue("<p dir='ltr' style='line-height:1.656;margin-top:0pt;margin-bottom:0pt;'><span style='font-size:12pt;font-family:'Times New Roman';color:#000000;background-color:transparent;font-weight:400;font-style:normal;font-variant:normal;text-decoration:none;vertical-align:baseline;white-space:pre;white-space:pre-wrap;'>Welcome to my Spotify Analysis App! To get started, enter the username &ldquo;jakerocksalot&rdquo; in the search bar on the left, and click &ldquo;search&rdquo;. This grabs all of my &ldquo;liked&rdquo; songs on Spotify, as well as my playlists. In the future, any username will be eligible, but first that must be cleared as an acceptable use of the Spotify API. Until then, we must stick with my data!&nbsp;</span></p>
  <p dir='ltr' style='line-height:1.38;margin-top:12pt;margin-bottom:8pt;'><span style='font-size:12pt;font-family:'Times New Roman';color:#000000;background-color:transparent;font-weight:400;font-style:normal;font-variant:normal;text-decoration:none;vertical-align:baseline;white-space:pre;white-space:pre-wrap;'>I designed this application to answer three questions</span></p>
    <ol style='margin-top:0;margin-bottom:0;padding-inline-start:48px;'>
      <li aria-level='1' dir='ltr' style='list-style-type:decimal;font-size:12pt;font-family:'Times New Roman';color:#000000;background-color:transparent;font-weight:400;font-style:normal;font-variant:normal;text-decoration:none;vertical-align:baseline;white-space:pre;'>
        <p dir='ltr' style='line-height:1.38;margin-top:12pt;margin-bottom:0pt;'><span style='font-size:12pt;font-family:'Times New Roman';color:#000000;background-color:transparent;font-weight:400;font-style:normal;font-variant:normal;text-decoration:none;vertical-align:baseline;white-space:pre;white-space:pre-wrap;'>How have my &ldquo;liked&rdquo; songs changed over time?</span></p>
          </li>
          <li aria-level='1' dir='ltr' style='list-style-type:decimal;font-size:12pt;font-family:'Times New Roman';color:#000000;background-color:transparent;font-weight:400;font-style:normal;font-variant:normal;text-decoration:none;vertical-align:baseline;white-space:pre;'>
            <p dir='ltr' style='line-height:1.38;margin-top:0pt;margin-bottom:0pt;'><span style='font-size:12pt;font-family:'Times New Roman';color:#000000;background-color:transparent;font-weight:400;font-style:normal;font-variant:normal;text-decoration:none;vertical-align:baseline;white-space:pre;white-space:pre-wrap;'>How do my playlists differ and do they live up to their names? Which are the happiest versus saddest? Are the saddest playlists aptly named?&nbsp;</span></p>
              </li>
              <li aria-level='1' dir='ltr' style='list-style-type:decimal;font-size:12pt;font-family:'Times New Roman';color:#000000;background-color:transparent;font-weight:400;font-style:normal;font-variant:normal;text-decoration:none;vertical-align:baseline;white-space:pre;'>
                <p dir='ltr' style='line-height:1.38;margin-top:0pt;margin-bottom:8pt;'><span style='font-size:12pt;font-family:'Times New Roman';color:#000000;background-color:transparent;font-weight:400;font-style:normal;font-variant:normal;text-decoration:none;vertical-align:baseline;white-space:pre;white-space:pre-wrap;'>Can I successfully train a model using popular music to predict which songs I will like?&nbsp;</span></p>
                  </li>
                  </ol>
                  <p dir='ltr' style='line-height:1.656;margin-top:12pt;margin-bottom:12pt;'><span style='font-size:12pt;font-family:'Times New Roman';color:#000000;background-color:transparent;font-weight:400;font-style:normal;font-variant:normal;text-decoration:none;vertical-align:baseline;white-space:pre;white-space:pre-wrap;'>To answer the first question, I created the&ldquo;Explore Liked Songs&rdquo; tab. This tab allows a user to select a &ldquo;main feature&rdquo; and &ldquo;comparison feature.&rdquo; After hitting &ldquo;view,&rdquo; the user can see the top 3-7 songs by the main feature (such as energy) and compare the main feature and comparison feature across all my tracks. I put &ldquo;date song saved&rdquo; on the x-axis and find remarkable stability in my music taste over time across all features, answering question one.&nbsp;</span></p>
                    <p dir='ltr' style='line-height:1.656;margin-top:12pt;margin-bottom:12pt;'><span style='font-size:12pt;font-family:'Times New Roman';color:#000000;background-color:transparent;font-weight:400;font-style:normal;font-variant:normal;text-decoration:none;vertical-align:baseline;white-space:pre;white-space:pre-wrap;'>To answer the second question, I created a similar &ldquo;Explore Playlists&rdquo; tab. This tab also allows a user to to select a &ldquo;main feature&rdquo; and &ldquo;comparison feature.&rdquo; In this tab however, the data can be viewed across three dimensions using a drop down selection menu. A user can analyze data</span></p>
                      <ul style='margin-top:0;margin-bottom:0;padding-inline-start:48px;'>
                        <li aria-level='1' dir='ltr' style='list-style-type:disc;font-size:12pt;font-family:'Times New Roman';color:#000000;background-color:transparent;font-weight:400;font-style:normal;font-variant:normal;text-decoration:none;vertical-align:baseline;white-space:pre;'>
                          <p dir='ltr' style='line-height:1.656;margin-top:12pt;margin-bottom:0pt;'><span style='font-size:12pt;font-family:'Times New Roman';color:#000000;background-color:transparent;font-weight:400;font-style:normal;font-variant:normal;text-decoration:none;vertical-align:baseline;white-space:pre;white-space:pre-wrap;'>Across playlists: comparing the median values of given features across playlists</span></p>
                            </li>
                            <li aria-level='1' dir='ltr' style='list-style-type:disc;font-size:12pt;font-family:'Times New Roman';color:#000000;background-color:transparent;font-weight:400;font-style:normal;font-variant:normal;text-decoration:none;vertical-align:baseline;white-space:pre;'>
                              <p dir='ltr' style='line-height:1.656;margin-top:0pt;margin-bottom:0pt;'><span style='font-size:12pt;font-family:'Times New Roman';color:#000000;background-color:transparent;font-weight:400;font-style:normal;font-variant:normal;text-decoration:none;vertical-align:baseline;white-space:pre;white-space:pre-wrap;'>Across songs within a given playlist: looking at the features of each song in a selected playlist</span></p>
                                </li>
                                <li aria-level='1' dir='ltr' style='list-style-type:disc;font-size:12pt;font-family:'Times New Roman';color:#000000;background-color:transparent;font-weight:400;font-style:normal;font-variant:normal;text-decoration:none;vertical-align:baseline;white-space:pre;'>
                                  <p dir='ltr' style='line-height:1.656;margin-top:0pt;margin-bottom:12pt;'><span style='font-size:12pt;font-family:'Times New Roman';color:#000000;background-color:transparent;font-weight:400;font-style:normal;font-variant:normal;text-decoration:none;vertical-align:baseline;white-space:pre;white-space:pre-wrap;'>Across songs in multiple playlists: analyzing the features of all songs in the set of selected playlists.</span></p>
                                    </li>
                                    </ul>
                                    <p dir='ltr' style='line-height:1.656;margin-top:12pt;margin-bottom:12pt;'><span style='font-size:12pt;font-family:'Times New Roman';color:#000000;background-color:transparent;font-weight:400;font-style:normal;font-variant:normal;text-decoration:none;vertical-align:baseline;white-space:pre;white-space:pre-wrap;'>I find a series of interesting results for my second question. For the most part, my names match the features of the playlists. For example, &ldquo;Bitter&rdquo; and &ldquo;Sad&rdquo; have low valence scores, while &ldquo;angry&rdquo; and &ldquo;rock&rdquo; have high energy scores. There are surprises however. For example, &ldquo;Melancholy 2000s&rdquo; has the highest valence of all!&nbsp;</span></p>
                                      <p dir='ltr' style='line-height:1.656;margin-top:12pt;margin-bottom:12pt;'><span style='font-size:12pt;font-family:'Times New Roman';color:#000000;background-color:transparent;font-weight:400;font-style:normal;font-variant:normal;text-decoration:none;vertical-align:baseline;white-space:pre;white-space:pre-wrap;'>Finally, I wanted to create a recommendation model that could predict whether I would &ldquo;like&rdquo; a given song. To do so, I used the tidymodels package to create a logistic classification model. I trained the model on the set of songs that appeared at least once in the Spotify Top 200 list between 2017 and 2020, with a &quot;success&quot; defined as one of these popular songs being found in my &quot;liked songs&quot;. Model metrics can be found in the &ldquo;Recommendation Model&rdquo; tab. In short, the model is adequate to good at identifying which popular songs I have liked. For songs it has never seen, it correctly identifies around 70% of liked songs as liked. Thus, we can use it cautiously to tell whether I&rsquo;d like a given song. To that end, a user can supply a song to the model and see whether it would predict that I would like said song. Simply enter an artist name, wait for their discography to load, select one of their songs, and see whether the model says I&apos;d like the song. This can sometimes take a moment to load, so please be patient. I also included an interactive plot to see which features drive the model, and in what direction. Please see the &ldquo;Methodology&rdquo; tab for more information.</span></p>"))
  }
  )
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Set Up Methodology tab ------------------------------------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$method_tab <- renderText({
    HTML(glue("<p dir='ltr' style='line-height:1.38;margin-top:0pt;margin-bottom:0pt;'><span style='font-size: 14px; font-family: 'Times New Roman'; color: rgb(0, 0, 0); background-color: transparent; font-weight: 700; font-style: normal; font-variant: normal; text-decoration: none; vertical-align: baseline; white-space: pre-wrap;'><strong>Data Collection:</strong></span></p>
  <p dir='ltr' style='line-height:1.38;text-indent: 36pt;margin-top:0pt;margin-bottom:0pt;'><span style='font-size: 14px;'><span style='font-family: 'Times New Roman'; color: rgb(0, 0, 0); background-color: transparent; font-weight: 400; font-style: normal; font-variant: normal; text-decoration: none; vertical-align: baseline; white-space: pre-wrap;'>The workhorse package for data collection in this app is the spotifyr package by Charlie Thompson. Plugging in my Spotify access token, I used a series of functions from this package to get my liked songs and playlists, as well as all the features for each song. Specifically, &nbsp;I used the get_my_saved_tracks() function to obtain the track ID for each of my liked songs, used &ldquo;for loops&rdquo; to apply functions like get_track_audio_features() to each ID, and used bind_rows to bind all of the results together. This resulted in a dataset that with 1,253 rows long and 33 columns, with no missing data at all. 1,018 of these songs are in one of my playlists (the others are just &ldquo;liked&rdquo; but not saved anywhere).</span></span></p>
    <p dir='ltr' style='line-height:1.38;text-indent: 36pt;margin-top:0pt;margin-bottom:0pt;'><span style='font-size: 14px;'><span style='font-family: 'Times New Roman'; color: rgb(0, 0, 0); background-color: transparent; font-weight: 400; font-style: normal; font-variant: normal; text-decoration: none; vertical-align: baseline; white-space: pre-wrap;'>I applied a nearly identical workflow to the set of songs that appeared in the Spotify Top 200, which I originally obtained via a web scraper I designed. However, Spotify has since discouraged users from scraping the Top 200 site, so I have stopped collecting this data (hence its 2017-2020 range). This resulted in a 7,384 row long data set. 50 of those rows contained missing data and thus dropped.&nbsp;</span></span></p>
      <p><span style='font-size: 14px;'><br></span></p>
        <p dir='ltr' style='line-height:1.38;margin-top:0pt;margin-bottom:0pt;'><span style='font-size: 14px;'><span style='font-family: 'Times New Roman'; color: rgb(0, 0, 0); background-color: transparent; font-weight: 700; font-style: normal; font-variant: normal; text-decoration: none; vertical-align: baseline; white-space: pre-wrap;'><strong>Data Cleaning</strong></span></span></p>
          <p dir='ltr' style='line-height:1.38;text-indent: 36pt;margin-top:0pt;margin-bottom:0pt;'><span style='font-size: 14px;'><span style='font-family: 'Times New Roman'; color: rgb(0, 0, 0); background-color: transparent; font-weight: 400; font-style: normal; font-variant: normal; text-decoration: none; vertical-align: baseline; white-space: pre-wrap;'>Much of the data outputted by the spotifyr functions was already clean. Only minor changes were necessary, such as changing character object dates into date objects, converting duration columns from milliseconds to minutes, and generating labels for use in the interactive tooltips using the glue() package and the rowwise() function. As mentioned, rows containing missing observations were dropped.</span></span></p>
            <p><span style='font-size: 14px;'><br></span></p>
              <p dir='ltr' style='line-height:1.38;margin-top:0pt;margin-bottom:0pt;'><span style='font-size: 14px;'><span style='font-family: 'Times New Roman'; color: rgb(0, 0, 0); background-color: transparent; font-weight: 700; font-style: normal; font-variant: normal; text-decoration: none; vertical-align: baseline; white-space: pre-wrap;'><strong>Visualizations</strong></span></span></p>
                <p dir='ltr' style='line-height:1.38;text-indent: 36pt;margin-top:0pt;margin-bottom:0pt;'><span style='font-size: 14px;'><span style='font-family: 'Times New Roman'; color: rgb(0, 0, 0); background-color: transparent; font-weight: 400; font-style: normal; font-variant: normal; text-decoration: none; vertical-align: baseline; white-space: pre-wrap;'>I used ggplot to create the plots themselves, used patchwork to join them, and ggiraph to make them interactive.&nbsp;</span></span></p>
                  <p><span style='font-size: 14px;'><br></span></p>
                    <p dir='ltr' style='line-height:1.38;margin-top:0pt;margin-bottom:0pt;'><span style='font-size: 14px;'><span style='font-family: 'Times New Roman'; color: rgb(0, 0, 0); background-color: transparent; font-weight: 700; font-style: normal; font-variant: normal; text-decoration: none; vertical-align: baseline; white-space: pre-wrap;'><strong>Modeling</strong></span></span></p>
                      <p dir='ltr' style='line-height:1.38;text-indent: 36pt;margin-top:0pt;margin-bottom:0pt;'><span style='font-size: 14px;'><span style='font-family: 'Times New Roman'; color: rgb(0, 0, 0); background-color: transparent; font-weight: 400; font-style: normal; font-variant: normal; text-decoration: none; vertical-align: baseline; white-space: pre-wrap;'>I used the tidymodels&nbsp;package to design a logistic classification model. As mentioned, I trained this model to predict whether a given song in the Spotify Top 200 list between 2017 and 2020 would be in my &ldquo;liked songs.&rdquo;&nbsp;</span></span></p>
                        <p dir='ltr' style='line-height:1.38;text-indent: 36pt;margin-top:0pt;margin-bottom:0pt;'><span style='font-size: 14px;'><span style='font-family: 'Times New Roman'; color: rgb(0, 0, 0); background-color: transparent; font-weight: 400; font-style: normal; font-variant: normal; text-decoration: none; vertical-align: baseline; white-space: pre-wrap;'>Ideally, I would have trained the model on a completely random set of Spotify songs. However, given the impracticality of obtaining such data, the drawback of only using popular music was an unavoidable limitation. This does make it more difficult for the model to tell whether I would like a given song that is perhaps not as popular as those that make the Top 200.&nbsp;</span></span></p>
                          <p dir='ltr' style='line-height:1.38;text-indent: 36pt;margin-top:0pt;margin-bottom:0pt;'><span style='font-size: 14px;'><span style='font-family: 'Times New Roman'; color: rgb(0, 0, 0); background-color: transparent; font-weight: 400; font-style: normal; font-variant: normal; text-decoration: none; vertical-align: baseline; white-space: pre-wrap;'>The set of features used as predictors were identical to the ones used in the &ldquo;Explore Liked Songs&rdquo; tab, with the addition of a genres feature, which was a comma separated string of up to 15 genres for a given song. I tokenized and tuned how many of the hundreds of genres to include (in the end, 125 genres optimized sensitivity in cross validation testing). I also unskewed some of the features, such as the number of artist followers, by taking the log.</span></span></p>
                            <p dir='ltr' style='line-height:1.38;text-indent: 36pt;margin-top:0pt;margin-bottom:0pt;'><span style='font-size: 14px;'><span style='font-family: 'Times New Roman'; color: rgb(0, 0, 0); background-color: transparent; font-weight: 400; font-style: normal; font-variant: normal; text-decoration: none; vertical-align: baseline; white-space: pre-wrap;'>The outcome variable was a simple logical variable indicating whether a given song in the Top 200 songs data was present in my &ldquo;liked songs&rdquo;. This was an unbalanced data set, with only a few hundred songs out of over 7,000 being &ldquo;liked&rdquo;. Thus, I used the step_smote() function from the tidymodels package to employ nearest neighbors upsampling (with 5 neighbors). This balanced the data.&nbsp;</span></span></p>
                              <p><span style='font-size: 14px;'><br></span></p>
                                <p dir='ltr' style='line-height:1.38;margin-top:0pt;margin-bottom:0pt;'><span style='font-size: 14px;'><span style='font-family: 'Times New Roman'; color: rgb(0, 0, 0); background-color: transparent; font-weight: 700; font-style: normal; font-variant: normal; text-decoration: none; vertical-align: baseline; white-space: pre-wrap;'><strong>Evaluation</strong></span></span></p>
                                  <p dir='ltr' style='line-height:1.38;text-indent: 36pt;margin-top:0pt;margin-bottom:0pt;'><span style='font-size: 14px;'><span style='font-family: 'Times New Roman'; color: rgb(0, 0, 0); background-color: transparent; font-weight: 400; font-style: normal; font-variant: normal; text-decoration: none; vertical-align: baseline; white-space: pre-wrap;'>To train the model, I used 10-fold cross validation. As a final evaluation, I tested the model on a hold out sample that I did not touch during the training process.&nbsp;</span></span></p>
                                    <p><span style='font-size: 14px;'><br></span></p>
                                      <p dir='ltr' style='line-height:1.38;margin-top:0pt;margin-bottom:0pt;'><span style='font-size: 14px;'><span style='font-family: 'Times New Roman'; color: rgb(0, 0, 0); background-color: transparent; font-weight: 700; font-style: normal; font-variant: normal; text-decoration: none; vertical-align: baseline; white-space: pre-wrap;'><strong>Results</strong></span></span></p>
                                        <p dir='ltr' style='line-height:1.38;text-indent: 36pt;margin-top:0pt;margin-bottom:0pt;'><span style='font-size: 14px;'><span style='font-family: 'Times New Roman'; color: rgb(0, 0, 0); background-color: transparent; font-weight: 400; font-style: normal; font-variant: normal; text-decoration: none; vertical-align: baseline; white-space: pre-wrap;'>This process resulted in a fair but not excellent predictive model, correctly identifying an actually liked song as liked just under 70% of the time. A random forest model had higher&nbsp;</span><span style='font-family: 'Times New Roman'; color: rgb(0, 0, 0); background-color: transparent; font-weight: 400; font-style: italic; font-variant: normal; text-decoration: none; vertical-align: baseline; white-space: pre-wrap;'>accuracy</span><span style='font-family: 'Times New Roman'; color: rgb(0, 0, 0); background-color: transparent; font-weight: 400; font-style: normal; font-variant: normal; text-decoration: none; vertical-align: baseline; white-space: pre-wrap;'>, but given I see this as a recommendation model, sensitivity is the more relevant metric.</span></span></p>
                                          <p><span style='font-size: 14px;'><br></span></p>
                                            <p dir='ltr' style='line-height:1.38;margin-top:0pt;margin-bottom:0pt;'><span style='font-size: 14px;'><span style='font-family: 'Times New Roman'; color: rgb(0, 0, 0); background-color: transparent; font-weight: 700; font-style: normal; font-variant: normal; text-decoration: none; vertical-align: baseline; white-space: pre-wrap;'><strong>Drawbacks and ethical concerns</strong></span></span></p>
                                              <p dir='ltr' style='line-height:1.38;text-indent: 36pt;margin-top:0pt;margin-bottom:0pt;'><span style='font-size: 14px; font-family: 'Times New Roman'; color: rgb(0, 0, 0); background-color: transparent; font-weight: 400; font-style: normal; font-variant: normal; text-decoration: none; vertical-align: baseline; white-space: pre-wrap;'>The data was largely cooperative. That said, using popular data only up to 2020 means the training data may not be reflective of current music, and thus the sensitivity score of my model may degrade over time (if it hasn&rsquo;t already). In addition, while nearest neighbors upsampling appeared to be successful, differenct balancing methods may yield superior results. Finally, in terms of the ethics, given that this is personal data I feel fairly comfortable. That said, in the future I plan to allow users to enter any username, which may come with some privacy concerns. However, users&apos; data being public is part of the user agreement when one signs up for Spotify. That leaves the only concern being following the API guidelines, which is an ongoing process. In the meantime, while collecting my data, I made sure the functions I used followed proper guidelines in terms of requests. </span></p>"))
  }
  )
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Set Up Methodology tab ------------------------------------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$reference_tab <- renderText({
    HTML(glue("<p dir='ltr' style='line-height:1.38;margin-top:0pt;margin-bottom:0pt;'><span style='font-size:12pt;font-family:'Times New Roman';color:#000000;background-color:transparent;font-weight:400;font-style:normal;font-variant:normal;text-decoration:none;vertical-align:baseline;white-space:pre;white-space:pre-wrap;'>Attali, D. &nbsp;(2020). shinyjs: Easily Improve the User Experience of Your Shiny Apps in Seconds. Retrieved from&nbsp;</span><a href='https://cran.r-project.org/package=shinyjs' style='text-decoration:none;'><span style='font-size:12pt;font-family:'Times New Roman';color:#1155cc;background-color:transparent;font-weight:400;font-style:normal;font-variant:normal;text-decoration:underline;-webkit-text-decoration-skip:none;text-decoration-skip-ink:none;vertical-align:baseline;white-space:pre;white-space:pre-wrap;'>https://CRAN.R-project.org/package=shinyjs</span></a></p>
  <p dir='ltr' style='line-height:1.38;margin-top:0pt;margin-bottom:0pt;'><span style='font-size:12pt;font-family:'Times New Roman';color:#000000;background-color:transparent;font-weight:400;font-style:normal;font-variant:normal;text-decoration:none;vertical-align:baseline;white-space:pre;white-space:pre-wrap;'>Attali, D., &amp; Edwards, T. (2020). shinyalert: Easily Create Pretty Popup Messages (Modals) in &apos;Shiny&apos;. Retrieved from https://CRAN.R-project.org/package=shinyalert</span></p>
  <p dir='ltr' style='line-height:1.38;margin-top:0pt;margin-bottom:0pt;'><span style='font-size:12pt;font-family:'Times New Roman';color:#000000;background-color:transparent;font-weight:400;font-style:normal;font-variant:normal;text-decoration:none;vertical-align:baseline;white-space:pre;white-space:pre-wrap;'>Chang, W., &amp; Borges Ribeiro, B. (2018). shinydashboard: Create Dashboards with &apos;Shiny&apos;. Retrieved from&nbsp;</span><a href='https://cran.r-project.org/package=shinydashboard' style='text-decoration:none;'><span style='font-size:12pt;font-family:'Times New Roman';color:#1155cc;background-color:transparent;font-weight:400;font-style:normal;font-variant:normal;text-decoration:underline;-webkit-text-decoration-skip:none;text-decoration-skip-ink:none;vertical-align:baseline;white-space:pre;white-space:pre-wrap;'>https://CRAN.R-project.org/package=shinydashboard</span></a><span style='font-size:12pt;font-family:'Times New Roman';color:#000000;background-color:transparent;font-weight:400;font-style:normal;font-variant:normal;text-decoration:none;vertical-align:baseline;white-space:pre;white-space:pre-wrap;'>.&nbsp;</span></p>
  <p dir='ltr' style='line-height:1.38;margin-top:0pt;margin-bottom:0pt;'><span style='font-size:12pt;font-family:'Times New Roman';color:#000000;background-color:transparent;font-weight:400;font-style:normal;font-variant:normal;text-decoration:none;vertical-align:baseline;white-space:pre;white-space:pre-wrap;'>Chang, W., et al. (2021). shiny: Web Application Framework for R. Retrieved from https://CRAN.R-project.org/package=shiny</span></p>
  <p dir='ltr' style='line-height:1.38;margin-top:0pt;margin-bottom:0pt;'><span style='font-size:12pt;font-family:'Times New Roman';color:#000000;background-color:transparent;font-weight:400;font-style:normal;font-variant:normal;text-decoration:none;vertical-align:baseline;white-space:pre;white-space:pre-wrap;'>Dantas, T. &nbsp;(2018). Rspotify: Access to Spotify API. Retrieved from https://CRAN.R-project.org/package=Rspotify</span></p>
  <p dir='ltr' style='line-height:1.38;margin-top:0pt;margin-bottom:0pt;'><span style='font-size:12pt;font-family:'Times New Roman';color:#000000;background-color:transparent;font-weight:400;font-style:normal;font-variant:normal;text-decoration:none;vertical-align:baseline;white-space:pre;white-space:pre-wrap;'>Gohel, D., &amp; Skintzos, P. (2021). ggiraph: Make &apos;ggplot2&apos; Graphics Interactive. Retrieved from &nbsp;&nbsp;</span><a href='https://cran.r-project.org/package=ggiraph' style='text-decoration:none;'><span style='font-size:12pt;font-family:'Times New Roman';color:#1155cc;background-color:transparent;font-weight:400;font-style:normal;font-variant:normal;text-decoration:underline;-webkit-text-decoration-skip:none;text-decoration-skip-ink:none;vertical-align:baseline;white-space:pre;white-space:pre-wrap;'>https://CRAN.R-project.org/package=ggiraph</span></a></p>
  <p dir='ltr' style='line-height:1.38;margin-top:0pt;margin-bottom:0pt;'><span style='font-size:12pt;font-family:'Times New Roman';color:#000000;background-color:transparent;font-weight:400;font-style:normal;font-variant:normal;text-decoration:none;vertical-align:baseline;white-space:pre;white-space:pre-wrap;'>Grolemund, G., &amp; Wickham, H. (2011). Dates and times made Easy with lubridate.&nbsp;</span><span style='font-size:12pt;font-family:'Times New Roman';color:#000000;background-color:transparent;font-weight:400;font-style:italic;font-variant:normal;text-decoration:none;vertical-align:baseline;white-space:pre;white-space:pre-wrap;'>Journal of Statistical Software</span><span style='font-size:12pt;font-family:'Times New Roman';color:#000000;background-color:transparent;font-weight:400;font-style:normal;font-variant:normal;text-decoration:none;vertical-align:baseline;white-space:pre;white-space:pre-wrap;'>, 40(3), 1-25.&nbsp;</span><a href='https://www.jstatsoft.org/v40/i03/' style='text-decoration:none;'><span style='font-size:12pt;font-family:'Times New Roman';color:#1155cc;background-color:transparent;font-weight:400;font-style:normal;font-variant:normal;text-decoration:underline;-webkit-text-decoration-skip:none;text-decoration-skip-ink:none;vertical-align:baseline;white-space:pre;white-space:pre-wrap;'>https://www.jstatsoft.org/v40/i03/</span></a><span style='font-size:12pt;font-family:'Times New Roman';color:#000000;background-color:transparent;font-weight:400;font-style:normal;font-variant:normal;text-decoration:none;vertical-align:baseline;white-space:pre;white-space:pre-wrap;'>.</span></p>
  <p dir='ltr' style='line-height:1.38;margin-top:0pt;margin-bottom:0pt;'><span style='font-size:12pt;font-family:'Times New Roman';color:#000000;background-color:transparent;font-weight:400;font-style:normal;font-variant:normal;text-decoration:none;vertical-align:baseline;white-space:pre;white-space:pre-wrap;'>Hester, J. (2020). glue: Interpreted String Literals. Retrieved from https://CRAN.R-project.org/package=glue</span></p>
  <p dir='ltr' style='line-height:1.38;margin-top:0pt;margin-bottom:0pt;'><span style='font-size:12pt;font-family:'Times New Roman';color:#000000;background-color:transparent;font-weight:400;font-style:normal;font-variant:normal;text-decoration:none;vertical-align:baseline;white-space:pre;white-space:pre-wrap;'>Kuhn et al., (2020). Tidymodels: a collection of packages for modeling and machine learning using tidyverse principles. Retrieved from&nbsp;</span><a href='https://www.tidymodels.org' style='text-decoration:none;'><span style='font-size:12pt;font-family:'Times New Roman';color:#1155cc;background-color:transparent;font-weight:400;font-style:normal;font-variant:normal;text-decoration:underline;-webkit-text-decoration-skip:none;text-decoration-skip-ink:none;vertical-align:baseline;white-space:pre;white-space:pre-wrap;'>https://www.tidymodels.org</span></a></p>
  <p dir='ltr' style='line-height:1.38;margin-top:0pt;margin-bottom:0pt;'><span style='font-size:12pt;font-family:'Times New Roman';color:#000000;background-color:transparent;font-weight:400;font-style:normal;font-variant:normal;text-decoration:none;vertical-align:baseline;white-space:pre;white-space:pre-wrap;'>Meyer, F., and Perrier, V. (2020). shinybusy: Busy Indicator for &apos;Shiny&apos; Applications. Retrieved from&nbsp;</span><a href='https://cran.r-project.org/package=shinybusy' style='text-decoration:none;'><span style='font-size:12pt;font-family:'Times New Roman';color:#1155cc;background-color:transparent;font-weight:400;font-style:normal;font-variant:normal;text-decoration:underline;-webkit-text-decoration-skip:none;text-decoration-skip-ink:none;vertical-align:baseline;white-space:pre;white-space:pre-wrap;'>https://CRAN.R-project.org/package=shinybusy</span></a></p>
  <p dir='ltr' style='line-height:1.38;margin-top:0pt;margin-bottom:0pt;'><span style='font-size:12pt;font-family:'Times New Roman';color:#000000;background-color:transparent;font-weight:400;font-style:normal;font-variant:normal;text-decoration:none;vertical-align:baseline;white-space:pre;white-space:pre-wrap;'>M&uuml;ller , K. (2020). here: A Simpler Way to Find Your Files. Retrieved from&nbsp;</span><a href='https://cran.r-project.org/package=here' style='text-decoration:none;'><span style='font-size:12pt;font-family:'Times New Roman';color:#1155cc;background-color:transparent;font-weight:400;font-style:normal;font-variant:normal;text-decoration:underline;-webkit-text-decoration-skip:none;text-decoration-skip-ink:none;vertical-align:baseline;white-space:pre;white-space:pre-wrap;'>https://CRAN.R-project.org/package=here</span></a></p>
  <p dir='ltr' style='line-height:1.38;margin-top:0pt;margin-bottom:0pt;'><span style='font-size:12pt;font-family:'Times New Roman';color:#000000;background-color:transparent;font-weight:400;font-style:normal;font-variant:normal;text-decoration:none;vertical-align:baseline;white-space:pre;white-space:pre-wrap;'>Pedersen, T. (2020). patchwork: The Composer of Plots. Retrieved from &nbsp;&nbsp;</span><a href='https://cran.r-project.org/package=patchwork' style='text-decoration:none;'><span style='font-size:12pt;font-family:'Times New Roman';color:#1155cc;background-color:transparent;font-weight:400;font-style:normal;font-variant:normal;text-decoration:underline;-webkit-text-decoration-skip:none;text-decoration-skip-ink:none;vertical-align:baseline;white-space:pre;white-space:pre-wrap;'>https://CRAN.R-project.org/package=patchwork</span></a></p>
  <p dir='ltr' style='line-height:1.38;margin-top:0pt;margin-bottom:0pt;'><span style='font-size:12pt;font-family:'Times New Roman';color:#000000;background-color:transparent;font-weight:400;font-style:normal;font-variant:normal;text-decoration:none;vertical-align:baseline;white-space:pre;white-space:pre-wrap;'>Perrier V., Meyer, F. &amp; Granjon, D. (2021). shinyWidgets: Custom Inputs Widgets for Shiny. Retrieved from&nbsp;</span><a href='https://cran.r-project.org/package=shinyWidgets' style='text-decoration:none;'><span style='font-size:12pt;font-family:'Times New Roman';color:#1155cc;background-color:transparent;font-weight:400;font-style:normal;font-variant:normal;text-decoration:underline;-webkit-text-decoration-skip:none;text-decoration-skip-ink:none;vertical-align:baseline;white-space:pre;white-space:pre-wrap;'>https://CRAN.R-project.org/package=shinyWidgets</span></a></p>
  <p dir='ltr' style='line-height:1.38;margin-top:0pt;margin-bottom:0pt;'><span style='font-size:12pt;font-family:'Times New Roman';color:#000000;background-color:transparent;font-weight:400;font-style:normal;font-variant:normal;text-decoration:none;vertical-align:baseline;white-space:pre;white-space:pre-wrap;'>Robinson, D., Hayes, A., &amp; Couch, S. (2021). broom: Convert Statistical Objects into Tidy Tibbles. Retrieved from&nbsp;</span><a href='https://cran.r-project.org/package=broom' style='text-decoration:none;'><span style='font-size:12pt;font-family:'Times New Roman';color:#1155cc;background-color:transparent;font-weight:400;font-style:normal;font-variant:normal;text-decoration:underline;-webkit-text-decoration-skip:none;text-decoration-skip-ink:none;vertical-align:baseline;white-space:pre;white-space:pre-wrap;'>https://CRAN.R-project.org/package=broom</span></a><span style='font-size:12pt;font-family:'Times New Roman';color:#000000;background-color:transparent;font-weight:400;font-style:normal;font-variant:normal;text-decoration:none;vertical-align:baseline;white-space:pre;white-space:pre-wrap;'>.</span></p>
  <p dir='ltr' style='line-height:1.38;margin-top:0pt;margin-bottom:0pt;'><span style='font-size:12pt;font-family:'Times New Roman';color:#000000;background-color:transparent;font-weight:400;font-style:normal;font-variant:normal;text-decoration:none;vertical-align:baseline;white-space:pre;white-space:pre-wrap;'>Robinson, D. [David Robinson] (2021, 07 June) ML Monday live screencast: Predicting board game ratings in R [Video]. Retrieved from&nbsp;</span><a href='https://www.youtube.com/watch?v=HBZyqkVjUgY&t=1286s' style='text-decoration:none;'><span style='font-size:12pt;font-family:'Times New Roman';color:#1155cc;background-color:transparent;font-weight:400;font-style:normal;font-variant:normal;text-decoration:underline;-webkit-text-decoration-skip:none;text-decoration-skip-ink:none;vertical-align:baseline;white-space:pre;white-space:pre-wrap;'>https://www.youtube.com/watch?v=HBZyqkVjUgY&amp;t=1286s</span></a></p>
  <p dir='ltr' style='line-height:1.38;margin-top:0pt;margin-bottom:0pt;'><span style='font-size:12pt;font-family:'Times New Roman';color:#000000;background-color:transparent;font-weight:400;font-style:normal;font-variant:normal;text-decoration:none;vertical-align:baseline;white-space:pre;white-space:pre-wrap;'>Sali, A., &amp; Attali, D. (2020). shinycssloaders: Add Loading Animations to a &apos;shiny&apos; Output While It&apos;s Recalculating. Retrieved from&nbsp;</span><a href='https://cran.r-project.org/package=shinycssloaders' style='text-decoration:none;'><span style='font-size:12pt;font-family:'Times New Roman';color:#1155cc;background-color:transparent;font-weight:400;font-style:normal;font-variant:normal;text-decoration:underline;-webkit-text-decoration-skip:none;text-decoration-skip-ink:none;vertical-align:baseline;white-space:pre;white-space:pre-wrap;'>https://CRAN.R-project.org/package=shinycssloaders</span></a></p>
  <p dir='ltr' style='line-height:1.38;margin-top:0pt;margin-bottom:0pt;'><span style='font-size:12pt;font-family:'Times New Roman';color:#000000;background-color:transparent;font-weight:400;font-style:normal;font-variant:normal;text-decoration:none;vertical-align:baseline;white-space:pre;white-space:pre-wrap;'>Silge, J. (2020, September 23). Handle class imbalance in #TidyTuesday climbing expedition data with tidymodels. Retrieved from&nbsp;</span><a href='https://juliasilge.com/blog/himalayan-climbing/' style='text-decoration:none;'><span style='font-size:12pt;font-family:'Times New Roman';color:#1155cc;background-color:transparent;font-weight:400;font-style:normal;font-variant:normal;text-decoration:underline;-webkit-text-decoration-skip:none;text-decoration-skip-ink:none;vertical-align:baseline;white-space:pre;white-space:pre-wrap;'>https://juliasilge.com/blog/himalayan-climbing/</span></a></p>
<p dir='ltr' style='line-height:1.38;margin-top:0pt;margin-bottom:0pt;'><span style='font-size:12pt;font-family:'Times New Roman';color:#000000;background-color:transparent;font-weight:400;font-style:normal;font-variant:normal;text-decoration:none;vertical-align:baseline;white-space:pre;white-space:pre-wrap;'>Thompson, C., Parry, J., Phipps, D., &amp; Wolff, T. (2021). spotifyr: R Wrapper for the &apos;Spotify&apos; Web API. Retrieved from&nbsp;</span><a href='http://github.com/charlie86/spotifyr' style='text-decoration:none;'><span style='font-size:12pt;font-family:'Times New Roman';color:#1155cc;background-color:transparent;font-weight:400;font-style:normal;font-variant:normal;text-decoration:underline;-webkit-text-decoration-skip:none;text-decoration-skip-ink:none;vertical-align:baseline;white-space:pre;white-space:pre-wrap;'>http://github.com/charlie86/spotifyr</span></a></p>
  <p dir='ltr' style='line-height:1.38;margin-top:0pt;margin-bottom:0pt;'><span style='font-size:12pt;font-family:'Times New Roman';color:#000000;background-color:transparent;font-weight:400;font-style:normal;font-variant:normal;text-decoration:none;vertical-align:baseline;white-space:pre;white-space:pre-wrap;'>Wickham et al., (2019). Welcome to the tidyverse.</span><span style='font-size:12pt;font-family:'Times New Roman';color:#000000;background-color:transparent;font-weight:400;font-style:italic;font-variant:normal;text-decoration:none;vertical-align:baseline;white-space:pre;white-space:pre-wrap;'>&nbsp;Journal of Open Source Software</span><span style='font-size:12pt;font-family:'Times New Roman';color:#000000;background-color:transparent;font-weight:400;font-style:normal;font-variant:normal;text-decoration:none;vertical-align:baseline;white-space:pre;white-space:pre-wrap;'>, 4(43), 1686, Retrieved from https://doi.org/10.21105/joss.01686</span></p>
  <p dir='ltr' style='line-height:1.38;margin-top:0pt;margin-bottom:0pt;'><span style='font-size:12pt;font-family:'Times New Roman';color:#000000;background-color:transparent;font-weight:400;font-style:normal;font-variant:normal;text-decoration:none;vertical-align:baseline;white-space:pre;white-space:pre-wrap;'>Wickham, H. (2019). stringr: Simple, Consistent Wrappers for Common String Operations. Retrieved from&nbsp;</span><a href='https://cran.r-project.org/package=stringr' style='text-decoration:none;'><span style='font-size:12pt;font-family:'Times New Roman';color:#1155cc;background-color:transparent;font-weight:400;font-style:normal;font-variant:normal;text-decoration:underline;-webkit-text-decoration-skip:none;text-decoration-skip-ink:none;vertical-align:baseline;white-space:pre;white-space:pre-wrap;'>https://CRAN.R-project.org/package=stringr</span></a></p>
  <p dir='ltr' style='line-height:1.38;margin-top:0pt;margin-bottom:0pt;'><span style='font-size:12pt;font-family:'Times New Roman';color:#000000;background-color:transparent;font-weight:400;font-style:normal;font-variant:normal;text-decoration:none;vertical-align:baseline;white-space:pre;white-space:pre-wrap;'>Wilke, C. (2020). ggtext: Improved Text Rendering Support for &apos;ggplot2&apos;. Retrieved from https://CRAN.R-project.org/package=ggtext</span></p>
  <p><span style='font-size:12pt;font-family:'Times New Roman';color:#000000;background-color:transparent;font-weight:400;font-style:normal;font-variant:normal;text-decoration:none;vertical-align:baseline;white-space:pre;white-space:pre-wrap;'>Zeileis, A., &amp; Grothendieck, G. (2005). zoo: S3 Infrastructure for Regular and Irregular Time Series.&nbsp;</span><span style='font-size:12pt;font-family:'Times New Roman';color:#000000;background-color:transparent;font-weight:400;font-style:italic;font-variant:normal;text-decoration:none;vertical-align:baseline;white-space:pre;white-space:pre-wrap;'>Journal of Statistical Software</span><span style='font-size:12pt;font-family:'Times New Roman';color:#000000;background-color:transparent;font-weight:400;font-style:normal;font-variant:normal;text-decoration:none;vertical-align:baseline;white-space:pre;white-space:pre-wrap;'>, 14(6), 1-27. 10.18637/jss.v014.i06</span></p>"))
  }
  )
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
    data_final <- readRDS(here("model_publish/Full_Logistic_Model.rds"))
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
           title="Values on the right positively predict me liking a song, values on the \nleft negatively predict me liking a song",
           subtitle = "Estimates from logistic classification model. Higher speechiness and energy predict \nI will like a song, higher artist popularity and duration predicts I won't like a song") +
      theme_minimal() +
      theme(plot.title.position = "plot",
            plot.title = element_text(size=rel(1.25)),
            plot.subtitle = element_text(size=rel(1), face="italic", color="grey30"),
            axis.text.y = element_text(size=rel(1)))
    
    girafe(ggobj = plot)
  })
}

shinyApp(ui, server)
