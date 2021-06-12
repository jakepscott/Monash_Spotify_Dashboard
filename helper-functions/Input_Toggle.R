input_toggle <- function(enable_or_disable){
  if (enable_or_disable=="disable") {
    #Disable exploration UI until data is collected
    #Disable All Songs Tab
    shinyjs::disable("main_variable_allsongs")
    shinyjs::disable("comp_variable_allsongs")
    shinyjs::disable("num_bars_allsongs")
    shinyjs::disable("all_songs_plot_go")
    #Disable Playlist Tab
    shinyjs::disable("main_variable_playlist")
    shinyjs::disable("comp_variable_playlist")
    shinyjs::disable("num_bars_playlist")
    shinyjs::disable("playlists_plot_go")
  } else if (enable_or_disable=="enable") {
    #All Songs UI
    shinyjs::enable("main_variable_allsongs")
    shinyjs::enable("comp_variable_allsongs")
    shinyjs::enable("num_bars_allsongs")
    shinyjs::enable("all_songs_plot_go")
    #Playlists UI
    shinyjs::enable("main_variable_playlist")
    shinyjs::enable("comp_variable_playlist")
    shinyjs::enable("num_bars_playlist")
    shinyjs::enable("playlists_plot_go")
  }
}
