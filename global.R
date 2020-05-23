
require("shinyWidgets")
require("shiny")
require("stringi")
require("tidyverse")
require("leaflet")
require("plotly")
require("shinyWidgets")
# Used packages
packages = c("shinyWidgets", "shiny", "stringi", "tidyverse", "leaflet","plotly",
            "shinyWidgets")

# Run the following command to verify that the required packages are installed. If some package
# is missing, it will be installed automatically
package.check <- plyr::llply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    require(x)
  } else{
    require(x)
  }
})

# Define working directory
load("data/database_premierleague.RData")


# functions

gramatical_rule <- function(x){
  
  if(stringr::str_sub(x, start = -1) == 's'){
    
    stringr::str_c(x, "' ")
    
  } else{
    
    stringr::str_c(x, "'s ")  
  }
}

normalize_name <- function(x){
  
  x %>% 
    stringr::str_to_lower(.) %>% 
    stringr::str_remove_all(., pattern = '\\s+') %>% 
    stringi::stri_trans_general(., "Latin-ASCII")
  
}

search_image <- function(.path, .team, format = '.png'){
  
  stringr::str_c(.path, normalize_name(.team), format)
}

link_team <- function(.team, .width = 100, .height = 100){
  
  shiny::actionLink(
    inputId = stringr::str_c('show_', normalize_name(.team)),
    label = shiny::tags$img(src = search_image('shields/shield-', .team), width = .width, height = .height), 
    align = 'center')
}

initial_page <- function(teams = database_premierleague$clubs$team){
  
  shiny::fluidRow(
    shiny::column(12,
                  link_team(.team = teams[1]),
                  shiny::HTML('&nbsp;'),
                  shiny::HTML('&nbsp;'),
                  link_team(.team = teams[2]),
                  shiny::HTML('&nbsp;'),
                  shiny::HTML('&nbsp;'),
                  link_team(.team = teams[3]),
                  shiny::HTML('&nbsp;'),
                  shiny::HTML('&nbsp;'),
                  link_team(.team = teams[4]),
                  shiny::HTML('&nbsp;'),
                  shiny::HTML('&nbsp;'),
                  link_team(.team = teams[5]),
                  shiny::br(),
                  shiny::br(),
                  shiny::br(),
                  link_team(.team = teams[6]),
                  shiny::HTML('&nbsp;'),
                  shiny::HTML('&nbsp;'),
                  link_team(.team = teams[7]),
                  shiny::HTML('&nbsp;'),
                  shiny::HTML('&nbsp;'),
                  link_team(.team = teams[8]),
                  shiny::HTML('&nbsp;'),
                  shiny::HTML('&nbsp;'),
                  link_team(.team = teams[9]),
                  shiny::HTML('&nbsp;'),
                  shiny::HTML('&nbsp;'),
                  link_team(.team = teams[10]),
                  shiny::br(),
                  shiny::br(),
                  shiny::br(),
                  link_team(.team = teams[11]),
                  shiny::HTML('&nbsp;'),
                  shiny::HTML('&nbsp;'),
                  link_team(.team = teams[12]),
                  shiny::HTML('&nbsp;'),
                  shiny::HTML('&nbsp;'),
                  link_team(.team = teams[13]),
                  shiny::HTML('&nbsp;'),
                  shiny::HTML('&nbsp;'),
                  link_team(.team = teams[14]),
                  shiny::HTML('&nbsp;'),
                  shiny::HTML('&nbsp;'),
                  link_team(.team = teams[15]),
                  shiny::br(),
                  shiny::br(),
                  shiny::br(),
                  link_team(.team = teams[16]),
                  shiny::HTML('&nbsp;'),
                  shiny::HTML('&nbsp;'),
                  link_team(.team = teams[17]),
                  shiny::HTML('&nbsp;'),
                  shiny::HTML('&nbsp;'),
                  link_team(.team = teams[18]),
                  shiny::HTML('&nbsp;'),
                  shiny::HTML('&nbsp;'),
                  link_team(.team = teams[19]),
                  shiny::HTML('&nbsp;'),
                  shiny::HTML('&nbsp;'),
                  link_team(.team = teams[20]),
                  shiny::br(),
                  align = 'center'))
  
}

click_link_plteams <- function(input, teams = database_premierleague$clubs$team){
  
  aux <- shiny::reactiveValues(team = NULL, buttom_on = FALSE)
  
  shiny::observeEvent(input$show_arsenal, {
    
    aux$team <- teams[1]
    aux$buttom_on <- TRUE
    
  })
  shiny::observeEvent(input$show_astonvilla, {
    
    aux$team <- teams[2]
    aux$buttom_on <- TRUE
    
  })
  shiny::observeEvent(input$show_bournemouth, {
    
    aux$team <- teams[3]
    aux$buttom_on <- TRUE
    
  })
  shiny::observeEvent(input$show_brightonandhovealbion, {
    
    aux$team <- teams[4]
    aux$buttom_on <- TRUE
    
  })
  shiny::observeEvent(input$show_burnley, {
    
    aux$team <- teams[5]
    aux$buttom_on <- TRUE
    
  })
  shiny::observeEvent(input$show_chelsea, {
    
    aux$team <- teams[6]
    aux$buttom_on <- TRUE
    
  })
  shiny::observeEvent(input$show_crystalpalace, {
    
    aux$team <- teams[7]
    aux$buttom_on <- TRUE
    
  })
  shiny::observeEvent(input$show_everton, {
    
    aux$team <- teams[8]
    aux$buttom_on <- TRUE
    
  })
  shiny::observeEvent(input$show_leicestercity, {
    
    aux$team <- teams[9]
    aux$buttom_on <- TRUE
    
  })
  shiny::observeEvent(input$show_liverpool, {
    
    aux$team <- teams[10]
    aux$buttom_on <- TRUE
    
  })
  shiny::observeEvent(input$show_manchestercity, {
    
    aux$team <- teams[11]
    aux$buttom_on <- TRUE
    
  })
  shiny::observeEvent(input$show_manchesterunited, {
    
    aux$team <- teams[12]
    aux$buttom_on <- TRUE
    
  })
  shiny::observeEvent(input$show_newcastleunited, {
    
    aux$team <- teams[13]
    aux$buttom_on <- TRUE
    
  })
  shiny::observeEvent(input$show_norwichcity, {
    
    aux$team <- teams[14]
    aux$buttom_on <- TRUE
    
  })
  shiny::observeEvent(input$show_sheffieldunited, {
    
    aux$team <- teams[15]
    aux$buttom_on <- TRUE
    
  })
  shiny::observeEvent(input$show_southampton, {
    
    aux$team <- teams[16]
    aux$buttom_on <- TRUE
    
  })
  shiny::observeEvent(input$show_tottenhamhotspur, {
    
    aux$team <- teams[17]
    aux$buttom_on <- TRUE
    
  })
  shiny::observeEvent(input$show_watford, {
    
    aux$team <- teams[18]
    aux$buttom_on <- TRUE
    
  })
  shiny::observeEvent(input$show_westhamunited, {
    
    aux$team <- teams[19]
    aux$buttom_on <- TRUE
    
  })
  shiny::observeEvent(input$show_wolverhamptonwanderers, {
    
    aux$team <- teams[20]
    aux$buttom_on <- TRUE
    
  })
  
  return(aux)
}

search_data <- function(.value, .vbl_return, .df, .vbl_filter = 'team'){
  
  .df$aux <- .df[[.vbl_filter]]
  
  .df <- .df %>% 
    dplyr::filter(aux %in% .value)
  
  tmp <- unlist(.df[.vbl_return]) 
  names(tmp) = NULL
  
  tmp
}

basecolor_plteams <- function(.team){
  
  dplyr::case_when(
    .team == "Arsenal" ~ "#EF0107",
    .team == "Aston Villa" ~ "#95BFE5",
    .team == "Bournemouth" ~ "#DA291C",
    .team == "Brighton and Hove Albion" ~ "#0057B8",
    .team == "Burnley" ~ "#6C1D45",
    .team == "Chelsea" ~ "#034694",
    .team == "Crystal Palace" ~ "#1B458F",
    .team == "Everton" ~ "#003399",
    .team == "Leicester City" ~ "#003090",
    .team == "Liverpool" ~ "#C8102E",
    .team == "Manchester City" ~ "#6CABDD",
    .team == "Manchester United" ~ "#DA291C",
    .team == "Newcastle United" ~ "#241F20",
    .team == "Norwich City" ~ "#FFF200",
    .team == "Sheffield United" ~ "#EE2737",
    .team == "Southampton" ~ "#D71920",
    .team == "Tottenham Hotspur" ~ "#132257",
    .team == "Watford" ~ "#FBEE23",
    .team == "West Ham United" ~ "#7A263A",
    .team == "Wolverhampton Wanderers" ~ "#FDB913"
  )
  
}

basic_title_page <- function(.image, .text){
  
  shiny::fluidRow(
    shiny::HTML('&nbsp;'),
    shiny::tags$img(src = .image, width = 40, height = 40),
    shiny::HTML('&nbsp;'),
    .text
  )
}

manager_title_page <- function(.team, .df = database_premierleague$managers){
  
  shiny::fluidRow(
    shiny::column(5,
                  shiny::HTML('&nbsp;'),
                  shiny::tags$img(src = search_image('shields/shield-', .team),  width = 40, height = 40),
                  shiny::HTML('&nbsp;'),
                  search_data(.team, 'name', .df)
    ),
    shiny::column(7,
                  shiny::strong('Age: '),            
                  search_data(.team, 'age', .df),
                  shiny::HTML('&nbsp;'),            
                  shiny::HTML('&nbsp;'),            
                  shiny::strong('Nationality: '),            
                  search_data(.team, 'country', .df),
                  align = 'right')
  )
  
}

player_title_page <- function(.player, .team, .df){
  
  shiny::fluidRow(
    shiny::column(6,
                  shiny::HTML('&nbsp;'),
                  shiny::tags$img(src = search_image('shields/shield-', .team),  width = 40, height = 40),
                  shiny::HTML('&nbsp;'),
                  search_data(.player, 'full_name', dplyr::filter(.df, team == .team), .vbl_filter = 'name') %>% 
                    ifelse(!is.na(.), ., .player)
    ),
    shiny::column(6,
                  shiny::strong('Age: '),            
                  search_data(.player, 'age', dplyr::filter(.df, team == .team), .vbl_filter = 'name') %>% 
                    ifelse(is.na(.), '', .),
                  shiny::HTML('&nbsp;'),            
                  shiny::HTML('&nbsp;'),            
                  shiny::strong('Nationality: '),            
                  search_data(.player, 'nationality', dplyr::filter(.df, team == .team), .vbl_filter = 'name') %>% 
                    ifelse(is.na(.), '', .),
                  align = 'right')
  )
  
}

menu_team <- function(.team){
  
  shiny::showModal(
    shiny::modalDialog(
      
      title = basic_title_page(search_image('shields/shield-', .team), .team),
      
      shiny::fluidRow(
        shiny::column(12,
                      shiny::tags$img(src = search_image('shields/shield-', .team),  width = 220, height = 220),
                      shiny::br(),
                      shiny::br(),
                      align = 'center'),
        shiny::column(12,
                      shinyWidgets::actionGroupButtons(
                        inputIds = c('club_info', 'shirts', 'stadium', 'manager', 'squad'),
                        labels = list("Club's infos", "Shirts", "Stadium", "Manager", "Squad"),
                        status = 'default', fullwidth = FALSE
                      ), 
                      align = 'center')),
      
      size = "m",
      fade = FALSE,
      easyClose = TRUE,
      footer = shiny::tagList(shiny::modalButton("Close"))
      
    )
  )
}

show_shirts <- function(.team, .df = database_premierleague$clubs){
  
  shiny::showModal(
    shiny::modalDialog(
      
      title = basic_title_page(search_image('shields/shield-', .team), stringr::str_c(gramatical_rule(.team), "shirts")),
      
      shiny::fluidRow(
        shiny::p(
          shiny::h6(
            shiny::HTML('&nbsp;'), 
            shiny::HTML('&nbsp;'), 
            shiny::strong('Kit manufacturer: '),
            search_data(.team, 'kit_manufacturer', .df),
            shiny::br(),
            shiny::HTML('&nbsp;'), 
            shiny::HTML('&nbsp;'), 
            shiny::strong('Sponsor(s): '),
            search_data(.team, c('shirt_sponsor_chest', 'shirt_sponsor_sleeve'), .df) %>% 
              unique() %>% 
              na.omit() %>% 
              paste0(collapse = ', '),
            shiny::br(),
            shiny::hr(),
            shiny::HTML('&nbsp;'), 
            shiny::HTML('&nbsp;'), 
            shiny::column(12,
                          shiny::tags$img(src = stringr::str_c('shirts/', normalize_name(.team), '-home.png')),
                          shiny::tags$img(src = stringr::str_c('shirts/', normalize_name(.team), '-away.png')),
                          shiny::tags$img(src = stringr::str_c('shirts/', normalize_name(.team), '-third.png')),
                          align = 'center'),
            style = "font-size:16px")
        )
      ),
      
      size = "l",
      fade = FALSE,
      easyClose = TRUE,
      footer = shiny::tagList(shiny::modalButton('Close'),
                              shiny::actionButton('dismiss', 'Back'))
    ))
}

show_manager <- function(.team, .df = database_premierleague$managers){
  
  shiny::showModal(
    shiny::modalDialog(
      shiny::fluidRow(
        shiny::column(1, ''),
        shiny::column(5,
                      shiny::p(
                        shiny::h6(
                          shiny::br(),
                          shiny::br(),
                          shiny::strong('Joined club: '),
                          search_data(.team, 'joined_club', .df),
                          shiny::br(), 
                          shiny::br(), 
                          shiny::strong('Months in club: '),
                          search_data(.team, 'months_in_club', .df),
                          shiny::br(),
                          shiny::br(),
                          shiny::strong('Premier League seasons: '),
                          search_data(.team, 'pl_seasons', .df),
                          shiny::br(),
                          shiny::br(),
                          shiny::br(),
                          shiny::br(),
                          shiny::br(),
                          style = "font-size:16px")),
                      shiny::tags$a(
                        target ="_blank",
                        href = search_data(.team, 'wiki_page', .df),
                        stringr::str_c('More about ', search_data(.team, 'name', .df))
                      ), align = 'left' 
        ),
        
        shiny::column(6, 
                      shiny::tags$img(src = search_image('managers/manager-',.team), width = 250, height = 250),
                      align = 'right')
      ),
      title = manager_title_page(.team),
      
      size = "m",
      fade = FALSE,
      easyClose = TRUE,
      footer = shiny::tagList(shiny::modalButton('Close'),
                              shiny::actionButton('dismiss', 'Back'))
    )
  )}

show_clubinfo <- function(.team, .list = database_premierleague){
  
  shiny::showModal(
    shiny::modalDialog(
      
      shiny::fluidRow(
        shiny::br(),
        shiny::column(4,
                      shiny::tags$img(src = search_image('shields/shield-', .team),  width = 160, height = 160), 
                      shiny::br(),
                      shiny::br(),
                      links_socialmedia(.team),
                      align = 'center'),
        
        shiny::column(8, 
                      shiny::p(
                        shiny::h6(
                          shiny::strong('Complete name: '),
                          search_data(.team, 'full_name', .list$clubs),
                          shiny::br(),
                          shiny::br(),
                          
                          shiny::strong('Founded year: '),
                          search_data(.team, 'founded_year', .list$clubs),
                          shiny::br(),
                          shiny::br(),
                          
                          shiny::strong('Nickname(s): '),
                          search_data(.team, 'nickname', .list$clubs),
                          shiny::br(),
                          shiny::br(),
                          
                          shiny::strong('Manager: '),
                          shiny::actionLink(inputId = 'clubinfo_to_manager',
                                            search_data(.team, 'name', .list$managers)),
                          shiny::br(),
                          shiny::br(),
                          
                          shiny::strong('Stadium: '),
                          shiny::actionLink(inputId = 'clubinfo_to_stadium',
                                            search_data(.team, 'stadium', .list$stadiums)),
                          
                          shiny::br(),
                          shiny::br(),
                          
                          shiny::strong('Official Website: '),
                          shiny::tags$a(target = '_blank', 
                                        href = search_data(.team, 'site_club', .list$clubs),
                                        stringr::str_c(gramatical_rule(.team), "website")),
                          shiny::br(),
                          shiny::br(),
                          style = "font-size:16px")
                      )
        )
      ),
      
      title = basic_title_page(search_image('shields/shield-', .team), .team),
      
      size ="m",
      fade = FALSE,
      easyClose = TRUE,
      footer = shiny::tagList(shiny::modalButton('Close'),
                              shiny::actionButton('dismiss', 'Back'))
      
    )
  )
}

base_profile_stadium <- function(.team, .df){
  
  map_stadium <- leaflet::leaflet()
  
  lng_stadium <- search_data(.team, 'longitude', .df)
  lat_stadium <- search_data(.team, 'latitude', .df)
  
  leaflet::renderLeaflet(
    map_stadium %>% 
      leaflet::setView(zoom = 6, lng = mean(lng_stadium), lat = mean(lat_stadium)) %>% 
      leaflet::addProviderTiles(providers$Esri.WorldTopoMap) %>% 
      leaflet::addMarkers(
        lng_stadium, lat_stadium, 
        popup = plyr::llply(.team, function(item){
          
          as.character(shiny::fluidPage(
            shiny::column(12,
                          shiny::tags$img(src = search_image('shields/shield-', item), width = 40),
                          shiny::br(),
                          shiny::br(),
                          align = 'center'),
            
            shiny::strong('Club: '), item,
            shiny::br(),
            shiny::strong('Stadium: '),
            search_data(item, 'stadium', .df))
          )
          
        })
      )
  )
  
}

show_stadium <- function(.team, .df = database_premierleague$stadiums){
  
  shiny::showModal(
    shiny::modalDialog(
      
      shiny::fluidRow(
        shiny::br(),
        shiny::column(5,
                      shiny::column(12,
                                    shiny::tags$img(src = search_image('stadiums/stadium-', .team, '.jpeg'), width = 330),
                                    shiny::br(),
                                    shiny::br(),
                                    align = 'center'),
                      shiny::column(10,
                                    shiny::p(
                                      shiny::h6(
                                        shiny::strong('Stadium: '),
                                        search_data(.team, 'stadium', .df),
                                        shiny::br(),
                                        shiny::strong('Capacity: '),
                                        search_data(.team, 'capacity', .df),
                                        shiny::br(),
                                        shiny::br(),
                                        style = "font-size:16px")
                                    ),
                                    shiny::tags$a(
                                      target ="_blank",
                                      href = search_data(.team, 'wiki_page', .df),
                                      stringr::str_c('More about ', search_data(.team, 'stadium', .df))
                                    ) 
                      )
        ),
        
        shiny::column(7, base_profile_stadium(.team, .df))
      ),
      
      title = basic_title_page(search_image('shields/shield-', .team), search_data(.team, 'stadium', .df)),
      
      size = "l",
      fade = FALSE,
      easyClose = TRUE,
      footer = shiny::tagList(shiny::modalButton('Close'),
                              shiny::actionButton('dismiss', 'Back'))
      
    ))
}

links_socialmedia <- function(.team, .df = database_premierleague$clubs){
  
  shiny::column(12,
                shiny::tags$a(
                  target = '_blank',
                  href = search_data(.team, 'plpage', .df),
                  shiny::tags$img(src = search_image('templates/pl_logo', ''), width = 18)
                ),            
                shiny::tags$a(
                  target = '_blank',
                  href = search_data(.team, 'wiki_page', .df),
                  shiny::tags$img(src = search_image('templates/wiki_logo', ''), width = 18)
                ),
                shiny::tags$a(
                  target = '_blank',
                  href = search_data(.team, 'facebook', .df),
                  shiny::tags$img(src = search_image('templates/facebook_logo', ''), width = 18)
                ),
                shiny::tags$a(
                  target = '_blank',
                  href = search_data(.team, 'twitter', .df),
                  shiny::tags$img(src = search_image('templates/twitter_logo', ''), width = 18)
                ),
                shiny::tags$a(
                  target = '_blank',
                  href = search_data(.team, 'instagram', .df),
                  shiny::tags$img(src = search_image('templates/instagram_logo', ''), width = 18)
                ),
                shiny::tags$a(
                  target = '_blank',
                  href = search_data(.team, 'youtube', .df),
                  shiny::tags$img(src = search_image('templates/youtube_logo', ''), width = 18)
                ),
                
                align = 'center')
}

showlist_squad <- function(.team, .position, .df = database_premierleague$players){
  
  .df %>% 
    dplyr::filter(team == .team & position %in% .position) %>% 
    dplyr::arrange(desc(currently_overall), desc(appearances)) %>% 
    dplyr::pull(name)
}

show_squad <- function(.team, .df =  database_premierleague$players){
  
  tmp_plot <- search_data(.team, 'currently_overall', .df) %>%
    cut(., seq(50,100, by = 5)) %>%
    table %>%
    dplyr::tbl_df() %>%
    setNames(nm = c('overall', 'freq'))
  
  shiny::showModal(
    shiny::modalDialog(
      
      title = basic_title_page(search_image('shields/shield-', .team), .text = stringr::str_c(gramatical_rule(.team), "squad")),
      
      shiny::fluidRow(
        shiny::column(4, 
                      shinyWidgets::pickerInput(
                        inputId = 'player', 
                        label = 'Select a player:',
                        choices = list(
                          'Goalkeepers' = showlist_squad(.team, 'Goalkeeper'),
                          'Defenders' = showlist_squad(.team, 'Defender'),
                          'Midfielders' = showlist_squad(.team, 'Midfielder'),
                          'Forwards' = showlist_squad(.team, 'Forward')),
                        options = list(`live-search` = TRUE)
                      ),  
                      shiny::actionButton(inputId = 'show_player', "Show the player's info"),
                      align = 'left'),
        shiny::column(8,
                      shiny::h6(shiny::strong('Quality of squad by FIFA 20'), style = "font-size:16px"),
                      plotly::renderPlotly({
                        plotly::plot_ly(tmp_plot,  x = ~overall, y = ~freq, type = 'bar', 
                                        color = I(basecolor_plteams(.team)), height = 337.5, width = 525)}), 
                      align = 'center'),
        
      ),
      
      size = "l",
      fade = FALSE,
      easyClose = TRUE,
      footer = shiny::tagList(shiny::modalButton('Close'),
                              shiny::actionButton('dismiss', 'Back'))
    ))    
  
}

show_perfomance <- function(.team, .player, .df = database_premierleague$players){
  
  position_player <- search_data(.player, 'position', dplyr::filter(.df, team == .team), 'name')
  
  if(position_player == 'Goalkeeper'){
    
    shiny::fluidPage(
      shiny::strong('Appearances: '),
      search_data(.player, 'appearances', dplyr::filter(.df, team == .team), 'name'),
      shiny::HTML('&nbsp;'),            
      shiny::HTML('&nbsp;'),   
      shiny::strong('Cleansheets: '),
      search_data(.player, 'cleansheets', dplyr::filter(.df, team == .team), 'name')
    )
  } else if(position_player == 'Defender'){
    
    shiny::fluidPage(    
      shiny::strong('Appearances: '),
      search_data(.player, 'appearances', dplyr::filter(.df, team == .team), 'name'),
      shiny::HTML('&nbsp;'),            
      shiny::HTML('&nbsp;'),   
      shiny::strong('Goals: '),
      search_data(.player, 'goals', dplyr::filter(.df, team == .team), 'name'),
      shiny::HTML('&nbsp;'),            
      shiny::HTML('&nbsp;'),   
      shiny::strong('Cleansheets: '),
      search_data(.player, 'cleansheets', dplyr::filter(.df, team == .team), 'name')
    )
    
  } else{
    
    shiny::fluidPage(
      shiny::strong('Appearances: '),
      search_data(.player, 'appearances', dplyr::filter(.df, team == .team), 'name'),
      shiny::HTML('&nbsp;'),            
      shiny::HTML('&nbsp;'),   
      shiny::strong('Goals: '),
      search_data(.player, 'goals', .df, 'name'),
      shiny::HTML('&nbsp;'),            
      shiny::HTML('&nbsp;'),   
      shiny::strong('Assistances: '),
      search_data(.player, 'assists', .df, 'name')
    )
  }
  
}

show_player <- function(.team, .player, .df = database_premierleague$players){
  
  shiny::showModal(
    shiny::modalDialog(
      
      title = player_title_page(.player, .team, .df),
      shiny::fluidRow(
        
        shiny::column(7, 
                      shiny::column(3, ''),
                      shiny::column(9, 
                                    shiny::h2(.player),
                                    shiny::column(12,
                                                  shiny::h4(shiny::strong('General infos')),
                                                  shiny::fluidPage( 
                                                    shiny::h6(
                                                      shiny::strong('Number: '),
                                                      search_data(.player, 'number', dplyr::filter(.df, team == .team), .vbl_filter = 'name') %>% 
                                                        ifelse(is.na(.), '', .),
                                                      shiny::br(),
                                                      
                                                      shiny::strong('Position: '),
                                                      search_data(.player, 'position', dplyr::filter(.df, team == .team), .vbl_filter = 'name'),
                                                      shiny::br(),
                                                      
                                                      shiny::strong('Functions in field: '),
                                                      search_data(.player, 'functions_field', dplyr::filter(.df, team == .team), .vbl_filter = 'name') %>% 
                                                        ifelse(is.na(.), '', .),
                                                      shiny::br(),
                                                      
                                                      shiny::strong('Date birth: '),
                                                      search_data(.player, 'date_birth', dplyr::filter(.df, team == .team), .vbl_filter = 'name'),
                                                      shiny::br(),
                                                      
                                                      shiny::strong('Height: '),
                                                      stringr::str_c(
                                                        search_data(.player, 'height_cm', dplyr::filter(.df, team == .team), .vbl_filter = 'name'),
                                                        ' cm') %>% 
                                                        ifelse(is.na(.), '', .),
                                                      shiny::br(),
                                                      
                                                      shiny::strong('Weight: '),
                                                      stringr::str_c(
                                                        search_data(.player, 'weight_kg', dplyr::filter(.df, team == .team), .vbl_filter = 'name'),
                                                        ' kg') %>% 
                                                        ifelse(is.na(.), '', .),
                                                      shiny::br(), 
                                                      style = "font-size:15px")),
                                                  shiny::h4(shiny::strong('Performance infos')),
                                                  shiny::h6(
                                                    show_perfomance(.team, .player), 
                                                    style = "font-size:14px"
                                                  ),
                                                  shiny::br(),
                                                  show_wikipage(.team, .player),
                                                  align = 'justify'),
                                    align = 'justify')),
        
        shiny::column(5, 
                      shiny::tags$img(src = search_image('players/player-', stringr::str_c(.team, '-', .player)), width = 280),
                      shiny::br(),
                      shiny::h6(
                        shiny::column(12, 
                                      shiny::strong('Overall: '),
                                      search_data(.player, 'currently_overall', dplyr::filter(.df, team == .team), .vbl_filter = 'name') %>% 
                                        ifelse(is.na(.), '', .),
                                      shiny::HTML('&nbsp;'),   
                                      shiny::HTML('&nbsp;'),   
                                      shiny::strong('Potential: '),
                                      search_data(.player, 'currently_potential', dplyr::filter(.df, team == .team), .vbl_filter = 'name') %>% 
                                        ifelse(is.na(.), '', .), 
                                      align = 'center'),
                        style = "font-size:18px"
                      ), 
                      align = 'center')
      ),
      
      size = "l",
      fade = FALSE,
      easyClose = TRUE,
      footer = shiny::tagList(shiny::modalButton('Close'),
                              shiny::actionButton('dismiss_playerpage', 'Back'))
    ))    
  
}

show_wikipage <- function(.team, .player, .df = database_premierleague$players){
  
  player_wikipage <- search_data(.player, 'wiki_page', dplyr::filter(.df, team == .team), .vbl_filter = 'name')
  
  if(is.na(player_wikipage)){
    
    shiny::br()
    
  } else{
    
    shiny::tags$a(target = '_blank',
                  href = player_wikipage,
                  stringr::str_c('More about ', .player)
    )
    
  }
  
}

sidebar_info <- function(){
  
  shiny::sidebarPanel(
    shiny::fluidRow(
      shiny::tags$style(".well {background-color:#E8DAEF;}"),
      shiny::column(12,
                    shiny::tags$a(
                      target = '_blank', 
                      href = 'https://www.premierleague.com/',
                      shiny::tags$img(src = search_image('templates/pl1_logo', ''))
                    ), align = 'center'),
      
      shiny::column(12,
                    shiny::hr(),
                    shiny::br(),
                    shiny::p(
                      'Premier League is the top level of the English football league system and probably the national football 
        league most watched in the world. To alleviate the suspension of professional football caused by the 
        coronavirus pandemic and to improve my knowledge in web scraping and Shiny, I decided to create this 
        application. The target of this shiny app is join informations about clubs and players features.',
                      shiny::strong('Click in a club to open your profile page.'),
                      style = "font-size:17px"),
                    shiny::br(),
                    shiny::p(shiny::strong('Source'), style = "font-size:20px"),
                    shiny::p(
                      'The data set was obtained from ',
                      shiny::tags$a(target ="_blank", href = 'https://www.premierleague.com', 'Premier League website,'), 
                      shiny::tags$a(target ="_blank", href = 'https://en.wikipedia.org/wiki/2019%E2%80%9320_Premier_League', 'page of Premier League in Wikipedia'),
                      'and ',
                      shiny::tags$a(target ="_blank", href = 'https://www.fifaindex.com/players/', 'fifaindex.com'), 
                      'using web scraping at 24/04/2020. 
        The dataset is available to download in', 
                      shiny::tags$a(target = "_blank", href = 'https://github.com/gustavohpgodinho/shiny_pl', 'page of project in github.'),
        'This app has not yet been finalized.',              
                      style = "font-size:17px"),
                    shiny::br(),
                    shiny::p(shiny::strong('Developer'), style = "font-size:20px"),
                    shiny::p(
                      
                      shiny::tags$a(
                        target = '_blank', 
                        href = "https://www.linkedin.com/in/gustavo-godinho-0b90aa141",
                        'Gustavo Godinho'
                      ),
                      
                      shiny::br(),
                      shiny::em(' gustavohpgodinho@gmail.com'),
                      shiny::br(),
                      style = 'font-size:17px'
                      
                    ), align = 'justify')
    )
  )
  
}
