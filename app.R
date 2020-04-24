source('global.R', echo=FALSE)

shiny::shinyApp(

  ui = shiny::bootstrapPage(
        theme = shinythemes::shinytheme('lumen'),

          title = "Premier League 2019/2020",
        
            sidebar_info(),                            
            shiny::mainPanel(
              shiny::br(),
              shiny::br(),
              shiny::br(),
              shiny::br(),
              shiny::br(),
              shiny::br(),
              initial_page())
),
  
  server = function(input, output){
    
    v = click_link_plteams(input)
    
    shiny::observe({
      
      if(isTRUE(v$buttom_on)){
        
        menu_team(v$team)
        v$buttom_on = FALSE
      }

    })

    shiny::observeEvent(input$club_info, show_clubinfo(v$team))
    shiny::observeEvent(input$shirts, show_shirts(v$team))
    shiny::observeEvent(input$manager, show_manager(v$team))
    shiny::observeEvent(input$stadium, show_stadium(v$team))
    shiny::observeEvent(input$squad, show_squad(v$team))
    shiny::observeEvent(input$dismiss, menu_team(v$team))
    shiny::observeEvent(input$clubinfo_to_manager, show_manager(v$team))
    shiny::observeEvent(input$clubinfo_to_stadium, show_stadium(v$team))
    shiny::observeEvent(input$show_player, show_player(v$team, input$player))
    shiny::observeEvent(input$dismiss_playerpage, show_squad(v$team))
  })

