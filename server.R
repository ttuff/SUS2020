##### SUS SERVER SCRIPT ########################################################

shinyServer(function(input, output, session) {
  
  # Waiter ------------------------------------------------------------------

  # show_modal_spinner() # show the modal window
  # remove_modal_spinner()
  
  #w <- Waiter$new()
  
  # give time for wait screen to show
  #Sys.sleep(3) 
  #hide_waiter()
  
  # observeEvent(input$show, {
  #   w$show(spinner)
  #   Sys.sleep(3) # give time for wait screen to show
  #   w$hide()
  # })
  
  # Plot output calls for all 'left' plots ----------------------------------
  # WILL GET MOVED INTO INDIVIDUAL MODULES
  
  # Commuter mode shift
  output$commuter_map_left <- renderCachedPlot({
    
    quant_car_share <- car_share %>% mutate(quant3 = ntile(car_share$Car_per, 3))
    
    p <- ggplot(quant_car_share) +
      geom_sf(aes(fill = as.factor(quant3)), color = "white", 
              size = 0.05) +
      scale_fill_manual(values = rev(colors[c(4:6)])) +
      theme_map()
    
    ggdraw() + 
      draw_image(dropshadow2, scale = 1.59, vjust = 0.003, hjust = 0.003) +
      draw_plot(p, scale = .85) 
    
  },
  cacheKeyExpr = paste("commute_mode_left"),
  cache = diskCache("./app-cache")
  )

  # Pedestrian social distancing capacity map
  output$pedestrian_map_left <- renderCachedPlot({
    
    p <- 
      ggplot() +
      geom_sf(data = census_circular, fill = "transparent", color = "black", 
              size = 0.05) +
      geom_sf(data = census_analysis_quantile,
              aes(fill = as.factor(
                social_distancing_capacity_pop_perc_2m_quant3)),
              color = "white", size = 0.03) +
      scale_fill_manual(values = rev(colors[c(1:3)])) +
      theme_void() +
      theme(legend.position = "none")
    
    ggdraw() + 
      draw_image(dropshadow2, scale = 1.85, vjust = 0.01) +
      draw_plot(p) +
      draw_image(uni_legend, scale = .45, vjust = 0.3, hjust = 0.3)
    
    },
    cacheKeyExpr = "pedestrian_left",
    cache = diskCache("./app-cache")
    )
  
  
  # Language button ---------------------------------------------------------
  
  sus_reactive_variables$active_language <- 
    eventReactive(input$language_button, {
      if (input$language_button[1] %% 2 != 0) "en" else "fr"
      }, ignoreNULL = FALSE)
  
  observeEvent(input$language_button,{
    if (input$language_button[1] %% 2 != 0) {
      updateActionButton(session, "language_button", label = "Français")
      update_lang(session, "en")
    } else {
      updateActionButton(session, "language_button", label = "English")
      update_lang(session, "fr")
    }
  })
  
  
  # Modules -----------------------------------------------------------------

  canale_server("canale")    
  #Pedestrian_realm_module_server("Pedestrian_realm_module")
  Mode_switch_module_server("Mode_switch_module")
  Biodiversity_module_server("biodiversity_module")
  Meet_the_team_server("meet_the_team_module")
  why_dash_server("why_dash")
  
})
