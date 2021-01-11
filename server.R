##### SUS SERVER SCRIPT ########################################################

shinyServer(function(input, output, session) {
  
  # show_modal_spinner() # show the modal window
  # remove_modal_spinner()
  
  waiter_hide()
  ### Render images from files #################################################
  
  output$homepic <- renderImage({
    filename <- normalizePath(file.path("www/SUSLOGO.png"))
    return(list(src = filename, height = "100%",  contentType = "image/png"))
    }, deleteFile = FALSE)
  
  output$mssipic <- renderImage({
    filename <- normalizePath(file.path("www/mssi_logo.png"))
    return(list(src = filename, contentType = "image/png",  width = 80,
                height = 80))
  }, deleteFile = FALSE)
  
  output$glamour_shot <- renderImage({
    filename <- normalizePath(file.path("www/glamour_shot.png"))
    return(list(src = filename, contentType = "image/png",  width = 600,
                height = 281))
  }, deleteFile = FALSE) 
  
  output$bivariate_legend <- renderImage({
    filename <- normalizePath(file.path("www/bivariate_legend_2.png"))
    return(list(src = filename, contentType = "image/png",  width = 200,
                height = 177))
  }, deleteFile = FALSE)

  
  ### Plot output calls for all 'left' plots ###################################
  
  # Active living potential
  output$active_map_left <- renderCachedPlot({
    
    # data_for_plot_left <- 
    #   data_bivar()
    # 
    # p <-
    #   ggplot(data_for_plot_left) +
    #   geom_sf(aes(fill = as.factor(left_variable)), color = "white", 
    #           size = 0.01) +
    #   scale_fill_manual(values = rev(colors[c(1:3)]), na.value = "grey70") +
    #   theme_map() + 
    #   theme(legend.position = "none")
    # 
    # ggdraw() + 
    #   draw_image(dropshadow2, scale = 1.59, vjust = 0.003, hjust = 0.003) +
    #   draw_plot(p) +
    #   draw_image(uni_legend, scale = .45, vjust = 0.25, hjust = 0.25) 
    
    }, 
    cacheKeyExpr = paste(rz$zoom, "left", sep = "_"),
    cache = diskCache("./app-cache"),
    bg = "white")
  
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
  
  
  # Language button
  sus_reactive_variables$active_language <- eventReactive(input$language_button, {
    if((input$language_button[1] %% 2) != 0){
      c("en")
    } else {
      c("fr")
    }
  }, ignoreNULL = FALSE)
  
  observeEvent(input$language_button,{
    if((input$language_button[1] %% 2) != 0){
      updateActionButton(session, "language_button",
                         label = "Français")
      update_lang(session, "en")
      
    } else {
      updateActionButton(session, "language_button",
                         label = "English")
      update_lang(session, "fr")
    }
  })
  
  
  # observeEvent(input$language_switch, {
  #   print(input$language_switch)
  #   if(input$language_switch == TRUE){
  #     
  #   i18n$set_translation_language("en")
  #     update_lang(session = session, language = "en")
  #   } else {
  #   i18n$set_translation_language("fr")
  #     update_lang( session = session, language = "fr")
  #   }
  #   return(i18n)
  #   })
  
  ### Active living potential ##################################################
  
  CanALE_module_server("CanALE_module")    
  
  ##############################################################################
  
  
  ### Pedestrian realm #########################################################
  Pedestrian_realm_module_server("Pedestrian_realm_module")
  
  
  ##############################################################################
  
  ### Commuter mode shift ######################################################
  Mode_switch_module_server("Mode_switch_module")
  
  
  ##############################################################################
  
  ### Biodiversity #########################################################
  
  ## module
  Biodiversity_module_server("biodiversity_module")
  
  
  ##############################################################################
  
  ### Meet the team #########################################################
  
  ## module
  Meet_the_team_server("meet_the_team_module")
  
  
  
})
