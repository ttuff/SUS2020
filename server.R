##### SUS SERVER SCRIPT ########################################################

shinyServer(function(input, output, session) {
  
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
  
  output$bivariate_legend_ped <- renderImage({
    filename <- normalizePath(file.path("www/bivariate_legend_2.png"))
    return(list(src = filename, contentType = "image/png",  width = 200,
                height = 177))
  }, deleteFile = FALSE)
  
  output$Univariate_left_legend <- renderImage({
    filename <- normalizePath(file.path("www/Univariate_left.png"))
    return(list(src = filename, contentType = "image/png",  width = 200,
                height = 200))
  }, deleteFile = FALSE)
  
  output$Univariate_right_legend <- renderImage({
    filename <- normalizePath(file.path("www/Univariate_right.png"))
    return(list(src = filename, contentType = "image/png",  width = 200,
                height = 200))
  }, deleteFile = FALSE)
  
  output$exemplar_ped <- renderImage({
    filename <- normalizePath(file.path("www/Exemplar.png"))
    return(list(src = filename, contentType = "image/png",  width = 550,
                height = 600))
  }, deleteFile = FALSE)
  
  output$sidewalk_calculation <- renderImage({
    filename <- normalizePath(file.path("www/sidewalk_calc.png"))
    return(list(src = filename, contentType = "image/png",  width = 400,
                height = 400))
  }, deleteFile = FALSE)
  
  output$univariate_legend_ped <- renderImage({
    filename <- normalizePath(file.path("www/legend_social_distancing_cap.png"))
    return(list(src = filename, contentType = "image/png",  width = 180,
                height = 140))
  }, deleteFile = FALSE)
  
  output$sidewalk_legend_ped <- renderImage({
    filename <- normalizePath(file.path("www/legend_sidewalk.png"))
    return(list(src = filename, contentType = "image/png",  width = 250,
                height = 140))
  }, deleteFile = FALSE)
  
  
  ### Plot output calls for all 'left' plots ###################################
  
  # Active living potential
  output$active_map_left <- renderCachedPlot({
    
    data_for_plot_left <- 
      data_bivar()
    
    p <-
      ggplot(data_for_plot_left) +
      geom_sf(aes(fill = as.factor(left_variable)), color = "white", 
              size = 0.01) +
      scale_fill_manual(values = rev(colors[c(1:3)]), na.value = "grey70") +
      theme_map() + 
      theme(legend.position = "none")
    
    ggdraw() + 
      draw_image(dropshadow2, scale = 1.59, vjust = 0.003, hjust = 0.003) +
      draw_plot(p) +
      draw_image(uni_legend, scale = .45, vjust = 0.25, hjust = 0.25) 
    
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
  
  
  languageButton_Server("language_button", global_session = session)
  
  
  
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
 
  
  
})
