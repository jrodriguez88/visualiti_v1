### import and read data sensor (soil/rain Visualiti Sensor) 
# Author: Rodriguez-Espinoza J.
# https://github.com/jrodriguez88/
# 2023
# Server App

################################
# Definir la lógica del servidor
################################

server <- function(input, output, session) {
  
  datos <- reactive({
    # Leer archivos de datos si se han cargado
    #if (!is.null(input$archivo_humedad_suelo) & !is.null(input$archivo_precipitacion)) {
    if (!is.null(input$archivo_sensor)) {  
      
      
      datos_raw <-  import_sensor_data(input$archivo_sensor$datapath) 
      #humedad_suelo <-  import_sensor_data(input$archivo_humedad_suelo$datapath) 
      #      precipitacion <- import_sensor_data(input$archivo_precipitacion$datapath) 
      
      #     datos_daily <- 
      
      # Actualizar el rango de fechas basado en los datos cargados
      updateSliderInput(session, "rango_fechas", min = min(datos_raw$date), max = max(datos_raw$date),
                        value = c(min(datos_raw$date), max(datos_raw$date)))
      
      
      return(list(datos_r = datos_raw))
      
    }
  })
  
  
  
  ######################
  #plot humedad de suelo
  ######################
  
  observeEvent(input$vis_plot1, { 
    
    updateTabsetPanel(session, "inTabset",
                      selected = "panel_ggplot")
    
    output$plot_data <- renderPlot({
      # Verificar si los datos están disponibles
      if (!is.null(datos())) {
        
        to_plot <- subset(datos()$datos_r, date >= input$rango_fechas[1] & date <= input$rango_fechas[2])
        
        if(unique(datos()$datos_r$var) == "rain") {
          
          to_plot %>%
            rainraw_to_daily() %>%
            plot_rain_data()
          
          
        } else if (unique(datos()$datos_r$var) == "soil") {
          
          to_plot %>%
            soilraw_to_daily() %>%
            plot_soil_data(., texture = input$TextureInput) 
        }
        
        
      }
    }, height = 500, width = 800)
  })
  
  
  observeEvent(input$vis_plot2, { 
    
    updateTabsetPanel(session, "inTabset",
                      selected = "panel_plotly")
    
    output$plot_data2 <- renderPlotly({
      # Verificar si los datos están disponibles
      if (!is.null(datos())) {
        # Filtrar datos según el rango de fechas seleccionado
        to_plot <- subset(datos()$datos_r, date >= input$rango_fechas[1] & date <= input$rango_fechas[2])
        
        
        
        
        if(unique(datos()$datos_r$var) == "rain") {
          
          data_plot <- to_plot %>%
            rainraw_to_daily() 
          
          # Crear un gráfico interactivo con plotly
          p <- plot_ly(data_plot, x = ~date, y = ~Precipitacion,
                       type = "bar",  name = "Precipitacion", 
                       marker = list(color = 'rgb(58,200,225)',
                                     line = list(color = 'rgb(8,48,107)', width = 1.5)))
          
          # Configuración adicional del gráfico (puedes personalizar según tus necesidades)
          p <- p %>% layout(title = "Registro de Precipitacion",
                            xaxis = list(title = "Fecha"),
                            yaxis = list(title = "Precipitacion(mm)"), height = 500, width = 800)
          
          
          
          
        } else if (unique(datos()$datos_r$var) == "soil") {
          
          # Crear un gráfico interactivo con plotly
          p <- plot_ly(data_plot, x = ~date_time, y = ~sensor_humedad_1, type = "scatter", mode = "lines", name = "Sensor 1")
          
          p <- p %>% add_trace(y = ~sensor_humedad_2, name = 'Sensor 2', mode = 'lines') 
          
          # Configuración adicional del gráfico (puedes personalizar según tus necesidades)
          p <- p %>% layout(title = "Evolución de Humedad del Suelo",
                            xaxis = list(title = "Fecha"),
                            yaxis = list(title = "Humedad del Suelo"), height = 500, width = 800)
          
          
        }
        
        
        
        
        
        
        return(p)
      }
    })
  })
  
  
  
  ####################################################
  # Crear una tabla interactiva para mostrar los datos
  ####################################################
  
  output$tabla_datos <- renderDT({
    if (!is.null(datos())) {
      
      to_table <- subset(datos()$datos_r, date >= input$rango_fechas[1] & date <= input$rango_fechas[2])
      
      if(unique(datos()$datos_r$var) == "rain") {
        
        tabla_sensor <- to_table %>%
          rainraw_to_daily() 
        
      } else if (unique(datos()$datos_r$var) == "soil") {
        
        tabla_sensor <- to_table %>%
          soilraw_to_daily() 
        
      }
      
      datatable(tabla_sensor, options = list(pageLength = 10))
    }
  })
  
  
  
  #########################################
  # Descargar datos en formato seleccionado
  #########################################
  
  
  ### Horarios
  
  output$descargar_datos_hora <- downloadHandler(
    
    filename = function() {
      if (input$formato_descarga == "csv") {
        return("datos_hora.csv")
      } else if (input$formato_descarga == "xlsx") {
        return("datos_hora.xlsx")
      }
    },
    
    content = function(file) {
      if (input$formato_descarga == "csv") {
        to_table <- subset(datos()$datos_r, date >= input$rango_fechas[1] & date <= input$rango_fechas[2])
        write_csv(to_table, file)
        
      } else if (input$formato_descarga == "xlsx") {
        
        to_table <- subset(datos()$datos_r, date >= input$rango_fechas[1] & date <= input$rango_fechas[2])
        write_xlsx(to_table, file)
        
      }
    }
  )
  
  
  
  #Diarios
  
  output$descargar_datos_dia <- downloadHandler(
    
    filename = function() {
      if (input$formato_descarga == "csv") {
        return("datos_dia.csv")
      } else if (input$formato_descarga == "xlsx") {
        return("datos_dia.xlsx")
      }
    },
    
    content = function(file) {
      if (input$formato_descarga == "csv") {
        to_table <- subset(datos()$datos_r, date >= input$rango_fechas[1] & date <= input$rango_fechas[2])
        
        if(unique(datos()$datos_r$var) == "rain") {
          
          tabla_sensor <- to_table %>%
            rainraw_to_daily() 
          
        } else if (unique(datos()$datos_r$var) == "soil") {
          
          tabla_sensor <- to_table %>%
            soilraw_to_daily() 
          
        }
        write_csv(tabla_sensor, file)
        
      } else if (input$formato_descarga == "xlsx") {
        
        to_table <- subset(datos()$datos_r, date >= input$rango_fechas[1] & date <= input$rango_fechas[2])
        
        if(unique(datos()$datos_r$var) == "rain") {
          
          tabla_sensor <- to_table %>%
            rainraw_to_daily() 
          
        } else if (unique(datos()$datos_r$var) == "soil") {
          
          tabla_sensor <- to_table %>%
            soilraw_to_daily() 
          
        }
        write_xlsx(tabla_sensor, file)
      }
    }
    
  )
  
  
  
  
}