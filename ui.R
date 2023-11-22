### import and read data sensor (soil/rain Visualiti Sensor) 
# Author: Rodriguez-Espinoza J.
# https://github.com/jrodriguez88/
# 2023
# User Interfaz 



################################
# Definir la interfaz de usuario
################################

ui <- fluidPage(
  shinythemes::themeSelector(),
  titlePanel("Rieguito. Modulo Sensores Visualiti"),
  sidebarLayout(
    sidebarPanel(
      fileInput("archivo_sensor", "Seleccionar Archivo del Sensor"),
      #fileInput("archivo_humedad_suelo", "Seleccionar Archivo de Humedad del Suelo"),
      #fileInput("archivo_precipitacion", "Seleccionar Archivo de Precipitación"),
      sliderInput("rango_fechas", "Seleccionar Rango de Fechas",
                  min = as.Date("2020-01-01"), max = as.Date("2022-01-01"),
                  value = c(as.Date("2020-01-01"), as.Date("2022-01-01"))),
      #      sliderInput("controller", "Controller", 1, 2, 1),
      #      numericInput("umbral_riego", "Umbral de agotamiento para Activar Riego", value = 10),
      selectInput("TextureInput","Textura del Suelo", 
                  choices=c("arenoso", "franco_arenoso", "franco", "franco_arcilloso", "franco_limoso", "arcilloso"), 
                  selected = "franco"),
      #      actionButton("vis_soil", "Visualizar Suelo"),
      actionButton("vis_plot1", "Visualizar datos 1"),
      actionButton("vis_plot2", "Visualizar datos 2"),
      selectInput("formato_descarga", "Formato de Descarga",
                  choices = c("CSV" = "csv", "Excel" = "xlsx"),
                  selected = "csv"),
      downloadButton("descargar_datos_hora", "Descargar Datos - Hora"),
      downloadButton("descargar_datos_dia", "Descargar Datos - Dia")
      
    ),
    mainPanel(
      mainPanel(
        tabsetPanel(id = "inTabset",
                    tabPanel(title = 'Grafico de datos ggplot', value = "panel_ggplot", plotOutput('plot_data', width = "100%", height = "70vh")),
                    tabPanel("Grafico de datos plotly", value = "panel_plotly", plotlyOutput("plot_data2",  width = "100%", height = "70vh")),
                    tabPanel(title = "Tabla de datos", value = "panel_tabla", DTOutput("tabla_datos"))
                    #        tabPanel('', sliderInput()),
                    
        ))
    )
  ))