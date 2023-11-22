### import and read data sensor (soil/rain Visualiti Sensor) 
# Author: Rodriguez-Espinoza J.
# https://github.com/jrodriguez88/
# 2023
# Read data - Soil moisture sensor


### libraries
#library(tidyverse)
#library(tidyr)
#library(dplyr)
#library(stringr)
#library(readr)
#library(purrr)
#library(lubridate)
#library(ggplot2)
#library(data.table)
#library(plotly)

## data 

## read txt files ---> ubicar la carpeta con los archivos txt descargados del sensor

#files <- list.files("data/", full.names = T, recursive = T, pattern = ".txt")

### funcion para importar datos de sensores Visualiti con conexion "Bluetooth Terminal HC-05
# file = txt file # - Bluetooth Terminal HC-05.txt

import_sensor_data <- function(file){
  
  tag_ini <- "---INI"
  tag_fin <- "---FIN"
  
  
  skip_lines <- read_lines(file) %>% str_detect(tag_ini) %>% which() + 1
  
  n_lines <- read_lines(file) %>% str_detect(tag_fin) %>% which() - skip_lines - 2
  
  
  raw_data <- fread(file, skip = skip_lines, nrows = n_lines) 
  
  var_names <- colnames(raw_data)
  
  
  if ("m3/m3"  %in% var_names) {
    
    cat("Datos de Sensor de humedad de suelo", sep = '\n')
    
    new_names <- c("id", "date", paste0("sensor_humedad_", 1:(length(var_names) - 2)))
    
    raw_data <- raw_data %>%
      set_names(new_names) %>%
      mutate(date_time = as_datetime(date), hour = format(date_time, "%H"), 
             date = as_date(date_time),
             var  = "soil") %>% 
      dplyr::select(id, var, date_time, date, hour, everything())
    
  } else if ("mm"  %in% var_names) {
    
    cat("Datos de Pluviometro", sep = '\n')
    new_names <- c("id", "date", paste0("pluviometro_", 1:(length(var_names) - 2)))
    
    raw_data <- raw_data %>%
      set_names(new_names) %>%
      mutate(date_time = as_datetime(date), 
             date = as_date(date_time),
             var = "rain") %>% 
      dplyr::select(id, var, date_time, date, everything())
    
    
  } else {cat("No se identifican datos")}
  
  
  return(raw_data)
  
} 
# Funciones que convierten datos crudos de sensores en datos diarios

soilraw_to_daily <- function(soilraw_data, nmin = 12){
  
  daily_data_soil <- soilraw_data %>% distinct() %>% group_by(id, date) %>% 
    summarise(n = n(), 
              Profundidad_0_20cm = mean(sensor_humedad_2, na.rm = T), 
              Profundidad_20_40cm = mean(sensor_humedad_1, na.rm = T), .groups = 'drop') %>%
    ungroup() %>% 
    filter(n>=nmin) %>% 
    pivot_longer(cols = -c(id, date, n), names_to = "Humedad_del_Suelo") # minimo % de los datos colectados por dia
  
  
  return(daily_data_soil)
  
  
}

rainraw_to_daily <- function(rainraw_data){
  
  rain_daily_data <- rainraw_data %>% distinct() %>%
    group_by(id, date) %>% 
    summarise(n = n(), Precipitacion = sum(pluviometro_1, na.rm = T), .groups = 'drop') %>%
    ungroup() ## add distinct data
  
  return(rain_daily_data)
  
}


# Funciones para graficar datos de sensores








plot_soil_data <- function(soil_daily_data, texture) {
  
  soil_params <- list(  
    suelo =  c("arenoso", "franco_arenoso", "franco", "franco_arcilloso", "franco_limoso", "arcilloso"),
    chs_min  = c(2.5, 1.3, 0.8, 0.25, 0.03, 0.01), #cm/h
    chs_max = c(25, 7.6, 2, 1.5, 0.5, 1), 
    cc_min = c(5, 10, 15, 25, 27, 30), 
    cc_max = c(16, 20, 30, 35, 40, 70), 
    pmp_min = c(2, 4, 8, 11, 13, 15), 
    pmp_max = c(6, 8, 12, 15, 17, 19),
    crad_min = c(6, 9, 14, 16, 18, 20), 
    crad_max = c(10, 15, 20, 22, 23, 25)
  ) %>% bind_cols() %>% dplyr::filter(suelo %in% texture)
  
  cc_rect <- data.frame(
    humedad_suelo = c("Capacidad de Campo", "Punto de Marchitez Permanente"),
    start=c(soil_params$cc_min, soil_params$pmp_min),
    end=c(soil_params$cc_max, soil_params$pmp_max))
  
  
  
  
  plot_sensor <- soil_daily_data %>% ggplot() + 
    geom_rect(
      data = cc_rect, 
      aes(NULL, NULL, ymin = start, ymax = end, fill = humedad_suelo), 
      xmin = min(soil_daily_data$date), xmax = max(soil_daily_data$date),
      alpha = 0.5) +
    geom_line(aes(date, value, color = Humedad_del_Suelo), cex = 1.1) +
#    ylim(0, cc_rect$end[1]+5) + 
    scale_x_date(date_breaks = "7 day", date_labels = "%b %d") +
    geom_hline(aes(yintercept = cc_rect$start[2], linetype = "Punto de Marchitez Permanente"), colour = "red") + 
    geom_text(aes(soil_daily_data$date[[1]], cc_rect$start[2], label = "Min", vjust = 0), size = 4) +

    geom_hline(aes(yintercept = cc_rect$end[1], linetype = "Capacidad de Campo"), colour = "blue") +
    geom_text(aes(soil_daily_data$date[[1]], cc_rect$end[1], label = "Max", vjust = 0), size = 4) +
    scale_fill_manual(values = alpha(c("darkgreen", "orange1"), 0.2)) +
    labs(title = paste0("Registro de humedad del suelo del sensor ", soil_daily_data$id[[1]]),
                 subtitle = paste0("Clase Textural = ", texture),
         x= "Dia",
         y= "Porcentaje (% -  m³/m³)",
         color = "Sensor: ", fill = "Reference : ") + 
    #       linetype = "Referencias tipo:") +
    #  scale_x_continuous(labels = function(x) month.abb[x], breaks = 1:12) +
    theme_bw() + scale_color_viridis_d(option = "E") +
    theme(#axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
      legend.position="bottom",
      #    legend.title = element_blank(),
      #    panel.grid.minor = element_blank(),
      strip.background=element_rect(fill="white", linewidth=1.5, linetype="solid"),
      strip.text = element_text(face = "bold")) + 
    guides(color = guide_legend(order = 2), linetype = "none") #+
#    scale_linetype_manual(name = "     Referencias: ", values = c(2, 2),
#                          guide = guide_legend(
#                            override.aes = list(color = c("green", "red"))))
  
  
  return(plot_sensor)
  
  
}

plot_rain_data <- function(rain_daily_data){

  
  plot_pluvio <- rain_daily_data %>% ggplot() +
    geom_col(aes(date, Precipitacion), color = "blue", width = 0.5, fill = "lightblue") +
    scale_x_date(date_breaks = "7 day", date_labels = "%b %d") +
    labs(title = paste0("Registro de lluvias del sensor ", rain_daily_data$id[[1]]),
         #        subtitle = "30-year Data 1990-2019 \nCrop Season:  A = May-Apr  --  B = Sep-Oct",
         x= "Dia",
         y= "Precipitacion (mm)") +
    #       color = "Humedad del suelo (% -  m³/m³): ", 
    #       linetype = "Referencias tipo:") +
    #  scale_x_continuous(labels = function(x) month.abb[x], breaks = 1:12) +
    theme_bw() + 
    theme(#axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
      #    legend.position="bottom",
      #    legend.title = element_blank(),
      #    panel.grid.minor = element_blank(),
      strip.background=element_rect(fill="white", linewidth=1.5, linetype="solid"),
      strip.text = element_text(face = "bold")) 
  
return(plot_pluvio)  
  
}


# prueba de uso - precipitacion -- lectura datos crudos
#test_data <- files %>% map(import_sensor_data) %>% 
#  map(~.x %>% nest(raw_data = -c(var))) %>% bind_rows()



# Definir fecha inicial de filtro, en algunos casos se registran datos con fechas anteriores ( 2000)
#ini_date <- make_date(2022, 3, 20)


## Datos precipitacion diarios

#rain <- test_data %>% filter(var == "rain") %>% unnest(raw_data) %>% 
#  group_split(id) %>% map(rainraw_to_daily) %>% 
#  map(~.x %>% filter(date > ini_date))


## datos humedad de  suelo
#soil <- test_data %>% filter(var == "soil") %>% unnest(raw_data) %>% 
#  group_split(id)  %>% map(~soilraw_to_daily(.x) %>% 
#  filter(date > ini_date)) 


#### graficar datos --->





