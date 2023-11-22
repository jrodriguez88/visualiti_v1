### import and read data sensor (soil/rain Visualiti Sensor) 
# Author: Rodriguez-Espinoza J.
# https://github.com/jrodriguez88/
# 2023
# Read data - Soil moisture sensor


#############################
# Cargar librerías necesarias
############################# 

library(magrittr)
library(dplyr)
library(shiny)
library(ggplot2)
library(shinyWidgets)
library(stringr)
library(readr)
library(tidyr)
library(data.table)
library(lubridate)
library(purrr)
library(DT) 
library(writexl)
library(plotly)
library(shinythemes)


# Source main functions
source("https://raw.githubusercontent.com/jrodriguez88/visualiti_v1/main/functions_visualiti_sensor.R", encoding = "UTF-8")


# Definir la interfaz de usuario
source("https://raw.githubusercontent.com/jrodriguez88/visualiti_v1/main/ui.R", encoding = "UTF-8")


# Definir la lógica del servidor
source("https://raw.githubusercontent.com/jrodriguez88/visualiti_v1/main/server.R", encoding = "UTF-8")

# Ejecutar la aplicación Shiny
shinyApp(ui, server)
