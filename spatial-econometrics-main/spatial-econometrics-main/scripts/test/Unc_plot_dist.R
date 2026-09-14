######### Packages  #########

packages <- c("readxl", "tseries","dynlm","mFilter","quantmod","forecast",
              "lmtest","ggplot2","ggfortify","vars","lmtest","dplyr",
              "leaflet","Rcpp","geosphere","ggmap","tidyverse",
              "ggmap","spData","rgeos", "ggmap", "rgdal","maptools", "tidyr","sf")
sapply(packages, require, character.only = TRUE)

#Para instalar paquetes.
#sapply(packages, install.packages,character.only = TRUE)

## para sf con fedora uso primero en terminal, para instalar dependencias, con windows no hay problema.

# sudo dnf install gdal-devel proj-devel proj-epsg proj-nad geos-devel sqlite-devel udunits2-devel

######### Base con API #########

## Clave de la API
register_google (key = "AIzaSyAq6y5sBAYBjWDwJTa22RUXBrdw0GYWyDk")

## Armamos vectores, id es el identificador que saldra en el mapa
## e "institución" es el nombre que mapea la API

#id <- c("UNC","UBA","UNL","UNLP","UNT","UNCUYO","UTN","UNNE","UNS",
#                   "UNR", "UNLaR","UNRC","UNCo","UNCA", "UNLZ","UNLu","UNSA", "UNER",
#                   "UNJu", "UNaM", "UNSJ", "UNSL", "UNSE", "UNLPam", "UNPSJB", "UNICEN",
#                   "UNMdP", "UNF", "UNLaM", "UNQ", "UNSAM","UNGS","UNPA","UNLA", "UNTREF",
#                   "UNVM", "UNNOBA", "UNdeC", "UNCAus", "UNRN", "UNAJ", "UNDAV", "UNPAZ",
#                   "UNM", "UNViME", "UNO", "UNTDF","UNDEF", "UNAHUR", "UNA", "UNLC","UNRaf",
#                   "UNSAdA", "UNAU", "UNSO", "UNaB")

provincia <- c("Ciudad de Buenos Aires", "Buenos Aires", "Catamarca",
                "Ciudad de Córdoba", "Corrientes", "Provincia del Chaco", "Chubut",
                "Entre Rios", "Formosa Argentina", "Jujuy", "La Pampa",
                "La Rioja Argentina", "Mendoza", "Misiones", "Neuquén",
                "Río Negro", "Salta", "San Juan Argentina", "Ciudad de San Luis",
                "Santa Cruz Argentina", "Ciudad de Santa Fe", "Santiago del Estero",
                "Tierra del Fuego", "Tucumán")

universidades <- c("Universidad Nacional de Córdoba",
                   "Universidad de Buenos Aires",
                   "Universidad Nacional del Litoral - Rectorado",
                   "Universidad Nacional de La Plata",
                   "Universidad Nacional de Tucumán ",
                   "Universidad Nacional de Cuyo",
                   "Universidad Tecnológica Nacional Facultad Regional Córdoba",
                   "Rectorado - Universidad Nacional del Nordeste",
                   "Universidad Nacional del Sur palihue",
                   "Universidad Nacional de Rosario",
                   "Universidad Nacional de La Rioja",
                   "Universidad Nacional de Río Cuarto",
                   "Servicio Social - UNCo - Sede Central",
                   "Universidad Nacional de Catamarca",
                   "Universidad Nacional de Lomas de Zamora",
                   "Universidad Nacional de Luján",
                   "Universidad Nacional de Salta",
                   "Rectorado Universidad Nacional de Entre Ríos",
                   "Rectorado Universidad Nacional de Jujuy",
                   "Universidad Nacional de Misiones",
                   "Rectorado de la Universidad Nacional de San Juan",
                   "Universidad Nacional de San Luis",
                   "Universidad Nacional de Santiago del Estero",
                   "UNLPam - Facultad de Ciencias Exactas y Naturales",
                   "Universidad Nacional de la Patagonia San Juan Bosco - Sede Comodoro Rivadavia",
                   "Universidad Nacional del Centro de la Provincia de Buenos Aires",
                   "Universidad Nacional de Mar del Plata",
                   "Universidad Nacional de Formosa",
                   "Universidad Nacional de La Matanza",
                   "Universidad Nacional de Quilmes",
                   "Universidad Nacional de San Martín - Campus Miguelete",
                   "Universidad Nacional de General Sarmiento (UNGS)",
                   "Universidad Nacional de la Patagonia Austral UNPA - Rectorado",
                   "Universidad Nacional de Lanús",
                   "Universidad Nacional de Tres de Febrero",
                   "Universidad Nacional de Villa María - Rectorado",
                   "UNNOBA - UNIVERSIDAD NACIONAL DEL NOROESTE DE LA PCIA DE BUENOS AIRES",
                   "Universidad Nacional de Chilecito (UNdeC)",
                   "Universidad Nacional del Chaco Austral",
                   "Universidad Nacional de Río Negro",
                   "Universidad Nacional Arturo Jauretche",
                   "Universidad nacional De Avellaneda",
                   "Universidad Nacional de José C. Paz",
                   "Universidad Nacional de Moreno",
                   "Universidad Nacional de Villa Mercedes",
                   "Universidad Nacional del Oeste",
                   "Universidad Nacional de Tierra del Fuego, Antártida e Islas del Atlántico Sur",
                   "UNDEF - Universidad de la Defensa Nacional",
                   "Universidad Nacional de Hurlingham",
                   "UNC - Facultad de Artes - Pabellón México",
                   "Universidad Nacional de los Comechingones",
                   "Universidad Nacional de Rafaela",
                   "Universidad Nacional de San Antonio de Areco",
                   "Universidad Nacional del Alto Uruguay",
                   "Universidad nacional Raúl Scalabrini Ortiz",
                   "Universidad Nacional Guillermo Brown - UNaB",
                   "Universidad Tecnológica Nacional Facultad Regional Santa Fe")

## geocode() busca las coordenadas

universidades <- data.frame(institucion, geocode(institucion)); base_popup = popup = institucion

provincias <- data.frame(provincia, geocode(provincia)); base_popup = popup = provincia

universidades <- data.frame(institucion, geocode(institucion)); base_popup = popup = institucion
######### Gráfico #########
## Ver formato
base %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers(popup = base_popup, clusterOptions = markerClusterOptions())

######### || #########
distHaversine(base[1,c(2,3)], base[2,c(2,3)]) / 1000 # la funcion devuelve metros por default

