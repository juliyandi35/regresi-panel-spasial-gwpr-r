######## Plot  ########
### universidades de baires
provincias <- c("Ciudad de Buenos Aires", "Buenos Aires", "Catamarca",
                "Ciudad de Córdoba", "Corrientes", "Provincia del Chaco", "Chubut",
                "Entre Rios", "Formosa Argentina", "Jujuy", "La Pampa",
                "La Rioja Argentina", "Mendoza", "Misiones", "Neuquén",
                "Río Negro", "Salta", "San Juan Argentina", "Ciudad de San Luis",
                "Santa Cruz Argentina", "Ciudad de Santa Fe", "Santiago del Estero", 
                "Tierra del Fuego", "Tucumán")

uni.ba <- c("Universidad Nacional de Mar del Plata", "Universidad Nacional de Quilmes",
            "Universidad Nacional del Centro de la Provincia de Buenos Aires",
            "Universidad Nacional de La Matanza", "Universidad Nacional de Tres de Febrero",
            "Universidad Nacional de San Martín - Campus Miguelete", "Universidad Nacional Arturo Jauretche",
            "Universidad Nacional de Luján", "Universidad Nacional de General Sarmiento (UNGS)",
            "Universidad Nacional del Sur", "Universidad Nacional de Lanús",
            "UNNOBA - UNIVERSIDAD NACIONAL DEL NOROESTE DE LA PCIA DE BUENOS AIRES",
            "Universidad de Buenos Aires", "UNDEF - Universidad de la Defensa Nacional",
            "Universidad Nacional de Lomas de Zamora", "Universidad Nacional de Moreno",
            "Universidad Nacional de José C. Paz")

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
                   "Servicio Social - UNComa - Sede Central",
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

universidades <- data.frame(universidades, geocode(universidades))

uni.ba <- data.frame(uni.ba, geocode(uni.ba))

provincias <- data.frame(provincias, geocode(provincias))

### Invoco los archivos de https://gadm.org/download_country_v3.html

argentina_prov <- readRDS("gadm36_ARG_1_sf")
argentina_dept <- readRDS("gadm36_ARG_2_sf")

### Plot introduciendo la geolocalizacion de las universidades

### Delimitando provincias
ggplot(data = argentina_prov) +
  geom_sf(fill = "white", color = "black") +
  xlab("Longitud") + ylab("Latitud")  +
  geom_point(aes(lon, lat), 
  alpha=0.8, color="blue", size=2, shape=16, data=universidades)

### Con delimitacion de departamentos

ggplot(data = argentina_dept) +
  geom_sf(fill = "white", color = "black") +
  xlab("Longitud") + ylab("Latitud")  +
  geom_point(aes(lon, lat), 
  alpha=0.8, color="blue", size=2, shape=16, data=universidades) 

### Baires se puede ver que hay más concentracion, plot aparte
### filtramos por departamento en la base argentina3

baires <- argentina_dept[argentina_dept$NAME_1=="Buenos Aires",]

### Plot de baires por departamento
### Plot 

ggplot(data = baires) +
  geom_sf(fill = "white", color = "black") +
  xlab("Longitud") + ylab("Latitud")  +
  geom_point(aes(lon,lat),color="blue", alpha=0.8, size=4, shape=18, data=uni.ba)

