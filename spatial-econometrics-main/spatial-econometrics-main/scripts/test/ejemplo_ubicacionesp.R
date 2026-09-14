########
library(leaflet)
library(Rcpp)



m <- leaflet() %>%
  addTiles() %>%
  addMarkers(lng=-5.664112384, lat=40.96500844, popup="Plaza Mayor de Salamanca")
m

########
m <- leaflet() %>% setView(lng = -5.6641, lat = 40.9650, zoom = 12)
m %>% addTiles()
R
m <- leaflet(options = leafletOptions(minZoom = 0, maxZoom = 14))
1
m <- leaflet(options = leafletOptions(minZoom = 0, maxZoom = 14))

########
m <- leaflet(options = leafletOptions(minZoom = 0, maxZoom = 14)) %>% setView(lng = -5.6641, lat = 40.9650, zoom = 12)%>%
  setMaxBounds(lng1 = -5.780, lat1 = 40.890,lng2 = -5.580, lat2 = 41.020)
m %>% addTiles()


##Mapas base
m <- leaflet() %>% setView(lng = -5.6641, lat = 40.9650, zoom = 14)
m %>% addProviderTiles(providers$Stamen.Toner)

########
salamanca <- data.frame(
  lat = c(40.96500844,40.96440736,40.96381613,40.96395408,40.96423656,40.9638227,40.96279789,40.96311321,40.9617862,40.96142488,40.95849481,40.95944086,40.96063653,40.96047886,40.96158254,40.96301467,40.96279132,40.96016352,40.9605577,40.96114896,40.96193729,40.96515625,40.96477523,40.96526792,40.96553725,40.96604307,40.96670818,40.96681986,40.96534182,40.96966415),
  lng = c(-5.664112384,-5.664499515,-5.665769654,-5.665743555,-5.666100238,-5.666117637,-5.66646562,-5.666413423,-5.667953249,-5.667170286,-5.669658367,-5.666848402,-5.665978444,-5.666657011,-5.665552164,-5.664342923,-5.663316372,-5.660201922,-5.662811796,-5.663333771,-5.66045421,-5.666735307,-5.666822303,-5.668057644,-5.669832358,-5.667205085,-5.664788776,-5.664980167,-5.659203645,-5.663814423))

salamanca_popup = popup = c("Plaza Mayor","Iglesia de San Martín","Palacio de Maldonado de Morille","Casa de los Solis","Convento de la Madre de Dios","Iglesia de San benito","La Clerecía","Universidad Pontificia","Palacio de los Abarca Maldonado - Museo Provincial de Bellas Artes","Universidad","Puente Romano","Casa Lis","Catedral Nueva","Catedral Vieja","Palacio de Anaya","Palacio de Fonseca (La Salina)","Torre del Clavero","Iglesia de Santo Tomas Cantuariense","Convento de San Esteban","Convento de las Dueñas","Convento de Santa Clara","Palacio de Monterrey","Convento de las Agustinas","Convento de los Capuchinos","Colegio Mayor Arzobispo Fonseca","Convento de las Ursulas","Iglesia del Carmen","Casa de Doña Maria La Brava","Iglesia de Sancti Spiritus","Iglesia de San Marcos")

salamanca %>% 
  leaflet() %>%
  addTiles() %>%
  addMarkers(popup = salamanca_popup, clusterOptions = markerClusterOptions())

########
salamanca %>% 
  leaflet() %>%
  addTiles() %>%
  addMarkers(popup = salamanca_popup, clusterOptions = markerClusterOptions())%>%
  addProviderTiles(providers$Esri.WorldStreetMap) %>%
  addMiniMap(
    tiles = providers$Esri.WorldStreetMap,
    toggleDisplay = TRUE)


############## PLot mundial
getOption("ggmap")
locations <- c("Hoensbroek", "Johannesburg", "Barrow-in-Furness",
               "Hong Kong", "Singapore", "Tangail", "Maastricht", "Bendigo") %>%
  geocode()
world <- map_data("world")
ggplot() +
  geom_polygon(data = world,  aes(long, lat, group = group), fill = "grey") +
  geom_point(data = locations, aes(lon, lat), colour = "red", size = 5) + 
  coord_map("ortho", orientation = c(30, 80, 0)) +
  theme_void()

?map_data
