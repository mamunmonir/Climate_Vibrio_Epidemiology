#remotes::install_github("ovirahman/bangladesh")
library(bangladesh)

country <- get_map("country")
division <- get_map("division")
district <- get_map("district")
upazila <- get_map("upazila")
union <- get_map("union")

bd_plot("country")
bd_plot("division")
bd_plot("district")

library(tmap)
population <- bangladesh::pop_district_2011[, c("district", "population")]
district <- get_map("district")

map_data <- dplyr::left_join(district, population, by = c("District" = "district"))

map <- tm_shape(map_data) + 
  tm_polygons("population",id = "District",palette = "Reds", title = "Population") +
  tm_style("cobalt")+
  tm_layout(
    "Bangladesh District Wise Population Map\nSource: BBS",
    title.position = c("left", "bottom"),
    legend.position = c("right", "top")
  )

tmap::tmap_mode("plot")
map

library(ggplot2)
ggplot(data = map_data) +
  geom_sf(aes(fill = population))+
  theme_void()+
  viridis::scale_fill_viridis(trans = "log", name="Population", labels = scales::unit_format(unit = "M", scale = 1e-6)) +
  labs(
    title = "Bangladesh Population Map",
    subtitle = "Population & Housing Census 2011",
    caption = "Data Source: BBS"
  )


division_map <- get_map("division")
division_centroids <- bangladesh::get_coordinates(level = "division")
knitr::kable(division_centroids, format = "html")

#union_centroids <-bangladesh::get_coordinates(level = "union")

ggplot(data = division_map) +
  geom_sf() +
  geom_sf_label(aes(label = Division)) +
  geom_point(data = division_centroids, x = division_centroids$lon, y = division_centroids$lat, col = "red", size = 3) +
  xlab("")+ ylab("")+
  theme_minimal()

sylhet <- get_divisions(divisions = "Sylhet",level =  "upazila")
# single division
ggplot(data = sylhet) +
  geom_sf() +
  xlab("")+ ylab("")+
  theme_minimal()

dhaka <- get_divisions(divisions = "Dhaka",level =  "union")
# single division
ggplot(data = dhaka) +
  geom_sf() +
  geom_sf_label(aes(label = Upazila)) +
  xlab("")+ ylab("")+
  theme_minimal()




#multiple division
sylhet_chittagong_dhaka <- get_divisions(divisions = c("Sylhet", "Chittagong", "Dhaka"),level =  "upazila")
ggplot(data = sylhet_chittagong_dhaka) +
  geom_sf() +
  xlab("")+ ylab("")+
  theme_minimal()

amtali <- bd_search("amtali", level = "union", as.is = T, coordinates = T)
knitr::kable(amtali, format = "html")


mirpur<-bd_search("Mirpur", level = "Upazila", as.is = T, coordinates = T)

ggplot(bangladesh::map_union) +
  geom_sf() +
  geom_point(data = amtali, x = amtali$lon, y = amtali$lat, col = "red", size = 3) 




################################################

library(leaflet)

m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=90.35480, lat=23.79230, popup="Mirpur")
m  # Print

library(maps)
mapStates = map("state", fill = TRUE, plot = FALSE)
leaflet(data = dhaka) %>% addTiles() %>%
  addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE)



############################
setwd("/Users/monirulmemlab/Research_data/Hospital_data_icddrb")
locations_cord<-as.data.frame(read.csv("Total_cases_years_locations_cholera.csv", header=TRUE))
library(leaflet)

leaflet(locations_cord) %>% setView(90.3898, 23.7584, zoom = 12) %>% addTiles() %>% #addLabelOnlyMarkers(~Long, ~Lat, label = ~Location, labelOptions = labelOptions(noHide = T, textOnly = TRUE)) %>% 
  addCircles(lng = ~Long, lat = ~Lat, weight = 1,
             radius = ~ Year_2019*18, popup = ~Location, color = "red", fillOpacity = 0.5) %>% 
  addCircleMarkers(90.3998, 23.7764, radius = 5, color = "", fillColor = "green", fillOpacity = 1, label = "icddr,b") %>%
  addMarkers(90.3998, 23.7764, label = "icddr,b", labelOptions = labelOptions(noHide = T, textOnly = TRUE))

carto = "http://a.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}.png"
terrain = "http://tile.stamen.com/terrain-background/{z}/{x}/{y}.jpg"
leaflet(quakes) %>% addTiles(terrain)

leaflet(locations_cord) %>% setView(90.3898, 23.7584, zoom = 12) %>% addTiles(carto) %>% addLabelOnlyMarkers(~Long, ~Lat, label = ~Location, labelOptions = labelOptions(noHide = T, textOnly = TRUE)) %>% 
  addCircles(lng = ~Long, lat = ~Lat, weight = 1,
             radius = ~ Year_2022*18, popup = ~Location, color = "red", fillOpacity = 0.5) %>% 
  addCircleMarkers(90.3998, 23.7764, radius = 5, color = "", fillColor = "green", fillOpacity = 1, label = "icddr,b") %>%
  addMarkers(90.3998, 23.7764, label = "icddr,b", labelOptions = labelOptions(noHide = T, textOnly = TRUE))




library(bangladesh)
library(ggplot2)
dhaka <- get_divisions(divisions = "Dhaka",level =  "union")
ggplot(data = dhaka) +
  geom_sf() +
  geom_point(data = locations_cord, x = locations_cord$Long, y = locations_cord$Lat, col = "red", size = locations_cord$Year_2008/20)+
  xlab("")+ ylab("")+
  theme_minimal()