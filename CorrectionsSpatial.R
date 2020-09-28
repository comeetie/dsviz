library(dplyr)
library(sf)
library(cartography)
library(stringr)


data_com=read.csv("./data/exo6_data.csv")

data_dep_naiss= data_com %>% 
  group_by(code_dept) %>% 
  summarise(naissances = sum(nombre_naissances_2011),
            pop=sum(Population_2011)) %>%
  mutate(txnaiss = naissances/pop*1000)

data_pol = read_sf("./data/exo6_dep/dep.shp")


as.character(data_dep_naiss$code_dept)
as.character(data_pol@data$code_insee)

setdiff(as.character(data_dep_naiss$code_dept),
        as.character(data_pol@data$code_insee))


data_dep_naiss = data_dep_naiss %>% 
  mutate(code_dept=as.character(code_dept)) %>%
  mutate(code_dept = ifelse(str_length(code_dept)==1,paste0("0",code_dept),code_dept))


setdiff(as.character(data_dep_naiss$code_dept),
        as.character(data_pol@data$code_insee))

data_map = data_pol %>% 
  left_join(data_dep_naiss,by=c("code_insee"="code_dept")) %>% 
  filter(!is.na(txnaiss))

plot(st_geometry(data_map))
library(cartography)
choroLayer(data_map,var="txnaiss")
title("Taux de naissance pour 1000 habitants")

## Exercice 2
monum      = read_sf("./data/monuments_paris.geojson")
meta_data  = read.csv("~/Projets/dsp5/data/monuments-historiques.csv", sep=";",stringsAsFactors = FALSE)
monum_full = monum %>% left_join(meta_data,by=c("id"="Référence"))


library(leaflet)

leaflet(data = monum_full) %>% 
  addTiles() %>%
  addMarkers( popup = ~as.character(Appellation.courante), 
              label = ~as.character(Appellation.courante))

leaflet(data = monum_full) %>% 
  addTiles() %>%
  addCircleMarkers( popup = ~as.character(Appellation.courante), 
              label = ~as.character(Appellation.courante),radius=5,stroke=FALSE)

# lecture et transformation des iris
iris = read_sf("./data/iris75.geojson")
iris2154 = iris %>% st_transform(2154)

# recherche des monuments dans chaque iris
lover = st_contains(iris2154,st_transform(monum_full,2154))
lover
# nouvelle variable
iris$nbmonum = sapply(lover,length)
# carte
plot(st_geometry(iris2154))
propSymbolsLayer(iris2154,var="nbmonum")

## Exo 3

library(rjson)


json = fromJSON(file="http://vlsstats.ifsttar.fr/data/input_NewYork.json")

lst = lapply(json$stationBeanList, function(s){as.data.frame(s)})
stations = do.call(rbind,lst)

st.sf = st_as_sf(stations %>% filter(longitude!=0,latitude!=0),coords = c("longitude","latitude"),crs=4326) 


plot(st_geometry(st.sf))

leaflet(data=st.sf) %>% addTiles() %>% 
  addCircleMarkers(radius = ~ sqrt(availableBikes),stroke=FALSE,fillOpacity = 1,color = "red")

library(cartography)
tiles <- getTiles(x = st.sf, type = "osm",crop=TRUE)
tilesLayer(tiles)
propSymbolsLayer(st.sf,var="availableBikes")
