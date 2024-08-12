options(scipen = 999)
library(sf)
library(tidyverse)
library(leaflet)


agro <- readr::read_csv("Ruta a la base de datos",
                        locale = locale(encoding = "WINDOWS-1252"))


municipios <- st_read("ruta a los archivos")


centroide  <- st_centroid(municipios) %>%
  select(-c(AREA, PERIMETER,COV_,COV_ID))


Lupulo <- agro %>%
  filter(Nomcultivo == "Lupulo") %>%
  select(Nomestado, Idestado, Idmunicipio, Nommunicipio, Volumenproduccion) %>%
  mutate(Idestado = case_when(str_length(Idestado) == 1 ~ paste0('0', Idestado),
                              str_length(Idestado) == 2 ~ paste0(Idestado)),
         Idmunicipio = case_when(str_length(Idmunicipio) == 1 ~ paste0('00', Idmunicipio),
                                 str_length(Idmunicipio) == 2 ~ paste0('0', Idmunicipio),
                                 str_length(Idmunicipio) == 3 ~ paste0(Idmunicipio))) %>%
  mutate(CVEGEO = paste0(Idestado,Idmunicipio)) %>%
  group_by(CVEGEO) %>%
  summarise(Volumenproduccion = sum(Volumenproduccion))



resulyados <- left_join(ctrd, jitomate, by = "CVEGEO") %>%
  filter(!is.na(Volumenproduccion))

resultados <- resultados %>%
  mutate(x = st_coordinates(a)[,1],
         y = st_coordinates(a)[,2])

class(resultados)

