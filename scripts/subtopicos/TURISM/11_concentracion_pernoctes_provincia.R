# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "TURISM"
output_name <- "concentracion_pernoctes_provincia.csv"
analista <- "Carola della Paolera & Joan Manuel Vezzato"

fuente1 <- 'R481C0'


geo_front <- argendataR::get_nomenclador_geografico_front() %>% 
  select(geocodigoFundar = geocodigo, geonombreFundar = name_long) %>% 
  dplyr::filter(grepl("AR-.*", geocodigoFundar), nchar(geocodigoFundar) == 4)


# Leer archivos
df_yvera <- argendataR::get_raw_path(fuente1) %>% 
  read.csv()

localidad_provincia <- c(
  "Bahía Blanca" = "Buenos Aires",
  "Bariloche" = "Río Negro",
  "CABA" = "CABA",
  "Cafayate" = "Salta",
  "Calafate" = "Santa Cruz",
  "Carlos Paz" = "Córdoba",
  "Catamarca" = "Catamarca",
  "Córdoba" = "Córdoba",
  "Corrientes" = "Corrientes",
  "Formosa" = "Formosa",
  "Gualeguaychú" = "Entre Ríos",
  "Jujuy" = "Jujuy",
  "La Angostura" = "Neuquén",
  "La Falda" = "Córdoba",
  "La Rioja" = "La Rioja",
  "Las Grutas" = "Río Negro",
  "Malargue-Las Leñas" = "Mendoza",
  "Mar Chiquita" = "Buenos Aires",
  "Mar del Plata" = "Buenos Aires",
  "Mendoza" = "Mendoza",
  "Merlo" = "San Luis",
  "Mina Clavero" = "Córdoba",
  "Neuquén" = "Neuquén",
  "Paraná" = "Entre Ríos",
  "Pinamar" = "Buenos Aires",
  "Posadas" = "Misiones",
  "Puerto Iguazú" = "Misiones",
  "Puerto Madryn" = "Chubut",
  "Quebrada" = "Jujuy",
  "Rafaela" = "Santa Fe",
  "Resistencia" = "Chaco",
  "Río Cuarto" = "Córdoba",
  "Rio Gallegos" = "Santa Cruz",
  "Rosario" = "Santa Fe",
  "Salta" = "Salta",
  "San Juan" = "San Juan",
  "San Luis" = "San Luis",
  "San Martín de los Andes" = "Neuquén",
  "San Rafael" = "Mendoza",
  "Santa Fe" = "Santa Fe",
  "Santa Rosa" = "La Pampa",
  "Santiago del Estero" = "Santiago del Estero",
  "Tandil" = "Buenos Aires",
  "Termas" = "Santiago del Estero",
  "Tucumán" = "Tucumán",
  "Ushuaia" = "Tierra del Fuego",
  "Valle de Uco" = "Mendoza",
  "Valles" = "Jujuy",
  "Viedma" = "Río Negro",
  "Villa General Belgrano" = "Córdoba",
  "Villa Gesell" = "Buenos Aires",
  "Yungas" = "Jujuy"
)



df_output <- df_yvera %>% 
  mutate(
    geonombreFundar = localidad_provincia[localidad], 
    anio = year(indice_tiempo)
    
  ) %>% 
  dplyr::filter(origen_pernoctes == "Total")  %>% 
  group_by(localidad, anio) %>% 
  dplyr::filter(n() == 12 ) %>% 
  ungroup() %>% 
  dplyr::filter(anio == max(anio)) %>% 
  group_by(anio, geonombreFundar) %>% 
  summarise(
    pernoctes = sum(pernoctes, na.rm = T)
  ) %>% 
  ungroup() %>% 
  mutate(
    share = 100 * pernoctes / sum(pernoctes)
  ) %>% 
  left_join(geo_front, join_by(geonombreFundar)) %>% 
  select(anio, geocodigoFundar, geonombreFundar, pernoctes,share)
