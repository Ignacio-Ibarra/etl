# especializacion por rama 

# librerias
library(tidyverse)

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

# Metadatos 
subtopico <- "INDUST"
output_name <- "09_vab_por_industria"
analista <- "Nicolás Sidicaro"
fuente1 <- 'tiva2023_indust_by_country.csv' # TiVA 2023 rama industria

# rutas 
outstub <- 'indust/input'
instub <- 'indust/raw'

# Cargar datos 
df <- read_csv(file.path(instub,fuente1))
df <- janitor::clean_names(df)

# seleccionar variables 
df <- df %>% 
  select(-matches('x[0-9]+'))

# Filtrar menores de 0 
df <- df %>% 
  filter(obs_value > 0)

# Sacar actividades que no interesan 
df <- df %>% 
  filter(!activity %in% c('_T','BTE'))

# seleccionar variables 
df <- df %>% 
  select(time_period,ref_area,activity,obs_value)
df_aux <- df %>% 
  filter(activity == 'C') %>% 
  rename(obs_value_ind = obs_value) %>% 
  distinct()
df_aux$activity <- NULL
df <- df %>% 
  left_join(df_aux,by=c('ref_area','time_period'))
df <- df %>% 
  mutate(prop = obs_value / obs_value_ind)
df <- df %>% 
  select(anio=time_period,pais=ref_area,activity,prop_sobre_industria=prop)
df <- df %>% 
  filter(activity != 'C')

# Codigos 
traducir_actividades_tiva <- function(codigo) {
  case_when(
    # Códigos agregados de manufactura
    codigo == "C10T12" ~ "Productos alimenticios, bebidas y tabaco",
    codigo == "C13T15" ~ "Textiles, productos textiles, cuero y calzado",
    codigo == "C16T18" ~ "Productos de madera, papel e impresión",
    codigo == "C19T23" ~ "Productos químicos y minerales no metálicos",
    codigo == "C24_25" ~ "Metales básicos y productos metálicos fabricados",
    codigo == "C26_27" ~ "Equipos informáticos, electrónicos y eléctricos",
    codigo == "C28" ~ "Maquinaria y equipos, n.c.o.p.",
    codigo == "C29_30" ~ "Equipos de transporte",
    codigo == "C31T33" ~ "Manufactura n.c.o.p.; reparación e instalación de maquinaria y equipos",
    
    # Códigos individuales detallados
    codigo == "C16" ~ "Madera y productos de madera y corcho",
    codigo == "C17_18" ~ "Productos de papel e impresión",
    codigo == "C19" ~ "Coque y productos derivados del petróleo refinado",
    codigo == "C20_21" ~ "Productos químicos y farmacéuticos",
    codigo == "C20" ~ "Productos químicos",
    codigo == "C21" ~ "Productos farmacéuticos, medicinales químicos y botánicos",
    codigo == "C22" ~ "Productos de caucho y plásticos",
    codigo == "C23" ~ "Otros productos minerales no metálicos",
    codigo == "C24" ~ "Metales básicos",
    codigo == "C25" ~ "Productos metálicos fabricados",
    codigo == "C26" ~ "Equipos informáticos, electrónicos y ópticos",
    codigo == "C27" ~ "Equipos eléctricos",
    codigo == "C29" ~ "Vehículos automotores, remolques y semirremolques",
    codigo == "C30" ~ "Otros equipos de transporte",
    
    # Códigos adicionales
    codigo == "_T" ~ "Total manufactura",
    codigo == "C" ~ "Total manufactura",
    codigo == "BTE" ~ "Servicios empresariales y telecomunicaciones",
    codigo == "CTOTAL" ~ "Total economía",
    codigo == "DTOTAL" ~ "Total industrias",
    
    # Si no encuentra el código, devuelve el original
    TRUE ~ paste0("Código no reconocido: ", codigo)
  )
}

df <- df %>% 
  mutate(actividad = traducir_actividades_tiva(activity))

# Agregar pais 
traducir_paises_tiva <- function(codigo) {
  case_when(
    # Países OECD y principales economías
    codigo == "AUS" ~ "Australia",
    codigo == "AUT" ~ "Austria", 
    codigo == "BEL" ~ "Bélgica",
    codigo == "CAN" ~ "Canadá",
    codigo == "CHL" ~ "Chile",
    codigo == "COL" ~ "Colombia",
    codigo == "CRI" ~ "Costa Rica",
    codigo == "CZE" ~ "Chequia",
    codigo == "DNK" ~ "Dinamarca",
    codigo == "EST" ~ "Estonia",
    codigo == "FIN" ~ "Finlandia",
    codigo == "FRA" ~ "Francia",
    codigo == "DEU" ~ "Alemania",
    codigo == "GRC" ~ "Grecia",
    codigo == "HUN" ~ "Hungría",
    codigo == "ISL" ~ "Islandia",
    codigo == "IRL" ~ "Irlanda",
    codigo == "ISR" ~ "Israel",
    codigo == "ITA" ~ "Italia",
    codigo == "JPN" ~ "Japón",
    codigo == "KOR" ~ "Corea del Sur",
    codigo == "LVA" ~ "Letonia",
    codigo == "LTU" ~ "Lituania",
    codigo == "LUX" ~ "Luxemburgo",
    codigo == "MEX" ~ "México",
    codigo == "NLD" ~ "Países Bajos",
    codigo == "NZL" ~ "Nueva Zelanda",
    codigo == "NOR" ~ "Noruega",
    codigo == "POL" ~ "Polonia",
    codigo == "PRT" ~ "Portugal",
    codigo == "SVK" ~ "Eslovaquia",
    codigo == "SVN" ~ "Eslovenia",
    codigo == "ESP" ~ "España",
    codigo == "SWE" ~ "Suecia",
    codigo == "CHE" ~ "Suiza",
    codigo == "TUR" ~ "Turquía",
    codigo == "GBR" ~ "Reino Unido",
    codigo == "USA" ~ "Estados Unidos",
    
    # Economías emergentes y otros países
    codigo == "ARG" ~ "Argentina",
    codigo == "BGD" ~ "Bangladesh",
    codigo == "BLR" ~ "Bielorrusia",
    codigo == "BRA" ~ "Brasil",
    codigo == "BRN" ~ "Brunéi Darussalam",
    codigo == "BGR" ~ "Bulgaria",
    codigo == "KHM" ~ "Camboya",
    codigo == "CMR" ~ "Camerún",
    codigo == "CHN" ~ "China (República Popular de)",
    codigo == "CIV" ~ "Côte d'Ivoire",
    codigo == "HRV" ~ "Croacia",
    codigo == "CYP" ~ "Chipre",
    codigo == "EGY" ~ "Egipto",
    codigo == "HKG" ~ "Hong Kong, China",
    codigo == "IND" ~ "India",
    codigo == "IDN" ~ "Indonesia",
    codigo == "JOR" ~ "Jordania",
    codigo == "KAZ" ~ "Kazajistán",
    codigo == "LAO" ~ "Laos (República Democrática Popular)",
    codigo == "MYS" ~ "Malasia",
    codigo == "MLT" ~ "Malta",
    codigo == "MAR" ~ "Marruecos",
    codigo == "MMR" ~ "Myanmar",
    codigo == "NGA" ~ "Nigeria",
    codigo == "PAK" ~ "Pakistán",
    codigo == "PER" ~ "Perú",
    codigo == "PHL" ~ "Filipinas",
    codigo == "ROU" ~ "Rumania",
    codigo == "RUS" ~ "Federación Rusa",
    codigo == "SAU" ~ "Arabia Saudí",
    codigo == "SEN" ~ "Senegal",
    codigo == "SGP" ~ "Singapur",
    codigo == "ZAF" ~ "Sudáfrica",
    codigo == "TWN" ~ "Taipéi Chino",
    codigo == "THA" ~ "Tailandia",
    codigo == "TUN" ~ "Túnez",
    codigo == "UKR" ~ "Ucrania",
    codigo == "VNM" ~ "Vietnam",
    codigo == "WXD" ~ "Resto del mundo",
    
    # Si no encuentra el código, devuelve el original
    TRUE ~ paste0("País no reconocido: ", codigo)
  )
}

df <- df %>% 
  mutate(pais2 = traducir_paises_tiva(pais))

# Guardar df 
df <- df %>% 
  select(pais=pais2,actividad,anio,prop_sobre_industria)

# Guardar 
readr::write_csv(df,file.path(outstub,paste0(output_name,'.csv')))
