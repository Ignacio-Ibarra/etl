
## hay traer dos fuentes, una para mundo y otro para argentina
## la data hay que filtrarla para 2016 en ambas fuentes

## descargo fuente raw para mundo 
descargar_fuente_raw(id_fuente = 125, tempdir())

# me quedo con el sheet 3 que es donde está la data de sectores 
emis_global_co2_sector_2016 <- readxl::read_xlsx(get_temp_path("R125C0"),
                                                 sheet = 3)

# limpio los nombres de las variables

emis_global_co2_sector_2016 <- emis_global_co2_sector_2016 %>% 
  clean_names()

# transformo la data original para generar share por sector 
emis_global_co2_sector_2016_final <- emis_global_co2_sector_2016 %>%
  mutate(region = "Mundo") %>% 
  mutate(sector = recode(sector,
                         "Energy" = "Energía",
                         "Industrial processes" = "Procesos industriales y uso de productos",
                         "Waste" = "Residuos",
                         "Agriculture, Forestry & Land Use (AFOLU)" = "AGSyOUT",
                         "Agricultura, ganadería, silvicultura y otros usos de la tierra" = "AGSyOUT")) %>% 
  rename(valor_en_porcent=share_of_global_greenhouse_gas_emissions_percent) %>%
  select (3,1,2) 

## descargo fuente raw para aergentina 
descargar_fuente_raw(id_fuente = 131, tempdir())

# traigo la data 
emis_sector_2016_arg <- readxl::read_xlsx (argendataR::get_temp_path("R131C0"),skip = 1) %>% 
  clean_names()

## tranformo para generar share sector argentina para 2016 
emis_sector_2016_arg_final <- emis_sector_2016_arg %>%
  filter(ano == 2016) %>%
  group_by(sector) %>%
  summarise(total_valor = sum(valor, na.rm = TRUE)) %>% 
  mutate(valor_en_porcent = round((total_valor / sum(total_valor)) * 100, 1)) %>% 
  mutate(region = "Argentina") %>% 
  mutate(sector = recode(sector,
                         "Energy" = "Energía",
                         "Industrial processes" = "Procesos industriales y uso de productos",
                         "Waste" = "Residuos",
                         "Agriculture, Forestry & Land Use (AFOLU)" = "AGSyOUT",
                         "Agricultura, ganadería, silvicultura y otros usos de la tierra" = "AGSyOUT")) %>% 
  select (4,1,3)  

## junto las dos bases
base_mundo_argentina <- rbind(emis_global_co2_sector_2016_final, emis_sector_2016_arg_final)

# guardo csv
write_csv_fundar(x = base_mundo_argentina,
                 file = glue::glue("{tempdir()}/emisiones_sector_mundo_argentina_2016.csv"))

# agrego fuente clean
agregar_fuente_clean(id_fuente_raw = 125, 
                     dir = tempdir(),
                     path_clean = "emisiones_sector_mundo_argentina_2016.csv",
                     nombre = "Emisiones globales por sector Argentina y Mundo. Año 2016.",
                     script = "limpieza_emisiones_arg_mundo.R")

# actualizo fuente clean
actualizar_fuente_clean(id_fuente_clean = 62)


