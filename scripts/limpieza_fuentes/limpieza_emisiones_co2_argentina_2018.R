

rm(list = ls())
list.files(tempdir())
limpiar_temps()

## descargo fuente raw
descargar_fuente_raw(id_fuente = 131, tempdir())

# traigo la data 
emis_sector_2018_arg <- read_xlsx (argendataR::get_temp_path("R131C0"),skip = 1) 

## filtro ultimo año 
ultimo_anio <- max(emis_sector_2018_arg$Año)  # Encuentra el último año presente en la columna 'anio'

## filtro la base
emis_sector_2018_arg_ultimo_año <- emis_sector_2018_arg %>% 
  filter(Año == ultimo_anio)

## me quedo con las variables necesarias para el dataset
emis_sector_2018_arg_ultimo_año_variables <- emis_sector_2018_arg_ultimo_año %>% 
  select(1,4,5,6,11) %>% 
  rename(anio=Año,
         sector=Sector,
         subsector="Subcategoria 1er Orden",
         valor_en_mtco2e=Valor,
         categoria=Categoria)
  #group_by(sector, subsector,anio) %>%
  #summarise(total_valor = sum(valor_en_mtco2e, na.rm = TRUE))


## armo todos los sectores subsectores como describe analista
emis_sector_2018_arg_ultimo_año_variables_final <- emis_sector_2018_arg_ultimo_año_variables %>%
  mutate(sector_nuevo = case_when(
    subsector == "Industrias de la energía" ~ "Industrias de la energía",
    subsector == "Industrias manufactureras y de la construcción" ~ "Industrias manufactureras y de la construcción",
    subsector == "Transporte" ~ "Transporte",
    subsector == "Otros sectores" ~ "Otros sectores",
    categoria == "Emisiones fugitivas provenientes de la fabricación de combustibles" ~ "Emisiones fugitivas provenientes de la fabricación de combustibles",
    categoria == "Industria de los minerales" ~ "Industria de los minerales",
    categoria == "Industria química" ~ "Industria química",
    categoria == "Industria de los metales" ~ "Industria de los metales",
    subsector %in% c("Uso de productos no energéticos de combustibles y de solvente","Usos de productos como sustitutos de las sustancias que agotan la capa de ozono","") ~ "Otros",
    categoria == "Ganado" ~ "Ganado",
    categoria == "Tierra" ~ "Tierra",
    subsector == "Emisiones de la quema de biomasa" ~ "Emisiones de la quema de biomasa",
    subsector %in% c("Emisiones directas de N2O de los suelos gestionados", "Emisiones indirectas de N2O de los suelos gestionados", "Emisiones indirectas de N2O resultantes de la gestión del estiércol", "Cultivo de Arroz") ~ "Emisiones directas e indirectas de N2O y otros",
    categoria %in% c("Eliminación de residuos sólidos", "Tratamiento biológico de los residuos sólidos", "Incineración de residuos") ~ "Residuos sólidos",
    categoria == "Tratamiento y eliminación de aguas residuales" ~ "Aguas residuales",
    TRUE ~ NA_character_
  )) %>%
  group_by(sector,sector_nuevo, anio) %>%
  summarise(total_valor = sum(valor_en_mtco2e, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(valor_en_porcent = (total_valor / sum(total_valor)) * 100) %>% 
  rename(subsector=sector_nuevo,
         valor_en_mtco2e=total_valor) %>% 
  select(1,3,2,4,5)

## cambio nombee a categoría agricultura ganadería y pesca  
emis_sector_2018_arg_ultimo_año_variables_final$sector <- ifelse(emis_sector_2018_arg_ultimo_año_variables_final$sector == "Agricultura, ganadería, silvicultura y otros usos de la tierra", "AGSyOUT", emis_sector_2018_arg_ultimo_año_variables_final$sector)

# saco los na
emis_sector_2018_arg_ultimo_año_variables_final <- emis_sector_2018_arg_ultimo_año_variables_final[!is.na(emis_sector_2018_arg_ultimo_año_variables_final$subsector), ]
  
# guardo csv
write_csv_fundar(x = emis_sector_2018_arg_ultimo_año_variables_final,
                 file = glue::glue("{tempdir()}/emis_Sector_argentina_2018.csv"))

# agrego fuente clean
agregar_fuente_clean(id_fuente_raw = 131, 
                     dir = tempdir(),
                     path_clean = "emis_Sector_argentina_2018.csv",
                     nombre = "Emisiones mtco2 por secto. Argentina. Año 2018",
                     script = "limpieza_emisiones_co2_argentina_2018.R")

# actualizo fuente clean
actualizar_fuente_clean(id_fuente_clean = 55)

