#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


get_raw_path <- function(codigo){
  prefix <- glue::glue("{Sys.getenv('RUTA_FUENTES')}raw/")
  df_fuentes_raw <- fuentes_raw() 
  path_raw <- df_fuentes_raw[df_fuentes_raw$codigo == codigo,c("path_raw")]
  return(paste0(prefix, path_raw))
}


id_fuente <- 131
fuente_raw <- sprintf("R%sC0",id_fuente)

sheet_name = "Serie INGEI 1990-2018"

# traigo la data 
df_clean <- readxl::read_xlsx(argendataR::get_raw_path(fuente_raw),skip = 1, sheet = sheet_name) %>% 
  janitor::clean_names() %>% 
  rename(anio = ano, valor_en_mtco2e = valor) %>%
  select(-cod_pcia_indec, -jurisdiccion)


nombre_archivo_raw <- "inventario-nacional-gei-emisiones_hasta_2018"

norm_sheet <- str_to_lower(sheet_name) %>% str_replace_all(., " ", "_")

clean_filename <- glue::glue("{norm_sheet}_{nombre_archivo_raw}_CLEAN.parquet")

path_clean <- glue::glue("{tempdir()}/{clean_filename}")

df_clean %>% arrow::write_parquet(., sink = path_clean)

code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)

# agregar_fuente_clean(id_fuente_raw = id_fuente,
#                      df = df_clean,
#                      path_clean = clean_filename,
#                      nombre = glue::glue("Inventario Nacional de Gases de Efecto Invernadero y Monitoreo de Medidas de Mitigación (2018) - Solapa: {sheet_name}"),
#                      script = code_name)

lista_comparacion <- comparar_fuente_clean(df_clean,
                      id = 55,
                      pk = df_clean %>% select(-c(valor_en_mtco2e)) %>% colnames())

actualizar_fuente_clean(id_fuente_clean = 55, nombre = glue::glue("Inventario Nacional de Gases de Efecto Invernadero y Monitoreo de Medidas de Mitigación (2018) - Solapa: {sheet_name}"),
                        path_clean = clean_filename, directorio = tempdir(), comparacion = lista_comparacion)

# ## filtro ultimo año 
# ultimo_anio <- max(emis_sector_2018_arg$Año)  # Encuentra el último año presente en la columna 'anio'
# 
# ## filtro la base
# emis_sector_2018_arg_ultimo_año <- emis_sector_2018_arg %>% 
#   filter(Año == ultimo_anio)
# 



# ## armo todos los sectores subsectores como describe analista
# emis_sector_2018_arg_ultimo_año_variables_final <- emis_sector_2018_arg_ultimo_año_variables %>%
#   mutate(sector_nuevo = case_when(
#     subsector == "Industrias de la energía" ~ "Industrias de la energía",
#     subsector == "Industrias manufactureras y de la construcción" ~ "Industrias manufactureras y de la construcción",
#     subsector == "Transporte" ~ "Transporte",
#     subsector == "Otros sectores" ~ "Otros sectores",
#     categoria == "Emisiones fugitivas provenientes de la fabricación de combustibles" ~ "Emisiones fugitivas provenientes de la fabricación de combustibles",
#     categoria == "Industria de los minerales" ~ "Industria de los minerales",
#     categoria == "Industria química" ~ "Industria química",
#     categoria == "Industria de los metales" ~ "Industria de los metales",
#     subsector %in% c("Uso de productos no energéticos de combustibles y de solvente","Usos de productos como sustitutos de las sustancias que agotan la capa de ozono","") ~ "Otros",
#     categoria == "Ganado" ~ "Ganado",
#     categoria == "Tierra" ~ "Tierra",
#     subsector == "Emisiones de la quema de biomasa" ~ "Emisiones de la quema de biomasa",
#     subsector %in% c("Emisiones directas de N2O de los suelos gestionados", "Emisiones indirectas de N2O de los suelos gestionados", "Emisiones indirectas de N2O resultantes de la gestión del estiércol", "Cultivo de Arroz") ~ "Emisiones directas e indirectas de N2O y otros",
#     categoria %in% c("Eliminación de residuos sólidos", "Tratamiento biológico de los residuos sólidos", "Incineración de residuos") ~ "Residuos sólidos",
#     categoria == "Tratamiento y eliminación de aguas residuales" ~ "Aguas residuales",
#     TRUE ~ NA_character_
#   )) %>%
#   group_by(sector,sector_nuevo, anio) %>%
#   summarise(total_valor = sum(valor_en_mtco2e, na.rm = TRUE)) %>% 
#   ungroup() %>% 
#   mutate(valor_en_porcent = (total_valor / sum(total_valor)) * 100) %>% 
#   rename(subsector=sector_nuevo,
#          valor_en_mtco2e=total_valor) %>% 
#   select(1,3,2,4,5)
# 
# ## cambio nombee a categoría agricultura ganadería y pesca  
# emis_sector_2018_arg_ultimo_año_variables_final$sector <- ifelse(emis_sector_2018_arg_ultimo_año_variables_final$sector == "Agricultura, ganadería, silvicultura y otros usos de la tierra", "AGSyOUT", emis_sector_2018_arg_ultimo_año_variables_final$sector)
# 
# # saco los na
# emis_sector_2018_arg_ultimo_año_variables_final <- emis_sector_2018_arg_ultimo_año_variables_final[!is.na(emis_sector_2018_arg_ultimo_año_variables_final$subsector), ]
#   
# # guardo csv
# write_csv_fundar(x = emis_sector_2018_arg_ultimo_año_variables_final,
#                  file = glue::glue("{tempdir()}/emis_Sector_argentina_2018.csv"))



