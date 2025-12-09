#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)

id_fuente <- 132
fuente_raw <- sprintf("R%sC0",id_fuente)

sufijo <- gsub(".*/|\\..*", "", get_raw_path(fuente_raw))

filename <- glue::glue("{sufijo}.parquet")

df <- readr::read_csv(get_raw_path("R132C0"))  

df <- df %>% 
  janitor::clean_names()
  
df %>% 
  select(-c(matches("x\\d{4}"))) %>% 
  colnames()

df %>% 
    pull(unit ) %>% 
  unique(.) %>% sort()

df <- df %>% 
  select(-c(source, provenance)) %>%
  pivot_longer(cols = starts_with("x"), names_to = "anio", values_to = "valor",
               names_transform = ~ gsub("^x", "", .))

#   filter(scenario_primap_hist == "HISTCR" & 
#            area_iso3 == "EARTH" &
#            entity == "KYOTOGHG (AR6GWP100)" 
#            ) %>% 
#   distinct(unit)
# filter(country == "EARTH" & category %in% c("CAT1", "CAT2", "CAT3", "CAT4", "CAT5", "CAT6", "CAT7") & unit == "GgCO2eq")
# 
# # dejo la variables que ncesitamos
# emis_global_sector_1850_2014 <- emis_global_sector_1850_2014 %>% 
#   select(-1,-2,-3,-5)
# 
# ## transformo los datos
# emis_global_sector_1850_2014_long <- pivot_longer(emis_global_sector_1850_2014, 
#                                             cols = -c(unit,category),  # Columnas a mantener fijas
#                                             names_to = "anio",             # Nombre para la columna de años
#                                             values_to = "valor")          # Nombre para la columna de valores
# # armo sectores
# emis_global_sector_1850_2014_long_sectores <- emis_global_sector_1850_2014_long %>% 
# mutate(sector = case_when(
#   category %in% c("CAT1") ~ "Energía",
#   category %in% c("CAT4", "CAT5") ~ "AGSyOUT",
#   category %in% c("CAT2", "CAT3") ~ "PIUP",
#   category == "CAT6" ~ "Residuos",
#   category == "CAT7" ~ "Otros",
#   TRUE ~ NA_character_  # Mantener NA para otras categorías no especificadas
# )) %>% 
#   group_by(anio, sector) %>%
#   summarise(total_valor = sum(valor, na.rm = TRUE)) %>% 
#   rename(valor_en_ggco2e=total_valor)
# 
# # guardo csv
# write_csv_fundar(x = df,
#                  file = glue::glue("{tempdir()}/emis_global_sectores_1850_2014.csv"))


# agrego fuente clean
# agregar_fuente_clean(id_fuente_raw = 132, 
#                      dir = tempdir(),
#                      # path_clean = "emis_global_sectores_1850_2014.csv",
#                      # nombre = "Emisiones globales por sector año 1850 - 2014",
#                      )



df_anterior <-  read_fuente_clean(56) %>% 
  filter(area_iso3 == "EARTH")

lista_comparacion <-  comparar_fuente_clean(df =  df %>% 
                                              filter(area_iso3 == "EARTH"),
                                            df_anterior,
                                            pk = c("anio", "scenario_primap_hist",
                                                   "category_ipcc2006_primap",
                                                   "area_iso3", "entity", "unit"))

metadata <- yaml::read_yaml("https://zenodo.org/records/17090760/files/Guetschow_et_al_2025a-PRIMAP-hist_v2.7_final_22-Aug-2025.yaml?download=1")
metadata <- metadata %>% yaml::as.yaml()

# actualizo fuente clean
actualizar_fuente_clean(id_fuente_clean = 56,df = df,
                        path_clean = filename,
                        descripcion = metadata, 
                        nombre = "The PRIMAP-hist national historical emissions time series (1750-2024) v2.7",
                        comparacion = lista_comparacion)
