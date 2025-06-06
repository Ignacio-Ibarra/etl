#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

subtopico <- "PESCAS"
output_name <- "01_produccion_pesquera_captura_y_acuicola_por_pais_ultimo_anio.csv"
analista <- "Ignacio Ibarra"
fuente1 <- 'R320C189' # FAO Fisheries and Aquaculture Data Collection. Global Aquaculture Production - File: Aquaculture_Quantity.csv
fuente2 <- 'R320C190' # FAO Fisheries and Aquaculture Data Collection. Global Aquaculture Production - File: CL_FI_SPECIES_GROUPS
fuente3 <- 'R320C191' # FAO Fisheries and Aquaculture Data Collection. Global Aquaculture Production - File: CL_FI_COUNTRY_GROUPS
fuente4 <- 'R320C192' # FAO Fisheries and Aquaculture Data Collection. Global Aquaculture Production - File: CL_FI_WATERAREA_GROUPS
fuente5 <- 'R321C193' # FAO Fisheries and Aquaculture Data Collection. Global Capture Production - File: Capture_Quantity.csv



df_captura <- argendataR::get_clean_path(fuente5) %>% 
  arrow::read_parquet(.)

df_acuicola <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet(.)

df_species <- argendataR::get_clean_path(fuente2) %>% 
  arrow::read_parquet(.)

df_countries <- argendataR::get_clean_path(fuente3) %>% 
  arrow::read_parquet(.)  %>% 
  select(un_code, iso3 = iso3_code) %>% 
  dplyr::filter(iso3 !="", un_code != 532)

geonomenclador <- argendataR::get_nomenclador_geografico_front() %>% 
  select(iso3 = geocodigo, pais_nombre = name_long)

df_areas <- argendataR::get_clean_path(fuente4) %>% 
  arrow::read_parquet(.)

un_codes <- df_countries %>% pull(un_code)

df_prod_acuicola <- df_acuicola %>% 
  dplyr::filter(country_un_code %in% un_codes) %>% 
  left_join(df_countries, join_by(country_un_code == un_code)) %>% 
  left_join(geonomenclador, join_by(iso3)) %>% 
  group_by(anio = period, iso3, pais_nombre) %>% 
  summarise(produccion_acuicola = sum(value, na.rm = T)) %>% 
  ungroup()


df_prod_pesquera <- df_captura %>% 
  dplyr::filter(country_un_code %in% un_codes) %>% 
  left_join(df_countries, join_by(country_un_code == un_code)) %>% 
  left_join(geonomenclador, join_by(iso3)) %>% 
  group_by(anio = period, iso3, pais_nombre) %>% 
  summarise(produccion_captura = sum(value, na.rm = T)) %>% 
  ungroup()


df_output <- df_prod_acuicola %>% 
  full_join(df_prod_pesquera, join_by(anio, iso3, pais_nombre)) %>% 
  dplyr::filter(anio == max(anio)) %>% 
  dplyr::filter(!(is.na(produccion_acuicola) & is.na(produccion_captura))) %>% 
  mutate(
    produccion_acuicola = replace_na(produccion_acuicola, 0),
    produccion_captura = replace_na(produccion_captura, 0),
    produccion_total = produccion_acuicola + produccion_captura,
    participacion = produccion_total /sum(produccion_total)
  ) %>% 
  select(anio, iso3, pais_nombre, produccion_captura, produccion_acuicola, produccion_total, participacion) 


df_output %>%
  argendataR::write_csv_fundar(output_name)


#### CUENTAS


# regiones <- argendataR::get_clean_path(fuente3) %>% 
#   arrow::read_parquet(.) %>% 
#   select(iso3 = iso3_code, region = geo_region_group_es, continente = continent_group_es) %>% 
#   mutate(region2 = case_when(
#     region %in% c("América del Sur", "América central", "Caribe") ~ "Latinoamérica y el Caribe",
#     region == "América del Norte" ~ region, 
#     TRUE ~ continente
#   ))
#            
# 
# por_region <- df_output %>% 
#   inner_join(regiones, join_by(iso3)) %>% 
#   group_by(region2) %>% 
#   summarise(
#     produccion_captura = sum(produccion_captura),
#     produccion_acuicola = sum(produccion_acuicola),
#     produccion_total = sum(produccion_total)
#   )
# 
# produccion_global <- df_output %>%
# summarise(
#   produccion_captura_global= sum(produccion_captura),
#   produccion_acuicola_global = sum(produccion_acuicola),
#   produccion_total_global = sum(produccion_total)
# )
# 




plot_data <- df_output %>%
  arrange(desc(participacion)) %>%
  slice_head(n = 20) %>%
  bind_rows(df_output %>% filter(pais_nombre == "Argentina")) %>%
  distinct() %>%  # Por si Argentina ya estaba en el top 20
  mutate(pais_nombre = factor(pais_nombre, levels = rev(unique(pais_nombre))))

regular_texto <- 0.001

ggplot(plot_data, aes(x = participacion, y = pais_nombre,
                      fill = case_when(
                        pais_nombre == "Argentina" ~ "Argentina",
                        TRUE ~ "Otros"
                      ))) +
  geom_col(color = "black", linewidth = 0.15, position = position_nudge(y = 0.2), width = 0.8) +
  scale_fill_manual(values = c("Argentina" = "#45bcc5", "Otros" = "#fc5a0a")) +  # Colores condicionales
  geom_text(
    aes(label = scales::percent(participacion, accuracy = 0.1),x = participacion + regular_texto),
    vjust = 0, hjust = 0, color = "black", size = 3) +  # Color de las etiquetas en blanco
  labs(y = "", x = "Participación en la producción global (%)") +
  theme_minimal() +
  theme(
    legend.position = "none",  # Oculta la leyenda
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black")
  )

df_anterior <- df_output # No hay output contra qué comparar


comparacion <- argendataR::comparar_outputs(
  df_anterior = df_anterior,
  df = df_output,
  nombre = output_name,
  pk = c("iso3"), # variables pk del dataset para hacer el join entre bases
  drop_joined_df =  F
)




colectar_fuentes <- function(pattern = "^fuente.*"){
  
  # Genero un vector de codigos posibles
  posibles_codigos <- c(fuentes_raw()$codigo,fuentes_clean()$codigo)
  
  # Usar ls() para buscar variables en el entorno global
  variable_names <- ls(pattern = pattern, envir = globalenv())
  
  # Obtener los valores de esas variables
  valores <- unlist(mget(variable_names, envir = globalenv()))
  
  # Filtrar aquellas variables que sean de tipo character (string)
  # Esto es para que la comparacion sea posible en la linea de abajo
  strings <- valores[sapply(valores, is.character)]
  
  # solo devuelvo las fuentes que existen
  return(valores[valores %in% posibles_codigos])
}


source("scripts/utils/metadata_output.R")


metadata <- metadata("PESCAS")

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    fuentes = colectar_fuentes(),
    analista = analista,
    pk =  c("iso3"),
    es_serie_tiempo = F,
    control = comparacion 
    # unidades = list("expo" = "miles de dólares",
    #                 "share" = "porcentaje")
  )


nombre_archivo <- tools::file_path_sans_ext(output_name)
mandar_data(paste0(nombre_archivo, ".csv"), subtopico = subtopico, branch = "dev")
mandar_data(paste0(nombre_archivo, ".json"), subtopico = subtopico,  branch = "dev")


