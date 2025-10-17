#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

# Metadatos 
subtopico <- "INDUST"
output_name <- "proporcion_importaciones_expo.csv"
analista <- "Nicolás Sidicaro"
fuente1 <- 'R104C0' # Atlas Location
fuente2 <- 'R457C0' # Atlas SITC
fuente3 <- 'R456C0' # BACI
fuente4 <- 'R458C298' # Clasificador HS02 a Lall
fuente5 <- 'R459C299' # Clasificador SITC REV 1 (Atlas) a Lall
fuente6 <- 'R462C0' # FRED GDP IPI DEFLATOR

geo_front <- argendataR::get_nomenclador_geografico_front() %>% 
  select(geocodigoFundar = geocodigo, geonombreFundar = name_long)


hs02_lall <- argendataR::get_clean_path(fuente4) %>% 
  arrow::read_parquet() %>% 
  select(hs02, lall_code)


atlas_lall <- argendataR::get_clean_path(fuente5) %>% 
  arrow::read_parquet() %>% 
  select(sitc_product_code, lall_code = lall_desc, lall_desc_full) %>% 
  distinct() %>% 
  setDT(.)


df_atlas_location <-  argendataR::get_raw_path(fuente1) %>% 
  read.csv() %>% 
  dplyr::filter(level == "country") %>% 
  select(location_id, iso3 = location_code) %>% 
  setDT(.)

df_atlas <- argendataR::get_raw_path(fuente2) %>% 
  fst::read_fst(., as.data.table = T) %>%  
  setDT(.)


df_atlas_sitc <- df_atlas[
  !is.na(suppressWarnings(as.numeric(sitc_product_code))),    
  .(iso3 = location_code, 
    anio =  year, 
    sitc_product_code = as.integer(as.numeric(sitc_product_code)), 
    export_value, 
    import_value)  
]


df_atlas_lall <- atlas_lall[
  df_atlas_sitc,
  on = .(sitc_product_code), 
  nomatch = 0,
  allow.cartesian = T
][
  
  ,
  .(export_value = sum(export_value, na.rm = TRUE),
    import_value = sum(import_value, na.rm = TRUE)),
  by = .(iso3, anio, lall_code, lall_desc_full)
  
]


rm(df_atlas)
rm(df_atlas_sitc)
gc()  

source("scripts/utils/baci_data.R")


con <- argendataR::get_raw_path(fuente3) %>% 
  BACI.get_db_from_zip(.)

dbWriteTable(con, 
             "hs_to_lall", 
             hs02_lall, 
             overwrite = TRUE)


query_output_expo <- glue::glue(
  "SELECT t as anio, c.i as m49_code, p.country_iso3 as iso3, h.lall_code, SUM(c.v) as expo
   FROM comercio as c
   LEFT JOIN paises as p ON c.i = p.country_code
   LEFT JOIN hs_to_lall as h ON h.hs02 = c.k
   GROUP BY t, c.i, p.country_iso3, h.lall_code"
)


df_query_expo <- dbGetQuery(con, query_output_expo) %>% 
  setDT(.)


query_output_impo <- glue::glue(
  "SELECT t as anio, c.j as m49_code, p.country_iso3 as iso3, h.lall_code, SUM(c.v) as impo
   FROM comercio as c
   LEFT JOIN paises as p ON c.j = p.country_code
   LEFT JOIN hs_to_lall as h ON h.hs02 = c.k
   GROUP BY t, c.j, p.country_iso3, h.lall_code"
)


df_query_impo <- dbGetQuery(con, query_output_impo) %>% 
  setDT(.)


df_baci <- df_query_expo %>% 
  full_join(df_query_impo, join_by(anio, m49_code, iso3, lall_code)) %>% 
  mutate(lall_code = tolower(lall_code)) %>% 
  dplyr::filter(!is.na(lall_code)) %>% 
  left_join(atlas_lall %>% 
              distinct(lall_code, lall_desc_full), join_by(lall_code)) %>% 
  mutate(
    expo = expo,
    impo = impo) %>%  
  select(anio, iso3, lall_code, lall_desc_full, expo, impo)

rm(df_query_expo)
rm(df_query_impo)
gc()

proyectar_con_indice <- function(t1_atlas, t1_baci_1) {
  
  baci_base2002 <- t1_baci_1 %>%
    filter(anio == 2002) %>%
    group_by(iso3, lall_code, lall_desc_full) %>%
    summarise(expo_2002 = first(expo),
              impo_2002 = first(impo), .groups = "drop")
  
  baci_indices <- t1_baci_1 %>%
    filter(anio >= 2002) %>%
    left_join(baci_base2002, by = c("iso3", "lall_code", "lall_desc_full")) %>%
    mutate(
      indice_expo = ifelse(is.na(expo_2002) | expo_2002 == 0,
                      100,
                      (expo / expo_2002) * 100),
      indice_impo = ifelse(is.na(impo_2002) | impo_2002 == 0,
                           100,
                           (impo / impo_2002) * 100)
    ) %>%
    select(anio, iso3, lall_code, lall_desc_full, indice_expo, indice_impo)
  
  # 2. Obtener valores base de Atlas en 2002
  atlas_2002 <- t1_atlas %>%
    filter(anio == 2002) %>% 
    ungroup() %>% 
    select(iso3, lall_code, lall_desc_full, expo_base_atlas = expo, impo_base_atlas = impo)
  
  # 3. Aplicar índices de BACI a valores base de Atlas
  projection_indices <- baci_indices %>%
    filter(anio > 2002) %>%
    left_join(atlas_2002, by = c("iso3","lall_code", "lall_desc_full")) %>%
    mutate(
      expo_projected = (expo_base_atlas * indice_expo) / 100,
      impo_projected = (impo_base_atlas * indice_impo) / 100
    ) %>%
    select(anio, iso3, lall_code, lall_desc_full, expo = expo_projected, impo = impo_projected)
  
  # 4. Combinar con datos históricos
  atlas_historical <- t1_atlas %>%
    filter(anio <= 2002)
  
  atlas_extended_indice <- bind_rows(
    atlas_historical,
    projection_indices
  ) %>%
    arrange(iso3, lall_code, lall_desc_full, anio) %>%
    mutate(
      source = case_when(
        anio <= 2002 ~ "atlas_original",
        TRUE ~ "proyeccion_indice_baci"
      )
    )
  
  return(atlas_extended_indice)
}



#### PREPARO BASES #######


t1_atlas <- df_atlas_lall %>% rename(expo = export_value, impo = import_value)

t2_atlas <- proyectar_con_indice(t1_atlas,df_baci)


##### US GDP IPI DEFLATOR #########


df_ipi_anual <- argendataR::get_raw_path(fuente6) %>% 
  read.csv() %>% 
  mutate(anio = year(as.Date(date))) %>% 
  group_by(anio) %>% 
  summarise(
    ipi_deflator = mean(value, na.rm = T)
  ) 

anio_base <- max(t2_atlas$anio)

ipi_anio_base <- df_ipi_anual %>%
  filter(anio == anio_base) %>%
  pull(ipi_deflator)

df_ipi_anual_base <- df_ipi_anual %>%
  mutate(ipi_base = (ipi_deflator / ipi_anio_base) * 100) %>% 
  select(anio, ipi_base)


df_output <- t2_atlas %>% 
  dplyr::filter(grepl("Manufacturas.*", lall_desc_full)) %>% 
  group_by(anio, iso3) %>% 
  summarise(
    `Exportaciones industriales` = sum(expo, na.rm = T),
    `Importaciones industriales` = sum(impo, na.rm = T)
  ) %>% 
  pivot_longer(
    cols = -c(anio, iso3),
    names_to = "flujo", 
    values_to = "valores_corrientes"
  ) %>% 
  left_join(df_ipi_anual_base, join_by(anio)) %>% 
  mutate(valores_constantes = valores_corrientes / ipi_base) %>% 
  left_join(geo_front, join_by(iso3 == geocodigoFundar)) %>% 
  select(anio, geocodigoFundar = iso3, geonombreFundar, flujo, valores_corrientes, valores_constantes)


df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico) 


pks_comparacion <- c('anio','geocodigoFundar', 'flujo')

comparacion <- argendataR::comparar_outputs(
  df = df_comparable,
  df_anterior = df_anterior,
  nombre = output_name,
  pk = pks_comparacion
)



armador_descripcion <- function(metadatos, etiquetas_nuevas = data.frame(), output_cols){
  # metadatos: data.frame sus columnas son variable_nombre y descripcion y 
  # proviene de la info declarada por el analista 
  # etiquetas_nuevas: data.frame, tiene que ser una dataframe con la columna 
  # variable_nombre y la descripcion
  # output_cols: vector, tiene las columnas del dataset que se quiere escribir
  
  etiquetas <- metadatos %>% 
    dplyr::filter(variable_nombre %in% output_cols) 
  
  
  etiquetas <- etiquetas %>% 
    bind_rows(etiquetas_nuevas)
  
  
  diff <- setdiff(output_cols, etiquetas$variable_nombre)
  
  stopifnot(`Error: algunas columnas de tu output no fueron descriptas` = length(diff) == 0)
  
  # En caso de que haya alguna variable que le haya cambiado la descripcion pero que
  # ya existia se va a quedar con la descripcion nueva. 
  
  etiquetas <- etiquetas %>% 
    group_by(variable_nombre) %>% 
    filter(if(n() == 1) row_number() == 1 else row_number() == n()) %>%
    ungroup()
  
  etiquetas <- stats::setNames(as.list(etiquetas$descripcion), etiquetas$variable_nombre)
  
  return(etiquetas)
  
}

# Tomo las variables output_name y subtopico declaradas arriba
metadatos <- argendataR::metadata(subtopico = subtopico) %>% 
  dplyr::filter(grepl(paste0("^", output_name), nombre_archivo)) %>% 
  distinct(variable_nombre, descripcion) 



# Guardo en una variable las columnas del output que queremos escribir
output_cols <- names(df_output) # lo puedo generar así si tengo df_output



descripcion <- armador_descripcion(metadatos = metadatos,
                                   # etiquetas_nuevas = etiquetas_nuevas,
                                   output_cols = output_cols)



df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    control = comparacion, 
    fuentes = argendataR::colectar_fuentes(),
    analista = analista,
    pk = pks,
    descripcion_columnas = descripcion, 
    unidad = list("poblacion" = "unidades", "share" = "porcentaje"))


output_name <- gsub("\\.csv", "", output_name)
mandar_data(paste0(output_name, ".csv"), subtopico = subtopico, branch = "main")
mandar_data(paste0(output_name, ".json"), subtopico = subtopico,  branch = "main")

# carga de fuentes - nico 
df_comex <- readr::read_csv(file.path(instub,fuente1))

# Función principal: SITC -> Sectores principales (descripción completa)
asignar_sector_principal <- function(sitc_codes) {
  sitc_numeric <- as.numeric(substr(as.character(sitc_codes), 1, 2))
  
  case_when(
    sitc_numeric >= 0 & sitc_numeric <= 9 ~ "Agricultura, ganadería, caza y silvicultura",
    sitc_numeric >= 21 & sitc_numeric <= 22 ~ "Agricultura, ganadería, caza y silvicultura",
    sitc_numeric == 29 ~ "Agricultura, ganadería, caza y silvicultura",
    sitc_numeric == 3 ~ "Pesca",
    sitc_numeric >= 23 & sitc_numeric <= 28 ~ "Explotación de minas y canteras",
    sitc_numeric >= 32 & sitc_numeric <= 35 ~ "Explotación de minas y canteras",
    sitc_numeric >= 68 & sitc_numeric <= 68 ~ "Explotación de minas y canteras",
    sitc_numeric >= 11 & sitc_numeric <= 12 ~ "Industrias manufactureras",
    sitc_numeric >= 41 & sitc_numeric <= 43 ~ "Industrias manufactureras",
    sitc_numeric >= 51 & sitc_numeric <= 59 ~ "Industrias manufactureras",
    sitc_numeric >= 61 & sitc_numeric <= 67 ~ "Industrias manufactureras",
    sitc_numeric == 69 ~ "Industrias manufactureras",
    sitc_numeric >= 71 & sitc_numeric <= 79 ~ "Industrias manufactureras",
    sitc_numeric >= 81 & sitc_numeric <= 89 ~ "Industrias manufactureras",
    TRUE ~ "No clasificado"
  )
}

# convertir sitc_product_code a numerico 
df_comex <- df_comex %>% 
  mutate(industria = as.numeric(product_sitc_code))
df_comex <- df_comex %>% 
  mutate(industria = asignar_sector_principal(industria))

# calcular exportaciones 
df_comex <- df_comex %>% 
  group_by(year,industria,country_iso3_code) %>% 
  summarize(exportaciones = sum(export_value),
            importaciones = sum(import_value))

# Filtrar por industria 
df_comex <- df_comex %>% 
  filter(industria == 'Industrias manufactureras')

# Calcular proporcion entre ambas variables 
df_comex <- df_comex %>% 
  mutate(prop_impo_expo = importaciones / exportaciones)
df_comex$exportaciones <- NULL
df_comex$importaciones <- NULL
df_comex$industria <- NULL



df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico, drive = T) 



# guardar resultados 
readr::write_csv(df_comex,file.path(outstub,paste0(output_name,'.csv')))
