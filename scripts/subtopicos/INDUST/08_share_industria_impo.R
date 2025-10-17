#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

library(data.table)
library(dplyr)

subtopico <- "INDUST"
output_name <- "share_industria_impo.csv"
analista <- "Nicolás Sidicaro"
fuente1 <- 'R104C0' # Atlas Location
fuente2 <- 'R457C0' # Atlas SITC
fuente3 <- 'R456C0' # BACI
fuente4 <- 'R458C298' # Clasificador HS02 a Lall
fuente5 <- 'R459C299' # Clasificador SITC REV 1 (Atlas) a Lall


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
    import_value)  
]


df_atlas_lall <- atlas_lall[
  df_atlas_sitc,
  on = .(sitc_product_code), 
  nomatch = 0,
  allow.cartesian = T
][
  
  ,
  .(import_value = sum(import_value, na.rm = TRUE)), 
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


query_output <- glue::glue(
  "SELECT t as anio, c.j as m49_code, p.country_iso3 as iso3, h.lall_code, SUM(c.v) as impo
   FROM comercio as c
   LEFT JOIN paises as p ON c.j = p.country_code
   LEFT JOIN hs_to_lall as h ON h.hs02 = c.k
   GROUP BY t, c.j, p.country_iso3, h.lall_code"
)


df_query <- dbGetQuery(con, query_output) %>% 
  setDT(.)


df_baci <- df_query %>% 
  mutate(lall_code = tolower(lall_code)) %>% 
  dplyr::filter(!is.na(lall_code)) %>% 
  left_join(atlas_lall %>% 
              distinct(lall_code, lall_desc_full), join_by(lall_code)) %>% 
  mutate(
    impo_baci = impo * 1000) %>%  
  select(anio, iso3, lall_code, lall_desc_full, impo_baci)

rm(df_query)
gc()

proyectar_con_indice <- function(t1_atlas, t1_baci_1) {
  
  baci_base2002 <- t1_baci_1 %>%
    filter(anio == 2002) %>%
    group_by(iso3, lall_code, lall_desc_full) %>%
    summarise(impo_2002 = first(impo), .groups = "drop")
  
  baci_indices <- t1_baci_1 %>%
    filter(anio >= 2002) %>%
    left_join(baci_base2002, by = c("iso3", "lall_code", "lall_desc_full")) %>%
    mutate(
      indice = ifelse(is.na(impo_2002) | impo_2002 == 0,
                      100,
                      (impo / impo_2002) * 100)
    ) %>%
    select(anio, iso3, lall_code, lall_desc_full, indice)
  
  # 2. Obtener valores base de Atlas en 2002
  atlas_2002 <- t1_atlas %>%
    filter(anio == 2002) %>% 
    ungroup() %>% 
    select(iso3, lall_code, lall_desc_full, impo_base_atlas = impo)
  
  # 3. Aplicar índices de BACI a valores base de Atlas
  projection_indices <- baci_indices %>%
    filter(anio > 2002) %>%
    left_join(atlas_2002, by = c("iso3","lall_code", "lall_desc_full")) %>%
    mutate(
      impo_projected = (impo_base_atlas * indice) / 100
    ) %>%
    select(anio, iso3, lall_code, lall_desc_full, impo = impo_projected)
  
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

t1_baci_1 <- df_baci %>% rename(impo = impo_baci)
t1_atlas <- df_atlas_lall %>% rename(impo = import_value)

t2_atlas <- proyectar_con_indice(t1_atlas,t1_baci_1)


df_output <- t2_atlas %>% 
  dplyr::filter(! lall_desc_full %in% c('Otros','Transacciones no clasificadas')) %>% 
  left_join(geo_front, join_by(iso3 == geocodigoFundar)) %>% 
  group_by(anio, iso3) %>% 
  mutate(
    prop = impo /sum(impo, na.rm = T)
  ) %>% 
  ungroup() %>% 
  select(anio, geocodigoFundar = iso3, geonombreFundar, lall_code, lall_desc_full, importaciones = impo, prop, source) %>% 
  drop_na(importaciones) 


df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico) 

pks_comparacion <- c('anio','geocodigoFundar', 'lall_code')

comparacion <- argendataR::comparar_outputs(
  df = df_output,
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
