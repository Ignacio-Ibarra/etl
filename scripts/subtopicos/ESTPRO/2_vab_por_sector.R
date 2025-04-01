################################################################################
##                              Dataset: nombre                               ##
################################################################################

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


subtopico <- "ESTPRO"
output_name <- "vab_por_sector"
analista = "Gisella Pascuariello"
fuente1 <- "R223C94"
fuente2 <- "R36C82"



get_raw_path <- function(codigo){
  prefix <- glue::glue("{Sys.getenv('RUTA_FUENTES')}raw/")
  df_fuentes_raw <- fuentes_raw() 
  path_raw <- df_fuentes_raw[df_fuentes_raw$codigo == codigo,c("path_raw")]
  return(paste0(prefix, path_raw))
}

get_clean_path <- function(codigo){
  prefix <- glue::glue("{Sys.getenv('RUTA_FUENTES')}clean/")
  df_fuentes_clean <- fuentes_clean() 
  path_clean <- df_fuentes_clean[df_fuentes_clean$codigo == codigo,c("path_clean")]
  return(paste0(prefix, path_clean))
}



vabpb_indec_df <- arrow::read_parquet(get_clean_path(fuente1)) %>% 
  dplyr::filter(trimestre == "Total") %>% 
  dplyr::filter(sub_sector == "Total sector") %>% 
  mutate(sector = case_when(
    sector %in% c("Enseñanza",
                  "Servicios sociales y de salud",
                  "Otras actividades de servicios comunitarias, sociales y personales",
                  "Hogares privados con servicio doméstico") ~ "Otros servicios",
    
    sector %in% c("Comercio mayorista, minorista y reparaciones", 
                  "Hoteles y restaurantes") ~ "Comercio, hoteles y restaurantes",
    sector == "Administración pública y defensa; planes de seguridad social de afiliación obligatoria" ~ "Administración pública, defensa y otros",
    TRUE ~ sector
    )) %>% 
  group_by(anio, sector) %>% 
  summarise(vabpb_indec = sum(vab_pb, na.rm = T))

vabpb_fnys_df <- arrow::read_parquet(get_clean_path(fuente2)) %>% 
  dplyr::filter(unidad == "mill. de $ 2004") %>% 
  dplyr::filter(!(indicador %in% c("PIB a precios de mercado",
                                   "PIB a precios básicos",
                                   "IVA + Imp a la Importación + Imp. netos de Subsidios + Residuo por Empalme"))) %>% 
  select(-unidad, -iso3) %>% 
  mutate(sector = case_when(
    indicador == "Comercio al por mayor y menor, y hoteles y restaurantes" ~ "Comercio, hoteles y restaurantes",
    indicador == "Industrias manufactureras" ~ "Industria manufacturera",
    indicador == "Administración pública, defensa y org extraterr." ~ "Administración pública, defensa y otros", 
    TRUE ~ indicador
  )) %>% 
  select(anio, sector, vabpb_fnys = valor) 
  

sector_letra <-  c('Agricultura, ganadería, caza y silvicultura', 
                   'Pesca', 
                   'Explotación de minas y canteras', 
                   'Industria manufacturera', 
                   'Electricidad, gas y agua', 
                   'Construcción', 
                   'Comercio mayorista, minorista y reparaciones', 
                   'Hoteles y restaurantes', 
                   'Transporte, almacenamiento y comunicaciones', 
                   'Intermediación financiera', 
                   'Actividades inmobiliarias, empresariales y de alquiler', 
                   "Administración pública, defensa y otros", 
                   'Enseñanza', 'Servicios sociales y de salud', 
                   'Otras actividades de servicios comunitarias, sociales y personales', 
                   'Hogares privados con servicio doméstico')

letra <-  c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P')

letra_desc <-  c('Agro', 
                 'Pesca', 
                 'Petróleo y minería', 
                 'Industria manufacturera', 
                 'Electricidad, gas y agua', 
                 'Construcción', 'Comercio', 
                 'Hotelería y restaurantes', 
                 'Transporte y comunicaciones', 
                 'Finanzas', 'Serv. inmobiliarios y profesionales', 
                 'Adm. pública y defensa', 
                 'Enseñanza', 
                 'Salud', 
                 'Serv. comunitarios, sociales y personales', 
                 'Servicio doméstico')

#añado descripción de letra de indec 
dicc_sector <- data.frame(sector_letra, letra, letra_desc)


df_output <- vabpb_indec_df %>% 
  full_join(vabpb_fnys_df, by=join_by(anio, sector)) %>% 
  mutate(vabpb_final = ifelse(is.na(vabpb_indec), vabpb_fnys, vabpb_indec)) %>% 
  select(anio, sector, vabpb = vabpb_final) %>% 
  left_join(dicc_sector, by=join_by(sector == sector_letra)) %>% 
  mutate(
    letra = case_when(
          sector == "Comercio, hoteles y restaurantes" ~ "GH",
          sector == "Otros servicios" ~ "Z",
          TRUE ~ letra),
    letra_desc = case_when(
      sector == "Comercio, hoteles y restaurantes" ~ "Comercio, hoteles y restaurantes",
      sector == "Otros servicios" ~ "Otros Servicios",
      TRUE ~ letra_desc)
  ) %>% 
  select(anio, letra, letra_desc, vabpb)
  



df_anterior <- argendataR::descargar_output(nombre = output_name, subtopico = subtopico, entrega_subtopico = "primera_entrega")  %>% 
  select(anio, letra, letra_desc, vabpb = vab)


comparacion <- argendataR::comparar_outputs(
  df_anterior = df_anterior,
  df = df_output,
  nombre = output_name,
  pk = c("anio","letra"), # variables pk del dataset para hacer el join entre bases
  drop_joined_df =  F
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
  dplyr::filter(grepl(output_name, dataset_archivo)) %>% 
  distinct(variable_nombre, descripcion) 


# Cargo las etiquetas a cambiar/modificar en un data.frame con esta estructura
etiquetas_nuevas <- data.frame(
  variable_nombre = c("vabpb", # variable nueva
                      "letra_desc", # variable existente 
                      "letra"), # variable existente 
  descripcion = c("VAB a precios básicos en pesos constantes de 2004", # descripcion variable nueva
                  "Descripción de la codificación del sector económico",
                  "Codificación del sector económico") # descripcion nueva variable existente
)


# Guardo en una variable las columnas del output que queremos escribir
output_cols <- names(df_output) # lo puedo generar así si tengo df_output

descripcion <- armador_descripcion(metadatos = metadatos,
                                   etiquetas_nuevas = etiquetas_nuevas,
                                   output_cols = output_cols)


df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    fuentes = c(fuente1),
    analista = analista,
    pk = c("anio", "letra"),
    es_serie_tiempo = T,
    columna_indice_tiempo = 'anio',
    nivel_agregacion = "país",
    descripcion_columnas = descripcion,
    unidades = list("vabpb" = "unidades"),
    cambio_nombre_cols = list("vabpb" = "vab")
  )

