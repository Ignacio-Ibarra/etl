#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)

id_fuente <- 115
fuente_raw <- sprintf("R%sC0",id_fuente)

# Guardado de archivo
nombre_archivo_raw <- sub("\\.[^.]*$", "", fuentes_raw() %>% 
                            filter(codigo == fuente_raw) %>% 
                            select(path_raw) %>% 
                            pull())

titulo.raw <- fuentes_raw() %>% 
  filter(codigo == fuente_raw) %>% 
  select(nombre) %>% pull()


# Cargo funciones para hacer limpieza
source("./scripts/limpieza_fuentes/funciones_limpieza_cedlas_sedlac.R")

TOPIC_PARAM <- "Employment"
SHEET_PARAM <- "informal_2"


cedlas_df <- readxl::read_excel(argendataR::get_raw_path(fuente_raw), sheet = SHEET_PARAM, col_names = F) 

cedlas_df <- quitar_string_source(df = cedlas_df)

cedlas_df <- cedlas_df %>%
  select_if(~ !all(is.na(.))) %>%
  filter(rowSums(is.na(.)) < ncol(.)) %>%
  mutate(across(everything(), as.character)) %>%
  as.data.frame()



tematica <- cedlas_df[[1,1]]
variable <- cedlas_df[[2,1]]

geonomenclador <- argendataR::get_nomenclador_geografico()
mapeo_iso3_pais <- setNames(geonomenclador$desc_fundar, geonomenclador$codigo_fundar)

mapeo_pais_iso3_adhoc <- c(
  'Argentina' = 'ARG', 
  'Bolivia' = 'BOL', 
  'Brazil' = 'BRA', 
  'Chile' = 'CHL', 
  'Colombia' = 'COL', 
  'Costa Rica' = 'CRI', 
  'Dominican Rep' = 'DOM', 
  'Ecuador' = 'ECU', 
  'El Salvador' = 'SLV', 
  'Guatemala' = 'GTM', 
  'Honduras' = 'HND', 
  'Mexico' = 'MEX', 
  'Nicaragua' = 'NIC', 
  'Panama' = 'PAN', 
  'Paraguay' = 'PRY', 
  'Peru' = 'PER', 
  'Uruguay' = 'URY', 
  'Venezuela' = 'VEN'
)

lista_paises <- names(mapeo_pais_iso3_adhoc)

df_original <- armar_serie_original(df = cedlas_df, 
                                    topico = TOPIC_PARAM,
                                    tematica = tematica,
                                    variable = variable,
                                    lista.paises = lista_paises, 
                                    mapper.paises_cedlas.a.isos = mapeo_pais_iso3_adhoc,
                                    mapper.isos.a.paises = mapeo_iso3_pais)


df_anual <- armar_serie_anualizada(df_original = df_original)

df_empalme <- armar_serie_empalme(df_anual = df_anual)

df_clean <- armar_tabla(df_anual = df_anual, 
                        df_empalme = df_empalme) 

df_clean$pais <- df_clean$pais %>% unname()
df_clean$apertura <- df_clean$apertura %>% unlist() %>% unname()
df_clean$anio <- as.integer(df_clean$anio)


norm_sheet <- str_to_lower(SHEET_PARAM) %>% str_replace(., " ", "_")

clean_filename <- glue::glue("{norm_sheet}_{nombre_archivo_raw}_CLEAN.parquet")

path_clean <- glue::glue("{tempdir()}/{clean_filename}")

df_clean %>% arrow::write_parquet(., sink = path_clean)

clean_title <- glue::glue("{titulo.raw} - Dataset limpio")

# agregar_fuente_clean(id_fuente_raw = id_fuente,
#                      path_clean = clean_filename,
#                      dir = tempdir(),
#                      nombre = glue::glue("{TOPIC_PARAM} - {tematica} - SEDLAC (serie original y empalmada)"),
#                      descripcion = "La limpieza consiste en llevar los datos de formato en Excel a formato tabular plano listo para poder consumir, se anualizaron los valores que poseían una frecuencia semestral y se calculó una serie empalmada",
#                      script = code_name)

id_fuente_clean <- 32
codigo_fuente_clean <- sprintf("R%sC%s", id_fuente, id_fuente_clean)


df_clean_anterior <- arrow::read_parquet(get_clean_path(codigo = codigo_fuente_clean )) %>% 
  mutate(anio = as.integer(anio))


comparacion <- comparar_fuente_clean(df_clean %>% dplyr::filter(serie == "Serie original"),
                                     df_clean_anterior %>% dplyr::filter(serie == "Serie original"),
                                     pk = c('iso3','anio','fuente', 'fuente_orden','apertura')
)


actualizar_fuente_clean(id_fuente_clean = id_fuente_clean,
                        path_clean = clean_filename,
                        nombre = clean_title, 
                        script = code_name,
                        comparacion = comparacion)
