
#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

limpiar_temps()


id_fuente <- 115
fuente_raw1 <- sprintf("R%sC0",id_fuente)

nombre_archivo_raw <- str_split_1(fuentes_raw() %>% 
                                    filter(codigo == fuente_raw1) %>% 
                                    select(path_raw) %>% 
                                    pull(), pattern = "\\.")[1]

descargar_fuente_raw(id_fuente = id_fuente, tempdir())

# Cargo funciones para hacer limpieza
source("./scripts/limpieza_fuentes/funciones_limpieza_cedlas_sedlac.R")

TOPIC_PARAM <- "Employment"
SHEET_PARAM <- "stru_sector"


cedlas_df <- readxl::read_excel(argendataR::get_temp_path(fuente_raw1), sheet = SHEET_PARAM, col_names = F) 

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



norm_sheet <- str_to_lower(SHEET_PARAM) %>% str_replace(., " ", "_")

clean_filename <- glue::glue("{norm_sheet}_{nombre_archivo_raw}_CLEAN.csv")

path_clean <- glue::glue("{tempdir()}/{clean_filename}")

df_clean %>% write_csv_fundar(., file = path_clean)

code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)

# agregar_fuente_clean(id_fuente_raw = id_fuente,
#                      path_clean = clean_filename,
#                      dir = tempdir(),
#                      nombre = glue::glue("{TOPIC_PARAM} - {tematica} - SEDLAC (serie original y empalmada)"),
#                      descripcion = "La limpieza consiste en llevar los datos de formato en Excel a formato tabular plano listo para poder consumir, se anualizaron los valores que poseían una frecuencia semestral y se calculó una serie empalmada",
#                      script = code_name)

actualizar_fuente_clean(id_fuente_clean = 38,
                        dir = tempdir())