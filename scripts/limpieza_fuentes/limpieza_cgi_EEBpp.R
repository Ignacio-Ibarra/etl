#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

limpiar_temps()


id_fuente <- 35
fuente_raw1 <- sprintf("R%sC0",id_fuente)

nombre_archivo_raw <- str_split_1(fuentes_raw() %>% 
                                    filter(codigo == fuente_raw1) %>% 
                                    select(path_raw) %>% 
                                    pull(), pattern = "\\.")[1]

descargar_fuente_raw(id_fuente = id_fuente, tempdir())


# Lectura datos 

SHEET_NAME <- "EEB pp"
serie_cgi <- readxl::read_excel(get_temp_path(fuente_raw1), sheet = SHEET_NAME)


# pivoteo la tabla a long
serie_cgi <- serie_cgi[-c(1,4:5),] %>%
  t() %>%
  tibble::as_tibble(.name_repair = "unique")

# asigno nombres de columnas limpios tomando fila 2
names(serie_cgi) <- serie_cgi[2,]

# quito filas 1:2
serie_cgi <- serie_cgi[-c(1:2),]

# nombres de cols anio y trim
names(serie_cgi)[1:2] <- c('anio','trim')

serie_cgi <- serie_cgi[, !is.na(names(serie_cgi))]

# anio a numerico sin marcas adicionale
serie_cgi <- serie_cgi %>%
  dplyr::mutate(anio = as.numeric(gsub(" .*", "", anio )))

# completo filas en blanco con valor de anio correspondiente
serie_cgi <- serie_cgi %>%
  tidyr::fill(anio)

# quito filas en blanco
serie_cgi <- serie_cgi %>%
  dplyr::filter(!is.na(trim))

# quito columnas vacias
serie_cgi <- serie_cgi[,!sapply(serie_cgi, function(x) {sum(is.na(x)) == length(x)})]

# pivoteo a la long estricto, agrego col unidades y paso valores de millones a unidades
df_clean <- serie_cgi %>% 
  pivot_longer(cols = -c(anio, trim),
               names_to = "indicador", values_to = "participacion", values_transform = as.numeric)

norm_sheet <- str_to_lower(SHEET_NAME) %>% str_replace(., " ", "_")

clean_filename <- glue::glue("{norm_sheet}_{nombre_archivo_raw}_CLEAN.csv")

path_clean <- glue::glue("{tempdir()}/{clean_filename}")

df_clean %>% write_csv_fundar(., file = path_clean)

code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)

# agregar_fuente_clean(id_fuente_raw = id_fuente,
#                      path_clean = clean_filename,
#                      dir = tempdir(),
#                      nombre = glue::glue("Cuenta Generacion del Ingreso - Cuadro: {SHEET_NAME} - INDEC"),
#                      descripcion = "La limpieza consiste en llevar los datos de formato en Excel a formato tabular plano listo para poder consumir",
#                      script = code_name)

actualizar_fuente_clean(id_fuente_clean = 79,
                        dir = tempdir())