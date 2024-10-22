#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


id_fuente <- 35
fuente_raw <- sprintf("R%sC0",id_fuente)

nombre_archivo_raw <- # Guardado de archivo
  nombre_archivo_raw <- sub("\\.[^.]*$", "", fuentes_raw() %>% 
                              filter(codigo == fuente_raw) %>% 
                              select(path_raw) %>% 
                              pull())




# Lectura datos 

SHEET_NAME <- "RTA pp"
serie_cgi <- readxl::read_excel(argendataR::get_raw_path(fuente_raw), sheet = SHEET_NAME)


# pivoteo la tabla a long
serie_cgi <- serie_cgi[-c(1,4:5),] %>%
  t() %>%
  tibble::as_tibble(.name_repair = "unique")

# Me armo un data.frame que sirva de diccionario de letra-sector
no_drop <- function(col) {
  sum(is.na(col)) == 0
}

diccionario_letra <- serie_cgi[1:2,] %>% select_if(~ no_drop(.)) %>% t() %>% as.data.frame()
names(diccionario_letra) <- c("letra","sector")

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
               names_to = "indicador", values_to = "participacion", values_transform = as.numeric) %>% 
  left_join(diccionario_letra, join_by(indicador == sector))

norm_sheet <- str_to_lower(SHEET_NAME) %>% str_replace(., " ", "_")

clean_filename <- glue::glue("{norm_sheet}_{nombre_archivo_raw}_CLEAN.parquet")

path_clean <- glue::glue("{tempdir()}/{clean_filename}")

df_clean %>% arrow::write_parquet(., sink = path_clean)

code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)

titulo.raw <- fuentes_raw() %>% 
  filter(codigo == fuente_raw) %>% 
  select(nombre) %>% pull()

clean_title <- glue::glue("{titulo.raw} - Cuadro: {SHEET_NAME}")

# agregar_fuente_clean(id_fuente_raw = id_fuente,
#                      path_clean = clean_filename,
#                      dir = tempdir(),
#                      nombre = glue::glue("Cuenta Generacion del Ingreso - Cuadro: {SHEET_NAME} - INDEC"),
#                      descripcion = "La limpieza consiste en llevar los datos de formato en Excel a formato tabular plano listo para poder consumir",
#                      script = code_name)

actualizar_fuente_clean(id_fuente_clean = 76,
                        path_clean = clean_filename,
                        nombre = clean_title, 
                        script = code_name)

