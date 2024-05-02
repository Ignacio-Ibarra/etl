# descargar fuente raw desde drive
descargar_fuente_raw(id_fuente = "R35C0", dir = tempdir())

sheets <- readxl::excel_sheets(get_temp_path("R35C0"))

# leer los datos
serie_cgi <- readxl::read_excel(get_temp_path("R35C0"), sheet = "VAB_pb")


# pivoteo la tabla a long
serie_cgi <- serie_cgi[-c(1,4:5),] %>%
  t() %>%
  tibble::as_tibble(.name_repair = "unique")

# asigno nombres de columnas limpios tomando fila 2
names(serie_cgi) <- serie_cgi[2,] %>%
  janitor::make_clean_names()

# quito filas 1:2
serie_cgi <- serie_cgi[-c(1:2),]

# nombres de cols anio y trim
serie_cgi <- serie_cgi %>%
  dplyr::rename(anio = na, trim = na_2)

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
serie_cgi <- serie_cgi %>% 
  pivot_longer(cols = -c(anio, trim),
               names_to = "indicador", values_to = "valor") %>% 
  mutate(unidades = "pesos corrientes", 
          valor = 1E6*as.numeric(valor)) %>% 
  relocate(unidades, .after = trim)

# limpio guiones bajos y numeros de nombres de indicador
serie_cgi <- serie_cgi %>% 
  mutate(indicador = gsub("_\\d$", "", indicador))

# guardo el df como csv con estilo fundar
write_csv_fundar(x = serie_cgi,
                 file = "data/_FUENTES/clean/serie_cgi_vab_indec.csv")

# lineas de carga de fuente clean en sheet del drive
# solo se ejecutan al cargar por primera vez la fuente clean

# agregar_fuente_clean(id_fuente_raw = 35,
#                      path_clean = "serie_cgi_vab_indec.csv",
#                      nombre = "CGI - VAB por sector",
#                      script = "limpieza_cgi_vab_mano_de_obra_indec.R")

# linea de actualizacion para futuras cargas
actualizar_fuente_clean(id_fuente_clean = 5)
