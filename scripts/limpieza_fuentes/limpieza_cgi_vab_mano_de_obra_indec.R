
descargar_fuente_raw(id_fuente = 35, dir = "data/_FUENTES/raw/")

sheets <- readxl::excel_sheets("data/_FUENTES/raw/serie_cgi_01_24.xls")

serie_cgi <- readxl::read_excel("data/_FUENTES/raw/serie_cgi_01_24.xls", sheet = 1)

serie_cgi <- serie_cgi[-c(1,4:5),] |>
  t() |>
  tibble::as_tibble(.name_repair = "unique")

names(serie_cgi) <- serie_cgi[2,] |>
  janitor::make_clean_names()

serie_cgi <- serie_cgi[-c(1:2),]

serie_cgi <- serie_cgi |>
  dplyr::rename(anio = na, trim = na_2)

serie_cgi <- serie_cgi |>
  dplyr::mutate(anio = as.numeric(gsub(" .*", "", anio )))

serie_cgi <- serie_cgi |>
  tidyr::fill(anio)

serie_cgi <- serie_cgi |>
  dplyr::filter(!is.na(trim))

serie_cgi <- serie_cgi[,!sapply(serie_cgi, function(x) {sum(is.na(x)) == length(x)})]

serie_cgi <- serie_cgi %>% 
  pivot_longer(cols = -c(anio, trim),
               names_to = "indicador", values_to = "valor") %>% 
  mutate(unidades = "pesos corrientes", 
          valor = 1E6*as.numeric(valor)) %>% 
  relocate(unidades, .after = trim)

serie_cgi <- serie_cgi %>% 
  mutate(indicador = gsub("_\\d$", "", indicador))

write_csv_fundar(x = serie_cgi, file = "data/_FUENTES/clean/serie_cgi_vab_indec.csv")


# agregar_fuente_clean(id_fuente_raw = 35,
#                      path_clean = "serie_cgi_vab_indec.csv",
#                      nombre = "CGI - VAB por sector",
#                      script = "limpieza_cgi_vab_mano_de_obra_indec.R")

actualizar_fuente_clean(id_fuente_clean = 5)
