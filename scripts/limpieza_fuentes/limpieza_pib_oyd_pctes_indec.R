
descargar_fuente_raw(id_fuente = 38, dir = "data/_FUENTES/raw/")


# cuadro 1 ----------------------------------------------------------------


oyd_pctes <- readxl::read_excel("data/_FUENTES/raw/sh_oferta_demanda_12_23.xls",
                   sheet = 2)

oyd_pctes <- oyd_pctes[-c(1:2),] |>
  t() |>
  tibble::as_tibble(.name_repair = "unique")

names(oyd_pctes) <- oyd_pctes[1,] |>
  janitor::make_clean_names()

oyd_pctes <- oyd_pctes |>
  dplyr::rename(anio = na, trim = na_2)

oyd_pctes <- oyd_pctes |>
  dplyr::mutate(anio = as.numeric(gsub(" .*", "", anio )))

oyd_pctes <- oyd_pctes |>
  tidyr::fill(anio)

oyd_pctes <- oyd_pctes |>
  dplyr::filter(!is.na(trim))

oyd_pctes <- oyd_pctes[,!sapply(oyd_pctes, function(x) {sum(is.na(x)) == length(x)})]


colnames(oyd_pctes) <- gsub("_\\d$","",colnames(oyd_pctes))

oyd_pctes <- oyd_pctes %>% 
  pivot_longer(-c(anio, trim), values_to = "valor", names_to = "indicador") %>% 
  mutate(valor = 1E6*as.numeric(valor),
         unidad = "pesos constantes 2004")

write_csv_fundar(x = oyd_pctes,
                 file = "data/_FUENTES/clean/oferta_demanda_pctes.csv")

# carga fuente limpia en el drive

agregar_fuente_clean(id_fuente_raw = 38,
                     path_clean = "oferta_demanda_pctes.csv",
                     nombre = "Oferta y Demanda Globales trimestrales a precios 2004",
                     script = "limpieza_pib_oyd_pctes_indec.R")

actualizar_fuente_clean(id_fuente_clean = 6)

rm(oyd_pctes)

# cuadro 12 ---------------------------------------------------------------

pib_categoria_pcorr <- readxl::read_excel("data/_FUENTES/raw/sh_oferta_demanda_12_23.xls",
                                sheet = 13)

pib_categoria_pcorr <- pib_categoria_pcorr[-c(1:2),] |>
  t() |>
  tibble::as_tibble(.name_repair = "unique")

names(pib_categoria_pcorr) <- pib_categoria_pcorr[1,] |>
  janitor::make_clean_names()

pib_categoria_pcorr <- pib_categoria_pcorr |>
  dplyr::rename(anio = na, trim = na_2)

pib_categoria_pcorr <- pib_categoria_pcorr |>
  dplyr::mutate(anio = as.numeric(gsub(" .*", "", anio )))

pib_categoria_pcorr <- pib_categoria_pcorr |>
  tidyr::fill(anio)

pib_categoria_pcorr <- pib_categoria_pcorr |>
  dplyr::filter(!is.na(trim))

pib_categoria_pcorr <- pib_categoria_pcorr[,!sapply(pib_categoria_pcorr,
                                                    function(x) {sum(is.na(x)) == length(x)})]


colnames(pib_categoria_pcorr) <- gsub("_\\d$","",colnames(pib_categoria_pcorr))

pib_categoria_pcorr <- pib_categoria_pcorr %>% 
  pivot_longer(-c(anio, trim),
               values_to = "valor",
               names_to = "indicador") %>% 
  mutate(valor = 1E6*as.numeric(valor),
         unidad = "pesos corrientes") %>% 
  relocate(unidad, .after = trim)

write_csv_fundar(x = pib_categoria_pcorr,
                 file = "data/_FUENTES/clean/pib_categoria_pcorr.csv")

agregar_fuente_clean(id_fuente_raw = 38,
                     path_clean = "pib_categoria_pcorr.csv",
                     nombre = "PIB por categoria de tabulacion a precios corientes",
                     script = "limpieza_pib_oyd_pctes_indec.R")

actualizar_fuente_clean(id_fuente_clean = 7)


