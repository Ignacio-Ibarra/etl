descargar_fuente_raw(id_fuente = 39, dir  = tempdir()) 


pob_indec <- readxl::read_excel(get_temp_path("R39C0"))


pob_indec <- pob_indec[-c(1:4),]
  

names(pob_indec) <- pob_indec[1,] %>%
  janitor::make_clean_names()

pob_indec <- pob_indec %>%
  dplyr::rename(anio = na)

pob_indec <- pob_indec %>%
  dplyr::mutate(anio = as.numeric(gsub(" .*", "", anio )))


pib_categoria_pcorr <- pib_categoria_pcorr %>%
  dplyr::filter(!is.na(anio))

pob_indec <- pob_indec[,!sapply(pob_indec,
                                                    function(x) {sum(is.na(x)) == length(x)})]


pob_indec <- pob_indec %>% 
  pivot_longer(-c(anio),
               values_to = "valor",
               names_to = "indicador") %>% 
  mutate(valor = as.numeric(valor))

pob_indec <- pob_indec %>% 
  filter(!is.na(anio))

write_csv_fundar(x = pob_indec,
                 file = "data/_FUENTES/clean/poblacion_indec.csv")

agregar_fuente_clean(id_fuente_raw = 39,
                     path_clean = "poblacion_indec.csv",
                     nombre = "Poblacion total pais",
                     script = "limpieza_pob_indec.R")

actualizar_fuente_clean(id_fuente_clean = 8)