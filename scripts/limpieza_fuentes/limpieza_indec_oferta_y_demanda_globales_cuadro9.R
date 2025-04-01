# limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)


id_fuente <- 38
fuente_raw <- sprintf("R%sC0",id_fuente)

# Guardado de archivo
nombre_archivo_raw <- str_split_1(fuentes_raw() %>% 
                                    filter(codigo == fuente_raw) %>% 
                                    select(path_raw) %>% 
                                    pull(), pattern = "\\.")[1]

titulo.raw <- fuentes_raw() %>% 
  filter(codigo == fuente_raw) %>% 
  select(nombre) %>% pull()


# cuadro 9 ---------------------------------------------------------------



sheet_name <- 'cuadro 9'

oyd_pcorr <- readxl::read_excel(get_raw_path(fuente_raw),
                                sheet = sheet_name)


oyd_pcorr <- oyd_pcorr[-c(1:2),] %>%
  t() %>%
  tibble::as_tibble(., .name_repair = "unique")

names(oyd_pcorr) <- oyd_pcorr[1,] %>%
  janitor::make_clean_names()

oyd_pcorr <- oyd_pcorr %>%
  dplyr::rename(anio = na, trim = na_2)

oyd_pcorr <- oyd_pcorr %>%
  dplyr::mutate(anio = as.numeric(gsub(" .*", "", anio )))

oyd_pcorr <- oyd_pcorr %>%
  tidyr::fill(anio)

oyd_pcorr <- oyd_pcorr %>%
  dplyr::filter(!is.na(trim))

oyd_pcorr <- oyd_pcorr[,!sapply(oyd_pcorr, function(x) {sum(is.na(x)) == length(x)})]

colnames(oyd_pcorr) <- gsub("_\\d$","",colnames(oyd_pcorr))

df_clean <- oyd_pcorr %>% 
  pivot_longer(-c(anio, trim), values_to = "valor", names_to = "indicador") %>% 
  mutate(valor = ifelse(valor == "///", NA,as.numeric(valor)),
         unidad = "indice de precios implícitos")

normalized_sheet_name <- sheet_name %>% janitor::make_clean_names(.)

clean_filename <- glue::glue("{nombre_archivo_raw}_{normalized_sheet_name}_CLEAN.parquet")

clean_title <- glue::glue("{titulo.raw} - {sheet_name}")

path_clean <- glue::glue("{tempdir()}/{clean_filename}")

df_clean %>% arrow::write_parquet(., sink = path_clean)

# 
# agregar_fuente_clean(id_fuente_raw = id_fuente,
#                      df = df_clean,
#                      nombre = clean_title,
#                      path_clean = clean_filename,
#                      script = code_name)


id_fuente_clean <- 194
codigo_fuente_clean <- sprintf("R%sC%s", id_fuente, id_fuente_clean)


df_clean_anterior <- arrow::read_parquet(get_clean_path(codigo = codigo_fuente_clean ))


comparacion <- comparar_fuente_clean(df_clean,
                                     df_clean_anterior,
                                     pk = c("anio", "trim", "indicador")
)

actualizar_fuente_clean(id_fuente_clean = id_fuente_clean,
                        path_clean = clean_filename,
                        nombre = clean_title, 
                        script = code_name,
                        comparacion = comparacion)