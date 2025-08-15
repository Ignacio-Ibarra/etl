#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)


id_fuente <- 426
fuente_raw <- sprintf("R%sC0",id_fuente)

pattern <- fuentes_raw() %>% 
  pull(path_raw) %>% 
  tools::file_ext(.) %>%
  unique() %>% 
  keep(., ~all(.x != '')) %>% 
  paste0(., collapse = "|") %>% 
  paste0("(.*)\\.(",.,")$")


nombre_archivo_raw <- str_extract(fuentes_raw() %>% 
                                    dplyr::filter(codigo == fuente_raw) %>% 
                                    select(path_raw) %>% 
                                    pull(), 
                                  pattern = pattern, 
                                  group = 1)
titulo.raw <- fuentes_raw() %>% 
  filter(codigo == fuente_raw) %>% 
  select(nombre) %>% pull()


path <- argendataR::get_raw_path(fuente_raw) %>% 
  unzip(., list = TRUE) %>% 
  dplyr::filter(grepl("Figure 1 to 4.xlsx", Name)) %>% 
  pull(Name)

argendataR::get_raw_path(fuente_raw) %>% 
  unzip(., files = path, junkpaths = T, exdir = tempdir())

df_stage <- readxl::read_excel(glue::glue("{tempdir()}/Figure 1 to 4.xlsx"),
                               sheet = "OUTPUT CHARTS - Figures 1 to 4",
                               range = "DL75:GH79",
                               col_names = F) %>%
  t() %>% # Transponer
  as.data.frame() %>% 
  fill(V1, .direction = "down")


colnames(df_stage) <- c("desarrollo_economia", "nombre_pais", "codigo_pais", "corr_gasto_ciclico_gdp", "volatilidad_gdp")


geo_front <- argendataR::get_nomenclador_geografico_front() %>% 
  select(codigo_pais = geocodigo, pais_nombre = name_long) %>% 
  dplyr::filter(nchar(codigo_pais) == 3) %>% 
  mutate(pais_join = janitor::make_clean_names(pais_nombre))

df_intermediate <- df_stage %>% 
  fill(desarrollo_economia, .direction = "down") %>% 
  mutate(pais_join = janitor::make_clean_names(nombre_pais)) %>% 
  left_join(geo_front, join_by(pais_join)) %>% 
  dplyr::filter(!is.na(pais_nombre)) %>% 
  select(iso3 = codigo_pais.y, pais_nombre, desarrollo_economia, corr_gasto_ciclico_gdp, volatilidad_gdp)


df_errors <- df_stage %>% 
  fill(desarrollo_economia, .direction = "down") %>% 
  mutate(pais_join = janitor::make_clean_names(nombre_pais)) %>% 
  left_join(geo_front, join_by(pais_join)) %>% 
  dplyr::filter(is.na(pais_nombre)) %>% 
  select(-c(codigo_pais.y, pais_nombre)) %>% 
  left_join(geo_front, join_by(codigo_pais.x == codigo_pais))


df_solucion1 <- df_errors %>% 
  dplyr::filter(!is.na(pais_nombre) | nombre_pais == "Norway", nombre_pais != "North Macedonia") %>% 
  select(iso3 = codigo_pais.x, pais_nombre, desarrollo_economia, corr_gasto_ciclico_gdp, volatilidad_gdp)


imputacion_iso <- c(
  "Bosnia and Herzegovina" = "BIH",
  "Malaysia" = "MYS", 
  "Moldova" = "MDA",
  "North Macedonia" = "MKD", 
  "Philippines" = "PHL",
  "Romania" = "ROU",
  "Türkiye" = "TUR", 
  "Croatia" = "HRV", 
  "Denmark" = "DNK",
  "Germany" = "DEU",
  "Greece" = "GRC",
  "Iceland" = "ISL",
  "Ireland" = "IRL",
  "Japan" = "JPN",
  "Latvia" = "LVA",
  "Lithuania" = "LTU",
  "Netherlands" = "NLD",
  "New Zealand" = "NZL", 
  "Singapore" = "SGP",
  "Slovak Republic" = "SVK",
  "Slovenia" = "SVN",
  "Spain" = "ESP",
  "Switzerland" = "CHE",
  "Taiwan Province of China" = "TWN",
  "United Kingdom" = "GBR",
  "United States" = "USA"
)

df_solucion2 <- df_errors %>% 
  dplyr::filter(is.na(pais_nombre) | nombre_pais == "North Macedonia", nombre_pais != "Norway") %>% 
  mutate(iso3 = imputacion_iso[nombre_pais]) %>% 
  select(iso3, desarrollo_economia, corr_gasto_ciclico_gdp, volatilidad_gdp) %>% 
  left_join(geo_front %>% select(-pais_join), join_by(iso3 == codigo_pais))



df_clean <- bind_rows(df_intermediate, df_solucion1, df_solucion2)


clean_filename <- glue::glue("{nombre_archivo_raw}_CLEAN.parquet")

path_clean <- glue::glue("{tempdir()}/{clean_filename}")

df_clean %>% arrow::write_parquet(., sink = path_clean)

clean_title <- glue::glue("{titulo.raw} - Dataset limpio")


# agregar_fuente_clean(id_fuente_raw = id_fuente,
#                      df = df_clean,
#                      path_clean = clean_filename,
#                      nombre = clean_filename,
#                      script = code_name)


id_fuente_clean <- 274
codigo_fuente_clean <- sprintf("R%sC%s", id_fuente, id_fuente_clean)


df_clean_anterior <- arrow::read_parquet(get_clean_path(codigo = codigo_fuente_clean ))


comparacion <- comparar_fuente_clean(df_clean,
                                     df_clean_anterior,
                                     pk = c("iso3")
)


actualizar_fuente_clean(id_fuente_clean = id_fuente_clean,
                        path_clean = clean_filename,
                        nombre = clean_title, 
                        script = code_name,
                        comparacion = comparacion)

