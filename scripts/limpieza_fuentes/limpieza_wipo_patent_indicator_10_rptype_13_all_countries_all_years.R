#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)


id_fuente <- 405
fuente_raw <- sprintf("R%sC0",id_fuente)

# Guardado de archivo
nombre_archivo_raw <- str_split_1(fuentes_raw() %>% 
                                    filter(codigo == fuente_raw) %>% 
                                    select(path_raw) %>% 
                                    pull(), pattern = "\\.")[1]

titulo.raw <- fuentes_raw() %>% 
  filter(codigo == fuente_raw) %>% 
  select(nombre) %>% pull()


json_data <- argendataR::get_raw_path(fuente_raw) %>% 
  jsonlite::read_json(.)

df_raw <- json_data$records %>% bind_rows() %>% 
  select(!matches("\\d+_SeqOrder"))


################
# Me traigo los iso2 desde la fuente. 

source("scripts/utils/scraper_wipo.R")
iso2_to_order <- WIPO.get_offices_tech(indicator = 10, rp_type = 13)$ipsOriginListSeq

order_to_iso2 <- setNames(as.list(names(iso2_to_order)), unlist(iso2_to_order)) %>% unlist(.)


geonomenclador <- argendataR::get_nomenclador_geografico_front() %>% 
  select(iso2 = iso_2, iso3 = geocodigo, nombre_pais = name_long) %>% 
  mutate(iso2 = ifelse(iso3 == "WLD", "WD", iso2)) 


paises <- df_raw %>% 
  select(matches("selectedOrigin.*")) %>% 
  mutate(iso2 = order_to_iso2[as.character(selectedOrigin_SeqOrder)]) %>% 
  left_join(geonomenclador %>% 
              dplyr::filter(grepl("\\w+",iso2)) %>%  
              select(iso2, iso3), 
            join_by(iso2))

anios <- df_raw %>% 
  select(!matches("selectedOrigin.*")) %>%
  select(sort(names(.))) 

df_clean <- bind_cols(paises, anios) %>% 
  arrange(selectedOrigin_SeqOrder) %>%
  pivot_longer(matches("\\d+") & !c(iso2, iso3), names_to = 'anio', values_to = 'valor', values_transform = as.numeric) %>% 
  drop_na(valor) %>% 
  mutate(
    iso3 = case_when(
      selectedOrigin == "Czechoslovakia" ~ "CSK",
      selectedOrigin == "German Democratic Republic" ~ "DDR",
      selectedOrigin == "Netherlands Antilles" ~ "ANT",
      selectedOrigin == "Soviet Union" ~ "SVU", 
      selectedOrigin == "Yugoslavia" ~ "SER",
      selectedOrigin == "Zaire" ~ "WIPO_ZAI",
      selectedOrigin == "European Union members" ~ "EUU",
      selectedOrigin == "Least Developed Countries" ~ "LDC",
      TRUE ~ iso3
      )
  ) %>% 
  left_join(geonomenclador %>% select(iso3, nombre_pais), join_by(iso3)) %>% 
  mutate(nombre_pais = ifelse(iso3 == "WIPO_ZAI", selectedOrigin, nombre_pais)) %>% 
  select(iso2, iso3, nombre_pais, metrica = selectedTechnology, anio, valor)


clean_filename <- glue::glue("{nombre_archivo_raw}_CLEAN.parquet")

clean_title <- glue::glue("{titulo.raw}")

path_clean <- glue::glue("{tempdir()}/{clean_filename}")

df_clean %>% arrow::write_parquet(., sink = path_clean)


# agregar_fuente_clean(id_fuente_raw = id_fuente,
#                      path_clean = clean_filename,
#                      nombre = clean_title,
#                      script = code_name,
#                      descripcion = "Se agrega código iso3 y nombre en español")



id_fuente_clean <- 256
codigo_fuente_clean <- sprintf("R%sC%s", id_fuente, id_fuente_clean)


df_clean_anterior <- arrow::read_parquet(get_clean_path(codigo = codigo_fuente_clean ))


comparacion <- comparar_fuente_clean(df_clean,
                                     df_clean_anterior,
                                     pk = c('iso3','anio', 'metrica')
)

actualizar_fuente_clean(id_fuente_clean = id_fuente_clean,
                        path_clean = clean_filename,
                        nombre = clean_title, 
                        script = code_name,
                        comparacion = comparacion)