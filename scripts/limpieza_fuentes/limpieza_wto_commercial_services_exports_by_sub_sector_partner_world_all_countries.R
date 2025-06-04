# limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)


id_fuente <- 317
fuente_raw <- sprintf("R%sC0",id_fuente)


# Guardado de archivo
nombre_archivo_raw <- sub("\\.[^.]*$", "", fuentes_raw() %>% 
                            filter(codigo == fuente_raw) %>% 
                            select(path_raw) %>% 
                            pull())

titulo.raw <- fuentes_raw() %>% 
  filter(codigo == fuente_raw) %>% 
  select(nombre) %>% pull()



df_raw <- argendataR::get_raw_path(fuente_raw) %>% 
  readr::read_csv(.) 


df_stage <- df_raw %>% 
  mutate(ReportingEconomyCode = case_when(
    ReportingEconomyCode == "892" ~ "688", # Serbia le pongo el nro correcto de m49 code
    ReportingEconomyCode == "893" ~ "499", # Montenegro le pongo el nro correcto de m49 code
    TRUE ~ ReportingEconomyCode
  ))

source("scripts/utils/wto_country_codes.R")

paises_wto <- get_wto_codes() %>% 
  select(m49_code, name_es, wto_code)


geonomenclador <- argendataR::get_nomenclador_geografico() %>% 
  dplyr::filter(!is.na(m49_code_unsd))%>% 
  mutate(m49_code = str_pad(m49_code_unsd, width = 3, side = "left", pad = "0")) %>% 
  select(iso3 = codigo_fundar, m49_code, desc_fundar) %>% 
  mutate(es_iso = TRUE)


geo_front <- argendataR::get_nomenclador_geografico_front() %>% 
  select(iso3 = geocodigo, pais_nombre = name_long)

diccionario_paises <- paises_wto %>% 
  full_join(geonomenclador, join_by(m49_code)) %>% 
  mutate(es_iso = replace_na(es_iso, FALSE)) %>% 
  mutate(
    iso3_completo = case_when(
      iso3 == "KOS" ~ "XKX",
      is.na(iso3) ~ wto_code,
      TRUE ~ iso3
      ),
    nombre_provisorio =  case_when(
      iso3 == "XKX" ~ "Kosovo",
      is.na(desc_fundar) ~ name_es, 
      TRUE ~ desc_fundar
    ),
    fuente = ifelse(is.na(iso3), "WTO", "UNSD")
  ) %>% 
  left_join(geo_front, join_by(iso3_completo == iso3)) %>% 
  mutate(
    pais_nombre = ifelse(is.na(pais_nombre), name_es, pais_nombre)
  ) %>% 
  select(m49_code, iso3_completo, pais_nombre, fuente, es_iso) %>% 
  bind_rows(.,
            data.frame(
              m49_code = "964",
              iso3_completo = "CEM",
              pais_nombre = "Comunidad Económica y Monetaria de Africa Central (CEMAC)",
              fuente = "WTO",
              es_iso = FALSE
            )) %>% 
  rename(
    iso3 = iso3_completo,
    fuente_iso3 = fuente
  )


df_clean <- df_stage  %>% 
  right_join(., diccionario_paises, join_by( ReportingEconomyCode == m49_code)) %>% 
  select(year = Year,
         m49_code = ReportingEconomyCode, 
         iso3,
         pais_nombre,
         product_code = ProductOrSectorCode, 
         product_desc = ProductOrSector, 
         unit = Unit,
         value_flag = ValueFlag,
         value = Value,
         fuente_iso3,
         es_iso) 



clean_filename <- glue::glue("{nombre_archivo_raw}_CLEAN.parquet")

path_clean <- glue::glue("{tempdir()}/{clean_filename}")

df_clean %>% arrow::write_parquet(., sink = path_clean)

clean_title <- glue::glue("{titulo.raw} - Dataset limpio")

# agregar_fuente_clean(id_fuente_raw = id_fuente,
#                      df = df_clean,
#                      nombre = clean_title,
#                      path_clean = clean_filename,
#                      script = code_name,
#                      descripcion = "Se sacan columnas demás, se cambian nombres de columnas, se filtra codigos de paises que no eran países c('EEC','CEM')"
#                      )

id_fuente_clean <- 186
codigo_fuente_clean <- sprintf("R%sC%s", id_fuente, id_fuente_clean)


df_clean_anterior <- arrow::read_parquet(get_clean_path(codigo = codigo_fuente_clean )) 


comparacion <- comparar_fuente_clean(df_clean,
                                     df_clean_anterior,
                                     pk = c('year','iso3','product_code', 'unit')
)


actualizar_fuente_clean(id_fuente_clean = id_fuente_clean,
                        path_clean = clean_filename,
                        nombre = clean_title, 
                        script = code_name,
                        comparacion = comparacion)
