# limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

library(data.table)

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)


id_fuente <- 319
fuente_raw <- sprintf("R%sC0",id_fuente)


# Guardado de archivo
nombre_archivo_raw <- sub("\\.[^.]*$", "", fuentes_raw() %>% 
                            filter(codigo == fuente_raw) %>% 
                            select(path_raw) %>% 
                            pull())

titulo.raw <- fuentes_raw() %>% 
  filter(codigo == fuente_raw) %>% 
  select(nombre) %>% pull()



df_raw <- fread(argendataR::get_raw_path(fuente_raw))

source("scripts/utils/indec_comex_ncm_8_digitos_impo_expo.R")

country_codes <- INDEC_COMEX.get_countries()


iso_codes <- argendataR::get_raw_path("R158C0") %>% 
  read_csv(.) %>% 
  select(iso2 = Alpha2code, iso3 = Alpha3code)


destinos <- country_codes %>% 
  left_join(iso_codes, join_by(iso2)) %>% 
  select(pdes = `_id`, iso2, iso3, nombre_destino = nombre)

setDT(destinos)

rm(country_codes, iso_codes)
gc()

df_clean <- merge(
  df_raw[, .(
    anio = `Año`,
    mes = Mes,
    ncm8 = stringr::str_pad(trimws(NCM), width = 8, side = "left", pad = "0"),
    pdes = Pdes,
    pnet_kg = as.numeric(gsub(",", ".", `Pnet.kg.`)),
    fob = as.numeric(gsub(",", ".", `FOB.u.s.`))
  )],
  destinos,
  by = "pdes",
  all.x = TRUE
)[, .(anio, mes, ncm8, pdes, iso2, iso3, nombre_destino, pnet_kg, fob)]


clean_filename <- glue::glue("{nombre_archivo_raw}_CLEAN.parquet")

path_clean <- glue::glue("{tempdir()}/{clean_filename}")

df_clean %>% arrow::write_parquet(., sink = path_clean)

clean_title <- glue::glue("{titulo.raw} - Dataset limpio")

# agregar_fuente_clean(id_fuente_raw = id_fuente,
#                      df = df_clean,
#                      nombre = clean_title,
#                      path_clean = clean_filename,
#                      script = code_name)

id_fuente_clean <- 188
codigo_fuente_clean <- sprintf("R%sC%s", id_fuente, id_fuente_clean)


df_clean_anterior <- arrow::read_parquet(get_clean_path(codigo = codigo_fuente_clean )) 

# Hago una comparación más corta porque sino se satura la memoria. 
X <- df_clean %>% arrange(anio, mes, ncm8, pdes) %>% slice(1:1000)
Y <- df_clean_anterior %>% arrange(anio, mes, ncm8, pdes) %>% slice(1:1000)

comparacion <- comparar_fuente_clean(X,
                                     Y,
                                     pk = c('anio','mes','ncm8', 'pdes', 'iso2', 'iso3', 'nombre_destino')
)


actualizar_fuente_clean(id_fuente_clean = id_fuente_clean,
                        path_clean = clean_filename,
                        nombre = clean_title, 
                        script = code_name,
                        comparacion = comparacion)
