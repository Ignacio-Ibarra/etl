#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

library(data.table)
library(dplyr)

subtopico <- "INDUST"
output_name <- "07_share_industria_expo.csv"
analista <- "Nicolás Sidicaro"
fuente1 <- 'R104C0' # Atlas Location
fuente2 <- 'R457C0' # Atlas SITC
fuente3 <- 'R456C0' # BACI
fuente4 <- 'R458C298' # Clasificador HS02 a Lall
fuente5 <- 'R459C299' # Clasificador SITC REV 1 (Atlas) a Lall


geo_front <- argendataR::get_nomenclador_geografico_front() %>% 
  select(geocodigoFundar = geocodigo, geonombreFundar = name_long)


hs02_lall <- argendataR::get_clean_path(fuente4) %>% 
  arrow::read_parquet() %>% 
  select(hs02, lall_code)


atlas_lall <- argendataR::get_clean_path(fuente5) %>% 
  arrow::read_parquet() %>% 
  select(sitc_product_code, lall_code = lall_desc, lall_desc_full) %>% 
  distinct() %>% 
  setDT(.)


df_atlas_location <-  argendataR::get_raw_path(fuente1) %>% 
  read.csv() %>% 
  dplyr::filter(level == "country") %>% 
  select(location_id, iso3 = location_code) %>% 
  setDT(.)

df_atlas <- argendataR::get_raw_path(fuente2) %>% 
  fst::read_fst(., as.data.table = T) %>%  
  setDT(.)


df_atlas_sitc <- df_atlas[
  !is.na(suppressWarnings(as.numeric(sitc_product_code))),    
  .(iso3 = location_code, anio =  year, sitc_product_code = as.integer(as.numeric(sitc_product_code)), export_value)  
]


df_atlas_lall <- atlas_lall[
  df_atlas_sitc,
  on = .(sitc_product_code), 
  nomatch = 0,
  allow.cartesian = T
][
  
  ,
  .(export_value = sum(export_value, na.rm = TRUE),
    fuente = "ATLAS"), 
  by = .(iso3, anio, lall_code, lall_desc_full)
  
]


rm(df_atlas)
rm(df_atlas_sitc)
gc()  

source("scripts/utils/baci_data.R")


con <- argendataR::get_raw_path(fuente3) %>% 
  BACI.get_db_from_zip(.)

dbWriteTable(con, 
             "hs_to_lall", 
             hs02_lall, 
             overwrite = TRUE)


query_output <- glue::glue(
  "SELECT t as anio, c.i as m49_code, p.country_iso3 as iso3, h.lall_code, SUM(c.v) as expo
   FROM comercio as c
   LEFT JOIN paises as p ON c.i = p.country_code
   LEFT JOIN hs_to_lall as h ON h.hs02 = c.k
   GROUP BY t, c.i, p.country_iso3, h.lall_code"
)


df_query <- dbGetQuery(con, query_output) %>% 
  setDT(.)


df_baci <- df_query %>% 
  mutate(lall_code = tolower(lall_code)) %>% 
  left_join(atlas_lall %>% 
              distinct(lall_code, lall_desc_full), join_by(lall_code)) %>% 
  mutate(fuente = "BACI") %>%  
  select(anio, iso3, lall_code, lall_desc_full, expo_baci = expo)




