#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)

id_fuente <- 471
fuente_raw <- sprintf("R%sC0",id_fuente)



## descargo fuente raw

get_raw_path(fuente_raw) %>% 
  readxl::excel_sheets()

sheet_name <- "total_by_sector"

df <- readxl::read_xlsx(get_raw_path(fuente_raw),
                               sheet = sheet_name)

df <- df %>% 
  mutate(sector_lv2 = gsub(".*\\|", "", sector_lv2) %>%
           str_trim() %>%
           str_to_sentence()) 

df <- df %>% 
  select(-starts_with("change")) %>% 
  pivot_longer(cols = matches("\\d{4}"), names_to = "anio") %>% 
  arrange(anio, sector_lv1, sector_lv2, sector_lv3)


df <- df %>%
  mutate(
    sector_lv1 = case_when(
      sector_lv1 == "Agriculture" ~ "Agricultura",
      sector_lv1 == "Energy" ~ "Energía",
      sector_lv1 == "Industrial processes" ~ "Procesos industriales",
      sector_lv1 == "Waste" ~ "Residuos",
      TRUE ~ sector_lv1
    ),
    sector_lv2 = case_when(
      sector_lv2 == "Agriculture" ~ "Agricultura",
      sector_lv2 == "Buildings & other" ~ "Edificios y otros",
      sector_lv2 == "Fuel production" ~ "Producción de combustibles",
      sector_lv2 == "Industry" ~ "Industria",
      sector_lv2 == "Power" ~ "Energía eléctrica",
      sector_lv2 == "Transport" ~ "Transporte",
      sector_lv2 == "Industrial processes" ~ "Procesos industriales",
      sector_lv2 == "Waste" ~ "Residuos",
      TRUE ~ sector_lv2
    ),
    sector_lv3 = case_when(
      sector_lv3 == "Biomass burning, soils and rice" ~ "Quema de biomasa, suelos y arroz",
      sector_lv3 == "Livestock" ~ "Ganadería",
      sector_lv3 == "Buildings" ~ "Edificios",
      sector_lv3 == "Other (Buildings)" ~ "Otros (Edificios)",
      sector_lv3 == "Oil and gas" ~ "Petróleo y gas",
      sector_lv3 == "Other (Fuel production)" ~ "Otros (Producción de combustibles)",
      sector_lv3 == "Solid fuels" ~ "Combustibles sólidos",
      sector_lv3 == "Industry" ~ "Industria",
      sector_lv3 == "Power" ~ "Generación eléctrica",
      sector_lv3 == "Aviation" ~ "Aviación",
      sector_lv3 == "Other (Transport)" ~ "Otros (Transporte)",
      sector_lv3 == "Road" ~ "Carretera",
      sector_lv3 == "Cement (excl. carbonation)" ~ "Cemento (excl. carbonatación)",
      sector_lv3 == "Chemicals" ~ "Productos químicos",
      sector_lv3 == "Metals" ~ "Metales",
      sector_lv3 == "Other (Industrial processes)" ~ "Otros (Procesos industriales)",
      sector_lv3 == "Liquid waste" ~ "Residuos líquidos",
      sector_lv3 == "Other (Indirect N2O and fossil fuel fires)" ~ "Otros (N2O indirecto e incendios de combustibles fósiles)",
      sector_lv3 == "Solid waste" ~ "Residuos sólidos",
      TRUE ~ sector_lv3
    )
  )




titulo.raw <- fuentes_raw() %>% 
  filter(codigo == fuente_raw) %>% 
  select(nombre) %>% pull()

clean_title <- glue::glue("{sheet_name} -{titulo.raw}")


sufijo <- gsub(".*/|\\..*", "", get_raw_path(fuente_raw))

filename <- glue::glue("{sheet_name}_{sufijo}.parquet")

# agregar_fuente_clean(id_fuente_raw = id_fuente,
#                      df = df,
#                      path_clean = filename,
#                      nombre = clean_title,
#                      script = code_name)

id_fuente_clean <- 307

codigo_fuente_clean <- sprintf("R%sC%s", id_fuente, id_fuente_clean)


df_clean_anterior <- arrow::read_parquet(argendataR::get_clean_path(codigo = codigo_fuente_clean )) 

comparacion <- comparar_fuente_clean(df,
                                     df_clean_anterior,
                                     pk = c('sector_lv2', "sector_lv3", 'sector_lv1', 'anio')
)

actualizar_fuente_clean(id_fuente_clean = id_fuente_clean,
                        df = df,
                        nombre = clean_title, 
                        script = code_name,
                        comparacion = comparacion)
