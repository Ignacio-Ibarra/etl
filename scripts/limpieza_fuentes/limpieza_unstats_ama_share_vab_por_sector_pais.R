#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

require(data.table)


id_fuente <- 224
fuente_raw <- sprintf("R%sC0",id_fuente)

nomenclador <- argendataR::get_nomenclador_geografico() %>%  
  select(iso3 = codigo_fundar, m49_code = m49_code_unsd, pais_nombre = desc_fundar, continente_fundar, nivel_agregacion) %>% 
  dplyr::filter(!is.na(m49_code))


base <- read_csv(argendataR::get_raw_path(fuente_raw)) %>% 
  select(m49_code = countryCode, anio = fiscalYear, itemName, particip_va_pib = observationValue) %>% 
  mutate(particip_va_pib = as.double(round(particip_va_pib, 2)))

# setDT(base)


# Generar todas las combinaciones posibles de m49_code, anio e itemName
full_combinations <- expand_grid(
              m49_code = unique(base$m49_code),
              anio = unique(base$anio),
              itemName = unique(base$itemName)
              )


# Realizar un join entre all_combinations y base, imputando NA en observationValue donde falte
base <- left_join(
  full_combinations, 
  base,
  by = join_by(m49_code, anio, itemName)) %>% 
  replace_na(list(particip_va_pib = 0))



# Reemplazo por la descripción reducida y agrego las columnas
base <- base %>%
  mutate(gran_sector = case_when(
    itemName == "Agriculture, hunting, forestry, fishing (ISIC A-B)" ~ "Agro y pesca",
    itemName == "Mining, Manufacturing, Utilities (ISIC C-E)" ~ "Energía y minería",
    itemName == "Manufacturing (ISIC D)" ~ "Industria manufacturera",
    itemName == "Construction (ISIC F)" ~ "Construcción",
    itemName == "Wholesale, retail trade, restaurants and hotels (ISIC G-H)" ~ "Comercio, hotelería y restaurantes",
    itemName == "Transport, storage and communication (ISIC I)" ~ "Transporte y comunicaciones",
    itemName == "Other Activities (ISIC J-P)" ~ "Otros servicios"
  ),
  letra = case_when(
    itemName == "Agriculture, hunting, forestry, fishing (ISIC A-B)" ~ "AB",
    itemName == "Mining, Manufacturing, Utilities (ISIC C-E)" ~ "CyE",
    itemName == "Manufacturing (ISIC D)" ~ "D",
    itemName == "Construction (ISIC F)" ~ "F",
    itemName == "Wholesale, retail trade, restaurants and hotels (ISIC G-H)" ~ "GH",
    itemName == "Transport, storage and communication (ISIC I)" ~ "I",
    itemName == "Other Activities (ISIC J-P)" ~ "JKLMNOP"
  ),
  tipo_sector = case_when(
    itemName %in% c("Agriculture, hunting, forestry, fishing (ISIC A-B)",
                       "Mining, Manufacturing, Utilities (ISIC C-E)",
                       "Manufacturing (ISIC D)",
                       "Construction (ISIC F)") ~ "Bienes",
    itemName %in% c("Wholesale, retail trade, restaurants and hotels (ISIC G-H)",
                       "Transport, storage and communication (ISIC I)",
                       "Other Activities (ISIC J-P)") ~ "Servicios"
  )) %>% 
  left_join(nomenclador, by=join_by(m49_code)) %>% 
  select(-itemName, m49_code)


#1) a los países con valores negativos les asignamos NA
base <- base %>% 
  mutate(particip_va_pib = ifelse(particip_va_pib<0, NA, particip_va_pib))

#2) imput NA a los que tienen más en el rubro D que en C,D y E agregados
imput_NA <- base %>% 
  dplyr::filter(letra %in% c("D", 'CyE')) %>% 
  pivot_wider(id_cols = all_of(c("m49_code", "anio")), names_from = letra, values_from = particip_va_pib) %>% 
  mutate(diff = CyE - D) %>% 
  dplyr::filter(diff <0) %>% 
  distinct(anio, m49_code) %>% 
  mutate(valor_input_NA = 1)



#le asigno NA A esos casos de la base
base <- base %>% 
  left_join(imput_NA, by=join_by(anio, m49_code)) %>% 
  mutate(particip_va_pib = ifelse(is.na(valor_input_NA), particip_va_pib, NA)) %>% 
  select(-valor_input_NA)


manufacturing <- base %>% 
  dplyr::filter(letra == "D") %>% 
  select(anio, m49_code, manufactura_pib = particip_va_pib)
  
mineria_energia <- base %>% 
  dplyr::filter(letra == "CyE") %>% 
  left_join(manufacturing, join_by(anio, m49_code)) %>% 
  mutate(min_energia_pib = particip_va_pib - manufactura_pib) %>% 
  select(anio, m49_code, letra, min_energia_pib)

base <- base %>% 
  left_join(mineria_energia, by=join_by(anio, m49_code, letra)) %>% 
  mutate(particip_va_pib = ifelse(letra == "CyE", min_energia_pib, particip_va_pib)) %>% 
  select(-min_energia_pib)

df_clean <- base %>% 
  group_by(anio, m49_code) %>% 
  mutate(suma2 = sum(particip_va_pib)) %>% 
  ungroup() %>%
  dplyr::filter(suma2>0) %>% 
  mutate(share_vab =  particip_va_pib / (suma2/100))%>% 
  select(anio, iso3, pais_nombre, letra, gran_sector, tipo_sector, continente_fundar, nivel_agregacion, share_vab)



# Guardado de archivo
nombre_archivo_raw <- str_split_1(fuentes_raw() %>% 
                                    filter(codigo == fuente_raw) %>% 
                                    select(path_raw) %>% 
                                    pull(), pattern = "\\.")[1]


clean_filename <- glue::glue("{nombre_archivo_raw}_CLEAN.parquet")

path_clean <- glue::glue("{tempdir()}/{clean_filename}")

df_clean %>% arrow::write_parquet(., sink = path_clean)

code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)

titulo.raw <- fuentes_raw() %>% 
  filter(codigo == fuente_raw) %>% 
  select(nombre) %>% pull()

clean_title <- glue::glue("{titulo.raw}")

# agregar_fuente_clean(id_fuente_raw = id_fuente,
#                       df = df_clean,
#                      nombre = clean_title,
#                      script = code_name,
#                     descripcion = "Se re-calcularon los valores del sector Minería y Energía (CyE en ISIC Rev 3) para cada país, restándole el valor de Industria Manufacturera (D en ISIC Rev 3) el cuál venía desagregado pero también incorporado a un sector más agregado que contenía los tres sectores. Además se re-escalaron los valores para que los valores sumados dieran 100 en el share."
#                     )

actualizar_fuente_clean(id_fuente_clean = 95, path_clean = clean_filename, directorio = tempdir(), nombre = clean_title, script = code_name)

