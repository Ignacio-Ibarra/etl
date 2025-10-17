#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

# Metadatos 
subtopico <- "INDUST"
output_name <- "pib_indust_per_capita_comparado.csv"
analista <- "Nicolás Sidicaro"
fuente1 <- 'R464C302' # National Accounts. Analysis of Main Aggregates (AMA). Value Added by activity - Constant 2015 -US Dollars - Limpio
fuente2 <- 'R466C303' # National Accounts. Analysis of Main Aggregates (AMA). Population

fuente3 <- "R223C93"
fuente4 <- "R36C82"
fuente5 <- 'R219C90' # Maddison
fuente6 <- 'R433C279' # World Population Proscpects - Demographic Indicators. 1950-2100, medium. CSV format
fuente7 <- 'R444C0' # Lattes



fill_missing_backwards <- function(data, value_col, growth_col, new_col_name = NULL, group_vars = NULL) {
  
  value_col_sym <- sym(value_col)
  growth_col_sym <- sym(growth_col)
  
  # Si no se especifica nombre para nueva columna, crear uno automáticamente
  if (is.null(new_col_name)) {
    new_col_name <- paste0(value_col, "_filled")
  }
  new_col_sym <- sym(new_col_name)
  
  if (!is.null(group_vars)) {
    data <- data %>% group_by(across(all_of(group_vars)))
  }
  
  result <- data %>%
    arrange(anio) %>%
    mutate(
      !!new_col_sym := {
        vals <- !!value_col_sym  # Copiar valores originales
        growth_vals <- !!growth_col_sym
        
        first_valid <- which(!is.na(vals))[1]
        
        if (!is.na(first_valid) && first_valid > 1) {
          for (i in (first_valid - 1):1) {
            if (is.na(vals[i])) {
              vals[i] <- vals[i + 1] / (1 + growth_vals[i])
            }
          }
        }
        vals
      }
    )
  
  if (!is.null(group_vars)) {
    result <- result %>% ungroup()
  }
  
  return(result)
}

df_break <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet()


df_pop_ama <- argendataR::get_clean_path(fuente2) %>% 
  arrow::read_parquet() %>% 
  select(anio, iso3, population = value)





df_break_manuf <- df_break %>% 
  dplyr::filter(iso3 != "WLD" ) %>% 
  select(-c(country_en,indicator_name, note, unit)) %>% 
  dplyr::filter(stringr::str_detect(sector_name,'ISIC D|ISIC C-E')) %>% 
  pivot_wider(., id_cols = c(iso3, pais_nombre, anio),
              names_from = sector_name, 
              values_from = value) %>% 
  janitor::clean_names() %>% 
  group_by(iso3) %>% 
  mutate(interanual_var = (lead(mining_manufacturing_utilities_isic_c_e) - mining_manufacturing_utilities_isic_c_e) / mining_manufacturing_utilities_isic_c_e) %>% 
  fill_missing_backwards("manufacturing_isic_d", "interanual_var",
                         new_col_name = "manufacturing_isic_d_complete" ,
                         group_vars = c("iso3", "pais_nombre"))  %>% 
  mutate(manufacturing_isic_d = if_else(is.na(manufacturing_isic_d) & !is.na(interanual_var),
                                        manufacturing_isic_d_complete,
                                        manufacturing_isic_d)) %>% 
  select(-c(interanual_var, manufacturing_isic_d_complete, mining_manufacturing_utilities_isic_c_e)) 


df_output_A <- df_pop_ama %>% 
  inner_join(df_break_manuf,by=c('iso3','anio')) %>% 
  dplyr::mutate(gdp_indust_pc = manufacturing_isic_d / population) %>% 
  dplyr::filter(!is.na(gdp_indust_pc)) %>% 
  select(geocodigoFundar = iso3, geonombreFundar = pais_nombre, anio, gdp_indust_pc) %>% 
  arrange(geocodigoFundar, anio) %>% 
  group_by(geocodigoFundar, geonombreFundar) %>%
  mutate(
    gdp_indust_pc_indice = 100 * gdp_indust_pc / first(gdp_indust_pc)
  ) %>% 
  ungroup() %>% 
  dplyr::filter(geocodigoFundar != "ARG")


# VAB INDUSTRIAL

vabpb_indec_df <- arrow::read_parquet(get_clean_path(fuente3)) %>% 
  dplyr::filter(trimestre == "Total") %>% 
  dplyr::filter(sub_sector == "Total sector") %>% 
  mutate(sector = case_when(
    sector %in% c("Enseñanza",
                  "Servicios sociales y de salud",
                  "Otras actividades de servicios comunitarias, sociales y personales",
                  "Hogares privados con servicio doméstico") ~ "Otros servicios",
    
    sector %in% c("Comercio mayorista, minorista y reparaciones", 
                  "Hoteles y restaurantes") ~ "Comercio, hoteles y restaurantes",
    sector == "Administración pública y defensa; planes de seguridad social de afiliación obligatoria" ~ "Administración pública, defensa y otros",
    TRUE ~ sector
  )) %>% 
  group_by(anio, sector) %>% 
  summarise(vabpb_indec = sum(vab_pb, na.rm = T))

vabpb_fnys_df <- arrow::read_parquet(get_clean_path(fuente4)) %>% 
  dplyr::filter(unidad == "mill. de $ 2004") %>% 
  dplyr::filter(!(indicador %in% c("PIB a precios de mercado",
                                   "PIB a precios básicos",
                                   "IVA + Imp a la Importación + Imp. netos de Subsidios + Residuo por Empalme"))) %>% 
  select(-unidad, -iso3) %>% 
  mutate(sector = case_when(
    indicador == "Comercio al por mayor y menor, y hoteles y restaurantes" ~ "Comercio, hoteles y restaurantes",
    indicador == "Industrias manufactureras" ~ "Industria manufacturera",
    indicador == "Administración pública, defensa y org extraterr." ~ "Administración pública, defensa y otros", 
    TRUE ~ indicador
  )) %>% 
  select(anio, sector, vabpb_fnys = valor) 


sector_letra <-  c('Agricultura, ganadería, caza y silvicultura', 
                   'Pesca', 
                   'Explotación de minas y canteras', 
                   'Industria manufacturera', 
                   'Electricidad, gas y agua', 
                   'Construcción', 
                   'Comercio mayorista, minorista y reparaciones', 
                   'Hoteles y restaurantes', 
                   'Transporte, almacenamiento y comunicaciones', 
                   'Intermediación financiera', 
                   'Actividades inmobiliarias, empresariales y de alquiler', 
                   "Administración pública, defensa y otros", 
                   'Enseñanza', 'Servicios sociales y de salud', 
                   'Otras actividades de servicios comunitarias, sociales y personales', 
                   'Hogares privados con servicio doméstico')

letra <-  c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P')

letra_desc <-  c('Agro', 
                 'Pesca', 
                 'Petróleo y minería', 
                 'Industria manufacturera', 
                 'Electricidad, gas y agua', 
                 'Construcción', 'Comercio', 
                 'Hotelería y restaurantes', 
                 'Transporte y comunicaciones', 
                 'Finanzas', 'Serv. inmobiliarios y profesionales', 
                 'Adm. pública y defensa', 
                 'Enseñanza', 
                 'Salud', 
                 'Serv. comunitarios, sociales y personales', 
                 'Servicio doméstico')

#añado descripción de letra de indec 
dicc_sector <- data.frame(sector_letra, letra, letra_desc)


df_gdp <- vabpb_indec_df %>% 
  full_join(vabpb_fnys_df, by=join_by(anio, sector)) %>% 
  mutate(vabpb_final = ifelse(is.na(vabpb_indec), vabpb_fnys, vabpb_indec)) %>% 
  select(anio, sector, vabpb = vabpb_final) %>% 
  left_join(dicc_sector, by=join_by(sector == sector_letra)) %>% 
  mutate(
    letra = case_when(
      sector == "Comercio, hoteles y restaurantes" ~ "GH",
      sector == "Otros servicios" ~ "Z",
      TRUE ~ letra),
    letra_desc = case_when(
      sector == "Comercio, hoteles y restaurantes" ~ "Comercio, hoteles y restaurantes",
      sector == "Otros servicios" ~ "Otros Servicios",
      TRUE ~ letra_desc)
  ) %>% 
  select(anio, letra, letra_desc, vabpb)


df_gdp_indust <- df_gdp %>% 
  filter(letra == 'D') %>% 
  select(anio, vabpb_industrial = vabpb)

# POBLACION LARGO PLAZO 


df_madd <- argendataR::get_clean_path(fuente5) %>% 
  arrow::read_parquet()

df_wpp <- argendataR::get_clean_path(fuente6) %>% 
  arrow::read_parquet()

df_lattes <- argendataR::get_raw_path(fuente7) %>% 
  read.csv

df_madd_arg <- df_madd %>% 
  dplyr::filter(iso3 == "ARG") %>% 
  mutate(fuente = "Maddison Project Database" ) %>% 
  select(anio, poblacion = pop, fuente) %>% 
  drop_na(poblacion)

df_wpp_arg <- df_wpp %>% 
  dplyr::filter(iso3_code == "ARG",
                time <= year(Sys.Date())) %>% 
  mutate(poblacion = 1000*t_population1july,
         fuente = "World Population Prospects (UN)") %>% 
  select(anio = time, poblacion, fuente)

df_lattes_pob <- df_lattes %>% 
  mutate(
    anio = str_extract(quinquenio, "\\d{4}") %>% as.integer(),
    poblacion = poblacion_inicial,
    fuente = "Lattes et al (1975)") %>% 
  select(anio, poblacion, fuente)

df_pob <- df_madd_arg %>% 
  dplyr::filter(anio < min(df_lattes_pob$anio)) %>% 
  bind_rows(
    df_lattes_pob %>% 
      dplyr::filter(anio < min(df_wpp_arg$anio))
  ) %>% 
  bind_rows(df_wpp_arg)


interpolacion_geometrica <- function(x1, y1, x2, y2, x_inter) {
  # x1, y1: punto inicial
  # x2, y2: punto final  
  # x_inter: años a interpolar
  
  # Calcular tasa de crecimiento anual compuesta
  n_years <- x2 - x1
  tasa_crecimiento <- (y2 / y1)^(1/n_years) - 1
  
  # Aplicar crecimiento geométrico
  valores_inter <- y1 * (1 + tasa_crecimiento)^(x_inter - x1)
  
  return(valores_inter)
}

# Función alternativa: interpolación exponencial suavizada
interpolacion_exponencial <- function(x1, y1, x2, y2, x_inter) {
  # Transformar a escala logarítmica para linearizar el crecimiento exponencial
  log_y1 <- log(y1)
  log_y2 <- log(y2)
  
  # Interpolación lineal en escala log
  log_y_inter <- log_y1 + (log_y2 - log_y1) * (x_inter - x1) / (x2 - x1)
  
  # Convertir de vuelta a escala original
  y_inter <- exp(log_y_inter)
  
  return(y_inter)
}


anios_completos <- seq(min(df_pob$anio), max(df_pob$anio), by = 1)
anios_faltantes <- setdiff(anios_completos, df_pob$anio)


print(paste("Años faltantes:", length(anios_faltantes)))
print("Principales gaps:")
if (length(anios_faltantes) > 0) {
  # Identificar grupos consecutivos de años faltantes
  diff_anios <- c(1, diff(anios_faltantes))
  grupos <- cumsum(diff_anios > 1)
  gaps_info <- split(anios_faltantes, grupos)
  
  for (i in 1:length(gaps_info)) {
    gap <- gaps_info[[i]]
    print(paste("Gap", i, ":", min(gap), "-", max(gap), "(", length(gap), "años)"))
  }
}

poblacion_completa <- df_pob %>% select(anio, poblacion) %>% 
  mutate(fuente = 'original')

for (anio_faltante in anios_faltantes) {
  # Encontrar los puntos de referencia más cercanos
  anios_menores <- df_pob$anio[df_pob$anio < anio_faltante]
  anios_mayores <- df_pob$anio[df_pob$anio > anio_faltante]
  
  if (length(anios_menores) > 0 && length(anios_mayores) > 0) {
    # Punto anterior más cercano
    x1 <- max(anios_menores)
    y1 <- df_pob$poblacion[df_pob$anio == x1]
    
    # Punto posterior más cercano
    x2 <- min(anios_mayores)
    y2 <- df_pob$poblacion[df_pob$anio == x2]
    
    # Aplicar interpolación geométrica (más realista para población)
    valor_interpolado <- interpolacion_geometrica(x1, y1, x2, y2, anio_faltante)
    
    # Agregar a la base de datos
    nueva_fila <- data.frame(
      anio = anio_faltante,
      poblacion = round(valor_interpolado),
      fuente = "Interpolación geométrica"
    )
    
    poblacion_completa <- rbind(poblacion_completa, nueva_fila)
  }
}


# VAB INDUSTRIAL PER CAPITA


df_intermediate <- df_gdp_indust %>% 
  left_join(poblacion_completa %>% 
              select(anio, poblacion), join_by(anio)) %>% 
  mutate(gdp_indust_pc = (vabpb_industrial * 1000000)/poblacion) %>% 
  drop_na(gdp_indust_pc) %>% 
  arrange(anio) 



vab_indust_pc_base <- df_intermediate %>% 
  dplyr::filter(anio == 1970) %>% 
  pull(gdp_indust_pc)


df_output_B <- df_intermediate %>% 
  mutate(gdp_indust_pc_indice = 100 * gdp_indust_pc / vab_indust_pc_base, 
         geocodigoFundar = "ARG", 
         geonombreFundar = "Argentina") %>% 
  select(geocodigoFundar, geonombreFundar, anio, gdp_indust_pc, gdp_indust_pc_indice) %>% 
  dplyr::filter(anio>=1970)


df_output <- bind_rows(df_output_B, df_output_A)

df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico) 

pks_comparacion <- c('anio','geocodigoFundar', 'sector')

comparacion <- argendataR::comparar_outputs(
  df = df_output,
  df_anterior = df_anterior,
  nombre = output_name,
  pk = pks_comparacion
)



armador_descripcion <- function(metadatos, etiquetas_nuevas = data.frame(), output_cols){
  # metadatos: data.frame sus columnas son variable_nombre y descripcion y 
  # proviene de la info declarada por el analista 
  # etiquetas_nuevas: data.frame, tiene que ser una dataframe con la columna 
  # variable_nombre y la descripcion
  # output_cols: vector, tiene las columnas del dataset que se quiere escribir
  
  etiquetas <- metadatos %>% 
    dplyr::filter(variable_nombre %in% output_cols) 
  
  
  etiquetas <- etiquetas %>% 
    bind_rows(etiquetas_nuevas)
  
  
  diff <- setdiff(output_cols, etiquetas$variable_nombre)
  
  stopifnot(`Error: algunas columnas de tu output no fueron descriptas` = length(diff) == 0)
  
  # En caso de que haya alguna variable que le haya cambiado la descripcion pero que
  # ya existia se va a quedar con la descripcion nueva. 
  
  etiquetas <- etiquetas %>% 
    group_by(variable_nombre) %>% 
    filter(if(n() == 1) row_number() == 1 else row_number() == n()) %>%
    ungroup()
  
  etiquetas <- stats::setNames(as.list(etiquetas$descripcion), etiquetas$variable_nombre)
  
  return(etiquetas)
  
}

# Tomo las variables output_name y subtopico declaradas arriba
metadatos <- argendataR::metadata(subtopico = subtopico) %>% 
  dplyr::filter(grepl(paste0("^", output_name), nombre_archivo)) %>% 
  distinct(variable_nombre, descripcion) 



# Guardo en una variable las columnas del output que queremos escribir
output_cols <- names(df_output) # lo puedo generar así si tengo df_output



descripcion <- armador_descripcion(metadatos = metadatos,
                                   # etiquetas_nuevas = etiquetas_nuevas,
                                   output_cols = output_cols)



df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    control = comparacion, 
    fuentes = argendataR::colectar_fuentes(),
    analista = analista,
    pk = pks,
    descripcion_columnas = descripcion, 
    unidad = list("poblacion" = "unidades", "share" = "porcentaje"))


output_name <- gsub("\\.csv", "", output_name)
mandar_data(paste0(output_name, ".csv"), subtopico = subtopico, branch = "main")
mandar_data(paste0(output_name, ".json"), subtopico = subtopico,  branch = "main")
