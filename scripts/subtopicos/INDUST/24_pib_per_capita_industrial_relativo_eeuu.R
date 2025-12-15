#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

# Metadatos 
subtopico <- "INDUST"
output_name <- "pib_per_capita_industrial_relativo_eeuu.csv"
analista <- "Nicolás Sidicaro"
fuente1 <- 'R464C302' # National Accounts. Analysis of Main Aggregates (AMA). Value Added by activity - Constant 2015 -US Dollars - Limpio
fuente2 <- 'R466C303' # National Accounts. Analysis of Main Aggregates (AMA). Population

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


geonomenclador <- argendataR::get_nomenclador_geografico() %>% 
  select(geocodigoFundar = codigo_fundar, continente = continente_fundar, subregion_unsd, intermediate_region_unsd) %>% 
  drop_na(continente)


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


df_intermediate <- df_pop_ama %>% 
  inner_join(df_break_manuf,by=c('iso3','anio')) %>% 
  left_join(geonomenclador, join_by(iso3 == geocodigoFundar)) %>% 
  mutate(region = case_when(subregion_unsd == 'América Latina y el Caribe' ~ 'América Latina',
                            iso3 == 'TUR' ~ "Asia",
                            continente == 'Europa' ~ 'Europa',
                            continente == 'África' ~ 'África',
                            iso3 == 'USA' ~ 'Estados Unidos',
                            continente == 'Asia' ~ 'Asia',
                            TRUE ~ 'Otros'))


df_indust_gdp_pc_base <- df_intermediate %>% 
  dplyr::filter(region == "Estados Unidos") %>% 
  mutate(indust_gdp_pc_base = manufacturing_isic_d / population) %>% 
  select(anio, indust_gdp_pc_base)

df_asia_paises <- df_intermediate %>% 
  dplyr::filter(region == 'Asia') %>% 
  mutate(indust_gdp_pc = manufacturing_isic_d / population) %>% 
  select(anio, iso3, pais_nombre, indust_gdp_pc) %>% 
  left_join(df_indust_gdp_pc_base, join_by(anio)) %>% 
  mutate(indust_gdp_pc_relative = 100 * indust_gdp_pc / indust_gdp_pc_base) %>% 
  select(anio, geocodigoFundar = iso3, geonombreFundar = pais_nombre, indust_gdp_pc, indust_gdp_pc_relative)


df_total_asia <- df_intermediate %>% 
  dplyr::filter(region == 'Asia') %>% 
  group_by(anio, geonombreFundar = "Promedio ponderado Asia") %>% 
  summarise(
    population = sum(population, na.rm = T), 
    manufacturing_isic_d = sum(manufacturing_isic_d, na.rm = T)
  ) %>% 
  left_join(df_indust_gdp_pc_base, join_by(anio)) %>% 
  mutate(
    indust_gdp_pc = manufacturing_isic_d / population,
    indust_gdp_pc_relative =  100 * indust_gdp_pc / indust_gdp_pc_base) %>% 
  select(anio, geonombreFundar, indust_gdp_pc, indust_gdp_pc_relative)


df_output <- df_asia_paises %>% 
  bind_rows(
    df_indust_gdp_pc_base %>% 
      mutate(geocodigoFundar = 'USA', geonombreFundar = "Estados Unidos", indust_gdp_pc_relative=100) %>% 
      select(anio, geocodigoFundar, geonombreFundar, indust_gdp_pc = indust_gdp_pc_base, indust_gdp_pc_relative)
  ) %>% 
  bind_rows(
    df_total_asia %>% 
      mutate(geocodigoFundar = NA)
  )


df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico, drive = T) 

pks_comparacion <- c('anio','region')

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