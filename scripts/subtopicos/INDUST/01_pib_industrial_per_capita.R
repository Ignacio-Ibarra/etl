#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

# Metadatos 
subtopico <- "INDUST"
output_name <- "pib_industrial_per_capita"
analista <- "Nicolás Sidicaro"
fuente1 <- 'R454C297' # National Accounts. Analysis of Main Aggregates (AMA). GDP, Per Capita GDP - US Dollars - Limpio
fuente2 <- 'R453C296' # National Accounts. Analysis of Main Aggregates (AMA). Percentage Distribution (Shares) of GDP. All countries for all years - sorted alphabetically - Cuadro: Download-Shares-countries


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


df_gdp <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet() %>% 
  select(iso3, anio, gdp_per_cap = value)


df_break <- argendataR::get_clean_path(fuente2) %>% 
  arrow::read_parquet()


df_break_manuf <- df_break %>% 
  select(-c(country_en,titulo)) %>% 
  dplyr::filter(stringr::str_detect(indicator_name,'ISIC D|ISIC C-E')) %>% 
  pivot_wider(., id_cols = c(iso3, pais_nombre, anio),
              names_from = indicator_name, 
              values_from = share) %>% 
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

 
df_output <- df_gdp %>% 
  dplyr::left_join(df_break_manuf,by=c('iso3','anio')) %>% 
  dplyr::mutate(gdp_indust_pc = (gdp_per_cap*(manufacturing_isic_d/100))) %>% 
  dplyr::filter(!is.na(gdp_per_cap)) %>% 
  select(iso3, pais_nombre, anio, gdp_indust_pc)


