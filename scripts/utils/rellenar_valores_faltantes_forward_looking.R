# Función para generar nueva columna con valores rellenados
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
    arrange(year) %>%
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


