map(list.files("data/_temp/DEUDAS1/definitivos/", full.names = T), \(x) {
  df <- read_csv(x)
  
  write.csv(x = df, 
            file = gsub(pattern = "\.csv", "_old.csv", x), 
            eol = "\n", na = "",row.names = F, fileEncoding = "UTF-8")
  
  df %>% 
    head() %>% print()
  
  dropq <- readline(prompt = "Drop pais? Y/N: ")
  
  
  if (dropq == "Y") {
    
    columna <- readline(prompt = "Columna: ")
    
    df <- df %>% 
      select(-all_of(columna))
    
    print(head(df))
    
    
  }
  
  joinq <- readline(prompt = "Joinear nombres de pais? Y/N: ")
  
  
  if (joinq == "Y") {
    
    iso_name <- readline(prompt = "Columna con iso: ")
    
    iso_col <- "iso3"
    
    names(iso_col) <- iso_name 
    
    df <- df %>% 
      left_join(get_iso_paises(), by = iso_col)
    
    print(head(df))
    
    guardarq <- readline(prompt = "Guardar? Y/N: ")
    
    if (guardarq == "Y") {
      write.csv(x = df, 
                file = x, 
                eol = "\n", na = "",row.names = F, fileEncoding = "UTF-8")
    }
    
  }
})
