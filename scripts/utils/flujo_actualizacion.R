ids_grafs <- get_ids_graficos(c("https://argendata.fund.ar/topico/emisiones-de-gases-de-efecto-invernadero/#el-di%C3%B3xido-de-carbono-es-el-gei-m%C3%A1s-relevante",
                   "https://argendata.fund.ar/notas/que-es-la-intensidad-de-carbono/"))

get_mappings <- function(x) {
  
  l <- lapply(x, get_mapping) 
  df <- bind_rows(l)
  df
}

maps <- get_mappings(c("CAMCLI", "TRANEN"))

maps <- maps %>% 
  filter(public %in% ids_grafs)

f_get_fuente <- function(dataset, public) {
  
    dataset <- gsub("\\.csv", ".json",  dataset)
    public <- gsub("_g\\d{2}", "", public)
    
    output <- get_output_repo(dataset, public)
    
    output$fuentes

}

get_fuente_from_mappings <- function(mappings) {
  
  assertthat::assert_that(all(c("dataset", "public", "private") %in% names(mappings)))
  
  mappings$fuentes <- mapply(f_get_fuente, mappings$dataset, mappings$public)
  
  mappings
  
}

maps <- get_fuente_from_mappings(maps)

maps <- unnest(maps, fuentes)


fuentes <- unique(maps$fuentes)

f_raw_idx <- unique(gsub("C\\d{1,2}", "C0", fuentes))

f_raw <- fuentes_raw() %>% filter(codigo %in% f_raw_idx)

f_clean_idx <- unique(grep("^(?!.*C0$).*", fuentes,perl = T , value = T))

f_clean <- fuentes_clean() %>% filter(codigo %in% f_clean_idx)


