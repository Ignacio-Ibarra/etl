topico <- "CAMCLI"

mapear_fuentes_outputs <- function(topico) {
  
  topico_mapping <- get_mapping(topico)
  
  
  outputs <- unique(topico_mapping$dataset)
  
  
  
  outputs_response <- list()
  
  
  for (i in outputs) {
    
    outputs_response[[i]] <- tryCatch({
      get_output_repo(gsub(".csv$", ".json", i), topico)},
      error = function(e) {warning(e); e$message}
    )
    
  }
  
  outputs_response <- outputs_response[sapply(outputs_response, is.list)]
  
  fuentes_outputs_map <- lapply(outputs_response,
                                function(x) {x["fuentes"]})
  
  fuentes_outputs_map <- bind_rows(fuentes_outputs_map, .id = "data")
  
  
  fuentes_outputs_map <- fuentes_outputs_map %>% 
    mutate(ftype = ifelse(grepl("C0$", fuentes), "raw", "clean"))
  
  fuentes_outputs_map %>% 
    pivot_wider(id_cols = data, names_from = ftype, values_from = fuentes)
  
  fuentes_outputs_map <- fuentes_outputs_map %>% 
    left_join(fuentes_clean() %>% 
                select(codigo, id_fuente_raw), by = c("fuentes" = "codigo"))
  
  fuentes_outputs_map <- fuentes_outputs_map %>% 
    mutate(fuentes_raw = ifelse(ftype == "raw", fuentes, glue::glue("R{id_fuente_raw}C0")),
           fuentes_clean = ifelse(ftype == "clean", fuentes, NA_character_))
  
  fuentes_outputs_map <- fuentes_outputs_map %>% 
    select(-c(fuentes, id_fuente_raw))
  
  fuentes_outputs_map <- fuentes_outputs_map %>% 
    pivot_longer(cols = c(fuentes_raw, fuentes_clean)) %>% 
    filter(!is.na(value)) %>% 
    select(-name)
  
  fuentes_outputs_map <- fuentes_outputs_map %>% 
    left_join(fuentes_raw() %>% 
                select(codigo, script), by = c("value" = "codigo"))
  
  fuentes_outputs_map <- fuentes_outputs_map %>% 
    left_join(fuentes_clean() %>% 
                select(codigo, script), by = c("value" = "codigo"))
  
  fuentes_outputs_map <- fuentes_outputs_map %>% 
    pivot_longer(c(script.x, script.y), names_to = "drop", values_to = "script")
  
  fuentes_outputs_map <- fuentes_outputs_map %>% 
    filter(!is.na(script)) %>% 
    select(-drop) %>% 
    arrange(data, value)
  
  fuentes_outputs_map <- topico_mapping %>% 
    select(-private) %>% 
    left_join(fuentes_outputs_map, by = c("dataset" = "data"))
  
  fuentes_outputs_map %>% 
    mutate(graf_link = glue::glue("https://graficador.argendata.fund.ar/grafico/{gsub('_g', '/', public)}"))
  
  fuentes_outputs_map
}

mapear_fuentes_outputs <- function(topico) {
  
  topico_mapping <- get_mapping(topico)
  
  outputs <- unique(topico_mapping$dataset)
  
  outputs_response <- list()
  
  for (i in outputs) {
    
    outputs_response[[i]] <- tryCatch({
      get_output_repo(gsub(".csv$", ".json", i), topico)},
      error = function(e) {warning(e); e$message}
    )
    
  }
  
  outputs_response <- outputs_response[sapply(outputs_response, is.list)]
  
  fuentes_outputs_map <- lapply(outputs_response,
                                function(x) {x["fuentes"]})
  
  fuentes_outputs_map <- dplyr::bind_rows(fuentes_outputs_map, .id = "data")
  
  
  fuentes_outputs_map <- fuentes_outputs_map %>%
    dplyr::mutate(ftype = ifelse(grepl("C0$", fuentes), "raw", "clean"))
  
  fuentes_outputs_map %>%
    tidyr::pivot_wider(id_cols = data, names_from = ftype, values_from = fuentes)
  
  fuentes_outputs_map <- fuentes_outputs_map %>%
    dplyr::left_join(fuentes_clean() %>%
                       dplyr::select("codigo", "id_fuente_raw"),
                     by = c("fuentes" = "codigo"))
  
  fuentes_outputs_map <- fuentes_outputs_map %>%
    dplyr::mutate(fuentes_raw = ifelse(ftype == "raw", fuentes, glue::glue("R{id_fuente_raw}C0")),
                  fuentes_clean = ifelse(ftype == "clean", fuentes, NA_character_))
  
  fuentes_outputs_map <- fuentes_outputs_map %>%
    dplyr::select(-c("fuentes", "id_fuente_raw"))
  
  fuentes_outputs_map <- fuentes_outputs_map %>%
    tidyr::pivot_longer(cols = c(fuentes_raw, fuentes_clean)) %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::select(-"name")
  
  fuentes_outputs_map <- fuentes_outputs_map %>%
    dplyr::left_join(fuentes_raw() %>%
                       dplyr::select("codigo", "script"), by = c("value" = "codigo"))
  
  fuentes_outputs_map <- fuentes_outputs_map %>%
    dplyr::left_join(fuentes_clean() %>%
                       dplyr::select("codigo", "script"), by = c("value" = "codigo"))
  
  fuentes_outputs_map <- fuentes_outputs_map %>%
    tidyr::pivot_longer(c(script.x, script.y), names_to = "drop", values_to = "script")
  
  fuentes_outputs_map <- fuentes_outputs_map %>%
    dplyr::filter(!is.na(script)) %>%
    dplyr::select(-"drop") %>%
    dplyr::arrange(.data$data, .data$value)
  
  fuentes_outputs_map <- topico_mapping %>%
    dplyr::select(-"private") %>%
    dplyr::left_join(fuentes_outputs_map, by = c("dataset" = "data"))
  
  fuentes_outputs_map %>%
    dplyr::mutate(graf_link = glue::glue("https://graficador.argendata.fund.ar/grafico/{gsub('_g', '/', public)}"))
  
  fuentes_outputs_map
}


fuentes_outputs_map <- mapear_fuentes_outputs(topico)


ids <- get_ids_graficos("https://argendata.fund.ar/topico/cambio-climatico/")

ids <- str_extract(ids, "[A-Z]{6}_[dg]{1}\\d{1,2}")

fuentes_outputs_map <- fuentes_outputs_map %>% 
  filter(public %in% ids)

ss <- gs4_create(
  glue::glue("update-{topico}-{Sys.time()}"), sheets = topico
)

fuentes_outputs_map %>% 
  googlesheets4::write_sheet(ss = ss, sheet = topico)


glue::glue("https://docs.google.com/spreadsheets/d/{as.character(googledrive::as_id(ss))}/")
