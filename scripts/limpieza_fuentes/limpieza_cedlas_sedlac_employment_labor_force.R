
#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

limpiar_temps()


id_fuente <- 115
fuente_raw1 <- sprintf("R%sC0",id_fuente)

nombre_archivo_raw <- str_split_1(fuentes_raw() %>% 
                                    filter(codigo == fuente_raw1) %>% 
                                    select(path_raw) %>% 
                                    pull(), pattern = "\\.")[1]

descargar_fuente_raw(id_fuente = id_fuente, tempdir())

# Cargo funciones para hacer limpieza
source("./scripts/limpieza_fuentes/funciones_limpieza_cedlas_sedlac.R")

TOPIC_PARAM <- "Employment"
SHEET_PARAM <- "labor force"


cedlas_df <- readxl::read_excel(argendataR::get_temp_path(fuente_raw1), sheet = SHEET_PARAM, col_names = F) 

cedlas_df <- quitar_string_source(df = cedlas_df)

cedlas_df <- cedlas_df %>%
  select_if(~ !all(is.na(.))) %>%
  filter(rowSums(is.na(.)) < ncol(.)) %>%
  mutate(across(everything(), as.character)) %>%
  as.data.frame()



tematica <- cedlas_df[[1,1]]
variable <- cedlas_df[[2,1]]

geonomenclador <- argendataR::get_nomenclador_geografico()
mapeo_iso3_pais <- setNames(geonomenclador$desc_fundar, geonomenclador$codigo_fundar)

mapeo_pais_iso3_adhoc <- c(
  'Argentina' = 'ARG', 
  'Bolivia' = 'BOL', 
  'Brazil' = 'BRA', 
  'Chile' = 'CHL', 
  'Colombia' = 'COL', 
  'Costa Rica' = 'CRI', 
  'Dominican Rep' = 'DOM', 
  'Ecuador' = 'ECU', 
  'El Salvador' = 'SLV', 
  'Guatemala' = 'GTM', 
  'Honduras' = 'HND', 
  'Mexico' = 'MEX', 
  'Nicaragua' = 'NIC', 
  'Panama' = 'PAN', 
  'Paraguay' = 'PRY', 
  'Peru' = 'PER', 
  'Uruguay' = 'URY', 
  'Venezuela' = 'VEN'
)

lista_paises <- names(mapeo_pais_iso3_adhoc)

df_original <- armar_serie_original(df = cedlas_df, 
                                    topico = TOPIC_PARAM,
                                    tematica = tematica,
                                    variable = variable,
                                    lista.paises = lista_paises, 
                                    mapper.paises_cedlas.a.isos = mapeo_pais_iso3_adhoc,
                                    mapper.isos.a.paises = mapeo_iso3_pais)


df_anual <- armar_serie_anualizada(df_original = df_original)


armar_serie_empalme_dev <- function(df_anual){
  data <- data.frame(df_anual)
  
  cols <- colnames(data)
  
  empalme <- data %>%
    arrange(iso3, pais, fuente_orden, anio) %>%
    mutate(id_fuente = paste(fuente_orden, fuente, sep = " - "))
  
  # Indico en que filas tengo empalmes
  empalme <- empalme %>%
    group_by(iso3, apertura, anio) %>%
    mutate(is_empalme = as.integer(n() == 2)) %>%
    ungroup()
  
  # Filtrar filas donde is_empalme es 1, agrupar y calcular max de fuente_orden
  max_fuente_orden <- empalme %>%
    dplyr::filter(is_empalme == 1) %>%
    group_by(iso3, apertura) %>%
    summarise(fuente_orden_max = max(fuente_orden, na.rm = TRUE), .groups = 'drop')
  
  # Unir esta información de vuelta al dataset original
  empalme <- empalme %>%
    left_join(max_fuente_orden, by = c("iso3", "apertura")) %>%
    mutate(fuente_orden_max = ifelse(is.na(fuente_orden_max), 0, fuente_orden_max))
  
  
  # Filtrar filas donde is_empalme es 1, agrupar y calcular min de fuente_orden
  min_fuente_orden <- empalme %>%
    dplyr::filter(is_empalme == 1) %>%
    group_by(iso3, apertura) %>%
    summarise(fuente_orden_min = min(fuente_orden, na.rm = TRUE), .groups = 'drop')
  
  # Unir esta información de vuelta al dataset original
  empalme <- empalme %>%
    left_join(min_fuente_orden, by = c("iso3", "apertura")) %>%
    mutate(fuente_orden_min = ifelse(is.na(fuente_orden_min), -1, fuente_orden_min))
  
  # genero una variable booleana
  empalme <- empalme %>%
    mutate(selection = as.integer(fuente_orden <= fuente_orden_max & fuente_orden >= fuente_orden_min))
  
  grid <- empalme %>% 
    dplyr::filter(selection == 1) %>% 
    distinct(topico, tematica, variable, iso3, pais, apertura)
  
  resultados_empalmes <- data.frame()
  # data <- data %>% mutate(serie = "Serie empalmada")
  for (i in 1:nrow(grid)){
    
    t = grid$topico[[i]]
    s = grid$tematica[[i]]
    v = grid$variable[[i]]
    iso = grid$iso3[[i]]
    pais = grid$pais[[i]]
    a = grid$apertura[[i]]
    fuente = NA
    fuente_orden = NA
    serie = "Serie empalmada"
    
    log <- sprintf("Haciendo empalme para ISO %s y Apertura %s\n", iso, a)
    cat(log)
    
    x <- empalme %>% 
      dplyr::filter(topico == t & tematica == s & variable == v & iso3 == iso & apertura == a & selection == 1) %>% 
      select(anio, id_fuente, valor) %>% 
      pivot_wider(names_from = id_fuente, values_from = valor)
    
    resultado_empalme_iso_ap <- x %>% select(anio)
    x1 <- x %>% select(-anio)
    
    sources <- colnames(x1)
    
    x2 <- as.data.frame(lapply(x1, function(x) lead(x,1)))
    colnames(x2) <- sources
    # shifted_sources <- c()
    # 
    # for (f in sources){
    #   shifted_src <- paste0(f,"_shifted")
    #   shifted_sources <- c(shifted_sources, shifted_src)
    #   x <- x %>% mutate(!!shifted_src := lead(get(f), 1))
    #   
    # }
    
    
    
    division <- x2/x1
    tasas <-  c(rowSums(division, na.rm = T)[1:(nrow(a)-1)], 1)
    idx <- 1:length(tasas)
    idx_rev <- sort(idx, decreasing = T)
    tasas_rev <- tasas[idx_rev]
    
    result <- x1[,length(sources)] %>% tail(1) %>% pull()
    emp <- c()
    for (tasa in tasas_rev){
      # cat("tengo la tasa ", tasa, " la divido por ", result,"\n")
      result <- result/tasa
      # cat("obtengo: ", result, "\n")
      emp <- c(emp, result)
    }
    resultado_empalme_iso_ap <- resultado_empalme_iso_ap %>% 
      mutate(
        empalme = emp[idx_rev],
        topico = t,
        tematica = s,
        variable = v,
        iso3 = iso,
        pais = pais, 
        apertura = a,
        serie = serie,
        fuente = fuente,
        fuente_orden = fuente_orden,
        pais = mapper[iso3]
      )
  
    resultados_empalmes <- rbind(resultados_empalmes, resultado_empalme_iso_ap)
  }
  
  
  
  return(data)
}



df_empalme <- armar_serie_empalme(df_anual = df_anual)


df_clean <- armar_tabla(df_anual = df_anual, 
                        df_empalme = df_empalme) 



norm_sheet <- str_to_lower(SHEET_PARAM) %>% str_replace(., " ", "_")

clean_filename <- glue::glue("{norm_sheet}_{nombre_archivo_raw}_CLEAN.csv")

path_clean <- glue::glue("{tempdir()}/{clean_filename}")

df_clean %>% write_csv_fundar(., file = path_clean)

code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)

# agregar_fuente_clean(id_fuente_raw = id_fuente,
#                      path_clean = clean_filename,
#                      dir = tempdir(),
#                      nombre = glue::glue("{TOPIC_PARAM} - {tematica} - SEDLAC (serie original y empalmada)"),
#                      descripcion = "La limpieza consiste en llevar los datos de formato en Excel a formato tabular plano listo para poder consumir, se anualizaron los valores que poseían una frecuencia semestral y se calculó una serie empalmada",
#                      script = code_name)

actualizar_fuente_clean(id_fuente_clean = 30,
                        dir = tempdir())
