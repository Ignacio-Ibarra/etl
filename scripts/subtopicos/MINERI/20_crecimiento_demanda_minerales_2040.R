################################################################################
##                              Dataset: nombre                               ##
################################################################################

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


subtopico <- "MINERI"
output_name <- "crecimiento_demanda_minerales_2040"
analista = "Kevin Corfield"
fuente1 <- "R279C150"


df_iea <- arrow::read_parquet(argendataR::get_clean_path(fuente1))


rare_earths <- c(
  "Lanthanum", "Cerium", "Praseodymium", "Neodymium", "Promethium", 
  "Samarium", "Europium", "Gadolinium", "Terbium", "Dysprosium", 
  "Holmium", "Erbium", "Thulium", "Ytterbium", "Lutetium", 
  "Other REEs","Yttrium"
)

minerales_es = c(
  'alumi' = 'Aluminio', 
  'prase' = 'Praseodimio',
  'zirco' = 'Circonio',
  'chrom' = 'Cromo', 
  'cobal' = 'Cobalto', 
  'coppe' = 'Cobre', 
  'feore' = 'Mineral de Hierro',
  'graph' = 'Grafito',
  'dyspr' = 'Disprosio',
  'titan' = 'Titanio',
  'yttri' = 'Itrio',
  'zinc' = 'Cinc',
  'arsen' = 'Arsénico',
  'terbi' = 'Terbio',
  'tellu' = 'Telurio',
  'silic' = 'Silicio',
  'lanth' = 'Lantano',
  'iridi' = 'Iridio',
  'indiu' = 'Indio',
  'hafni' = 'Hafnio',
  'galli' = 'Galio',
  'gold' = 'Oro', 
  'lead' = 'Plomo', 
  'manga' = 'Manganeso',
  'magne' = 'Magnesio',
  'nicke' = 'Níquel', 
  'palla' = 'Paladio', 
  'plati' = 'Platino', 
  'tin' = 'Estaño', 
  'vanad' = 'Vanadio',
  'cadmi' = 'Cadmio',
  'antim' = 'Antimonio',
  'lithi' = 'Litio', 
  'mercu' = 'Mercurio', 
  'tungs' = 'Tungsteno',
  'silve' = 'Plata',
  'molyb' = "Molibdeno",
  'raree' = "Tierras Raras",
  'neody' = 'Neodimio',
  'niobi' = 'Niobio',
  'selen' = 'Selenio'
)

df_iea_esp <- df_iea %>% 
  mutate(
    mineral_esp = case_when(
      mineral == 'Platinum group metals' ~ "Grupo del Platino (Rutenio, Rodio, Paladio, Osmio, Iridio y Platino)",
      mineral %in% rare_earths ~ 'Elementos de tierras raras',
      mineral == 'Others' ~ 'Otros',
      TRUE ~ recode(str_sub(tolower(mineral), start = 1L, end = 5L), !!!minerales_es)
    )
  )

df_2020 <- df_iea_esp %>% 
  dplyr::filter(year == 2020) %>% 
  group_by(mineral = mineral_esp, scenario) %>% 
  summarise(
    demand_2020 = sum(demand_thousand_tonnes, na.rm = T)
  )

df_2040 <- df_iea_esp %>% 
  dplyr::filter(year  == 2040) %>% 
  group_by(mineral = mineral_esp, scenario) %>% 
  summarise(
    demand_2040 = sum(demand_thousand_tonnes, na.rm = T)
  )



df_output <- df_2020 %>% 
  left_join(df_2040, join_by(mineral, scenario)) %>% 
  mutate(
    growth = demand_2040 / demand_2020,
    scenario = ifelse(grepl("Stated.*",scenario),"escenario_base","escenario_desarrollo_sostenible")
   ) %>% 
  select(!starts_with('demand_')) %>% 
  pivot_wider(
    id_cols = mineral,
    names_from = scenario,
    values_from = growth
  ) %>% 
  dplyr::filter(escenario_base != Inf)





df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico,
                                            entrega_subtopico = "primera_entrega") 


comparacion <- argendataR::comparar_outputs(
  df_anterior = df_anterior,
  df = df_output,
  nombre = output_name,
  pk = c("mineral"), # variables pk del dataset para hacer el join entre bases
  drop_joined_df =  F
)


#-- Exportar Output ----

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
  dplyr::filter(grepl(paste0("^", output_name,".csv"), dataset_archivo)) %>% 
  distinct(variable_nombre, descripcion) 


# Guardo en una variable las columnas del output que queremos escribir
output_cols <- names(df_output) # lo puedo generar así si tengo df_output




descripcion <- armador_descripcion(metadatos = metadatos,
                                   # etiquetas_nuevas = etiquetas_nuevas,
                                   output_cols = output_cols)


colectar_fuentes <- function(pattern = "^fuente.*"){
  
  # Genero un vector de codigos posibles
  posibles_codigos <- c(fuentes_raw()$codigo,fuentes_clean()$codigo)
  
  # Usar ls() para buscar variables en el entorno global
  variable_names <- ls(pattern = pattern, envir = globalenv())
  
  # Obtener los valores de esas variables
  valores <- unlist(mget(variable_names, envir = globalenv()))
  
  # Filtrar aquellas variables que sean de tipo character (string)
  # Esto es para que la comparacion sea posible en la linea de abajo
  strings <- valores[sapply(valores, is.character)]
  
  # solo devuelvo las fuentes que existen
  return(valores[valores %in% posibles_codigos])
}



df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    fuentes = colectar_fuentes(),
    analista = analista,
    pk = c("mineral"),
    es_serie_tiempo = F,
    control = comparacion, 
    descripcion_columnas = descripcion,
    unidades = list("escenario_base" = "unidades","escenario_desarrollo_sostenible" = "unidades")
  )
