################################################################################
##                              Dataset: nombre                               ##
################################################################################

# Este script es una copia del script ~/etl/scripts/subtopicos/ACECON/7_pib_comp_va.R


#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


subtopico <- "MINERI"
output_name <- "minerales_vbp_mundial"
analista = "Kevin Corfield"
fuente1 <- "R264C134" # USGS world.zip
fuente2 <- "R267C135" # USGS salient.zip
fuente3 <- "R265C0" # IMF Precios commodities Data
fuente4 <- "R266C0" # IMF Precios commodities Metadata


geonomenclador <- argendataR::get_nomenclador_geografico()


df_usgs_world <- arrow::read_parquet(argendataR::get_clean_path(fuente1))

df_usgs_produccion <- df_usgs_world %>% 
  dplyr::filter(!grepl(".*prod_note.*", variable)) %>% 
  dplyr::filter(grepl(".*prod_.*", variable)) %>% 
  mutate(anio = as.numeric(str_extract(variable, pattern = ".*(\\d{4}).*", group = 1))) %>% 
  mutate(value_num = as.numeric(str_replace_all(trimws(value), ",", ""))) %>% 
  mutate(unit = ifelse(id == "alumi", "thousand metric tons", unit)) %>% 
  drop_na(value_num) %>% 
  mutate(value = value_num) %>% 
  select(-c(value_num,variable)) %>% 
  mutate(id = ifelse(grepl(".*Palladium.*", type), "palla", id))

df_usgs_salient <- arrow::read_parquet(argendataR::get_clean_path(fuente2)) 

df_usgs_precios <- df_usgs_salient %>% 
  dplyr::filter(!grepl(".*prod_note.*", variable)) %>% 
  dplyr::filter(grepl(".*price.*", variable)) %>% 
  mutate(id = ifelse(grepl(".*Palladium.*", definition), "palla", id))


json_imf_metada <- jsonlite::read_json(argendataR::get_raw_path(fuente4))

unit_metadata <- json_imf_metada$UNIT_MEASURE %>% 
  bind_rows() %>%
  setNames(c("concept_id", "lang", "text", "dimension")) %>%
  mutate(
    group = cumsum(concept_id == "UNIT_MEASURE_NAME")
  ) %>%
  select(-lang) %>%
  pivot_wider(
    id_cols = c(dimension, group),
    names_from = concept_id,
    values_from = text
  ) %>%
  janitor::clean_names() %>% 
  select(unit_measure_code, unit_measure_name)

comm_metadata <- json_imf_metada$COMMODITY %>% 
  bind_rows() %>%
  setNames(c("concept_id", "lang", "text", "dimension")) %>%
  mutate(
    group = cumsum(concept_id == "COMMODITY_NAME")
  ) %>%
  select(-lang) %>%
  pivot_wider(
    id_cols = c(dimension, group),
    names_from = concept_id,
    values_from = text
  ) %>%
  janitor::clean_names() %>% 
  select(commodity_code, commodity_name, commodity_definition)


minerals_imf <- c("Aluminum", "Cobalt", "Copper", "Gold", "Iron Ore", "Lead", 
                  "Nickel", "Tin", "Uranium", "Zinc", "Palladium", "Platinum", 
                  "Chromium", "Lithium", "Manganese", "Rare Earth Elements", 
                  "Silicon", "Vanadium", "Silver")

df_imf_data <- read.csv(argendataR::get_raw_path(fuente3)) %>% 
  dplyr::filter(freq == "A") %>% 
  dplyr::filter(unit_measure ==  "USD") %>% 
  select(-c(freq,ref_area, time_format, unit_measure)) %>% 
  left_join(comm_metadata, join_by(commodity == commodity_code)) %>%
  mutate(commodity_name = trimws(commodity_name)) %>% 
  dplyr::filter(tolower(commodity_name) %in% tolower(minerals_imf)) %>% 
  mutate(id = case_when(
    trimws(tolower(commodity_name %>% str_sub(., start = 1, end = 5))) == "iron" ~ "feore",
    TRUE ~ trimws(tolower(commodity_name %>% str_sub(., start = 1, end = 5)))
  ),
  time_period = as.numeric(time_period))


df_usgs_produccion_con_precios_imf <- df_usgs_produccion %>% 
  inner_join(df_imf_data, join_by(id, anio == time_period)) %>% 
  mutate(usd_unit_price = case_when(
    !is.na(str_extract(commodity_definition, ".*(US\\$.*|USD\\/.*)", group = 1)) ~ str_extract(commodity_definition, ".*(US\\$.*|USD\\/.*)", group = 1),
    TRUE ~ "USD per metric tons")) %>% 
  group_by(country, id) %>% 
  fill(unit, .direction = "downup") %>% 
  ungroup() %>% 
  mutate(value = case_when(
    grepl(".*thousand metric tons.*", unit) ~ value * 1000,
    grepl(".*kilograms.*", unit) ~ value / 1000,
    grepl("^metric ton.*", unit) ~ value *1,
    TRUE ~ value
  ),
  unit = case_when(
    grepl(".*thousand metric tons.*", unit) ~ "metric tons",
    grepl(".*kilograms.*", unit) ~ "metric tons",
    grepl("^metric ton.*", unit) ~ "metric tons",
    TRUE ~ unit
  )) %>% 
  dplyr::filter(!(value==0)) %>% 
  mutate(multiplier = case_when(
    grepl(".*metric ton.*", usd_unit_price) ~ 1,
    grepl(".*troy ounce", usd_unit_price) ~ 2204.62/0.068571,
    TRUE ~ NA_real_
  )) %>% 
  mutate(valor_prod_norm_mill_usd = value * obs_value * multiplier / 1000000) %>% 
  arrange(id, anio, -valor_prod_norm_mill_usd) %>% # en millones de usd
  select(-definition, -source, -unit_mult, -commodity) %>% 
  dplyr::filter(!grepl("Iron ore - mine production - iron content.*|Refinery production, copper.*", type)) %>% 
  rename(production = value, 
         production_unit = unit,
         price_value = obs_value) %>% 
  dplyr::filter(id != "lithi") %>% 
  mutate(production = round(production, 3),
         price_value = round(price_value, 3))


# Busco p y q para Litio Antimonio Tungsteno Mercurio

id_minerales_solo_usgs <- c("lithi" = "Lithium","antim" = "Antimonium", "tungs" = "Tungsten", "mercu"="Mercury", "molyb" = "Molybdenum")

df_usgs_produccion_solo_usgs <- df_usgs_produccion %>% 
  dplyr::filter(id %in% names(id_minerales_solo_usgs)) %>% 
  dplyr::filter(value != 0) %>% 
  group_by(id) %>% 
  fill(unit, .direction = "downup") %>% 
  ungroup() %>% 
  rename(production = value, 
         production_unit = unit)

df_usgs_precios_solo_usgs <- df_usgs_precios %>% 
  dplyr::filter(id %in% names(id_minerales_solo_usgs)) %>% 
  mutate(year = as.numeric(year),
         value = as.numeric(value)) %>% 
  rename(usd_unit_price = unit, 
         price_value = value,
         commodity_name = commodity,
         anio = year,
         price_definition = definition) %>% 
  select(-country, -variable, -data_source) 


df_usgs_solo_usgs_con_precios <- df_usgs_produccion_solo_usgs %>% 
  left_join(df_usgs_precios_solo_usgs, join_by(id, anio)) %>% 
  mutate(
    multiplier = case_when(
      grepl(".*metric.*ton.*", usd_unit_price) ~ 1,
      grepl(".*kilogram.*", usd_unit_price) ~ 1000,
      grepl(".*pound.*", usd_unit_price) ~ 2204.62,
    )
  ) %>% 
  mutate(production = round(production, 3),
         price_value = round(price_value, 3)) %>% 
  mutate(valor_prod_norm_mill_usd = production * price_value * multiplier / 1000000) 


consolidado_df <- df_usgs_produccion_con_precios_imf %>% select(-commodity_definition) %>% 
  bind_rows(df_usgs_solo_usgs_con_precios %>% select(-source, -definition,-price_definition))


world_totals_df <- consolidado_df %>% dplyr::filter(grepl("World.*", country)) %>% 
  select(id, anio, tamanio_mercado = valor_prod_norm_mill_usd)

countries_df <- consolidado_df %>% dplyr::filter(!grepl("World.*", country))

country_names <- c(
  "China" = "China",
  "India" = "India",
  "Russia" = "Rusia",
  "Canada" = "Canadá",
  "United Arab Emirates" = "Emiratos Árabes Unidos",
  "Bahrain" = "Bahrein",
  "Australia" = "Australia",
  "Norway" = "Noruega",
  "Malaysia" = "Malasia",
  "United States" = "Estados Unidos",
  "Brazil" = "Brasil",
  "Iceland" = "Islandia",
  "South Africa" = "Sudáfrica",
  "Kazakhstan" = "Kazajstán",
  "Turkey" = "Turquía",
  "Finland" = "Finlandia",
  "Congo (Kinshasa)" = "República Democrática del Congo",
  "Indonesia" = "Indonesia",
  "Philippines" = "Filipinas",
  "Cuba" = "Cuba",
  "Madagascar" = "Madagascar",
  "Papua New Guinea" = "Papua Nueva Guinea",
  "New Caledonia11" = "Nueva Caledonia",
  "Chile" = "Chile",
  "Peru" = "Perú",
  "Zambia" = "Zambia",
  "Mexico" = "México",
  "Poland" = "Polonia",
  "Iran" = "Irán",
  "Sweden" = "Suecia",
  "Ukraine" = "Ucrania",
  "Mauritania" = "Mauritania",
  "Uzbekistan" = "Uzbekistán",
  "Ghana" = "Ghana",
  "Mali" = "Malí",
  "Burkina Faso" = "Burkina Faso",
  "Tanzania" = "Tanzanía",
  "Bolivia" = "Bolivia",
  "Tajikistan" = "Tayikistán",
  "Gabon" = "Gabón",
  "Côte d’Ivoire" = "Costa de Marfil",
  "Burma" = "Myanmar",
  "Georgia" = "Georgia",
  "Vietnam" = "Vietnam",
  "Zimbabwe" = "Zimbabwe",
  "Nigeria" = "Nigeria",
  "Rwanda" = "Ruanda",
  "Laos" = "Lao",
  "Guatemala" = "Guatemala",
  "Kyrgyzstan" = "Kirguistán",
  "Pakistan" = "Pakistán",
  "Argentina" = "Argentina",
  "Portugal" = "Portugal",
  "Morocco" = "Marruecos",
  "Peru (exports)" = "Perú",
  "Austria" = "Austria",
  "Korea, North" = "Corea del Norte",
  "Spain" = "España",
  "Kazakhstan, concentrate" = "Kazajstán",
  "Russiae" = "Rusia",
  "Ukraine, concentrate" = "Ucrania",
  "New Caledonia9" = "Nueva Caledonia"
)


minerales_es = c(
  'alumi' = 'Aluminio', 
  'chrom' = 'Cromo', 
  'cobal' = 'Cobalto', 
  'coppe' = 'Cobre', 
  'feore' = 'Mineral de Hierro', 
  'gold' = 'Oro', 
  'lead' = 'Plomo', 
  'manga' = 'Manganeso', 
  'nicke' = 'Níquel', 
  'palla' = 'Paladio', 
  'plati' = 'Platino', 
  'tin' = 'Estaño', 
  'vanad' = 'Vanadio', 
  'zinc' = 'Zinc', 
  'antim' = 'Antimonio',
  'lithi' = 'Litio', 
  'mercu' = 'Mercurio', 
  'tungs' = 'Tungsteno',
  'silve' = 'Plata',
  'molyb' = "Molibdeno"
)

df_output <- countries_df %>% 
  left_join(world_totals_df, join_by(id, anio)) %>% 
  mutate(share = valor_prod_norm_mill_usd / tamanio_mercado) %>% 
  dplyr::filter(!grepl(".*other countries", country, ignore.case = T)) %>% 
  group_by(anio, id) %>%
  mutate(rank_share = rank(-share, ties.method = "first")) %>%
  ungroup() %>% 
  mutate(country = trimws(country)) %>% 
  mutate(country_es = recode(country, !!!country_names)) %>% 
  left_join(geonomenclador  %>%
              dplyr::filter(fuente_codigo_fundar == 'iso') %>%
              select(codigo_pais = codigo_fundar, country_es = desc_fundar),
            join_by(country_es)) %>% 
  group_by(country) %>% 
  fill(codigo_pais, .direction = "downup") %>% 
  fill(country_es, .direction = "downup") %>% 
  ungroup() %>% 
  mutate(commodity_name = recode(id, !!!minerales_es))


V <- df_output %>% dplyr::filter(is.na(codigo_pais)) %>% distinct(country) %>% pull()

df_output[df_output$country %in% V, c("codigo_pais")] <- c("VNM", "VNM", "USA", "USA", "USA", "USA", "KOR", "KOR")

df_output <- df_output %>% 
  select(-country, -country_es) %>% 
  left_join(geonomenclador  %>%
              dplyr::filter(fuente_codigo_fundar == 'iso') %>%
              select(codigo_pais = codigo_fundar, pais = desc_fundar),
            join_by(codigo_pais)) %>% 
  select(id, commodity_name, type, anio, codigo_pais, pais, production, production_unit, price_value, usd_unit_price, multiplier, valor_prod_norm_mill_usd, tamanio_mercado, rank_share, share)


comparable_df <- df_output %>% 
  dplyr::filter(anio == 2022) %>% 
  dplyr::filter(rank_share == 1) %>% 
  mutate(tamanio_mercado = tamanio_mercado * 1000000) %>% 
  select(codigo_pais, pais, mineral = commodity_name, tamanio_mercado, share)
  

df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico,
                                            entrega_subtopico = "primera_entrega")


comparacion <- argendataR::comparar_outputs(
  df_anterior = df_anterior,
  df = comparable_df,
  nombre = output_name,
  pk = c("codigo_pais","mineral"), # variables pk del dataset para hacer el join entre bases
  drop_joined_df =  F
)


df_output_final <- df_output %>% select(anio, codigo_pais, pais, mineral = commodity_name, valor_prod_norm_mill_usd, tamanio_mercado, share, rank_share)

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
output_cols <- names(df_output_final) # lo puedo generar así si tengo df_output


etiquetas_nuevas <- data.frame(
  variable_nombre = c("anio",
                      "codigo_pais",
                      "pais",
                      "mineral",
                      "valor_prod_norm_mill_usd",
                      "tamanio_mercado",
                      "share",
                      "rank_share"),
  descripcion = c("Año de referencia",
                  "Código de país según ISO 3166-1 alpha-3",
                  "Nombre de país",
                  "Nombre del mineral commodity",
                  "Valor de la producción, valuado a precios internacionales",
                  "Valor de la producción global",
                  "Participación en la producción global",
                  "Ranking de la participación")
)

descripcion <- armador_descripcion(metadatos = metadatos,
                                   etiquetas_nuevas = etiquetas_nuevas,
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
# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso


aclaracion <- paste0("Se actualizó la fuente de información. ",
                     "Se agregaron años, antes era sólo 2022, ahora es 2022 y 2023. ",
                     "Se agregaron más minerales que los que había anteriormente. ",
                     "Se incluyeron más países y además se incluyó su lugar en el ranking.",
                     "Se agregó el valor absoluto de la producción, antes solo se contaba con la participación")

df_output_final %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    fuentes = colectar_fuentes(),
    analista = analista,
    pk = c("anio","codigo_pais","mineral"),
    es_serie_tiempo = T,
    control = comparacion, 
    columna_indice_tiempo = 'anio',
    columna_geo_referencia = 'codigo_pais',
    descripcion_columnas = descripcion,
    unidades = list("valor_prod_norm_mill_usd" = "Millones de dólares corrientes",
                    "share" = "proporción",
                    "tamanio_mercado" = "Millones de dólares corrientes",
                    "rank_share" = "unidades"),
    aclaracion = aclaracion
  )

