#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)


id_fuente <- 431
fuente_raw <- sprintf("R%sC0",id_fuente)

# Guardado de archivo
nombre_archivo_raw <- str_split_1(fuentes_raw() %>% 
                                    filter(codigo == fuente_raw) %>% 
                                    select(path_raw) %>% 
                                    pull(), pattern = "\\.")[1]

titulo.raw <- fuentes_raw() %>% 
  filter(codigo == fuente_raw) %>% 
  select(nombre) %>% pull()

# Función para verificar si el número de NAs en cada fila es mayor o igual a un umbral
check_na_threshold <- function(df, threshold) {
  apply(df, 1, function(row) {
    sum(is.na(row)) >= threshold
  })
}

white_cols <- function(df) {
  sapply(df, function (col) all(is.na(col)))
}


range <- "F14:LV26"

sheet_name <- "E11.m+p FiscalInterventions"



data_raw <- argendataR::get_raw_path(fuente_raw) %>% 
  readxl::read_excel(., sheet = sheet_name,
                     col_names = F,
                     range = "C14:LV26") %>% 
  slice(3:n())


cols_ <- readxl::read_excel(argendataR::get_raw_path(fuente_raw), 
                            sheet = sheet_name,
                            range = "F14:LV26",
                            col_names = F) %>% slice(1:2)

cols <- cols_[!white_cols(cols_)] %>%
  t() %>% # Transponer
  as.data.frame() %>% 
  fill(V2, .direction = "down")

cols$concatenado <- apply(cols, 1, function(x) {
  paste(stats::na.omit(x), collapse = "#")
})


colnames(data_raw) <- c("DECIL","Population Total", "Population Share", cols$concatenado)

data_raw$`Population Total` <- as.numeric(data_raw$`Population Total`)

data_raw$`Population Share` <- as.numeric(data_raw$`Population Share`)

diccionario_texto <- '"codigo","es_agregacion","desagregacion","gran_categoria","categoria","subcategoria","rubro","name_eng","name_esp"
"1","1","1","Ingreso de mercado + Pensiones","","","","Market Income + Pensions","Ingreso de mercado + Jubilaciones"
"2","1","1","Impuestos y contribuciones","","","","All taxes and contributions","Total – Impuestos y contribuciones"
"2.1","1","2","Impuestos y contribuciones","Impuestos","","","All taxes","Total – Impuestos"
"2.1.1","1","3","Impuestos y contribuciones","Impuestos","Impuestos directos","","All direct taxes","Total – Impuestos directos"
"2.1.1.01","0","9","Impuestos y contribuciones","Impuestos","Impuestos directos","","Payroll Tax (per capita)","Impuestos a la nómina salarial (PAMI, sindicatos, etc.)"
"2.1.1.02","0","9","Impuestos y contribuciones","Impuestos","Impuestos directos","","Personal Income Tax (per capita)","Impuesto a los ingresos"
"2.1.2","1","3","Impuestos y contribuciones","Impuestos","Impuestos indirectos","","All indirect taxes","Total – Impuestos indirectos"
"2.1.2.01","0","9","Impuestos y contribuciones","Impuestos","Impuestos indirectos","","Excises taxes - IIBB (per capita)","Impuesto a los ingresos brutos"
"2.1.2.02","0","9","Impuestos y contribuciones","Impuestos","Impuestos indirectos","","Indirect taxes: fuel tax (per capita)","Impuesto al combustible"
"2.1.2.03","0","9","Impuestos y contribuciones","Impuestos","Impuestos indirectos","","Excises taxes - Internos (per capita)","Impuestos internos"
"2.1.2.04","0","9","Impuestos y contribuciones","Impuestos","Impuestos indirectos","","Value-Added Tax (per capita)","IVA"
"2.2","1","2","Impuestos y contribuciones","Contribuciones","","","All contributions","Total – Contribuciones"
"2.2.01","0","9","Impuestos y contribuciones","Contribuciones","Contribuciones","","Contributions to the public health care system (per capita)","Contribuciones al sistema de salud"
"2.2.02","0","9","Impuestos y contribuciones","Contribuciones","Contribuciones","","Contributions to the Social Security Institute (SSI) (per capita)","Contribuciones a la seguridad social"
"3","1","2","Transferencias y subsidios","Transferencias y subsidios","Total transf. y subsidios incl. contribuciones","","All net transfers and subsidies incl contributory pensions","Total – Transferencias y subsidios (incluyendo jubilaciones contributivas)"
"3.1.1","1","3","Transferencias y subsidios","Transferencias","Transferencias monetarias","","All direct transfers incl contributory pensions","Total – Transferencias directas (incluyendo jubilaciones contributivas)"
"3.1.1.1","0","9","Transferencias y subsidios","Transferencias","Transferencias monetarias","Contributivas","All contributory pensions","Todas las pensiones contributivas"
"3.1.1.2","1","3","Transferencias y subsidios","Transferencias","Transferencias monetarias","No contributivas","All direct transfers excl contributory pensions","Total – Transferencias directas (excluyendo jubilaciones contributivas)"
"3.1.1.1.01","0","9","Transferencias y subsidios","Transferencias","Transferencias monetarias","Contributivas","Jubilacion (per capita)","Jubilación (per cápita)"
"3.1.1.2.01","0","9","Transferencias y subsidios","Transferencias","Transferencias monetarias","No contributivas","Asignaciones Familiares (per capita)","Asignaciones familiares"
"3.1.1.2.02","0","9","Transferencias y subsidios","Transferencias","Transferencias monetarias","No contributivas","Becas estudiantiles (per capita)","Becas estudiantiles"
"3.1.1.2.03","0","9","Transferencias y subsidios","Transferencias","Transferencias monetarias","No contributivas","Capacitacion y empleo (per capita)","Capacitación y empleo"
"3.1.1.2.04","0","9","Transferencias y subsidios","Transferencias","Transferencias monetarias","No contributivas","CCT AUH (per capita)","Asignación Universal por Hijo (AUH)"
"3.1.1.2.05","0","9","Transferencias y subsidios","Transferencias","Transferencias monetarias","No contributivas","Comedores (per capita)","Comedores"
"3.1.1.2.06","0","9","Transferencias y subsidios","Transferencias","Transferencias monetarias","No contributivas","Moratoria previsional (per capita)","Moratoria previsional"
"3.1.1.2.07","0","9","Transferencias y subsidios","Transferencias","Transferencias monetarias","No contributivas","Jovenes con mas y mejor trabajo (per capita)","Jóvenes con más y mejor trabajo"
"3.1.1.2.08","0","9","Transferencias y subsidios","Transferencias","Transferencias monetarias","No contributivas","PNC (per capita)","Pensiones no contributivas"
"3.1.1.2.09","0","9","Transferencias y subsidios","Transferencias","Transferencias monetarias","No contributivas","Progresar (per capita)","Progresar"
"3.1.1.2.10","0","9","Transferencias y subsidios","Transferencias","Transferencias monetarias","No contributivas","Unemployment insurance (per capita)","Seguro de desempleo"
"3.1.2","1","3","Transferencias y subsidios","Transferencias","Transferencias en especie","","All net in-kind transfers","Total – Transferencias en especie"
"3.1.2.1","1","4","Transferencias y subsidios","Transferencias","Transferencias en especie","Salud","Net health transfers","Total – Salud"
"3.1.2.1.01","0","9","Transferencias y subsidios","Transferencias","Transferencias en especie","Salud","In-kind Health Benefits - Hospitales (per capita)","Hospital público"
"3.1.2.1.02","0","9","Transferencias y subsidios","Transferencias","Transferencias en especie","Salud","In-kind Health Benefits - Obra social (per capita)","Obras sociales"
"3.1.2.1.03","0","9","Transferencias y subsidios","Transferencias","Transferencias en especie","Salud","In-kind Health Benefits - PAMI (per capita)","PAMI"
"3.1.2.2","1","4","Transferencias y subsidios","Transferencias","Transferencias en especie","Educación","Net education transfers","Total – Educación"
"3.1.2.2.01","0","9","Transferencias y subsidios","Transferencias","Transferencias en especie","Educación","In-kind education benefits: initial level (per capita)","Educación inicial"
"3.1.2.2.02","0","9","Transferencias y subsidios","Transferencias","Transferencias en especie","Educación","In-kind education benefits: primary level (per capita)","Educación primaria"
"3.1.2.2.03","0","9","Transferencias y subsidios","Transferencias","Transferencias en especie","Educación","In-kind education benefits: secondary level (per capita)","Educación secundaria"
"3.1.2.2.04","0","9","Transferencias y subsidios","Transferencias","Transferencias en especie","Educación","In-kind education benefits: tertiary level (per capita)","Educación superior"
"3.2","1","2","Transferencias y subsidios","Subsidios","","","All indirect subsidies","Total – Subsidios"
"3.2.01","0","9","Transferencias y subsidios","Subsidios","Subsidios","","Subsidy to bus transportation (per capita)","Colectivo"
"3.2.02","0","9","Transferencias y subsidios","Subsidios","Subsidios","","Subsidy to electricity (per capita)","Electricidad"
"3.2.03","0","9","Transferencias y subsidios","Subsidios","Subsidios","","Subsidy to gas bottle (per capita)","Gas de garrafa"
"3.2.04","0","9","Transferencias y subsidios","Subsidios","Subsidios","","Subsidy to gas (per capita)","Gas de red"
"3.2.05","0","9","Transferencias y subsidios","Subsidios","Subsidios","","Subsidy to train transportation (per capita)","Tren y subte"
"2.X","1","3","Impuestos y contribuciones","Impuestos y contribuciones","Impuestos directos y contribuciones","","All direct taxes and contributions","Total – Impuestos directos y contribuciones"
"3.X","1","2","Transferencias y subsidios","Transferencias y subsidios","","","All net transfers and subsidies excl contributory pensions","Total – Transferencias y subsidios (excluyendo jubilaciones contributivas)"
'

diccionario_manual <- read.csv(text = diccionario_texto, sep = ",")


df_stage <- data_raw %>% 
  pivot_longer(., cols = !all_of(c("DECIL","Population Total", "Population Share")), 
               names_to = c("name_eng", "variable"), 
               names_sep = "#", 
               values_to = "value",
               values_transform = as.numeric) 

df_clean <- df_stage %>% 
  left_join(diccionario_manual, join_by(name_eng)) %>% 
  janitor::clean_names()


sheet_name_normalizada <- sheet_name %>% janitor::make_clean_names()

clean_filename <- glue::glue("{nombre_archivo_raw}_{sheet_name_normalizada}_CLEAN.parquet")

clean_title <- glue::glue("{titulo.raw} - {sheet_name}")

path_clean <- glue::glue("{tempdir()}/{clean_filename}")

df_clean %>% arrow::write_parquet(., sink = path_clean)


# agregar_fuente_clean(id_fuente_raw = id_fuente,
#                      path_clean = clean_filename,
#                      nombre = clean_title,
#                      script = code_name)



id_fuente_clean <- 277
codigo_fuente_clean <- sprintf("R%sC%s", id_fuente, id_fuente_clean)


df_clean_anterior <- arrow::read_parquet(get_clean_path(codigo = codigo_fuente_clean ))


comparacion <- comparar_fuente_clean(df_clean %>% select(decil, name_eng, variable, value),
                                     df_clean_anterior %>% select(decil, name_eng, variable, value),
                                     pk = c('decil', 'name_eng', 'variable')
)

actualizar_fuente_clean(id_fuente_clean = id_fuente_clean,
                        path_clean = clean_filename,
                        nombre = clean_title, 
                        script = code_name,
                        comparacion = comparacion)