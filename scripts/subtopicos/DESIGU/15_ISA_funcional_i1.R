#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

limpiar_temps()

code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)
subtopico <- 'DESIGU'
output_name <- 'ISA_funcional_i1'


fuente_1 <- "R210C0"
fuente_2 <- "R35C76"
fuente_3 <- "R211C77"



# Participación en el Valor Agregado Bruto a precios básicos (por sector o total de la economía)
ceped_df <- readxl::read_excel(argendataR::get_temp_path(fuente_1)) %>% 
  dplyr::filter(variable == "particip.vab.pb") %>% 
  select(anio = Anio, particip_vab_pb_total = Total)
  
# Cuenta Generacion del Ingeso (RTA pp) - INDEC
cgi_df <- read_csv(argendataR::get_temp_path(fuente_2)) %>% 
  dplyr::filter(trim == "Total") %>% 
  dplyr::filter(indicador == "Total general") %>% 
  select(anio, participacion)

grania_df <- read_csv(argendataR::get_temp_path(fuente_3)) %>% select(anio, masa_salarial)


data_total <- grania_df %>% 
  full_join(., ceped_df, by=join_by(anio)) %>% 
  full_join(., cgi_df, by=join_by(anio)) %>%
  arrange(-anio)


data_total_shifted <- as.data.frame(lapply(data_total %>% select(-anio), function(x) lead(x,1)))

X <- data_total %>% bind_cols(data_total_shifted)

X <- X %>% 
  mutate(
    relat = case_when(
      anio > 2016 ~ `participacion...4` / `participacion...7`,
      anio <= 2016 & anio > 1993 ~ `particip_vab_pb_total...3` / `particip_vab_pb_total...6`,
      TRUE  ~  `masa_salarial...2`/ `masa_salarial...5`
    )
           )


X$relat <- c(1, X$relat[1:(length(X$relat)-1)])



hacer_empalme = function(tasas, comienzo){
  empalme <- c()
  valor <- comienzo
  for (t in tasas){
    valor <- valor/t
    empalme <- c(empalme, valor)
  }
  return(empalme)
}


X$part_salarial_vab <- hacer_empalme(X$relat, X$participacion...4[[1]])


df_output <- X %>% 
  select(anio, part_salarial_vab) %>% 
  arrange(anio) 

df_anterior <- argendataR::descargar_output(nombre ='ISA_funcional_i1', 
                                            subtopico = "DESIGU", 
                                            entrega_subtopico = "datasets_primera_entrega") %>% 
  pivot_wider(names_from = "variable", values_from = 'valor') %>% 
  select(anio = ano, part_salarial_vab = participacion)


comparacion <- argendataR::comparar_outputs(
  df_output,
  df_anterior,
  pk = c("anio"), # variables pk del dataset para hacer el join entre bases
  drop_joined_df = F
)


#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

etiquetas <- meta_desigu %>% 
  filter(dataset_archivo == output_name) %>% 
  pull(descripcion) %>% 
  as.list()

names(etiquetas) <- meta_desigu %>% 
  filter(dataset_archivo == output_name) %>% 
  pull(variable_nombre)

pks <- meta_desigu %>% 
  filter(dataset_archivo == output_name & primary_key == "TRUE") %>% 
  pull(variable_nombre)

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    fuentes = c(fuente_1, fuente_2, fuente_3),
    analista = "",
    pk = pks,
    es_serie_tiempo = T,
    control = comparacion,
    columna_indice_tiempo = "anio",
    aclaraciones = "El dataset posee algunas diferencias con respecto al realizado por el analista",
    etiquetas_indicadores = list("part_salarial_vab" = "Participación de la masa salarial en el Valor Agregado Bruto a precios básicos"),
    unidades = list("part_salarial_vab" = "porcentaje")
  )

