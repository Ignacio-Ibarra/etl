################################################################################
##    Dataset: Ingresos laborales (relativos a la media nacional)             ##
##    y tasa de empleo femenina (18-65 años) por provincia, 2022              ##
################################################################################

#-- Descripcion ----
#' Breve descripcion de output creado
#'

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


subtopico <- "MERTRA"
output_name <- "tasa_empleo_mujer_salario_provincia_anio"
fuente1 <- "R49C16"  
fuente2 <- "R84C14"


#-- Lectura de Datos ----

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R" 
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:
ephtu_df <- readr::read_csv(argendataR::get_temp_path(fuente1))
# ephtu_df <- ephtu_df %>% rename_with(tolower, everything()) #esta linea no haría falta que esté cuando cambiemos el input de fuente1 por la fuente clean. 

codigos <- readr::read_csv(argendataR::get_temp_path(fuente2))
codigos <- codigos %>% select(aglomerado = aglom_cod_indec, provincia = prov_cod, prov_desc)

#-- Procesamiento ----

ephtu_df <- ephtu_df %>% 
  left_join(codigos, by = join_by(aglomerado, provincia)) # Joineo así por los casos en que hay mismo aglomerado pero distinta provincia e.g. San Nicolás-Villa Constitucion

ephtu_df <- ephtu_df %>% mutate(
  # activo = case_when(
  #   estado == 1 | estado == 2 ~ 'activo',
  #   TRUE ~ 'no_activo'
  # ),
  ocupado = case_when(
    estado == 1 ~ 'ocupado',
    TRUE ~ 'no_ocupado'
  )
)

base <- ephtu_df %>% 
  select(anio = ano4, ocupado, provincia = prov_desc, sexo = ch04, edad = ch06, pondera, pondiio, p21) 


# Salario medio nacional
salario_medio_nacional <- base %>% 
  filter(ocupado == 'ocupado') %>% 
  group_by(anio) %>% 
  summarise(salario_medio = weighted.mean(p21, pondiio))

#Salario relativo a la media nacional por provincia y anio
salario_medio_relativo_prov <- base %>% 
  filter(ocupado == 'ocupado') %>% 
  group_by(anio, provincia) %>% 
  summarise(salario_medio_prov = weighted.mean(p21, pondiio)) %>% 
  left_join(., salario_medio_nacional, by=join_by(anio)) %>% 
  mutate(salario_relativo = 100*salario_medio_prov / salario_medio) %>% 
  select(anio, provincia, salario_relativo)


# Tasa de empleo mujer entre 18 y 65 años
empleo_mujer_prov <- base %>% 
  filter(edad>=18 & edad<=65 & sexo == 2) %>%
  group_by(anio, provincia, ocupado) %>% 
  summarize(pondera = sum(pondera)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = ocupado, values_from = pondera, values_fill = 0) %>% 
  mutate(tasa_empleo_mujer = ocupado / (no_ocupado + ocupado)) %>% 
  select(anio, provincia, tasa_empleo_mujer)

df_output <- inner_join(salario_medio_relativo_prov, empleo_mujer_prov, by=join_by(anio, provincia))




#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso

# Las diferencias con el output anterior radican en que CABA era antes Ciudad de Buenos Aires, debido a cambios en el nomenclador usado. 
comparacion <- argendataR::comparar_outputs(
  df_output,
  nombre = output_name,
  pk = c("anio", "provincia"),
  drop_output_drive = F
)

#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    fuentes = c(fuente1, fuente2),
    analista = "",
    pk = c("anio", "provincia"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    etiquetas_indicadores = list("salario_relativo" = "Ingreso laboral mensual medio, relativo a la media nacional (=100)",
                                 "tasa_empleo_mujer" = "Tasa de empleo femenina, de personas entre 18 y 65 años") ,
    unidades = list("salario_relativo" = "unidades",
                    "tasa_empleo_mujer" = "unidades")
  )