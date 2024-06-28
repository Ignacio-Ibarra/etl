source("scripts/subtopicos/MERTRA/fuentes_MERTRA.R")

 subtopico <-  "MERTRA"
 analista <-  c("")

  entrega <- "datasets_update"


#-- Sources -----



archivos <- list.files(glue::glue("~/etl/scripts/subtopicos/{subtopico}/"))
scripts <- archivos[grepl("\\.R$", archivos) &
                      ! archivos %in% c(glue::glue("0_{subtopico}.R"), glue::glue("fuentes_{subtopico}.R"))]

#scripts_out <- scripts[3] 
#
#scripts <- scripts %>% as_tibble() %>% filter(!value %in% scripts_out) %>% pull()
#

walk(scripts, function(x) {
  source(glue::glue("~/etl/scripts/subtopicos/{subtopico}/{x}"), local = T)
})


salidas <- list.files(tempdir(), full.names = T)[list.files(tempdir()) %in% subtopico_outputs(subtopico_nombre = subtopico,
                                                                                              entrega_subtopico = entrega)$name]



path_data <- glue::glue("~/data/{subtopico}")


salidas_json <- list.files(tempdir(), full.names = T) %>% 
  as_tibble() %>% 
  filter(str_detect(value, "json")) %>% 
  pull()


purrr::walk(salidas_json, 
            function (x) {
              
              file.copy(from = x, to = path_data, overwrite = T) 
              message(glue::glue("{x} copiado a {path_data}."))
            })





#generados <- salidas %>% 
#  as_tibble() %>% 
#  mutate(limpia = str_remove_all(value, "/tmp/RtmpwwtZ7X/")) %>% 
#  transmute(file = str_remove_all(limpia, ".csv")) 
#
#
#
#scripts_corridos <- scripts %>% 
#  as_tibble() %>% 
#  mutate(limpia = str_remove_all(value, ".R")) %>% 
#  transmute(file = str_remove_all(limpia, "^\\d+_"))
#
#anti_join(scripts_corridos, generados)








#source("scripts/subtopicos/MERTRA/1_tasa_participacion_censos.R")
#source("scripts/subtopicos/MERTRA/10_tasa_empleo_anio_provincia_sexo.R")
#source("scripts/subtopicos/MERTRA/11_tasa_empleo_menores_provincia.R")
#source("scripts/subtopicos/MERTRA/12_establecimientos_tasa_empleo_provincia.R")
#source("scripts/subtopicos/MERTRA/13_tasa_empleo_mujer_salario_provincia_anio.R")
#source("scripts/subtopicos/MERTRA/14_lavarropas_tasa_empleo_fem_provincia.R")
#source("scripts/subtopicos/MERTRA/15_share_acuerdo_varones_empleo_region_arg.R")
#source("scripts/subtopicos/MERTRA/16_tasa_empleo_provincia_nivel_educativo.R")
#source("scripts/subtopicos/MERTRA/17_tiempo_social_trabajo_sexo.R")
#source("scripts/subtopicos/MERTRA/18_tiempo_social_trabajo_sexo_edad.R")
#source("scripts/subtopicos/MERTRA/19_tiempo_social_trabajo_sexo_niveleducativo.R")
#source("scripts/subtopicos/MERTRA/2_ratio_tasa_actividad_mujer_varon_por_pais_anio.R")
#source("scripts/subtopicos/MERTRA/20_tipo_trabajo_no_rem_sexo.R")
#source("scripts/subtopicos/MERTRA/21_trabajo_no_remunerado_sexo_internacional.R")
#source("scripts/subtopicos/MERTRA/3_tasa_actividad_por_pais_anio.R")
#source("scripts/subtopicos/MERTRA/4_media_movil_por_edad_de_tasa_actividad.R")
#source("scripts/subtopicos/MERTRA/5_media_movil_por_edad_sexo_de_tasa_actividad_brecha.R")
#source("scripts/subtopicos/MERTRA/6_media_movil_por_edad_reg_desc_fundar_de_tasa_actividad.R")
#source("scripts/subtopicos/MERTRA/7_puestos_percapita_anio_pais.R")
#source("scripts/subtopicos/MERTRA/8_tasa_empleo_anio_provincia.R")
#source("scripts/subtopicos/MERTRA/9_tasa_empleo_por_franja_etaria_anio_provincia.R")
#