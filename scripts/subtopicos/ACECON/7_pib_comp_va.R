# 7_pib_comp_va
# descripcion

# vars config del script
output_name <- "7_pib_comp_va"
# periodo, etc.,

# Insumos -------

# sectores como % del pib a precios de mercado y PIB a precio de mercado como % respecto a precios basicos
pbi_fnys <- readxl::read_excel(path = glue::glue("data/{subtopico}/datasets/raw/cuentas-nacionales-fund-norte-y-sur.xlsx"),
                                       sheet = "PBI por sectores %"
                               )

# sectores como % del pib a precios de mercado y PIB a precio de mercado como % respecto a precios basicos
pbisectores_fnys <- readxl::read_excel(path = glue::glue("data/{subtopico}/datasets/raw/cuentas-nacionales-fund-norte-y-sur.xlsx"),
                               sheet = "PBI por sectores %"
                               )

# pib en usd “PIB a precios de mercado, en miles de $ 2018
pbiusd_fnys <- readxl::read_excel(path = glue::glue("data/{subtopico}/datasets/raw/cuentas-nacionales-fund-norte-y-sur.xlsx"),
                                  sheet = "PBI en US$", range = "B225", col_names = F) %>% 
  rename(pib = ...1) %>% mutate(anio = 2018)

# pib por sectores a precios corrientes en millones de pesos
pbisectores_indec <- readxl::read_xls(glue::glue("data/{subtopico}/datasets/raw/sh_oferta_demanda_12_23.xls"),
                                       sheet = "cuadro 12", col_names = F)

# Procesamiento -------

pbisectores_indec <- tidy_indec(pbisectores_indec, tabla = "sh_oferta_demanda")

pbisectores_indec <- pbisectores_indec %>% 
  filter(trim == "Total") %>% 
  select(-trim) 

pbisectores_indec <- pbisectores_indec %>%
  filter(anio %in% 2018:2022)

pbisectores_indec <- pbisectores_indec %>%
  select(anio, producto_interno_bruto, a_agricultura_ganaderia_caza_y_silvicultura,
         b_pesca, c_explotacion_de_minas_y_canteras, d_industria_manufacturera,
         e_electricidad_gas_y_agua, f_construccion,
         g_comercio_mayorista_minorista_y_reparaciones,h_hoteles_y_restaurantes, i_transporte_almacenamiento_y_comunicaciones,
         j_intermediacion_financiera, k_actividades_inmobiliarias_empresariales_y_de_alquiler,
         l_administracion_publica_y_defensa_planes_de_seguridad_social_de_afiliacion_obligatoria,
         m_ensenanza, n_servicios_sociales_y_de_salud,
         o_otras_actividades_de_servicios_comunitarias_sociales_y_personales, p_hogares_privados_con_servicio_domestico)

pbisectores_indec <- pbisectores_indec %>% 
  mutate(across(everything(), as.numeric))

pbisectores_indec <- pbisectores_indec %>% 
  group_by(anio) %>% 
  mutate(agricultura_caza_silvicultura_y_pesca = sum(c(a_agricultura_ganaderia_caza_y_silvicultura,
                                                     b_pesca), na.rm = T),
         construccion_y_ega = sum(c(f_construccion,
                                    e_electricidad_gas_y_agua), na.rm = T),
         comercio_y_servicios = sum(pick(g_comercio_mayorista_minorista_y_reparaciones:p_hogares_privados_con_servicio_domestico),
                                    na.rm = T)
         ) %>% 
  ungroup()

pbisectores_indec <- pbisectores_indec %>% 
  select(anio, producto_interno_bruto, agricultura_caza_silvicultura_y_pesca,
         explotacion_minas_y_canteras = c_explotacion_de_minas_y_canteras,
         industria_manufacturera = d_industria_manufacturera,
         construccion_y_ega, comercio_y_servicios)

pbisectores_indec <- pbisectores_indec %>%
  mutate(across(-anio, \(x) 100*x/producto_interno_bruto,
                .names = "prop_{.col}"))

pbisectores_indec <- pbisectores_indec %>%
  mutate(across(-c(anio, matches("prop_")), \(x) x/lag(x), .names = "var_{.col}"))



pbiusd_fnys <- pbiusd_fnys %>% 
  mutate(pib = pib/1000)


pbisectores_fnys <- pbisectores_fnys %>% 
  select(1,3:12,16)


colnames(pbisectores_fnys) <- c("anio",
                                "pib_pbpm",
                                "Agricultura, caza y silvicultura",
                                "Pesca",
                                "Explotación de minas y canteras",
                                "Industrias Manufactureras",
                                "Electricidad, Gas y Agua",
                                "Construcción",
                                "Comercio al por mayor y menor y hoteles y restaurantes",
                                "Transporte, almacenamiento y comunicaciones",
                                "Intermediación financiera y actividades inmobiliarias",
                                "Administrac. pública y defensa y serv. soc.") %>% 
  janitor::make_clean_names()

pbisectores_fnys <- pbisectores_fnys %>% 
  mutate(across(everything(), as.numeric))
  
pbisectores_fnys <- pbisectores_fnys %>%
  filter(anio %in% 1935:2018)

pbisectores_fnys <- pbisectores_fnys %>%
  group_by(anio) %>% 
  mutate(pib_pbpm = ifelse(is.na(pib_pbpm), sum(c_across(-c(pib_pbpm)), na.rm = T ),
                           pib_pbpm)) %>% 
  ungroup()

pbisectores_fnys <- pbisectores_fnys %>%
  group_by(anio) %>% 
  mutate(agricultura_caza_silvicultura_y_pesca = sum(c(agricultura_caza_y_silvicultura,pesca), na.rm = T),
         construccion_y_ega = sum(c(construccion,electricidad_gas_y_agua), na.rm = T),
         comercio_y_servicios = sum(c(comercio_al_por_mayor_y_menor_y_hoteles_y_restaurantes, 
                                      transporte_almacenamiento_y_comunicaciones,
                                      intermediacion_financiera_y_actividades_inmobiliarias,
                                      administrac_publica_y_defensa_y_serv_soc), na.rm = T)) %>% 
  select(anio, pib_pbpm, agricultura_caza_silvicultura_y_pesca,
         explotacion_minas_y_canteras = explotacion_de_minas_y_canteras,
         industria_manufacturera = industrias_manufactureras,
         construccion_y_ega, comercio_y_servicios)



pbisectores_fnys <- bind_rows(pbisectores_fnys, pbiusd_fnys)

pbisectores_fnys %>% 
  ungroup() %>% 
  mutate(across(-c(anio, pib), \(x) {x/100*pib},
                .names = "bruto_{.col}")) %>% 
  view


pbisectores_fnys <- pbisectores_fnys %>%
  mutate(across(everything(), \(x) x/pib_pbpm*100))

pbisectores_fnys <- pbisectores_fnys %>%
  ungroup()




# Control vs output previo -------

# descargo outout primera entrega del drive
# se puede leer outoput del drive directo desde la url
out_prev <- read.csv2(file = glue::glue("https://drive.usercontent.google.com/download?id={outputs$id[grepl(output_name, outputs$name)]}"))

out_prev <- out_prev %>% 
mutate(across(-c(), as.numeric))

vs <- out_prev %>% 
left_join(df_output, by = c())

diff <-  vs %>% 
  mutate(across(where(is.numeric), \(x) round(x, 2))) %>% 
  filter(var.x !=  var.y ) 

diff %>% 
  write_argendata(file_name = glue::glue("_diff_{output_name}.csv"),
  subtopico =  subtopico)

# Write output ------


df_output %>% 
  write_argendata(file_name = glue::glue("{output_name}.csv"),
  subtopico = subtopico)