# evolución temperatura aire 1850-2020  -----------

temp_aire_1850_2020_url <- "https://dap.ceda.ac.uk/badc/ar6_wg1/data/spm/spm_01/v20221116/panel_b/gmst_changes_model_and_obs.csv?download=1"

download.file(url = temp_aire_1850_2020_url, 
               mode = "wb", # archivos tipo xlsx requieren escritura tipo binaria
               destfile = glue::glue("{tempdir()}/temp_aire_1850_2020.csv"))

agregar_fuente_raw(url = "https://dap.ceda.ac.uk/badc/ar6_wg1/data/spm/spm_01/v20221116/panel_b/gmst_changes_model_and_obs.csv?download=1", institucion = "CEDA - National Centre for Earth Observation", actualizable = T,
                   fecha_descarga = Sys.Date(),
                   path_raw = "temp_aire_1850_2020.csv", 
                   dir = tempdir(),
                   script = "descarga_evolucion_temperatura_aire_1850_2020.R",
                   nombre = "Evolución temperatura aire 1850-2020"
)

actualizar_fuente_raw(id_fuente=112,url = "https://dap.ceda.ac.uk/badc/ar6_wg1/data/spm/spm_01/v20221116/panel_b/gmst_changes_model_and_obs.csv?download=1",
                      fecha_descarga = Sys.Date())
