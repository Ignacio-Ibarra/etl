# Genero archivos parquet en /srv para consultar en scripts para outputs
# Load necessary package
library(arrow)


code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)

fuente_baci <- "BACI"
url_fuente <- "http://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=37"
institution <- "Centre d'Études Prospectives et d'Informations Internationales (CEPII)"


## Wrangle RAW data csv 2 parquet ----

#### Obtener datos crudos en .csv ----

# Specify the directory to search in
directory_path <- glue::glue("{Sys.getenv('BACI_PATH')}")


# Get the list of all zip files in the specified directory
all_files <- list.files(directory_path, full.names = T, pattern = "zip")

directorio_temporal <- tempdir() # ZIP files


# Unzip in temp
unzip(all_files[1], exdir = directorio_temporal) # [1] "/srv/server_data/argendata/baci_comext/BACI_HS07_V202401b.zip"

unzip(all_files[2], exdir = directorio_temporal) # [2] "/srv/server_data/argendata/baci_comext/BACI_HS96_V202401b.zip"


# Get the list of all files in the specified directory
all_csv_files <- list.files(directorio_temporal, full.names = T)



##### CODES (country and product) -----

codes <- grep('codes', all_csv_files, value = TRUE)

#codes present in all_csv_files
#[1] "/tmp/Rtmp3qibtu/country_codes_V202401b.csv"     
#[2] "/tmp/Rtmp3qibtu/product_codes_HS07_V202401b.csv"
#[3] "/tmp/Rtmp3qibtu/product_codes_HS96_V202401b.csv"


##### HS07  ----

codesHS07 <- grep('HS96', codes, value = TRUE, invert = TRUE)


#Filter the files that contain both "HS07" and ".csv"
HS07files <- grep("BACI_HS07.*\\.csv$", all_csv_files, value = TRUE)



for(i in codesHS07){
  
  file <- stringr::str_remove(string = i, pattern = tempdir())
  
  read_csv(file = i) %>% 
    write_parquet(paste0("/srv/server_data/argendata/baci_comext/BACI_HS07/", file))
}

# Define input and output folder paths
folder_input <- tempdir()
folder_output <- "/srv/server_data/argendata/baci_comext/"

# Loop over the years from 1996 to 2021
for (i in 2007:2022) {
  # Construct file paths
  input_file <- paste0(folder_input, "/BACI_HS07_Y", i, "_V202401b.csv")
  output_file <- paste0(folder_output, "BACI_HS07/BACI_HS07_Y", i, "_V202401b.parquet")
  
  # Import CSV file
  data <- read_csv(input_file)
  
  # Save the data frame as a Parquet file
  write_parquet(data, output_file)
}






##### HS96 -----

codesHS96 <- grep('HS07', codes, value = TRUE, invert = TRUE)




for(i in codesHS96){
  
  file <- stringr::str_remove(string = i, pattern = tempdir())
  
  read_csv(file = i) %>% 
    write_parquet(paste0("/srv/server_data/argendata/baci_comext/BACI_HS96/", file))
}


# Filter the files that contain both "HS96" and ".csv"

HS96files <- grep("BACI_HS96.*\\.csv$", all_csv_files, value = TRUE)


# Loop over the years from 1996 to 2021
for (i in 1996:2021) {
  # Construct file paths
  input_file <- paste0(folder_input, "/BACI_HS96_Y", i, "_V202401b.csv")
  output_file <- paste0(folder_output, "BACI_HS96/BACI_HS96_Y", i, "_V202401b.parquet")
  
  # Import CSV file
  data <- read_csv(input_file)
  
  # Save the data frame as a Parquet file
  write_parquet(data, output_file)
}

##### Classification Codes - The Micro-D Classification: A New Approach to Identifying Differentiated Exports ----
link_hallak <- "0B19niEgxbWCrUTM3bHNodU9Lenc"

drive_download(as_id(link_hallak), path = glue::glue("{tempdir()}/Micro-D-Codes.zip"), overwrite = TRUE)

unzip(glue::glue("{tempdir()}/Micro-D-Codes.zip"), exdir = tempdir()) 


haven::read_dta(glue::glue("{tempdir()}/Micro-D.dta")) %>% 
  write_csv("/srv/server_data/argendata/baci_comext/Micro-D.csv")


haven::read_dta(glue::glue("{tempdir()}/6-digit Micro-D.dta")) %>% 
  write_csv("/srv/server_data/argendata/baci_comext/6-digit Micro-D.csv")

haven::read_dta(glue::glue("{tempdir()}/Combined SIM for Micro-D.dta")) %>% 
  write_csv("/srv/server_data/argendata/baci_comext/combined_SIM_for_Micro-D.csv")



# CLEAN DATASETS #####

# IMPORT DATA 


hs96_arg_2020 <- arrow::read_parquet(glue::glue("{directory_path}/BACI_HS96/BACI_HS96_Y2020_V202401b.parquet")) %>% arrow::to_duckdb() 

hs07_arg_2020 <- arrow::read_parquet(glue::glue("{directory_path}/BACI_HS07/BACI_HS07_Y2020_V202401b.parquet")) %>% arrow::to_duckdb() 


country_codes <- arrow::read_parquet(glue::glue("{directory_path}/BACI_HS07/country_codes_V202401b.csv"))  %>% arrow::to_duckdb()

country_codes <- country_codes %>% collect()
country_codes[country_codes$country_code == 490, "country_iso3" ] <- "TWN"
country_codes <- country_codes %>% arrow::to_duckdb()


#-- Procesamiento ----

#####-- HS96----


df_output_export96 <- hs96_arg_2020 %>% 
  # Select only the required columns
  select(t, i, k, v) %>%
  # Group by t, i, and k, and then sum v
  group_by(t, i, k) %>%
  summarise(v = sum(v, na.rm = TRUE)) %>%
  # Rename i to country_code and v to export_value
  rename(country_code = i, export_value = v, year = t, hs6 = k) %>% 
  mutate(export_value = export_value/1000)


df_output_import96 <- hs96_arg_2020 %>% 
  # Select only the required columns
  select(t, j, k, v) %>%
  # Group by t, i, and k, and then sum v
  group_by(t, j, k) %>%
  summarise(v = sum(v, na.rm = TRUE)) %>%
  # Rename i to country_code and v to export_value
  rename(country_code = j, import_value = v, year = t, hs6 = k) %>% 
  mutate(import_value = import_value/1000)




merged96 <- left_join(df_output_export96, df_output_import96) 


data_final96 <- merged96 %>% 
  left_join(country_codes) %>%
  mutate(hs6 = as.numeric(hs6),
         hs2 = hs6 %/% 10000)


# Generate sector_bp variable
data_final96 <- data_final96 %>%
  mutate(sector_bp = case_when(
    (hs2 >= 1 & hs2 <= 15) | hs2 == 23 ~ 1,
    hs2 >= 16 & hs2 <= 24 & hs2 != 23 ~ 2,
    hs2 >= 25 & hs2 <= 27 ~ 3,
    hs2 >= 28 & hs2 <= 38 ~ 4,
    hs2 >= 39 & hs2 <= 40 ~ 5,
    hs2 >= 41 & hs2 <= 43 ~ 6,
    hs2 >= 44 & hs2 <= 49 ~ 7,
    hs2 >= 50 & hs2 <= 67 ~ 8,
    hs2 >= 68 & hs2 <= 71 ~ 9,
    hs2 >= 72 & hs2 <= 83 ~ 10,
    hs2 >= 84 & hs2 <= 85 ~ 11,
    hs2 >= 86 & hs2 <= 89 ~ 12,
    hs2 >= 90 & hs2 <= 97 ~ 13,
    TRUE ~ NA_real_
  ))


sector_labels <- c(
  "1" = "Productos agropecuarios",
  "2" = "Alimentos procesados",
  "3" = "Minerales",
  "4" = "Químicos",
  "5" = "Plásticos y gomas",
  "6" = "Cueros y pieles",
  "7" = "Madera",
  "8" = "Textiles, indumentaria y calzado",
  "9" = "Piedra y vidrio",
  "10" = "Metales",
  "11" = "Maquinaria",
  "12" = "Transporte",
  "13" = "Otros productos industriales"
)

sector_labels_tibble <- tibble(
  sector_bp = as.numeric(names(sector_labels)),
  sector_bp_name = sector_labels
)


# Colapsamos
imports_exports_brambilla_porto <- data_final96 %>%
  group_by(year, country_code, sector_bp, country_iso3, country_name) %>%
  summarise(
    export_value = sum(export_value, na.rm = TRUE),
    import_value = sum(import_value, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  collect() %>% 
  group_by(year, country_code) %>%
  mutate(
    export_value_pc = 100 * export_value / sum(export_value, na.rm = TRUE),
    import_value_pc = 100 * import_value / sum(import_value, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  left_join(sector_labels_tibble) %>% 
  rename(iso3 = country_iso3, 
         country_name_abbreviation = country_name) 





# guardar como csv
write_csv_fundar(imports_exports_brambilla_porto,
                 file = glue::glue("{tempdir()}/imports_exports_brambilla_porto.csv"))  

# carga en sheet fuentes clean
#argendataR::agregar_fuente_clean(id_fuente_raw = 113,
#                    path_clean = "imports_exports_brambilla_porto.csv",
#                    nombre = "Composición de las exportaciones e importaciones de Argentina según sectores. En porcentaje del total. Año 2020",, 
#                    script = code_name, )

actualizar_fuente_clean(id_fuente_clean = 57, 
                        nombre = "Composición de las exportaciones e importaciones de Argentina según sectores. En porcentaje del total. Año 2020.")


#####-- HS07----

df_output_export07 <- hs07_arg_2020 %>% 
  # Select only the required columns
  select(t, i, k, v) %>%
  # Group by t, i, and k, and then sum v
  group_by(t, i, k) %>%
  summarise(v = sum(v, na.rm = TRUE)) %>%
  # Rename i to country_code and v to export_value
  rename(country_code = i, export_value = v, year = t) %>% 
  mutate(export_value = export_value/1000)


df_output_import07 <- hs07_arg_2020 %>% 
  # Select only the required columns
  select(t, j, k, v) %>%
  # Group by t, i, and k, and then sum v
  group_by(t, j, k) %>%
  summarise(v = sum(v, na.rm = TRUE)) %>%
  # Rename i to country_code and v to export_value
  rename(country_code = j, import_value = v, year = t) %>% 
  mutate(import_value = import_value/1000)




merged07 <- left_join(df_output_export07, df_output_import07) %>% 
  left_join(country_codes)


microD_6_digits <- read_csv("/srv/server_data/argendata/baci_comext/6-digit Micro-D.csv") %>% 
  arrow::to_duckdb()

imports_exports_micro_D_berinini <- merged07 %>%
  rename(hs2007 = k) %>% 
  left_join(microD_6_digits) %>% 
  rename(microd = microd_6dig) %>% 
  group_by(year, country_code, country_name, country_iso3, microd) %>% 
  summarise(export_value = sum(export_value), 
            import_value = sum(import_value))  %>% 
  group_by(year, country_code) %>%
  mutate(
    export_value_pc = 100 * export_value / sum(export_value, na.rm = TRUE),
    import_value_pc = 100 * import_value / sum(import_value, na.rm = TRUE)
  ) %>% 
  rename(iso3 = country_iso3) %>% 
  collect()
 


# guardar como csv
write_csv_fundar(imports_exports_micro_D_berinini,
                 file = glue::glue("{tempdir()}/imports_exports_micro_D_berinini.csv"))  

# carga en sheet fuentes clean
#argendataR::agregar_fuente_clean(id_fuente_raw = 113,
#                    path_clean = "imports_exports_micro_D_berinini.csv",
#                    nombre = "Composición de las exportaciones e importaciones de Argentina según grado de diferenciación del producto. En porcentaje del total. Año 2020",
#                    script = code_name)

argendataR::actualizar_fuente_clean(id_fuente_clean = 59)
