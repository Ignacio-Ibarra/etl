# Genero archivos parquet en /srv para consultar en scripts para outputs
# Load necessary package
library(arrow)


code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)

fuente_baci <- "BACI"
url_fuente <- "http://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=37"
institution <- "Centre d'Études Prospectives et d'Informations Internationales (CEPII)"



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



## Importar y transformar ----


## CODES (country and product)

codes <- grep('codes', all_csv_files, value = TRUE)

#codes present in all_csv_files
#[1] "/tmp/Rtmp3qibtu/country_codes_V202401b.csv"     
#[2] "/tmp/Rtmp3qibtu/product_codes_HS07_V202401b.csv"
#[3] "/tmp/Rtmp3qibtu/product_codes_HS96_V202401b.csv"


###### HS07  ----

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

### Classification Codes - The Micro-D Classification: A New Approach to Identifying Differentiated Exports ----
link_hallak <- "0B19niEgxbWCrUTM3bHNodU9Lenc"

drive_download(as_id(link_hallak), path = glue::glue("{tempdir()}/Micro-D-Codes.zip"), overwrite = TRUE)

unzip(glue::glue("{tempdir()}/Micro-D-Codes.zip"), exdir = directory_path) 

