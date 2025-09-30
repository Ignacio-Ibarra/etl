
# emisiones_sector_global_1850-2014 co2  -----------

# traigo url del gráfico en owid al que corresponde el dataset 
emisiones_glob_sector_1850_2014 <- readr::read_csv("https://datapub.gfz-potsdam.de/download/10.5880.PIK.2016.003/PRIMAP-hist_v1.0_14-Apr-2016.csv")

#ruta_archivo_temporal <- glue::glue("{tempdir()}/emisiones_glob_sect_1850_2014.csv")
#datos_desde_temporal <- read.csv(ruta_archivo_temporal)

#emisiones_glob_sect_1850_2014 <- readr::read_csv("https://datapub.gfz-potsdam.de/download/10.5880.PIK.2016.003/PRIMAP-hist_v1.0_14-Apr-2016.csv")
write.csv(emisiones_glob_sector_1850_2014, glue::glue("{tempdir()}/emisiones_glob_sector_1850_2014.csv"))

# agrego la fuente
# agregar_fuente_raw(url = "https://datapub.gfz-potsdam.de/download/10.5880.PIK.2016.003/PRIMAP-hist_v1.0_14-Apr-2016.csv", 
#                    institucion = "Postdam Institute for Climate Research", 
#                    actualizable = T,
#                    fecha_descarga = Sys.Date(),
#                    path_raw = "emisiones_glob_sector_1850_2014.csv", 
#                    dir = tempdir(),
#                    script = "descarga_emisiones_global_sec_1850_2014.R",
#                    nombre = "Emisiones globales co2 por sector año 1850 a 2014 (FUENTE)"
# )

actualizar_fuente_raw(id_fuente=132 , 
                      fecha_actualizar = "Sin informacion")

#list.files(tempdir())

#fuentes() %>% 
#  view()

url <- "https://zenodo.org/records/15016289/files/Guetschow_et_al_2025-PRIMAP-hist_v2.6.1_final_13-Mar-2025.csv?download=1"

df1 <- read_csv(url)
df2 <- read_csv(get_raw_path("R132C0"))

comparar_df(df1, df2)

library(dplyr)
library(tidyr)
library(stringr)

# Path to the new PRIMAP-hist CSV (update this to your local path or use your existing download function)
new_csv_path <- "Guetschow_et_al_2025-PRIMAP-hist_v2.6.1_final_13-Mar-2025.csv"

# Read the new dataset
emis_new <- readr::read_csv(new_csv_path)

# Create a simplified global-sector dataset for comparison
emis_global_sector_1750_2023 <- df1 %>%
  # keep global totals; the area column is the ISO3 code:contentReference[oaicite:2]{index=2}  
  filter(area == "EARTH") %>%
  # choose categories at the top level (1=Energy, 2=IPPU, 3=Agriculture, 4=Waste, 5=Other):contentReference[oaicite:3]{index=3}
  # we extract the first part before any dot to group subcategories
  mutate(top_cat = str_extract(category, "^[^.]+")) %>%
  filter(top_cat %in% c("1", "2", "3", "4", "5")) %>%
  # select an entity that gives CO2-equivalent values (e.g., Kyoto GHG with AR4 GWP)
  filter(entity == "KYOTOGHG (AR4GWP100)") %>%
  # keep unit, top-level category and year columns (years are four-digit column names)
  select(unit, category = top_cat, matches("^\\d{4}$")) %>%
  # pivot year columns to long format
  pivot_longer(
    cols = matches("^\\d{4}$"),
    names_to = "anio",
    values_to = "valor"
  ) %>%
  # map top-level category codes to sector names analogous to the original mapping
  mutate(sector = case_when(
    category == "1" ~ "Energía",
    category == "2" ~ "PIUP",
    category == "3" ~ "AGSyOUT",   # Agriculture (land-use is not included in the main file:contentReference[oaicite:4]{index=4})
    category == "4" ~ "Residuos",
    category == "5" ~ "Otros",
    TRUE ~ NA_character_
  )) %>%
  # aggregate by year and sector
  group_by(anio, sector) %>%
  summarise(
    valor_en_ggco2e = sum(valor, na.rm = TRUE),
    .groups = "drop"
  )

# 'emis_global_sector_1750_2023' now has columns: anio (year), sector, and valor_en_ggco2e.
