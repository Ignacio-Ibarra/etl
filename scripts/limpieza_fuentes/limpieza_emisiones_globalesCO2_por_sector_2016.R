
## descargo fuente raw
descargar_fuente_raw(id_fuente = 125, tempdir())

# me quedo con el sheet 1 que es donde está la data 
emis_global_co2_sector_2016 <- readxl::read_xlsx(get_temp_path("R125C0"),
                               sheet = 1)

# limpio nombre variables y cambio nombre sub_sector
emis_global_co2_sector_2016<-clean_names(emis_global_co2_sector_2016) %>%
  rename(sub_sector_ingles=sub_sector)

# la base raw tiene solo sub_sector en ingles y la base que sibió analista, sector, subsector y subsubsector (castellano)
# genero las variables con los valores de ingles a castellano

emis_global_co2_sector_2016 <- emis_global_co2_sector_2016 %>%
  mutate(
    sub_sector_castellano = case_when(
      sub_sector_ingles == "Road" ~ "Carretera",
      sub_sector_ingles == "Aviation" ~ "Aviación",
      sub_sector_ingles == "Rail" ~ "Tren",
      sub_sector_ingles == "Pipeline" ~ "Tuberías",
      sub_sector_ingles == "Ship" ~ "Marítimo",
      sub_sector_ingles == "Rail" ~ "Tren",
      sub_sector_ingles == "Ship" ~ "Marítimo",
      sub_sector_ingles =="Residential" ~ "Residencial",
      sub_sector_ingles =="Commercial" ~ "Comercial",
      sub_sector_ingles =="Iron & Steel" ~ "Hierro y acero",
      sub_sector_ingles =="Non-ferous metals" ~ "Metales no ferrosos",
      sub_sector_ingles =="Machinery" ~ "Maquinaria",
      sub_sector_ingles =="Food and tobacco" ~ "Alimentación y tabaco",
      sub_sector_ingles =="Paper, pulp & printing" ~ "Papel",
      sub_sector_ingles =="Chemical & petrochemical (energy)" ~ "Química y petroquímica",
      sub_sector_ingles =="Other industry" ~ "Otras industrias",
      sub_sector_ingles =="Energy in Agri & Fishing" ~ "Agricultura, ganadería y pesca",
      sub_sector_ingles =="Unallocated fuel combustion" ~ "Industria de la energía",
      sub_sector_ingles =="Coal" ~ "Carbón",
      sub_sector_ingles =="Oil & Natural Gas" ~ "Petróleo y gas",
      sub_sector_ingles =="Cement" ~ "Cemento",
      sub_sector_ingles =="Chemical & petrochemical (industrial)" ~ "Industria química y petroquímica",
      sub_sector_ingles =="Livestock & Manure" ~ "Ganadería y manejo de estiércol",
      sub_sector_ingles =="Rice Cultivation" ~ "Cultivo de arroz",
      sub_sector_ingles =="Agricultural Soils" ~ "Suelos agrícolas",
      sub_sector_ingles =="Crop Burning" ~ "Quema de biomasa",
      sub_sector_ingles =="Forest Land" ~ "Tierras forestales",
      sub_sector_ingles =="Cropland" ~ "Tierras de cultivo",
      sub_sector_ingles =="Grassland" ~ "Pastizales",
      sub_sector_ingles =="Landfills" ~ "Residuos sólidos",
      sub_sector_ingles =="Wastewater" ~ "Aguas residuales",
      TRUE ~ NA_character_),
    sector = case_when(
        sub_sector_ingles %in% c("Road",
                               "Aviation",
                               "Rail",
                               "Pipeline",
                               "Ship",
                               "Residential",
                               "Commercial",
                               "Iron & Steel",
                               "Non-ferous metals",
                               "Machinery",
                               "Food and tobacco",
                               "Paper, pulp & printing",
                               "Chemical & petrochemical (energy)",
                               "Other industry",
                               "Energy in Agri & Fishing",
                               "Unallocated fuel combustion",
                               "Coal",
                               "Oil & Natural Gas") ~ "Energía",
      sub_sector_ingles %in% c("Cement", 
                               "Chemical & petrochemical (industrial)") ~ "Industria Química",
      sub_sector_ingles %in% c("Livestock & Manure",
                                     "Rice Cultivation",
                                     "Agricultural Soils",
                                     "Crop Burning",
                                     "Forest Land",
                                     "Cropland",
                                     "Grassland") ~ "AGSyOUT",
      sub_sector_ingles %in% c("Landfills",
                                     "Wastewater") ~ "Residuos",
      TRUE ~ NA_character_
    ),
    sub_subsector = case_when(
      sub_sector_ingles %in% c("Road", "Aviation","Rail", "Pipeline", "Ship") ~ "Transporte",
      sub_sector_ingles %in% c("Residential", "Commercial") ~ "Edificaciones",
      sub_sector_ingles %in% c("Iron & Steel",
                               "Non-ferous metals",
                               "Machinery",
                               "Food and tobacco",
                               "Paper, pulp & printing",
                               "Chemical & petrochemical (energy)",
                               "Other industry") ~ "Edificaciones",
      sub_sector_ingles %in% c("Iron & Steel",
                               "Non-ferous metals",
                               "Machinery",
                               "Food and tobacco",
                               "Paper, pulp & printing",
                               "Chemical & petrochemical (energy)",
                               "Other industry") ~ "Industrias manufactureras y de la construcción",
      sub_sector_ingles %in% c("Energy in Agri & Fishing") ~ "Agricultura, ganadería y pesca",
    sub_sector_ingles %in% c("Unallocated fuel combustion") ~ "Industria de la energía",
    sub_sector_ingles %in% c("Coal","Oil & Natural Gas") ~ "Industria de la energía",
    sub_sector_ingles %in% c("Cement") ~ "Industria de los minerales",
    sub_sector_ingles %in% c("Chemical & petrochemical (industrial)") ~ "Industria química y petroquímica",
    sub_sector_ingles %in% c("Livestock & Manure") ~ "Ganadería y manejo de estiércol",
    sub_sector_ingles %in% c("Rice Cultivation") ~ "Cultivo de arroz",
    sub_sector_ingles %in% c("Agricultural Soils","Forest Land","Cropland","Grassland") ~ "Tierras",
    sub_sector_ingles %in% c("Crop Burning") ~ "Quema de biomasa",
    sub_sector_ingles %in% c("Landfills") ~ "Residuos sólidos",
    sub_sector_ingles %in% c("Wastewater") ~ "Aguas residuales",
    TRUE ~ NA_character_
  ))

# me quedo con las columnas año y co2 y multiplico año *-1
emis_global_co2_sector_2016_final <- emis_global_co2_sector_2016 %>% 
  select(4,5,3,2) %>%
  rename(subsector=sub_subsector,
         subsubsector=sub_sector_castellano,
         valor_en_porcent=share_of_global_greenhouse_gas_emissions_percent)

# guardo csv

write_csv_fundar(x = emis_global_co2_sector_2016_final,
                 file = glue::glue("{tempdir()}/emis_global_co2_sector_2016.csv"))

# agrego fuente clean
agregar_fuente_clean(id_fuente_raw = 125, 
                     dir = tempdir(),
                     path_clean = "emis_global_co2_sector_2016.csv",
                     nombre = "Emisiones globales CO2 por sector año 2016",
                     script = "limpieza_emisiones_globalesCO2_por_sector_2016.R")

# actualizo fuente clean
actualizar_fuente_clean(id_fuente_clean = 51)
