descargar_fuente("R136C0")

archivo <- "ipc_base2021_ppales_aperturas_2022_adelante_dgec_ba.csv"

df <- readxl::read_excel(get_temp_path("R136C0"), skip = 1)

df <- df %>% 
  filter(if_any(-Apertura, function(x) !is.na(x)))

fechas <- df[1,] %>% 
  as.numeric() %>% 
  as.Date(., origin = "1899-12-30")

fechas <- fechas[!is.na(fechas)]

colnames(df) <- c("apertura", as.character(fechas))

df <- df %>% 
  filter(!is.na(apertura))

df <- df %>% 
  pivot_longer(-apertura, names_to = "fecha", values_to = "indice")

df <- df %>% 
  mutate(fecha = as.Date(fecha))


df <- df %>%
  mutate(codigo = case_when(
    apertura == "Nivel General" ~ "0",
    apertura == "Alimentos y bebidas no alcohólicas" ~ "01",
    apertura == "Alimentos" ~ "01.1",
    apertura == "Pan y cereales" ~ "01.1.1",
    apertura == "Carnes y derivados" ~ "01.1.2",
    apertura == "Pescados y mariscos" ~ "01.1.3",
    apertura == "Leche, productos lácteos y huevos" ~ "01.1.4",
    apertura == "Aceites, mantecas y otras grasas" ~ "01.1.5",
    apertura == "Frutas" ~ "01.1.6",
    apertura == "Verduras, tubérculos y legumbres" ~ "01.1.7",
    apertura == "Azúcar, repostería y postres" ~ "01.1.8",
    apertura == "Otros alimentos" ~ "01.1.9",
    apertura == "Bebidas no alcohólicas" ~ "01.2",
    apertura == "Bebidas alcohólicas y tabaco" ~ "02",
    apertura == "Bebidas alcohólicas" ~ "02.1",
    apertura == "Vinos" ~ "02.1.1",
    apertura == "Cerveza" ~ "02.1.2",
    apertura == "Tabaco" ~ "02.2",
    apertura == "Prendas de vestir y calzado" ~ "03",
    apertura == "Prendas de vestir" ~ "03.1",
    apertura == "Calzado" ~ "03.2",
    apertura == "Vivienda, agua, electricidad, gas y otros combustibles" ~ "04",
    apertura == "Alquiler de la vivienda" ~ "04.1",
    apertura == "Mantenimiento y reparación de la vivienda" ~ "04.2",
    apertura == "Suministro de agua y otros servicios relacionados con la vivienda" ~ "04.3",
    apertura == "Suministro de agua" ~ "04.3.1",
    apertura == "Gastos comunes por la vivienda" ~ "04.3.2",
    apertura == "Electricidad, gas y otros combustibles" ~ "04.4",
    apertura == "Electricidad" ~ "04.4.1",
    apertura == "Gas" ~ "04.4.2",
    apertura == "Equipamiento y mantenimiento del hogar" ~ "05",
    apertura == "Muebles, accesorios y alfombras" ~ "05.1",
    apertura == "Artículos textiles para el hogar" ~ "05.2",
    apertura == "Electrodomésticos" ~ "05.3",
    apertura == "Cristalería, vajilla y utensilios para el hogar" ~ "05.4",
    apertura == "Herramientas y accesorios para la casa y el jardín" ~ "05.5",
    apertura == "Bienes y servicios para la conservación del hogar" ~ "05.6",
    apertura == "Salud" ~ "06",
    apertura == "Medicamentos y productos medicinales" ~ "06.1",
    apertura == "Medicamentos" ~ "06.1.1",
    apertura == "Productos medicinales" ~ "06.1.2",
    apertura == "Artefactos y equipos terapéuticos" ~ "06.1.3",
    apertura == "Servicios para pacientes externos" ~ "06.1.4",
    apertura == "Seguros médicos" ~ "06.2",
    apertura == "Transporte" ~ "07",
    apertura == "Adquisición de vehículos" ~ "07.1",
    apertura == "Funcionamiento de equipos de transporte personal" ~ "07.2",
    apertura == "Servicios de transporte de pasajeros" ~ "07.3",
    apertura == "Transporte de pasajeros por ferrocarril" ~ "07.3.1",
    apertura == "Transporte de pasajeros por carretera" ~ "07.3.2",
    apertura == "Transporte de pasajeros por aire" ~ "07.3.3",
    apertura == "Información y comunicación" ~ "08",
    apertura == "Recreación y cultura" ~ "09",
    apertura == "Productos de recreación" ~ "09.1",
    apertura == "Servicios de recreación" ~ "09.2",
    apertura == "Servicios culturales" ~ "09.3",
    apertura == "Diarios, libros y artículos de papelería y dibujo" ~ "09.4",
    apertura == "Paquetes turísticos" ~ "09.5",
    apertura == "Educación" ~ "10",
    apertura == "Restaurantes y hoteles" ~ "11",
    apertura == "Servicios de alimentos y bebidas preparadas fuera del hogar" ~ "11.1",
    apertura == "Servicios de alojamiento" ~ "11.2",
    apertura == "Seguros y servicios financieros" ~ "12",
    apertura == "Seguros" ~ "12.1",
    apertura == "Servicios financieros" ~ "12.2",
    apertura == "Cuidado personal, protección social y otros productos" ~ "13",
    apertura == "Cuidado personal" ~ "13.1",
    apertura == "Efectos personales" ~ "13.2",
    apertura == "Otros servicios" ~ "13.3",
    TRUE ~ apertura  # Deja el valor original si no hay coincidencia
  ))

# Generar variable de nivel de desagregación según cantidad de caracteres de la variable codigo
df <- df %>%
  mutate(nivel = case_when(
    nchar(codigo) == 1 ~ 0,
    nchar(codigo) == 2 ~ 1,
    nchar(codigo) == 4 ~ 2,
    nchar(codigo) == 6 ~ 3,
    TRUE ~ NA_real_  # Deja como NA si no hay coincidencia
  ))



df %>% 
  write_csv_fundar(glue::glue("{tempdir()}/{archivo}"))

# agregar_fuente_clean(
#   id_fuente_raw = 136,
#   path_clean = archivo,
  # nombre =  glue::glue(
  #   "IPCBA (base 2021 = 100) según principales aperturas. Índice mensual. Ciudad de Buenos Aires. Febrero de 2022 / {Sys.Date()-months(1)}"
  # ),
#   script = "scripts/limpieza_fuentes/limpieza_ipcba_ppales_aperturas_2022_adelante_dgec_ba.R"
# )

actualizar_fuente_clean(64,
                        nombre =  glue::glue(
                          "IPCBA (base 2021 = 100) según principales aperturas. Índice mensual. Ciudad de Buenos Aires. Febrero de 2022 / {Sys.Date()-months(1)}"))
