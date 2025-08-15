#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)

fecha_actualizar <- "Sin informacion"


url <- "https://cefip.econo.unlp.edu.ar/wp-content/uploads/2024/02/dt_2020_05_01_v01_f01.pdf"


datos_str <- "1913 11.9% 1962 25.1% 2005 26.9%
1914 13.0% 1963 24.2% 2006 27.8%
1915 11.7% 1964 23.4% 2007 30.8%
1916 9.8% 1965 22.1% 2008 31.9%
1917 10.1% 1966 24.3% 2009 36.0%
1918 8.1% 1967 25.1% 2010 34.7%
1919 7.9% 1968 25.0% 2011 36.3%
1920 8.5% 1969 24.5% 2012 38.2%
1921 10.2% 1970 25.1% 2013 38.9%
1922 10.6% 1971 25.2% 2014 40.3%
1923 10.3% 1972 24.5% 2015 42.8%
1924 10.2% 1973 26.6% 2016 42.9%
1925 10.8% 1974 29.4% 2017 42.7%
1926 11.6% 1975 28.5% 2018 40.4%
1927 14.3% 1976 26.3% 2019 39.3%
1928 12.1% 1977 23.6%
1929 13.0% 1978 26.0%
1930 15.1% 1979 25.4%
1931 17.7% 1980 28.3%
1932 19.2% 1981 30.9%
1933 18.4% 1982 28.3%
1934 17.8% 1983 26.2%
1935 17.7% 1984 25.3%
1936 17.2% 1985 27.6%
1937 16.4% 1986 26.0%
1938 18.1% 1987 26.9%
1939 19.5% 1988 25.4%
1940 18.3% 1989 26.0%
1941 17.3% 1990 22.5%
1942 19.8% 1991 22.1%
1943 21.1% 1992 23.4%
1944 23.4% 1993 25.6%
1945 23.3% 1994 26.1%
1946 20.7% 1995 26.3%
1947 23.6% 1996 25.4%
1948 45.4% 1997 24.9%
1949 35.0% 1998 25.7%
1950 32.3% 1999 28.5%
1951 30.2% 2000 28.3%
1952 27.9% 2001 30.6%
1953 28.7% 2002 24.6%
1954 32.9% 2003 25.2%
1955 30.3% 2004 25.4%
1956 28.5%
1957 23.3%
1958 30.3%
1959 24.5%
1960 24.4%
1961 26.0%"

valores <- unlist(strsplit(datos_str, "\\s+"))

# Paso 2: armar un dataframe con dos columnas (anio y valor)
df_raw <- data.frame(
  anio = as.integer(valores[seq(1, length(valores), 2)]),
  gasto_consolidado_pib = as.numeric(sub("%", "", valores[seq(2, length(valores), 2)]))) %>%
  arrange(anio)


nombre <- "Porto, Alberto (2020). Documento de trabajo N° 36. Evolución del sector público argentino en el largo plazo. Tabla A1:  Gasto del sector público consolidado en % del PIB 1913-2019, pág 68"

institucion <- "Centro de Estudios en Finanzas Públicas (CEFIP). Instituto de Investigaciones Económicas. Facultad de Ciencias Económicas UNLP)"

download_filename <- "cefip_unlp_dtnro36_porto.csv"

destfile <- glue::glue("{tempdir()}/{download_filename}")

df_raw %>% write_csv_fundar(., destfile)


# agregar_fuente_raw(url = url,
#                    institucion = institucion,
#                    nombre = nombre,
#                    actualizable = F,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar,
#                    api = F
# )

actualizar_fuente_raw(id_fuente = 428,
                      url = url, 
                      nombre = nombre, 
                      institucion = institucion,
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename,
                      script = code_name)