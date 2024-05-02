# MERTRA

## Lista de datasets


- [ ] 1_tasa_participacion_censos
- [ ] 2_ratio_tasa_actividad_mujer_varon_por_pais_anio
- [ ] 3_tasa_actividad_por_pais_anio
- [ ] 4_media_movil_por_edad_de_tasa_actividad
- [ ] 5_media_movil_por_edad_sexo_de_tasa_actividad_brecha
- [ ] 6_media_movil_por_edad_reg_desc_fundar_de_tasa_actividad
- [ ] 7_puestos_percapita_anio_pais
- [ ] 8_tasa_empleo_anio_provincia
- [ ] 9_tasa_empleo_por_franja_etaria_anio_provincia
- [ ] 10_tasa_empleo_anio_provincia_sexo
- [ ] 11_tasa_empleo_menores_provincia
- [ ] 12_establecimientos_tasa_empleo_provincia_2021_22
- [ ] 13_tasa_empleo_mujer_salario_provincia_anio
- [ ] 14_lavarropas_tasa_empleo_fem_provincia
- [ ] 15_share_acuerdo_varones_empleo_region_arg
- [ ] 16_tasa_empleo_provincia_nivel_educativo
- [ ] 17_tiempo_social_trabajo_sexo
- [ ] 18_tiempo_social_trabajo_sexo_edad
- [ ] 19_tiempo_social_trabajo_sexo_niveleducativo
- [ ] 20_tipo_trabajo_no_rem_sexo
- [ ] 21_trabajo_no_remunerado_sexo_internacional


## Referencias datasets



- `1_tasa_participacion_censos`:

  **Scripts:**
-- censos ocupados
-- tasa_participacion_censos.do
  **Fuentes:**
-- Censo Nacional de Población 1960: https://biblioteca.indec.gob.ar/bases/minde/1c1960_1.pdf
-- La población de Argentina. Serie de Investigaciones Demográficas (1974). Compilado por Zulma Recchini de Latttes y Alfredo E. Lattes: https://biblioteca.indec.gob.ar/bases/minde/4si8_1.pdf
-- Minnesota Population Center. Integrated Public Use Microdata Series, International: Version 7.3 (2020): https://international.ipums.org/international/
-- Poblacion. Censos. Censo 2022. Total del país.: https://www.indec.gob.ar/indec/web/Nivel4-Tema-2-41-170
-- Minnesota Population Center. Integrated Public Use Microdata Series, International: Version 7.3 (2020). Censo 1970, 1980, 1991, 2001, 2010: https://international.ipums.org/international/

- `2_ratio_tasa_actividad_mujer_varon_por_pais_anio`:

  **Scripts:**
-- catalogo.ipynb
  **Fuentes:**
-- Base de Datos de Indicadores Mundiales de Desarrollo: https://data.worldbank.org/indicator/SL.TLF.CACT.FM.NE.ZS

- `3_tasa_actividad_por_pais_anio`:

  **Scripts:**
-- catalogo.ipynb
  **Fuentes:**
-- Base de Datos de Indicadores Mundiales de Desarrollo: https://data.worldbank.org/indicator/SL.TLF.TOTL.IN
-- Base de Datos de Indicadores Mundiales de Desarrollo: https://data.worldbank.org/indicator/SP.POP.TOTL
-- Complilación de Códigos ISO 3166-2 Alpha, realizado por Fundar (Banco Mundial, UNSD, Maddison, INDEC, OEC), 2023: https://docs.google.com/spreadsheets/d/1AQvdxZmE3JJsWm8dZ8USRu4zC7umy59S/edit?usp=drive_link&ouid=104529103904611843855&rtpof=true&sd=true

- `4_media_movil_por_edad_de_tasa_actividad`:

  **Scripts:**
-- catalogo.ipynb
-- codigo_1_stata.do
-- ephtu_appendeada.R
  **Fuentes:**
-- Encuesta Permanente de Hogares Total Urbano: https://www.indec.gob.ar/indec/web/Institucional-Indec-BasesDeDatos

- `5_media_movil_por_edad_sexo_de_tasa_actividad_brecha`:

  **Scripts:**
-- ephtu_appendeada.R
-- catalogo.ipynb
-- codigo_1_stata.do
  **Fuentes:**
-- Encuesta Permanente de Hogares Total Urbano: https://www.indec.gob.ar/indec/web/Institucional-Indec-BasesDeDatos

- `6_media_movil_por_edad_reg_desc_fundar_de_tasa_actividad`:

  **Scripts:**
-- catalogo.ipynb
-- codigo_1_stata.do
  **Fuentes:**
-- Encuesta Permanente de Hogares Total Urbano: https://www.indec.gob.ar/indec/web/Institucional-Indec-BasesDeDatos

- `7_puestos_percapita_anio_pais`:

  **Scripts:**
-- catalogo.ipynb
  **Fuentes:**
-- Penn World Tables: https://www.rug.nl/ggdc/productivity/pwt/?lang=en

- `8_tasa_empleo_anio_provincia`:

  **Scripts:**
-- catalogo.ipynb
-- ephtu_appendeada.R
  **Fuentes:**
-- Encuesta Permanente de Hogares Total Urbano: https://www.indec.gob.ar/indec/web/Institucional-Indec-BasesDeDatos

- `9_tasa_empleo_por_franja_etaria_anio_provincia`:

  **Scripts:**
-- catalogo.ipynb
-- ephtu_appendeada.R
-- codigo_1_stata.do
  **Fuentes:**
-- Encuesta Permanente de Hogares Total Urbano: https://www.indec.gob.ar/indec/web/Institucional-Indec-BasesDeDatos

- `10_tasa_empleo_anio_provincia_sexo`:

  **Scripts:**
-- catalogo.ipynb
-- ephtu_appendeada.R
-- codigo_1_stata.do
  **Fuentes:**
-- Encuesta Permanente de Hogares Total Urbano: https://www.indec.gob.ar/indec/web/Institucional-Indec-BasesDeDatos

- `11_tasa_empleo_menores_provincia`:

  **Scripts:**
-- ephtu_appendeada.R
-- script_tasa_empleo_menores_provincia.do
  **Fuentes:**
-- Encuesta Permanente de Hogares Total Urbano: https://www.indec.gob.ar/indec/web/Institucional-Indec-BasesDeDatos

- `12_establecimientos_tasa_empleo_provincia_2021_22`:

  **Scripts:**
-- script_establecimientos_tasa_empleo_provincia_2021_22
  **Fuentes:**
-- Censo Nacional de Población, Hogares y Viviendas 2022: https://censo.gob.ar/index.php/datos_provisionales/
-- Encuesta Permanente de Hogares Total Urbano: https://www.indec.gob.ar/indec/web/Institucional-Indec-BasesDeDatos
-- Mapa Productivo Laboral Argentino: https://datos.gob.ar/dataset/produccion-distribucion-geografica-establecimientos-productivos

- `13_tasa_empleo_mujer_salario_provincia_anio`:

  **Scripts:**
-- tasa_empleo_mujer_salario_provincia_anio.do
-- ephtu_appendeada.R
  **Fuentes:**
-- Encuesta Permanente de Hogares Total Urbano: https://www.indec.gob.ar/indec/web/Institucional-Indec-BasesDeDatos

- `14_lavarropas_tasa_empleo_fem_provincia`:

  **Scripts:**
-- ephtu_appendeada.R
-- script_lavarropas_tasa_empleo_fem_provincia.do
  **Fuentes:**
-- Encuesta Nacional de Gastos de los Hogares 2017-2018. Base de Equipamiento de los hogares: https://www.indec.gob.ar/indec/web/Institucional-Indec-BasesDeDatos
-- Encuesta Permanente de Hogares Total Urbano: https://www.indec.gob.ar/indec/web/Institucional-Indec-BasesDeDatos

- `15_share_acuerdo_varones_empleo_region_arg`:

  **Scripts:**
-- script_share_acuerdo_varones_empleo_region_arg.do
  **Fuentes:**
-- World Values Survey Wave 7 (2017-2022): https://www.worldvaluessurvey.org/WVSDocumentationWV7.jsp

- `16_tasa_empleo_provincia_nivel_educativo`:

  **Scripts:**
-- ephtu_appendeada.R
-- dofile_grafico_14_tasa_empleo_nivel_educativo.do
  **Fuentes:**
-- Encuesta Permanente de Hogares Total Urbano: https://www.indec.gob.ar/indec/web/Institucional-Indec-BasesDeDatos

- `17_tiempo_social_trabajo_sexo`:

  **Scripts:**
-- tiempo_social_trabajo_sexo.do
  **Fuentes:**
-- Encuesta Nacional de Uso del Tiempo 2021. Resultados Definitivos.: https://www.indec.gob.ar/indec/web/Nivel4-Tema-4-31-117

- `18_tiempo_social_trabajo_sexo_edad`:

  **Scripts:**
-- tiempo_social_trabajo_sexo_edad.do
  **Fuentes:**
-- Encuesta Nacional de Uso del Tiempo 2021. Resultados Definitivos.: https://www.indec.gob.ar/indec/web/Nivel4-Tema-4-31-117

- `19_tiempo_social_trabajo_sexo_niveleducativo`:

  **Scripts:**
-- tiempo_social_trabajo_sexo_niveleducativo.do
  **Fuentes:**
-- Encuesta Nacional de Uso del Tiempo 2021. Resultados Definitivos.: https://www.indec.gob.ar/indec/web/Nivel4-Tema-4-31-117

- `20_tipo_trabajo_no_rem_sexo`:

  **Scripts:**
-- tipo_trabajo_no_rem_sexo.do
  **Fuentes:**
-- Encuesta Nacional de Uso del Tiempo 2021. Resultados Definitivos.: https://www.indec.gob.ar/indec/web/Nivel4-Tema-4-31-117

- `21_trabajo_no_remunerado_sexo_internacional`:

  **Scripts:**
-- trabajo_no_remunerado_sexo_internacional.do
  **Fuentes:**
-- Variety and change of patterns in the gender balance between unpaid care-work, paid work and free time across the world and over time: A measure of wellbeing?. Wellbeing, Space and Society. Volume 3. 2022: https://doi.org/10.1016/j.wss.2022.100081
