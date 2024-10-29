# ESTPRO

## Lista de datasets


- [ ] 1_particip_sector_vab
- [ ] 2_vab_por_sector
- [ ] 3_va_sectorial_pais_bys
- [ ] 4_va_sectorial_pais
- [ ] 5_particip_va_intensivos_id_bys
- [ ] 6_particip_sectorial_empleo
- [ ] 7_particip_sectorial_empleoyvab
- [ ] 8_empleo_sectores_ggdc_1950_2018
- [ ] 9_empn_tim_ocde
- [ ] 10_particip_calificados_sector
- [ ] 11_particip_mujer_varon
- [ ] 12_particip_mujer
- [ ] 13_vab_sectorial_granregion
- [ ] 14_vab_sectorial_provincia
- [ ] 15_productividad_internacional
- [ ] 16_pib_por_ocupado
- [ ] 17_va_por_trabajador
- [ ] 18_va_por_trabajador_pais
- [ ] 19_empresas_nacional
- [ ] 20_sector_escala
- [ ] 21_escala_salarios
- [ ] 22_densidad_empresarial_depto
- [ ] 23_tasa_informalidad_tamanio_empresa
- [ ] 24_densidad_nbi
- [ ] 25_empleo_invdes_tim_ocde


## Referencias datasets



- `1_particip_sector_vab`:

  **Scripts:**
-- particip_sector_vab.R

  **Fuentes:**
-- Agregados macroeconómicos (PIB) de INDEC: https://www.indec.gob.ar/ftp/cuadros/economia/sh_VBP_VAB_03_24.xls

- `2_vab_por_sector`:

  **Scripts:**
-- vab_por_sector.do
-- consolidado_vab_por_sector.do

  **Fuentes:**
-- Cuentas Nacionales: https://dossiglos.fundacionnorteysur.org.ar/series/cuentas-nacionales
-- Agregados macroeconómicos (PIB): https://www.indec.gob.ar/indec/web/Nivel4-Tema-3-9-47

- `3_va_sectorial_pais_bys`:

  **Scripts:**
-- va_sectorial_pais_bys.R

  **Fuentes:**
-- Descargas - Distribución de PIB por año y país.: https://unstats.un.org/unsd/amaapi/api/file/22
-- Complilación de Códigos ISO 3166-2 Alpha, realizado por Fundar (Banco Mundial, UNSD, Maddison, INDEC, OEC), 2023: https://docs.google.com/spreadsheets/d/1CtzvmxaMLzj_OyOjn2E3zHJU7yI5nCfe/edit?usp=drive_link&ouid=112515321577541670265&rtpof=true&sd=true

- `4_va_sectorial_pais`:

  **Scripts:**
-- va_sectorial_pais.R

  **Fuentes:**
-- Descargas - Distribución de PIB por año y país.: https://unstats.un.org/unsd/amaapi/api/file/22
-- Complilación de Códigos ISO 3166-2 Alpha, realizado por Fundar (Banco Mundial, UNSD, Maddison, INDEC, OEC), 2023: https://docs.google.com/spreadsheets/d/1CtzvmxaMLzj_OyOjn2E3zHJU7yI5nCfe/edit?usp=drive_link&ouid=112515321577541670265&rtpof=true&sd=true

- `5_particip_va_intensivos_id_bys`:

  **Scripts:**
-- particip_va_intensivos_id_bys.R

  **Fuentes:**
-- Trade in Value Added (TiVA) database 2023: https://stats.oecd.org/Index.aspx?DataSetCode=TIVA_2022_C1
-- Complilación de Códigos ISO 3166-2 Alpha, realizado por Fundar (Banco Mundial, UNSD, Maddison, INDEC, OEC), 2023: https://docs.google.com/spreadsheets/d/1CtzvmxaMLzj_OyOjn2E3zHJU7yI5nCfe/edit?usp=drive_link&ouid=112515321577541670265&rtpof=true&sd=true

- `6_particip_sectorial_empleo`:

  **Scripts:**
-- particip_sectorial_empleo.R

  **Fuentes:**
-- Generación del ingreso e insumo de mano de obra - Remuneración al trabajo asalariado, ingreso mixto e insumo de mano de obra, por sexo y tramos de edad. Años 2016-2022: https://www.indec.gob.ar/ftp/cuadros/economia/series_cgi_sexo_edad.xlsx
-- Fichas Sectoriales CEP XXI: https://datos.gob.ar/dataset/produccion-fichas-sectoriales-cep-xxi

- `7_particip_sectorial_empleoyvab`:

  **Scripts:**
-- particip_sectorial_empleoyvab.R

  **Fuentes:**
-- Fichas Sectoriales CEP XXI: https://datos.gob.ar/dataset/produccion-fichas-sectoriales-cep-xxi
-- Generación del ingreso e insumo de mano de obra - Remuneración al trabajo asalariado, ingreso mixto e insumo de mano de obra, por sexo y tramos de edad. Años 2016-2022: https://www.indec.gob.ar/ftp/cuadros/economia/series_cgi_sexo_edad.xlsx
-- Series por sector de actividad económica: valor bruto de producción y valor agregado bruto. Años 2004-2023, por trimestre: https://www.indec.gob.ar/ftp/cuadros/economia/sh_VBP_VAB_03_24.xls

- `8_empleo_sectores_ggdc_1950_2018`:

  **Scripts:**
-- empleo_sectores_ggdc_1950_2018.do

  **Fuentes:**
-- Complilación de Códigos ISO 3166-2 Alpha, realizado por Fundar (Banco Mundial, UNSD, Maddison, INDEC, OEC), 2023: https://docs.google.com/spreadsheets/d/1CtzvmxaMLzj_OyOjn2E3zHJU7yI5nCfe/edit?usp=drive_link&ouid=112515321577541670265&rtpof=true&sd=true
-- Economic Transformation Database: https://dataverse.nl/api/access/datafile/356908

- `9_empn_tim_ocde`:

  **Scripts:**
-- empn_tim_ocde.do

  **Fuentes:**
-- Complilación de Códigos ISO 3166-2 Alpha, realizado por Fundar (Banco Mundial, UNSD, Maddison, INDEC, OEC), 2023: https://docs.google.com/spreadsheets/d/1CtzvmxaMLzj_OyOjn2E3zHJU7yI5nCfe/edit?usp=drive_link&ouid=112515321577541670265&rtpof=true&sd=true
-- Trade in Employment database: https://stats.oecd.org/Index.aspx?DataSetCode=TIM_2021

- `10_particip_calificados_sector`:

  **Scripts:**
-- particip_calificados_sector.R

  **Fuentes:**
-- Caja de Herramientas para el procesamiento de la Encuesta Permanente de Hogares: https://cran.r-project.org/web/packages/eph/eph.pdf
-- Encuesta Permanente de Hogares: https://www.indec.gob.ar/indec/web/Institucional-Indec-BasesDeDatos

- `11_particip_mujer_varon`:

  **Scripts:**
-- particip_mujer_varon.R

  **Fuentes:**
-- Generación del ingreso e insumo de mano de obra - Remuneración al trabajo asalariado, ingreso mixto e insumo de mano de obra, por sexo y tramos de edad. Años 2016-2022: https://www.indec.gob.ar/ftp/cuadros/economia/series_cgi_sexo_edad.xlsx

- `12_particip_mujer`:

  **Scripts:**
-- particip_mujer.R

  **Fuentes:**
-- Boletín de Estadìsticas segun sexo: https://www.argentina.gob.ar/sites/default/files/boletin_estadisticas_laborales_segun_sexo_al_ii_trim_2023.xlsx

- `13_vab_sectorial_granregion`:

  **Scripts:**
-- vab_sectorial_granregion.R

  **Fuentes:**
-- Desagregación provincial del valor agregado bruto de la Argentina, base 2004: https://repositorio.cepal.org/server/api/core/bitstreams/7399c6c9-0827-42da-b433-d176cb4107c7/content

- `14_vab_sectorial_provincia`:

  **Scripts:**
-- vab_sectorial_provincia.R

  **Fuentes:**
-- Desagregación provincial del valor agregado bruto de la Argentina, base 2004: https://repositorio.cepal.org/server/api/core/bitstreams/7399c6c9-0827-42da-b433-d176cb4107c7/content

- `15_productividad_internacional`:

  **Scripts:**
-- productividad_internacional.do

  **Fuentes:**
-- Penn World Table version 10.01: https://dataverse.nl/api/access/datafile/354098

- `16_pib_por_ocupado`:

  **Scripts:**
-- pib_por_ocupado.do

  **Fuentes:**
-- Penn World Table version 10.01: https://www.rug.nl/ggdc/productivity/pwt/?lang=en

- `17_va_por_trabajador`:

  **Scripts:**
-- va_por_trabajador.R

  **Fuentes:**
-- Valor agregado bruto e insumo de mano de obra por sector de actividad económica. Primer trimestre de 2016 a cuarto trimestre de 2023: https://www.indec.gob.ar/ftp/cuadros/economia/serie_cgi_04_24.xls

- `18_va_por_trabajador_pais`:

  **Scripts:**
-- va_por_trabajador_pais.R

  **Fuentes:**
-- Trade in employment (TiM) database: https://stats.oecd.org/Index.aspx?DataSetCode=TIM_2021
-- Complilación de Códigos ISO 3166-2 Alpha, realizado por Fundar (Banco Mundial, UNSD, Maddison, INDEC, OEC), 2023: https://docs.google.com/spreadsheets/d/1CtzvmxaMLzj_OyOjn2E3zHJU7yI5nCfe/edit?usp=drive_link&ouid=112515321577541670265&rtpof=true&sd=true

- `19_empresas_nacional`:

  **Scripts:**
-- empresas_nacional.R

  **Fuentes:**
-- Caracterización y evolución de la cantidad de empresas (Serie anual) - actualizado 20230222: https://www.argentina.gob.ar/sites/default/files/nacional_serie_empresas_actualizado20230222.xlsx

- `20_sector_escala`:

  **Scripts:**
-- sector_escala.R

  **Fuentes:**
-- Caracterización y evolución de la cantidad de empresas (Serie anual) - actualizado 20230222: https://www.argentina.gob.ar/sites/default/files/nacional_serie_empresas_actualizado20230222.xlsx
-- Caracterización y evolución del empleo registrado (Serie anual) - actualizado 20230222: https://www.argentina.gob.ar/sites/default/files/nacional_serie_empleo_anual_actualizado20230222.xlsx

- `21_escala_salarios`:

  **Scripts:**
-- escala_salarios.R

  **Fuentes:**
-- Caracterización y evolución de las remuneraciones de los trabajadores registrados (Serie anual) - actualizado 20230222: https://www.argentina.gob.ar/sites/default/files/nacional_serie_remuneraciones_anual_actualizado20230222.xlsx
-- Caracterización y evolución de la cantidad de empresas (Serie anual) - actualizado 20230222: https://www.argentina.gob.ar/sites/default/files/nacional_serie_empresas_actualizado20230222.xlsx
-- Caracterización y evolución del empleo registrado (Serie anual) - actualizado 20230222: https://www.argentina.gob.ar/sites/default/files/nacional_serie_empleo_anual_actualizado20230222.xlsx

- `22_densidad_empresarial_depto`:

  **Scripts:**
-- densidad_empresarial_depto.R

  **Fuentes:**
-- Mapa Productivo Laboral Argentino: http://datos.produccion.gob.ar/dataset/distribucion-geografica-de-los-establecimientos-productivos
-- Población estimada por departamento: https://www.indec.gob.ar/indec/web/Nivel4-Tema-2-24-119

- `23_tasa_informalidad_tamanio_empresa`:

  **Scripts:**
-- tasa_informalidad_tamanio_empresa.do

  **Fuentes:**
-- Encuesta Permanente de Hogares: https://www.indec.gob.ar/indec/web/Institucional-Indec-BasesDeDatos

- `24_densidad_nbi`:

  **Scripts:**
-- densidad_nbi.R
-- share_pb_nbi.do

  **Fuentes:**
-- Mapa Productivo Laboral Argentino: http://datos.produccion.gob.ar/dataset/distribucion-geografica-de-los-establecimientos-productivos
-- Población estimada por departamento: https://www.indec.gob.ar/indec/web/Nivel4-Tema-2-24-119
-- Censo Nacional de Población y Viviendas (2010): http://datar.info/dataset/censo-2010-microdatos
-- http://datar.info/dataset/censo-2010-microdatos: Censo Nacional de Población, Hogares y Viviendas (2010)

- `25_empleo_invdes_tim_ocde`:

  **Scripts:**
-- empn_tim_ocde.do

  **Fuentes:**
-- Complilación de Códigos ISO 3166-2 Alpha, realizado por Fundar (Banco Mundial, UNSD, Maddison, INDEC, OEC), 2023: https://docs.google.com/spreadsheets/d/1CtzvmxaMLzj_OyOjn2E3zHJU7yI5nCfe/edit?usp=drive_link&ouid=112515321577541670265&rtpof=true&sd=true
-- Trade in Employment database: https://stats.oecd.org/Index.aspx?DataSetCode=TIM_2021
