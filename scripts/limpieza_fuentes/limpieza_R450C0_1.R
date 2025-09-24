rm(list = ls())

df <- readr::read_csv(get_raw_path("R450C0"))



# # agrego fuente clean
# agregar_fuente_clean(df = df, path_clean = "anomalia_temperatura_anual_owid_met.parquet",
#                      id_fuente_raw = 450,
#                      descripcion =  "Diferencia en la temperatura media de la superficie terrestre y del mar en comparación con la media del período 1861-1890, en grados Celsius. // Cita: Morice, C.P., J.J. Kennedy, N.A. Rayner, J.P. Winn, E. Hogan, R.E. Killick, R.J.H. Dunn, T.J. Osborn, P.D. Jones and I.R. Simpson (in press) An updated assessment of near-surface temperature change from 1850: the HadCRUT5 dataset. Journal of Geophysical Research (Atmospheres) [doi:10.1029/2019JD032361](https://www.metoffice.gov.uk/hadobs/hadcrut5/HadCRUT5_accepted.pdf) ([supporting information](https://www.metoffice.gov.uk/hadobs/hadcrut5/HadCRUT5_supporting_information_accepted.pdf)).",
#                      nombre = "Evolución de la anomalía anual de temperatura base 1861 - 1890",
#                      script = "limpieza_R450C0_1.R")

# actualizo fuente clean
actualizar_fuente_clean(id_fuente_clean = 291)

