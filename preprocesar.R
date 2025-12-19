library(dplyr)
library(arrow)
library(sf)
library(purrr)

# cargar datos
dataset <- arrow::open_dataset("datos/Cartografia_censo2024_Pais/Cartografia_censo2024_Pais_Manzanas.parquet",
                               partitioning = c("COD_REGION", "CUT")
)

# datos_region <- dataset |>
#   filter(COD_REGION == 7,
#          CUT == 7103) |>
#   collect()


cut_comunas <- dataset |> 
  distinct(COD_REGION, REGION, COMUNA, CUT) |>
  arrange(COD_REGION, CUT) |>
  mutate(across(c(REGION, COMUNA),
                ~stringr::str_to_title(.x))) |> 
  collect() |> 
  mutate(across(c(REGION, COMUNA),
                ~stringr::str_replace_all(.x, 
                                        c(" De " = " de ",
                                          " La " = " la ",
                                          " Las " = " las ",
                                          " Del " = " del ",
                                          " Y " = " y ")
                                        ))
  )

cut_comunas |> print(n=Inf)

readr::write_rds(cut_comunas, "cut_comunas.rds")

columnas <- dataset |> 
  select(where(is.numeric)) |> 
  select(-contains("SHAPE")) |> 
  names()

readr::write_rds(columnas, "columnas.rds")
