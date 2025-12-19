library(dplyr)
library(arrow)
library(sf)
library(purrr)

# https://jthomasmock.github.io/bigger-data/#49

# copiar a duckdb ----

tictoc::tic()

manzanas <- read_parquet("datos/Cartografia_censo2024_Pais/Cartografia_censo2024_Pais_Manzanas.parquet") |>
  to_duckdb()

datos_region <- manzanas |>
  filter(COD_REGION == 7,
         CUT == 7103) |>
  collect()

datos_region

tictoc::toc() # 0.918 segundos



# registrar datos en duckdb ----
tictoc::tic()

con <- DBI::dbConnect(duckdb::duckdb()) # crear base temporal

duckdb::duckdb_register(con, "censo",
                        read_parquet("datos/Cartografia_censo2024_Pais/Cartografia_censo2024_Pais_Manzanas.parquet")
)

datos_region <- tbl(con, "censo") |>
  filter(COD_REGION == 7,
         CUT == 7103) |>
  collect()

tictoc::toc() # 1.154 segundos


# cargar dataset ----
tictoc::tic()

dataset <- arrow::open_dataset("datos/Cartografia_censo2024_Pais/Cartografia_censo2024_Pais_Manzanas.parquet",
                            partitioning = c("COD_REGION", "CUT")
)

datos_region <- dataset |>
  filter(COD_REGION == 7,
         CUT == 7103) |>
  collect()

tictoc::toc() # 0.178 segundos

.region <- 8

dataset |>
  filter(COD_REGION == .region) |>
  collect()
