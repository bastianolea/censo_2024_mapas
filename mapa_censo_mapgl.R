library(dplyr)
library(arrow)
library(mapgl)

manzanas <- read_parquet("datos/Cartografia_censo2024_Pais/Cartografia_censo2024_Pais_Manzanas.parquet")

manzanas |> names()
manzanas |> glimpse()

library(sf)


# manzanas por comuna ----

# filtrar
manzanas_comuna <- manzanas |> 
  filter(COMUNA == "ANGOL")

# convertir a sf
manzanas_comuna_sf <- manzanas_comuna |> 
  st_as_sf(crs = 4326) |> 
  select(REGION, n_edad_60_mas, SHAPE) |>
  rename(geometry = SHAPE)

# para usar mapbox se necesita un token en .Renviron
# usethis::edit_r_environ()
# así que usaremos maplibre

maplibre()

mapa_comuna <- manzanas_comuna_sf |> 
  maplibre_view(column = "n_edad_60_mas",
                palette = \(n) RColorBrewer::brewer.pal(n, "RdPu")
                )

mapa_comuna



# manzanas por región ----

# filtrar
manzanas_region <- manzanas |> 
  filter(REGION == "METROPOLITANA DE SANTIAGO")

# convertir a sf
manzanas_region_sf <- manzanas_region |> 
  st_as_sf(sf_column_name = "SHAPE", crs = 4326) |> 
  select(REGION, n_edad_60_mas, SHAPE) |>
  rename(geometry = SHAPE)

# generar mapa interactivo
mapa_region <- manzanas_region_sf |>
  maplibre_view(column = "n_edad_60_mas",
                palette = \(n) RColorBrewer::brewer.pal(n, "PRGn"))

# ver
mapa_region
