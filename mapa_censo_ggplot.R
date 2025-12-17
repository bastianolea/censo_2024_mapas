library(dplyr)
library(arrow)
library(sf)
library(ggplot2)

# cargar datos
manzanas <- read_parquet("datos/Cartografia_censo2024_Pais/Cartografia_censo2024_Pais_Manzanas.parquet")

manzanas |> names()
manzanas |> glimpse()

# filtrar
manzanas_comuna <- manzanas |> 
  filter(COMUNA == "LA FLORIDA")

# convertir a sf
manzanas_comuna_sf <- manzanas_comuna |> 
  st_as_sf(sf_column_name = "SHAPE", crs = 4326)

# gráfico
manzanas_comuna_sf |> 
  ggplot() +
  aes(fill = n_edad_60_mas) +
  geom_sf(color = "white", linewidth = 0.1) +
  scale_fill_fermenter(palette = 13) +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 90, vjust = .5)) +
  guides(fill = guide_legend(title = "Población 60 años y más",
                             position = "top")) +
  labs(title = "Población de 60 años y más por manzana",
       subtitle = "Comuna de La Florida, Censo 2024",
       caption = "Fuente: Censo 2024, INE")
