## Mapas del Censo 2024 con R

[Tutorial completo aquí.](https://bastianolea.rbind.io/blog/mapas_censo_2024)

Ejemplos para graficar mapas con datos del Censo 2024 por manzanas censales utilizando R.

### Scripts

- `mapa_censo_ggplot.R`: mapas comunales por manzana censal con `{ggplot2}`
- `mapa_censo_mapgl.R`: mapas comunales y regionales interactivos por manzana censal con `{mapgl}`


### Aplicación
Hice una [pequeña aplicación de prueba](https://bastianoleah.shinyapps.io/censo_2024_mapas/) en el script `app.R`. Se conecta a los datos en formato `geoparquet` abriéndolos como una base de datos, y aplicando _queries_ desde `{dplyr}` para maximizar la velocidad. En pruebas hechas en el script `prueba_duckdb.R` los datos a nivel de manzana demoran 0.1 segundos en cargarse y filtrarse por región y comuna. En el script `preprocesar_app.R` se prueba la carga de datos y se generan dos tablas, una que contiene los valores únicos de las regiones y las comunas, y otra que contienen los nombres de las variables. Así la aplicación puede usar estos datos para generar los selectores sin necesidad de cargar la base de datos.

### Datos
[Descarga los datos cartográficos del Censo 2024 desde esta página del INE](https://censo2024.ine.gob.cl/resultados/), entrando a _Cartografía Censal_ y luego descargando el archivo **Cartografía País Censo 2024 (geoparquet)**.