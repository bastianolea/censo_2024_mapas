library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(ggiraph)
library(arrow)
library(sf)
library(shinycssloaders)
library(shinydisconnect)


# cargar datos de comunas y variables para acelerar app
cut_comunas <- readRDS("cut_comunas.rds")
columnas <- readRDS("columnas.rds")

# opciones para spinner
options(spinner.color = "#AE027E",
        spinner.size = 1,
        spinner.type = 8
)

ui <- page_fluid(
  
  # tema de la app
  theme = bs_theme(
    bg = "#181818",
    fg = "#FFFFFF",
    primary = "#AE027E",
    base_font = font_google("Inter"),
    font_scale = .9
  ),
  
  # estilos css
  includeCSS("styles.css"),
  
  # texto de desconexión
  disconnectMessage(
    text = "La conexión se perdió. Por favor, recarga la página.",
    refresh = "Recargar",
    background = "#181818",
    colour = "#FFFFFF",
    refreshColour = "#AE027E"
  ),
  
  title = "Datos censo 2024",
  
  div(
    style = "margin-top: 12px;",
    
    layout_columns(
      col_widths = c(4, 8),
      
      # header ----
      div(
        h3("Censo 2024 por manzana"),
        
        markdown("Desarrollado por [Bastián Olea Herrera](https://bastianolea.rbind.io) en R."),
        
        p("Visualización de datos del Censo 2024 a nivel de manzana censal. Selecciona una región y comuna, y elige una variable para obtener el mapa."),
        
        markdown("Aprende a hacer estos mapas en R [siguiendo este tutorial.](https://bastianolea.rbind.io/blog/mapas_censo_2024/)"),
      ),
      
      # selectores ----
      div(
        # sidebar = sidebar(
        layout_columns(
          
          selectInput("region", "Región",
                      choices = cut_comunas |> 
                        distinct(REGION, COD_REGION) |> 
                        tibble::deframe(),
                      selected = c("Metropolitana De Santiago" = 13)),
          
          div(
            selectInput("comuna", "Comuna", 
                        choices = NULL, width = "100%"),
            
            div(class = "azar",
                actionLink("azar_comuna", "Comuna aleatoria")
            )
          )
        ),
        
        div(style = "margin-top: -18px;",
            selectInput("variable", "Variable",
                        choices = columnas,
                        selected = "n_inmigrantes",
                        width = "100%"
            ),
            
            div(class = "azar",
                actionLink("azar_variable", "Variable aleatoria")
            )
        )
      )
    ),
    
    # mapa ----
    div(style = "max-width: 600px; margin: auto; margin-top: -6px;",
        
        layout_columns(
          col_widths = c(6, 6),
          div(
            h4(textOutput("titulo_comuna")),
            h5(textOutput("titulo_region")),
            div(
              style = "display: flex; gap: 4px;",
              span("Variable:"),
              span(textOutput("titulo_variable"), class = "id_variable")
            )
          ),
          div(
            style = "display: flex; flex-direction: column; justify-content: flex-end; height: 100%;",
              em(
                class = "explicacion",
                "Para hacer zoom, presionar ícono de lupa",
                img(src = "lupa_a.png", height = "20px"),  
                "y hacer scroll sobre el mapa, o presionar la segunda lupa",
                img(src = "lupa_b.png", height = "20px"),
                "y seleccionar el área."
            )
          )
        ),
        
        # plotOutput("mapa", height = "600px")
        
        girafeOutput("mapa_interactivo", height = "600px") |> 
          withSpinner(proxy.height = 400),
        
        # verbatimTextOutput("tabla")
        
        
        br(),
    ),
    
    
    # footer ----
    div(style = "margin-top: 28px;",
      # hr(),
      markdown("Desarrollado y programado por [Bastián Olea Herrera](https://bastianolea.rbind.io) en R."),
      
      markdown("Puedes explorar otras [aplicaciones interactivas sobre datos sociales en mi portafolio.](https://bastianolea.github.io/shiny_apps/)"),
      
      markdown("Código de fuente de esta app y del procesamiento de los datos [disponible en GitHub.](https://github.com/bastianolea/censo_2024_mapas)")
    )
    
  )
)

server <- function(input, output, session) {
  
  # selectores ----
  comunas_filtradas <- reactive({
    cut_comunas |>  
      filter(COD_REGION == input$region)
  })
  
  # filtrar selector de comunas
  observeEvent(input$region, {
    lista <- comunas_filtradas() |> 
      select(COMUNA, CUT) |> 
      tibble::deframe()
    
    updateSelectInput(session, "comuna", 
                      choices = lista)
  })
  
  
  # variable aleatoria
  observeEvent(input$azar_variable, {
    variable_aleatoria <- sample(columnas, 1)
    updateSelectInput(session, "variable",
                      selected = variable_aleatoria)
  })
  
  # comuna aleatoria
  observeEvent(input$azar_comuna, {
    req(input$region)
    
    comuna_aleatoria <- comunas_filtradas() |> 
      slice_sample(n = 1) |> 
      pull(CUT)
    
    updateSelectInput(session, "comuna",
                      selected = comuna_aleatoria)
  })
  
  
  
  # datos ----
  
  # cargar archivo geoparquet como base de datos
  datos <- reactive({
    # cargar datos
    arrow::open_dataset("datos/Cartografia_censo2024_Pais/Cartografia_censo2024_Pais_Manzanas.parquet",
                        partitioning = c("COD_REGION", "CUT")
    )
  })
  
  # desde la base de datos, filtrar por región y comuna seleccionadas, y selecciona columnas relevantes
  datos_filtrados <- reactive({
    req(input$comuna)
    
    # browser()
    datos_fitrados <- datos() |> 
      filter(COD_REGION == as.numeric(input$region),
             CUT == as.numeric(input$comuna)) |>
      select(#COD_REGION, REGION, COMUNA, CUT, 
        all_of(input$variable), OBJECTID, SHAPE) |> 
      collect() |> 
      # obtener datos desde la base de datos
      st_as_sf(crs = 4326) # convertir a sf para mapas
    
  })
  
  # tabla de prueba para ver datos crudos
  output$tabla <- renderPrint({
    # browser()
    datos_filtrados()
  })
  
  # obtener datos de territorios de región y comuna seleccionadas
  territorio <- reactive({
    cut_comunas |> 
      filter(COD_REGION == input$region,
             CUT == input$comuna)
  })
  
  # salidas en base a datos de territorios seleccionados
  output$titulo_comuna <- renderText(territorio()$COMUNA)
  output$titulo_region <- renderText(territorio()$REGION)
  output$titulo_variable <- renderText(input$variable)
  
  
  
  # mapa ----
  # output$mapa <- renderPlot({
  mapa <- reactive({
    req(input$variable)
    req(input$comuna)
    req(nrow(datos_filtrados()) > 1)
    
    datos_filtrados() |> 
      ggplot() +
      aes(fill = !!sym(input$variable),
          data_id = OBJECTID) +
      # geom_sf(color = "#181818", linewidth = 0.1) +
      geom_sf_interactive(
        # aes(tooltip = round(!!sym(input$variable), 2)),
        aes(tooltip = paste0("<span class='id_variable'>", input$variable, ":</span> ", round(!!sym(input$variable), 2))),
        color = "#181818", linewidth = 0.1) +
      # paleta de colores
      scale_fill_fermenter(palette = 13,
                           na.value = "#181818") +
      # tema
      theme(axis.text.x = element_text(angle = 90, vjust = .5),
            axis.ticks = element_blank(),
            panel.background = element_blank(),
            plot.background = element_rect(fill = "#181818", color = NA),
            legend.background = element_rect(fill = "#181818", color = NA),
            panel.grid = element_line(color = "#333333"),
            axis.text = element_text(color = "#444444"),
            legend.key.size = unit(5, "mm"),
            legend.text = element_text(color = "#666666", 
                                       margin = margin(l = 4, r = 6))) +
      guides(fill = guide_legend(title = NULL,
                                 position = "top"))
    # labs(title = paste("Comuna de", unique(datos_filtrados()$COMUNA), ", Censo 2024"),
    # caption = "Fuente: Censo 2024, INE")
  })
  
  # interactivo ----
  output$mapa_interactivo <- renderGirafe({
    req(mapa())
    
    girafe(ggobj = mapa(), bg = "#181818", 
           width_svg = 7,
           height_svg = 7,
           options = list(
             opts_sizing(rescale = TRUE),
             opts_toolbar(hidden = c("selection"),
                          fixed = TRUE,
                          tooltips = list(zoom_on = "activar zoom y desplazamiento",
                                          zoom_off = 'desactivar zoom', 
                                          zoom_rect = 'zoom desde cuadro de selección',
                                          zoom_reset = 'resetear zoom'),
                          saveaspng = FALSE),
             opts_sizing(width = .7),
             opts_selection(type = "none"),
             opts_zoom(duration = 400, min = 1, max = 10),
             opts_hover(css = "stroke: #AE027E; stroke-width: 1;"),
             opts_tooltip(css = "background-color: #181818; 
                          color: #E8E8E8; font-size: 9pt;
                          padding: 3px; 
                          border-radius: 3px;")
           )
    )
  })
}

shinyApp(ui, server)
