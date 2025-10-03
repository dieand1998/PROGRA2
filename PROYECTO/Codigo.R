# ========================================================
# Shiny App - IPC Interanual Ponderado (Departamentos GT)
# ========================================================

graphics.off(); rm(list=ls()); cat("\014")

library(shiny)
library(readxl)
library(jsonlite)
library(sf)
library(dplyr)
library(tidyr)
library(stringr)
library(stringi)
library(leaflet)

# -----------------------------
# Helpers
# -----------------------------
norm    <- function(x) x |> str_replace_all("\\s+"," ") |> str_trim()
fmt_pct <- function(x) { x <- as.numeric(x); ifelse(is.finite(x), sprintf("%.1f%%", x*100), "s/d") }
fmt_ym  <- function(f) strftime(as.Date(f), "%Y-%m")
find_path <- function(base, exts) {
  cand <- file.path(getwd(), paste0(base, ".", exts))
  hit  <- cand[file.exists(cand)]
  if (!length(hit)) stop("No encontré archivo para ", base)
  hit[1]
}

map_region_excel_a_oficial <- c(
  "Reg. I"    = "Región I - Metropolitana",
  "Reg. II"   = "Región II - Norte",
  "Reg. III"  = "Región III - Nororiente",
  "Reg. IV"   = "Región IV - Suroriente",
  "Reg. V"    = "Región V - Central",
  "Reg. VI"   = "Región VI - Suroccidente",
  "Reg. VII"  = "Región VII - Noroccidente",
  "Reg. VIII" = "Región VIII - Petén"
)

mes_map <- c(
  "enero"=1,"febrero"=2,"marzo"=3,"abril"=4,"mayo"=5,"junio"=6,
  "julio"=7,"agosto"=8,"septiembre"=9,"setiembre"=9,"octubre"=10,
  "noviembre"=11,"diciembre"=12
)

# -----------------------------
# Lector IPC (robusto)
# -----------------------------
leer_ipc_ancho <- function(ruta_excel){
  raw <- read_excel(ruta_excel)
  names(raw) <- str_trim(names(raw))
  
  col_cod <- names(raw)[str_detect(names(raw), regex("^c(o|ó)digo$", ignore_case=TRUE))]
  col_ano <- names(raw)[str_detect(names(raw), regex("^a(ñ|n)o$",   ignore_case=TRUE))]
  col_mes <- names(raw)[str_detect(names(raw), regex("^mes$",       ignore_case=TRUE))]
  
  raw <- raw |> mutate(.COD = suppressWarnings(as.numeric(.data[[col_cod]]))) |> filter(.COD == 0)
  
  cols_reg <- names(raw)[str_detect(names(raw), regex("^reg", ignore_case=TRUE))]
  enc_norm <- str_to_upper(cols_reg)
  rom      <- str_match(enc_norm, "REG[\\.|I|IÓN]*\\s*([IVX]+)")[,2]
  cols_reg_norm <- paste0("Reg. ", rom)
  names(raw)[match(cols_reg, names(raw))] <- cols_reg_norm
  
  raw |>
    rename(Año = all_of(col_ano), Mes = all_of(col_mes)) |>
    pivot_longer(cols=all_of(cols_reg_norm), names_to="region_excel", values_to="indice") |>
    mutate(
      region_excel = str_squish(region_excel),
      Mes          = str_squish(Mes),
      Mes_num      = unname(mes_map[tolower(Mes)]),
      indice       = suppressWarnings(as.numeric(indice)),
      fecha        = as.Date(ISOdate(as.integer(Año), as.integer(Mes_num)+1, 1)) - 1,
      region       = norm(unname(map_region_excel_a_oficial[region_excel]))
    ) |>
    filter(!is.na(region), !is.na(indice), !is.na(fecha)) |>
    select(region, fecha, indice) |>
    arrange(region, fecha) |>
    distinct(region, fecha, .keep_all = TRUE)
}

# -----------------------------
# Cargar datos
# -----------------------------
ruta_excel_ipc_2022 <- find_path("input/IPC_2022", c("xlsx","xls"))
ruta_excel_ipc_2023 <- find_path("input/IPC_2023", c("xlsx","xls"))
ruta_excel_ipc_2024 <- find_path("input/IPC_2024", c("xlsx","xls"))
ruta_geojson        <- find_path("deptos",    c("geojson","json"))
ruta_mapeo          <- find_path("deptos_region", c("json"))
ruta_censo          <- find_path("input/Censo_2018", c("xlsx","xls"))

ipc_panel <- bind_rows(
  leer_ipc_ancho(ruta_excel_ipc_2022),
  leer_ipc_ancho(ruta_excel_ipc_2023),
  leer_ipc_ancho(ruta_excel_ipc_2024)
) |> arrange(region, fecha)

# Interanual (join mismo mes año anterior)
df_idx  <- ipc_panel |> mutate(y=as.integer(format(fecha,"%Y")), m=as.integer(format(fecha,"%m")))
df_prev <- df_idx |> transmute(region, y=y+1L, m, indice_prev=indice)

ipc_inter <- df_idx |>
  left_join(df_prev, by=c("region","y","m")) |>
  mutate(inflacion_y = ifelse(is.finite(indice_prev), indice/indice_prev - 1, NA_real_),
         fecha       = as.Date(ISOdate(y, m+1, 1)) - 1) |>
  select(region, fecha, inflacion_y)

fechas_y <- sort(unique(ipc_inter$fecha[is.finite(ipc_inter$inflacion_y)]))

# -----------------------------
# Geo y Censo
# -----------------------------
map_depto_region <- fromJSON(ruta_mapeo, flatten=TRUE) |>
  as_tibble() |> mutate(departamento=norm(departamento), region=norm(region))

shp <- st_read(ruta_geojson, quiet=TRUE)
candidatos   <- names(shp)[grepl("NOMBRE|DEPTO|DEPART|shapeName|NAME", names(shp), ignore.case=TRUE)]
MAP_NAME_COL <- candidatos[1]
shp <- shp |> mutate(depto_mapa = norm(.data[[MAP_NAME_COL]]))

censo <- read_excel(ruta_censo, skip=8) |>
  transmute(departamento=norm(Departamento), poblacion=as.numeric(`Total de personas`)) |>
  filter(!is.na(departamento), departamento != "")

censo_reg <- censo |>
  left_join(map_depto_region, by="departamento") |>
  group_by(region) |>
  mutate(peso = poblacion / sum(poblacion, na.rm=TRUE)) |>
  ungroup()

# -----------------------------
# UI
# -----------------------------
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .container-fluid { max-width: 1600px; }
      h2 { margin-top: 10px; margin-bottom: 10px; }
      .control-card {
        background:#fff; padding:12px 14px; border-radius:10px;
        box-shadow: 0 1px 4px rgba(0,0,0,.12);
        position: sticky; top: 10px;
        max-height: 72vh; overflow-y: auto;
      }
    "))
  ),
  h2("Inflación Interanual Ponderada por Departamento"),
  fluidRow(
    column(
      width = 3,
      div(class="control-card",
          tags$label("Selecciona mes:"),
          selectInput("fecha", NULL,
                      choices = setNames(fechas_y, fmt_ym(fechas_y)),
                      selected = tail(fechas_y,1)),
          br(),
          sliderInput("nbins", "Número de tramos (cuantiles)",
                      min=5, max=9, value=7, step=1)
      )
    ),
    column(width=9, leafletOutput("map", height="80vh"))
  )
)

# -----------------------------
# SERVER
# -----------------------------
server <- function(input, output, session){
  
  # Datos ponderados del mes seleccionado
  df_mes <- reactive({
    fecha_sel <- as.Date(input$fecha)
    df_reg_mes <- ipc_inter |> filter(fecha==fecha_sel) |> transmute(region, inflacion_reg=inflacion_y)
    df_depto   <- censo_reg |> left_join(df_reg_mes, by="region") |> mutate(inflacion = inflacion_reg * peso)
    shp |> left_join(df_depto, by=c("depto_mapa"="departamento"))
  })
  
  output$map <- renderLeaflet({
    leaflet() |> addTiles() |> setView(lng=-90.3, lat=15.4, zoom=7)
  })
  
  observe({
    capa <- df_mes()
    vals <- capa$inflacion[is.finite(capa$inflacion)]
    if (length(vals)==0) vals <- c(0)
    
    # paleta por cuantiles
    nb    <- input$nbins
    probs <- seq(0,1,length.out=nb)
    bins  <- unique(quantile(vals, probs=probs, na.rm=TRUE))
    if (length(bins)<2) bins <- range(vals)
    pal   <- colorBin("YlOrRd", bins=bins, domain=vals, na.color="#cccccc")
    
    capa$popup_txt <- sprintf(
      "<b>%s</b><br/>Región: %s<br/>Inflación interanual ponderada: %s",
      capa$depto_mapa, capa$region, fmt_pct(capa$inflacion)
    )
    
    leafletProxy("map", data=capa) |>
      clearShapes() |>
      clearControls() |>
      addPolygons(fillColor=~pal(inflacion),
                  color="#444", weight=1, fillOpacity=0.85,
                  popup=~popup_txt) |>
      addLegend("bottomright",
                pal=pal, values=vals,
                title=paste("Interanual", fmt_ym(input$fecha)),
                labFormat=labelFormat(transform=function(x) 100*x, digits=1, suffix="%"))
  })
}

shinyApp(ui, server)


