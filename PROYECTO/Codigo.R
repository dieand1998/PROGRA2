graphics.off()
rm(list=ls())
cat("\014")



setwd("C:/Users/David del Cid/Desktop/Programacion II/PROGRA2/PROYECTO")


# ============== Paquetes ==============
# install.packages(c("readxl","jsonlite","sf","dplyr","tidyr","stringr","leaflet","htmlwidgets"))
library(readxl)
library(jsonlite)
library(sf)
library(dplyr)
library(tidyr)
library(stringr)
library(leaflet)
library(htmlwidgets)

# ============== Rutas robustas ==============
find_path <- function(base, exts) {
    cand <- file.path(getwd(), paste0(base, ".", exts))
    hit  <- cand[file.exists(cand)]
    if (!length(hit)) stop("No encontré archivo para '", base, "' con extensiones: ", paste(exts, collapse=", "))
    hit[1]
}
ruta_excel_ipc_2022 <- find_path("IPC_2022", c("xlsx","xls"))
ruta_excel_ipc_2023 <- find_path("IPC_2023", c("xlsx","xls"))
ruta_excel_ipc_2024 <- find_path("IPC_2024", c("xlsx","xls"))
ruta_geojson        <- find_path("deptos",    c("geojson","json"))
ruta_mapeo          <- find_path("deptos_region", c("json"))

# ============== Helpers ==============
norm <- function(x) x |> str_replace_all("\\s+"," ") |> str_trim()

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

leer_ipc_ancho <- function(ruta_excel){
    raw <- read_excel(ruta_excel)
    names(raw) <- str_trim(names(raw))
    ipc_gen <- raw |>
        rename_with(~str_replace_all(.x, "\\s+", " ")) |>
        dplyr::filter(as.numeric(`Código`) == 0) |>
        dplyr::select(Año, Mes, dplyr::starts_with("Reg"))
    ipc_largo <- ipc_gen |>
        tidyr::pivot_longer(
            cols      = dplyr::starts_with("Reg"),
            names_to  = "region_excel",
            values_to = "indice"
        ) |>
        dplyr::mutate(
            region_excel = norm(region_excel),
            Mes          = norm(Mes),
            Mes_num      = unname(mes_map[tolower(Mes)]),
            indice       = as.numeric(indice),
            fecha        = as.Date(ISOdate(as.integer(Año), as.integer(Mes_num) + 1, 1)) - 1, # fin de mes
            region       = norm(unname(map_region_excel_a_oficial[region_excel]))
        ) |>
        dplyr::select(region, fecha, indice) |>
        dplyr::arrange(region, fecha)
    ipc_largo
}

# ============== Utilidades seguras (sin format/prettyNum) ==============
fmt_pct <- function(x) { x <- as.numeric(x); ifelse(is.finite(x), sprintf("%.1f%%", x*100), NA_character_) }
fmt_ym  <- function(f) strftime(f, "%Y-%m")
prev_year_same_month <- function(d) {
    y <- as.integer(strftime(d, "%Y")) - 1L
    m <- as.integer(strftime(d, "%m"))
    as.Date(ISOdate(y, m + 1, 1)) - 1
}

# ============== 1) Datos e interanual ==============
ipc_2022 <- leer_ipc_ancho(ruta_excel_ipc_2022)
ipc_2023 <- leer_ipc_ancho(ruta_excel_ipc_2023)
ipc_2024 <- leer_ipc_ancho(ruta_excel_ipc_2024)

ipc_panel <- bind_rows(ipc_2022, ipc_2023, ipc_2024) |> arrange(region, fecha)

ipc_inter <- ipc_panel |>
    group_by(region) |>
    arrange(fecha, .by_group = TRUE) |>
    mutate(inflacion_y = indice / lag(indice, 12) - 1) |>
    ungroup()

fechas_y <- sort(unique(ipc_inter$fecha[is.finite(ipc_inter$inflacion_y)]))

# ============== 2) Geometría y puente ==============
map_depto_region <- fromJSON(ruta_mapeo, flatten = TRUE) |>
    as_tibble() |>
    mutate(departamento = norm(departamento),
           region       = norm(region))

shp <- st_read(ruta_geojson, quiet = TRUE)
candidatos <- names(shp)[grepl("NOMBRE|DEPTO|DEPART|shapeName|NAME", names(shp), ignore.case = TRUE)]
if (!length(candidatos)) stop("No se encuentra la columna con nombre de departamento en el GeoJSON.")
MAP_NAME_COL <- candidatos[1]
shp <- shp |> mutate(depto_mapa = norm(.data[[MAP_NAME_COL]]))

make_layer_inter <- function(fecha_sel) {
    df_reg <- ipc_inter |> filter(fecha == fecha_sel) |> transmute(region, inflacion = inflacion_y)
    shp |>
        left_join(map_depto_region |> left_join(df_reg, by = "region"),
                  by = c("depto_mapa" = "departamento"))
}

# ============== 3) Paleta ==============
dom_y <- range(ipc_inter$inflacion_y, na.rm = TRUE)
pal_y <- colorNumeric("YlOrRd", domain = dom_y, na.color = "#cccccc")

# ============== 4) Mapa ==============
m <- leaflet() |> addTiles()

# Si quieres limitar a últimos N meses: N <- 24; fechas_y_to_plot <- tail(fechas_y, N)
fechas_y_to_plot <- fechas_y

for (f in fechas_y_to_plot) {
    capa  <- make_layer_inter(f)
    f_str <- fmt_ym(f)
    f_prev_str <- fmt_ym(prev_year_same_month(f))
    capa$popup_txt <- sprintf(
        "<b>%s</b><br/>Región INE: %s<br/>Interanual (%s vs %s): %s",
        capa$depto_mapa, capa$region, f_prev_str, f_str,
        ifelse(is.na(capa$inflacion), "s/d", fmt_pct(capa$inflacion))
    )
    m <- m |>
        addPolygons(
            data = capa,
            fillColor = ~pal_y(inflacion),
            color = "#444", weight = 1, fillOpacity = 0.85,
            group = paste0("Interanual ", f_str),
            popup = ~popup_txt
        )
}

# Nombres de grupos y estado inicial
grupos_y <- paste0("Interanual ", fmt_ym(fechas_y_to_plot))
m <- m |> hideGroup(grupos_y) |> showGroup(tail(grupos_y, 1))

# Leyenda HTML personalizada
legend_html <- (function(pal, domain, title = "Inflación interanual", n = 6) {
    cuts <- seq(domain[1], domain[2], length.out = n)
    labels <- sprintf("%.1f%%", cuts * 100)
    cols <- pal((cuts[-1] + cuts[-length(cuts)]) / 2)
    items <- paste0(
        "<div style='display:flex;align-items:center;margin:2px 0;'>",
        "<div style='width:18px;height:12px;background:", cols, ";border:1px solid #999;margin-right:6px;'></div>",
        "<div>", labels[-length(labels)], " – ", labels[-1], "</div>",
        "</div>"
    )
    paste0(
        "<div style='background:white;padding:8px 10px;border-radius:6px;",
        "box-shadow:0 1px 4px rgba(0,0,0,0.3);font-size:12px;'>",
        "<div style='font-weight:600;margin-bottom:6px;'>", title, "</div>",
        paste(items, collapse = ""),
        "</div>"
    )
})(pal = pal_y, domain = dom_y)

# Control con overlays y exclusividad real usando JS:
m <- m |>
    addLayersControl(
        overlayGroups = grupos_y,
        options = layersControlOptions(collapsed = FALSE)
    ) |>
    addControl(legend_html, position = "bottomright")

# JS: cuando se activa una capa, ocultar TODAS las demás y desmarcar sus casillas.
m <- htmlwidgets::onRender(
    m,
    sprintf(
        'function(el, x){
  var map = this;
  var groups = %s;

  function showOnly(name){
    // ocultar/mostrar grupos
    groups.forEach(function(g){
      if (g === name) { map.layerManager && map.layerManager.showGroup(g); }
      else            { map.layerManager && map.layerManager.hideGroup(g); }
    });
    // desmarcar/ marcar checkboxes del control
    var labels = el.querySelectorAll(".leaflet-control-layers-overlays label");
    labels.forEach(function(lb){
      var txt = lb.textContent.trim();
      var cb  = lb.querySelector("input[type=checkbox]");
      if (cb) cb.checked = (txt === name);
    });
  }

  // Al añadir un overlay desde el control:
  map.on("overlayadd", function(e){ showOnly(e.name); });

  // También si el usuario hace clic directo sobre la etiqueta:
  setTimeout(function(){
    var labels = el.querySelectorAll(".leaflet-control-layers-overlays label");
    labels.forEach(function(lb){
      lb.addEventListener("click", function(){
        var name = lb.textContent.trim();
        // esperar a que leaflet procese el toggle
        setTimeout(function(){ showOnly(name); }, 0);
      });
    });
  }, 0);
}', jsonlite::toJSON(grupos_y))
)

m
