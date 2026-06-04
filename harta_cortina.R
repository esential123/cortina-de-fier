# ============================================================
#  CORTINA DE FIER - harta interactiva in R (Leaflet)
#  Proiect LPSIG - punctul 2
#  Reproduce harta OpenLayers de la punctul 1
# ============================================================

# ---- 1. PACHETE (se instaleaza automat daca lipsesc) ----
pachete <- c("leaflet", "sf", "htmlwidgets", "htmltools")
for (p in pachete) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p, repos = "https://cloud.r-project.org")
  }
}
library(leaflet)
library(sf)
library(htmlwidgets)
library(htmltools)

# ---- 2. SETARE FOLDER DE LUCRU ----
# Scriptul presupune ca cele 5 fisiere .geojson sunt in ACELASI folder cu scriptul.
# In RStudio: Session -> Set Working Directory -> To Source File Location
# (sau decomenteaza linia urmatoare si pune calea ta)
# setwd("C:/Users/bancu/Desktop/cortina-de-fier")

# ---- 3. CITIRE DATE ----
tari_est        <- st_read("Tari_Cortina_de_fier_final.geojson", quiet = TRUE)
tari_nealiniate <- st_read("comuniste_nealiniate_final.geojson", quiet = TRUE)
tari_vest       <- st_read("Tari_nealiniate_final.geojson", quiet = TRUE)
capitale        <- st_read("capitale_final.geojson", quiet = TRUE)
cortina         <- st_read("cortina_final.geojson", quiet = TRUE)

# ---- 4. CULORI PE BLOCURI (sincron cu OpenLayers) ----
culoare_bloc <- function(bloc) {
  bloc <- as.character(bloc)
  if (is.na(bloc)) return("#888888")
  switch(bloc,
    "Estic"     = "#9e2b25",   # rosu sovietic
    "Nealiniat" = "#c97b29",   # portocaliu ars
    "Vestic"    = "#3a5a6e",   # albastru-gri NATO
    "Neutru"    = "#6b7a5a",   # verde-oliv
    "#888888"
  )
}

# eticheta text pentru un bloc
eticheta_bloc <- function(bloc) {
  bloc <- as.character(bloc)
  if (is.na(bloc)) return("")
  switch(bloc,
    "Estic"     = "Blocul de Est",
    "Nealiniat" = "Comunist nealiniat",
    "Vestic"    = "Vest \u00b7 NATO",
    "Neutru"    = "Stat neutru",
    bloc)
}

# ---- 5. POP-UP-uri (HTML, cu poza + 3 atribute) ----
# Pozele sunt referentiate relativ (img/...), la fel ca pe GitHub Pages.

# pop-up pentru tari (cu imagine)
popup_tara <- function(nume, bloc, info, imagine) {
  eticheta <- eticheta_bloc(bloc)
  col <- culoare_bloc(bloc)
  img_html <- ""
  if (!is.na(imagine) && nzchar(imagine)) {
    img_html <- sprintf(
      "<img src='%s' style='width:100%%;height:140px;object-fit:cover;display:block;border-bottom:2px solid #8a6d3b;filter:sepia(0.3) contrast(1.05);'>",
      imagine)
  }
  sprintf(
    "<div style='width:250px;font-family:Georgia,serif;'>
       %s
       <div style='padding:10px 13px 13px;'>
         <span style='display:inline-block;font-size:10px;letter-spacing:1.5px;text-transform:uppercase;padding:3px 8px;border-radius:2px;color:#fff;background:%s;margin-bottom:7px;'>%s</span>
         <h3 style='margin:0 0 5px;font-size:18px;color:#2b2118;'>%s</h3>
         <p style='margin:0;font-size:13px;line-height:1.5;color:#4a3c2c;'>%s</p>
       </div>
     </div>",
    img_html, col, eticheta, nume, info)
}

# pop-up pentru capitale (fara imagine)
popup_capitala <- function(nume, bloc, info) {
  eticheta <- eticheta_bloc(bloc)
  col <- culoare_bloc(bloc)
  sprintf(
    "<div style='width:220px;font-family:Georgia,serif;padding:4px 6px;'>
       <span style='display:inline-block;font-size:10px;letter-spacing:1.5px;text-transform:uppercase;padding:3px 8px;border-radius:2px;color:#fff;background:%s;margin-bottom:6px;'>%s</span>
       <h3 style='margin:0 0 4px;font-size:16px;color:#2b2118;'>%s</h3>
       <p style='margin:0 0 5px;font-size:11px;font-style:italic;color:#8a6d3b;'>Capital\u0103</p>
       <p style='margin:0;font-size:12px;line-height:1.45;color:#4a3c2c;'>%s</p>
     </div>",
    col, eticheta, nume, info)
}

# pop-up pentru linia Cortinei (cu imagine)
popup_cortina <- function(nume, perioada, lungime_km, info, imagine) {
  img_html <- ""
  if (!is.na(imagine) && nzchar(imagine)) {
    img_html <- sprintf(
      "<img src='%s' style='width:100%%;height:140px;object-fit:cover;display:block;border-bottom:2px solid #8a6d3b;filter:sepia(0.3) contrast(1.05);'>",
      imagine)
  }
  sprintf(
    "<div style='width:260px;font-family:Georgia,serif;'>
       %s
       <div style='padding:10px 13px 13px;'>
         <span style='display:inline-block;font-size:10px;letter-spacing:1.5px;text-transform:uppercase;padding:3px 8px;border-radius:2px;color:#fff;background:#1c1410;margin-bottom:7px;'>Demarca\u021bie</span>
         <h3 style='margin:0 0 5px;font-size:18px;color:#2b2118;'>%s</h3>
         <p style='margin:0 0 7px;font-size:12px;font-style:italic;color:#8a6d3b;'>%s \u00b7 %s km</p>
         <p style='margin:0;font-size:13px;line-height:1.5;color:#4a3c2c;'>%s</p>
       </div>
     </div>",
    img_html, nume, perioada, lungime_km, info)
}

# generez vectorii de pop-up pentru fiecare strat
# (folosesc vapply ca sa garantez un vector de caractere - evita eroarea cu sf)
fac_popup_tari <- function(df) {
  vapply(seq_len(nrow(df)), function(i) {
    popup_tara(df$nume[i], df$bloc[i], df$info[i], df$imagine[i])
  }, character(1))
}
fac_popup_capitale <- function(df) {
  vapply(seq_len(nrow(df)), function(i) {
    popup_capitala(df$nume[i], df$bloc[i], df$info[i])
  }, character(1))
}

tari_est$popup        <- fac_popup_tari(tari_est)
tari_nealiniate$popup <- fac_popup_tari(tari_nealiniate)
tari_vest$popup       <- fac_popup_tari(tari_vest)
capitale$popup        <- fac_popup_capitale(capitale)
cortina$popup         <- popup_cortina(cortina$nume[1], cortina$perioada[1], cortina$lungime_km[1], cortina$info[1], cortina$imagine[1])

# pre-calculez culoarea ca o coloana (evita probleme cu sapply in formule leaflet)
tari_est$culoare        <- vapply(tari_est$bloc, culoare_bloc, character(1))
tari_nealiniate$culoare <- vapply(tari_nealiniate$bloc, culoare_bloc, character(1))
tari_vest$culoare       <- vapply(tari_vest$bloc, culoare_bloc, character(1))
capitale$culoare        <- vapply(capitale$bloc, culoare_bloc, character(1))

# functii de stil pentru poligoane (culoare dupa bloc)
stil_poligon <- function(df) {
  sapply(df$bloc, culoare_bloc)
}

# ---- 6. CONSTRUIRE HARTA ----
harta <- leaflet(options = leafletOptions(minZoom = 3, maxZoom = 12)) %>%

  # --- 3 BASEMAP-uri raster ---
  addProviderTiles("CartoDB.Voyager", group = "H\u00e2rtie de epoc\u0103") %>%
  addProviderTiles("CartoDB.DarkMatter", group = "Relief sobru") %>%
  addProviderTiles("Esri.WorldShadedRelief", group = "Relief fizic") %>%

  # --- Strat poligoane: VEST + NEUTRU ---
  addPolygons(
    data = tari_vest,
    fillColor = ~culoare,
    fillOpacity = 0.45, color = "#2c4654", weight = 1,
    popup = ~popup, group = "Vest & state neutre",
    highlightOptions = highlightOptions(weight = 2.5, fillOpacity = 0.7, bringToFront = TRUE)
  ) %>%

  # --- Strat poligoane: COMUNISTE NEALINIATE ---
  addPolygons(
    data = tari_nealiniate,
    fillColor = ~culoare,
    fillOpacity = 0.5, color = "#9c5d1f", weight = 1,
    popup = ~popup, group = "Comuniste nealiniate",
    highlightOptions = highlightOptions(weight = 2.5, fillOpacity = 0.78, bringToFront = TRUE)
  ) %>%

  # --- Strat poligoane: BLOCUL DE EST ---
  addPolygons(
    data = tari_est,
    fillColor = ~culoare,
    fillOpacity = 0.55, color = "#7a201c", weight = 1,
    popup = ~popup, group = "Blocul de Est",
    highlightOptions = highlightOptions(weight = 2.5, fillOpacity = 0.8, bringToFront = TRUE)
  ) %>%

  # --- Strat linie: CORTINA DE FIER (aura + punctat) ---
  addPolylines(
    data = cortina, color = "#1c1410", weight = 11, opacity = 0.18,
    group = "Cortina de Fier"
  ) %>%
  addPolylines(
    data = cortina, color = "#1c1410", weight = 2.6, opacity = 1,
    dashArray = "9,7", popup = ~popup, group = "Cortina de Fier"
  ) %>%

  # --- Strat puncte: CAPITALE ---
  addCircleMarkers(
    data = capitale,
    radius = 5, fillColor = ~culoare, fillOpacity = 1,
    color = "#e9e0c9", weight = 1.6,
    label = ~nume,
    labelOptions = labelOptions(
      textOnly = TRUE,
      style = list("font-family" = "Georgia, serif", "font-style" = "italic",
                   "font-size" = "12px", "color" = "#2b2118",
                   "text-shadow" = "1px 1px 2px rgba(233,224,201,0.9)")
    ),
    popup = ~popup, group = "Capitale"
  ) %>%

  # --- Control straturi (basemap + overlay selectabile) ---
  addLayersControl(
    baseGroups = c("H\u00e2rtie de epoc\u0103", "Relief sobru", "Relief fizic"),
    overlayGroups = c("Blocul de Est", "Comuniste nealiniate",
                      "Vest & state neutre", "Capitale", "Cortina de Fier"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%

  # --- Legenda ---
  addLegend(
    position = "bottomleft",
    colors = c("#9e2b25", "#c97b29", "#3a5a6e", "#6b7a5a", "#1c1410"),
    labels = c("Blocul de Est", "Comuniste nealiniate", "Vest (NATO)",
               "State neutre", "Traseul Cortinei"),
    title = "Cortina de Fier<br>1945-1991",
    opacity = 0.8
  ) %>%

  # --- Vedere initiala (centrata pe Europa) ---
  setView(lng = 16, lat = 51, zoom = 4)

# ---- 7. TITLU pe harta (control HTML personalizat) ----
titlu <- tags$div(
  HTML("<div style='font-family:Georgia,serif;background:rgba(43,33,24,0.85);
        color:#e9e0c9;padding:8px 16px;border-radius:3px;'>
        <div style='font-size:10px;letter-spacing:4px;text-transform:uppercase;color:#b39b6e;'>Atlas istoric</div>
        <div style='font-size:22px;font-weight:bold;'>Cortina de Fier</div>
        <div style='font-size:12px;font-style:italic;'>Europa divizat\u0103 \u00een dou\u0103 blocuri</div>
        </div>")
)
harta <- harta %>% addControl(titlu, position = "topright")

# ---- 8. AFISARE + SALVARE ----
print(harta)  # arata harta in RStudio (Viewer)

# salveaza ca fisier HTML autonom -> acesta se urca pe GitHub Pages
saveWidget(harta, "index.html", selfcontained = FALSE,
           title = "Cortina de Fier - Europa divizata (1945-1991)")

cat("\n\nGata! S-a generat 'index.html' in folderul de lucru.\n")
cat("Urca-l pe GitHub (in repo-ul cortina-de-fier) impreuna cu folderul img/.\n")
