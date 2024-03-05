#-----------------------------------------------------------------------
# Carregar pacotes necessários.

library(rgdal)
library(leaflet)
library(sf)
library(tidyverse)

#-----------------------------------------------------------------------
# Importação dos arquivos shapefile.

# Definir o caminho para o arquivo shapefile (.shp).
caminho_arquivo <- "mapa_subregiones_formato_gis_2021-06-19/Subregiones 2021.shp"

# Ler o arquivo shapefile
dados_shapefile <- readOGR(dsn = caminho_arquivo)
str(dados_shapefile)

# Converter para sf.
dados_sf <- st_as_sf(dados_shapefile)
str(dados_sf)

dados_sf |>
    dplyr::select(1:SUB_ANTER) |>
    sf::st_drop_geometry() |>
    dplyr::arrange(SUB_NUM)

# Visualização com ggplot2.
ggplot(data = dados_sf) +
    geom_sf() +
    theme_minimal()

# Visualização com leaflet.
mapa_leaflet <- leaflet(data = dados_sf) %>%
    addProviderTiles("OpenStreetMap.Mapnik") %>%
    addPolygons(fillColor = "orange", stroke = TRUE, fillOpacity = 0.3)

# Exibir o mapa interativo.
mapa_leaflet

#-----------------------------------------------------------------------
# Vamos dar uma simplificada no mapa para melhorar a visualização.

library(rmapshaper)
ls("package:rmapshaper")

# Simplificar o mapa.
dados_sf_simplificado <- ms_simplify(dados_sf, keep = 0.01)

dados_sf_simplificado |>
    sf::st_centroid()

dados_sf_simplificado$centroid <-
    sf::st_centroid(dados_sf_simplificado$geometry)

dados_sf_simplificado$area <-
    sf::st_area(dados_sf_simplificado$geometry) |>
    as.numeric()

# Visualização com ggplot2.
dados_sf_simplificado |>
    mutate(label = str_wrap(SUB_NOM, width = 10)) |>
    ggplot(data = _) +
    geom_sf(mapping = aes(fill = area),
            color = "black",
            show.legend = FALSE) +
    geom_sf_text(
        mapping = aes(label = label),
        size = 2.5) +
    # ggrepel::geom_text_repel(
    #     mapping = aes(label = label, geometry = geometry),
    #     stat = "sf_coordinates",
    #     # min.segment.length = 0,
    #     box.padding = 0.5,
    #     max.overlaps = 30,
    #     size = 2.5
    # ) +
    # scale_fill_viridis_c(direction = -1) +
    scale_fill_distiller(palette = "Spectral", direction = 1) +
    theme_minimal() +
    labs(title = "Subregiones trigueras", x = NULL, y = NULL)

ggsave("mapa_subregiones_trigueras.svg", width = 10, height = 10)

# Visualização com leaflet.
mapa_leaflet <- leaflet(data = dados_sf_simplificado) %>%
    addProviderTiles("OpenStreetMap.Mapnik") %>%
    addPolygons(fillColor = "orange", stroke = TRUE, fillOpacity = 0.3)

# Exibir o mapa interativo.
mapa_leaflet

#-----------------------------------------------------------------------
