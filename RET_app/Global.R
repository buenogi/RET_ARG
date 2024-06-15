################################ Script Global #################################
##### Análises confirmatórias e modularização para construção do dashboard #####
################################################################################
library(leaflet)
library(sf)
library(tidyverse)
library(dplyr)

# 1ª FUNÇÃO - Tema para os gráficos --------------------------------------------------------

require(extrafont)

COR.1="#ead693"
COR.2="#392502"

meutema <- function() {
  theme_minimal() +
    theme(
      axis.title = element_text(size = 15, family = "AvantGarde", colour = COR.2), 
      axis.text = element_text(size = 10, family = "AvantGarde", colour = COR.2),
      plot.title = element_text(size = 20, family = "AvantGarde", colour = COR.2, face = "bold", hjust = 0.5),
      legend.text = element_text(size = 10, family = "AvantGarde", colour = COR.2, face = "bold"),
      legend.title = element_text(size = 20, family = "AvantGarde", colour = COR.2, face = "bold"),
      plot.background = element_rect(fill = NA, colour = NA),
      panel.background = element_rect(fill = NA, colour = NA),
      legend.background = element_rect(fill = NA, colour = NA),
      legend.box.background = element_rect(fill = NA, colour = NA),
      strip.background = element_rect(fill = COR.1, colour = NA),
      strip.text = element_text(family = "AvantGarde", size = 15),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
}

# 2ª FUNÇÃO - Extração das coordenadas ------------------------------------------------------

extracao_coordenadas <- function(dados, caminho_arquivo){

  dados_shapefile <- read_sf(dsn = caminho_arquivo)
  
  
  dados_shapefile <- st_make_valid(dados_shapefile)
  
  # Transformar a projeção para WGS 84 (EPSG:4326)
  dados_shapefile <- st_transform(dados_shapefile, crs = 4326)
  
  # Calcular os centroides das geometrias
  dados_shapefile <- dados_shapefile %>%
    st_centroid()
  
  # Extrair as coordenadas dos centroides
  coords <- st_coordinates(dados_shapefile)
  
  # Adicionar as coordenadas como novas colunas no shapefile
  dados_shapefile$Longitude <- coords[, "X"]
  dados_shapefile$Latitude <- coords[, "Y"]
  
  # Manter apenas as colunas necessárias
  dados_shapefile <- dados_shapefile %>%
    select(SUB_ABR, Longitude, Latitude)
  
  # Ajustar a coluna 'subregiao_abrev' no dataframe de dados
  dados <- dados %>%
    mutate(subregiao_abrev = str_extract(subregiao_abrev, "^.{1,3}"))
  
  # Realizar a junção entre os dados e o shapefile baseado na subregiao_abrev e SUB_ABR
  dados <- left_join(dados, dados_shapefile, by = c("subregiao_abrev" = "SUB_ABR"))
  
  return(dados)
}

# Filtragem

# selecao <- ifelse(filtro == "empresa", empresa_selecionada, 
#                   ifelse(filtro == "localidade", localidade_selecionada, cultivar_selecionado))

filtroSelecao <- function(dados, filtro, selecao){
  if(filtro == "empresa"){
    dados_filtrados <- dados%>%
      filter(empresa == selecao)
  }
  if(filtro == "localidade"){
    dados_filtrados <- dados%>%
      filter(localidade == selecao)
  } 
  if(filtro == "cultivar"){
    dados_filtrados <- dados%>%
      filter(cultivar == selecao)
  }
  return(dados_filtrados)
}



# 3ª FUNÇÃO - Filtragem ano e seleção de empresa ---------------------------
get_opcoes_ano_empresa <- function(dados, ano, grupo__qualidade) {
  opcoes <- switch(
    as.character(ano),
    "2021"  = {
      dados %>%
        filter(ano == 2021 & grupo__qualidade == grupo__qualidade) %>%
        pull(empresa)
    },
    "2022"  = {
      dados %>%
        filter(ano == 2022 & grupo__qualidade == grupo__qualidade) %>%
        pull(empresa)
    },
    "2023"  = {
      dados %>%
        filter(ano == 2023 & grupo__qualidade == grupo__qualidade) %>%
        pull(empresa)
    }
  )
  return(unique(opcoes))
}


# 4º FUNÇÃO - Filtragem localidade ---------------------------------------------

get_opcoes_localidade <- function(dados,ano, grupo__qualidade, selecionar_empresa, empresa_selecionada){
  if (selecionar_empresa == T & !is.null(empresa_selecionada)) {
    opcoes <- dados %>%
      filter(ano == ano & grupo__qualidade == grupo__qualidade & empresa == empresa_selecionada)
  } else {
    opcoes <- dados
  }
  opcoes <- opcoes %>%
    select(subregiao_nome)
  return(unique(opcoes))
}


# 5ª FUNÇÃO - Filtragem de localidade ------------------------------------------

get_opcoes_cultivar <- function(dados, ano, grupo__qualidade, selecionar_empresa, selecionar_localidade, empresa_selecionada, localidade_selecionada){
  if (selecionar_empresa == T && !is.null(empresa_selecionada) && selecionar_localidade == T && !is.null(localidade_selecionada)) {
    opcoes <- dados %>%
      filter(ano == ano & grupo__qualidade == grupo__qualidade & empresa == empresa_selecionada & localidade == localidade_selecionada)
  } else if (selecionar_empresa == T && !is.null(empresa_selecionada)) {
    opcoes <- dados %>%
      filter(ano == ano & grupo__qualidade == grupo__qualidade & empresa == empresa_selecionada)
  } else if (selecionar_localidade == T && !is.null(localidade_selecionada)) {
    opcoes <- dados %>%
      filter(ano == ano & grupo__qualidade == grupo__qualidade & localidade == localidade_selecionada)
  } else {
    opcoes <- dados
  }
  opcoes <- opcoes %>%
    select(cultivar)
  return(opcoes)
}

