################################ Script Global #################################
##### Análises confirmatórias e modularização para construção do dashboard #####
################################################################################
library(leaflet)
library(sf)
library(tidyverse)
library(dplyr)
library(leaflet)
library(sf)
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

# 3º FUNÇÃO - Filtragem------------

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


# 5º FUNÇÃO - Cálculo do rendimento médio ------------------------------------
  
  CalcRendimento <- function(dados) {
    
    for (i in 1:nrow(dados)) {
      dados$rendimento_medio[i] <- mean(c(dados$rep_i[i], dados$rep_ii[i], dados$rep_iii[i], dados$rep_iv[i]), na.rm = TRUE)
      dados$rendimento_desvio[i] <- sd(c(dados$rep_i[i], dados$rep_ii[i], dados$rep_iii[i], dados$rep_iv[i]), na.rm = TRUE)
    }
    
    return(dados)
  }

# 6º FUNÇÃO - Gerar caterpilar plot

# caterpilar <- function(dados, selecao){
#   
#   caterpilar_plot <- dados %>%
#     select(ano, fungicida, selecao, rendimento_medio, ciclo) %>%
#     group_by(localidade) %>%
#     summarise(
#       mean_rendimento = mean(rendimento_medio, na.rm = TRUE),
#       sd_rendimento = sd(rendimento_medio, na.rm = TRUE)
#     ) %>%
#     ungroup() %>%
#     ggplot(aes(x = reorder(selecao, mean_rendimento, decreasing = F), y = mean_rendimento,
#     )) +
#     geom_point() +
#     geom_errorbar(aes(ymin = mean_rendimento - sd_rendimento, ymax = mean_rendimento + sd_rendimento), width = 0.2) +
#     coord_flip() +
#     #facet_wrap(vars(fungicida), nrow = 3) +
#     labs(x = paste0(selecao), y = "Rendimento médio (ton/ha)") +
#     meutema()
#   return(caterpilar_plot)
# }

caterpilar <- function(dados, filtro) {
  if(filtro == "cultivar"){
    variavel <- localidade
  } else {
    variavel <- cultivar
  }
  caterpilar_plot <- dados %>%
    select(ano, fungicida, variavel, rendimento_medio, ciclo) %>%
    group_by(!!variavel) %>%
    summarise(
      mean_rendimento = mean(rendimento_medio, na.rm = TRUE),
      sd_rendimento = sd(rendimento_medio, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    ggplot(aes(x = reorder(variavel, mean_rendimento, decreasing = F), y = mean_rendimento)) +
    geom_point() +
    geom_errorbar(aes(ymin = mean_rendimento - sd_rendimento, ymax = mean_rendimento + sd_rendimento), width = 0.2) +
    coord_flip() +
    #labs(x = paste0(filtro), y = "Rendimento médio (ton/ha)") +
    theme_minimal()  # Supondo que `meutema()` seja um tema customizado
  return(caterpilar_plot)
}

# Exemplo de uso
# Assumindo que o dataframe `dados` existe e tem as colunas apropriadas

# 6º FUNÇÃO - Cria o mapa

mapa <- function(shapefile, dados, filtro) {

  set.seed(123)  
    dados <- dados %>%
      mutate(
        Latitude_jitter = Latitude + runif(n(), min = -0.5, max = 0.5),
        Longitude_jitter = Longitude + runif(n(), min = -0.5, max = 0.5)
      )
    
  mapa_leaflet <- leaflet(data = shapefile) %>%
    addProviderTiles("Esri.WorldTopoMap") %>%
    addPolygons(fillColor = "#392502", color = "#392502", stroke = TRUE, fillOpacity = 0.5)%>%
        addCircleMarkers(data = dados, lat = ~Latitude_jitter, lng = ~Longitude_jitter,
                         radius = ~sqrt(rendimento_medio) / 4,
                         color = "#00525b",
                         fillOpacity = 0.7,
                         label = ~paste("Rendimento:", round(rendimento_medio, 2),"kg/ha"))

  return(mapa_leaflet)
}



# mapa <- function(caminho_arquivo, dados, filtro) {
#  
#   # Carregar dados do shapefile
#   dados_shapefile <- read_sf(dsn = caminho_arquivo)
#   
#   # # Converter Latitude e Longitude para numérico
#   # dados$Latitude <- as.numeric(dados$Latitude)
#   # dados$Longitude <- as.numeric(dados$Longitude)
#   # 
#   # # Definir cor 
#   # cor <- "#00525b"
#   # 
#   # # Garantir que 'empresa' seja um fator
#   # dados$filtro <- factor(dados$filtro)
#   # 
#   # # Definir tamanhos para a legenda
#   # sizes <- c(sqrt(fivenum(dados$media)[1]),
#   #            sqrt(fivenum(dados$media)[2]),
#   #            sqrt(fivenum(dados$media)[3]),
#   #            sqrt(fivenum(dados$media)[5]))
#   # 
#   # # Calcular os intervalos para a legenda com base nos tamanhos
#   # media_max <- max(dados$media)
#   # intervalos <- seq(0, max(dados$media), length.out = length(sizes) + 1)
#   # labels <- paste0(round(intervalos[-length(intervalos)]), "-", round(intervalos[-1]))
#   # 
#   # # Adicionar um pequeno deslocamento aleatório às coordenadas
#   # set.seed(123)  
#   # dados <- dados %>%
#   #   mutate(
#   #     Latitude_jitter = Latitude + runif(n(), min = -0.05, max = 0.05),
#   #     Longitude_jitter = Longitude + runif(n(), min = -0.05, max = 0.05)
#   #   )
#   # 
#   # Criar o mapa com os marcadores de bolhas e a legenda personalizada
#   mapa_leaflet <- leaflet(data = dados_shapefile) %>%
#     addProviderTiles("Esri.WorldTopoMap") %>%
#     addPolygons(fillColor = "#392502", color = "#392502", stroke = TRUE, fillOpacity = 0.5) 
#   # %>%
#   #   addCircleMarkers(data = dados, lat = ~Latitude_jitter, lng = ~Longitude_jitter,
#   #                    radius = ~sqrt(rendimento_medio) / 3,
#   #                    color = ~cores[as.integer(dados$filtro)],
#   #                    fillOpacity = 0.7,
#   #                    label = ~paste("Rendimento:", round(rendimento_medio, 2),"kg/ha"))
#                      #,
#                      #labelOptions = labelOptions(noHide = F, direction = "top", textsize = "15px")) %>%
#     # addLegendCustom(position = "bottomright",
#     #                 sizes = sizes,
#     #                 labels = labels) %>%
#     # addLegend(position = "bottomright",
#     #           colors = cor,
#     #           labels = filtro)
#   
#   return(mapa_leaflet)
# }


# 7º FUNÇÃO - Comparação de médias de proutividade

testeAnova <- function(dados, filtro){
if(filtro == "cultivar"){
  modelo_anova <- aov(rendimento_medio ~ localidade, data = dados)

  } else {
  modelo_anova <- aov(rendimento_medio ~ cultivar, data = dados)
} 
  return(modelo_anova)
}
# 
#   
#   modelo_anova <- aov(rendimento_medio ~ localidade, data = dados)
#   
# modelo_anova <- aov(rendimento_medio ~ cultivar*subregiao_abrev, data = selecionados)
# modelo_anova <- aov(rendimento_medio ~ cultivar, data = selecionados)
# summary(modelo_anova)
# posthoc_tukey<- TukeyHSD(modelo_anova)
