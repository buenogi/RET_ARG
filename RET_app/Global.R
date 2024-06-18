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
    addPolygons(fillColor = "#392502", color = "#392502", stroke = TRUE, fillOpacity = 0.3)%>%
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
# 6 º FUNÇÃO - Avaliação das variáveis climáticas
dados_clima <- function(dados){
  pluviosidade <- dados%>%
    mutate_at(vars(starts_with("chuvas_")),as.numeric)%>%
    select(chuvas_janeiro,chuvas_fevereiro,chuvas_marco,chuvas_abril,
           chuvas_maio,chuvas_junho, chuvas_julho, chuvas_agosto, 
           chuvas_setembro, chuvas_outubro, chuvas_novembro, chuvas_dezembro,
           ano, localidade)%>%
    group_by(ano,localidade)%>%
    summarise(m_chuvas_janeiro = mean(chuvas_janeiro, na.rm = TRUE),
              m_chuvas_fevereiro = mean(chuvas_fevereiro, na.rm = TRUE),
              m_chuvas_marco = mean(chuvas_marco, na.rm = TRUE),
              m_chuvas_abril = mean(chuvas_abril, na.rm = TRUE),
              m_chuvas_maio = mean(chuvas_maio, na.rm = TRUE),
              m_chuvas_junho = mean(chuvas_junho, na.rm = TRUE),
              m_chuvas_julho = mean(chuvas_julho, na.rm = TRUE),
              m_chuvas_agosto = mean(chuvas_agosto, na.rm = TRUE),
              m_chuvas_setembro = mean(chuvas_setembro, na.rm = TRUE),
              m_chuvas_outubro = mean(chuvas_outubro, na.rm = TRUE),
              m_chuvas_novembro = mean(chuvas_novembro, na.rm = TRUE),
              m_chuvas_dezembro = mean(chuvas_dezembro, na.rm = TRUE))
  
  pluviosidade <- pivot_longer(pluviosidade,
                               cols = c(m_chuvas_janeiro,
                                        m_chuvas_fevereiro,
                                        m_chuvas_marco,
                                        m_chuvas_abril,
                                        m_chuvas_maio,
                                        m_chuvas_junho,
                                        m_chuvas_julho,
                                        m_chuvas_agosto,
                                        m_chuvas_setembro,
                                        m_chuvas_outubro,
                                        m_chuvas_novembro,
                                        m_chuvas_dezembro),
                               names_to = "mes",
                               values_to = "pluviosidade")
  pluviosidade$mes <- gsub("m_chuvas_", "", pluviosidade$mes)
  
  temperatura <- dados%>%
    mutate_at(vars(starts_with("temp_")),as.numeric)%>%
    select(temp_janeiro,temp_fevereiro,temp_marco,temp_abril,
           temp_maio,temp_junho, temp_julho, temp_agosto, 
           temp_setembro, temp_outubro, temp_novembro, temp_dezembro,
           ano, localidade)%>%
    group_by(ano,localidade)%>%
    summarise(m_temp_janeiro = mean(temp_janeiro, na.rm = TRUE),
              m_temp_fevereiro = mean(temp_fevereiro, na.rm = TRUE),
              m_temp_marco = mean(temp_marco, na.rm = TRUE),
              m_temp_abril = mean(temp_abril, na.rm = TRUE),
              m_temp_maio = mean(temp_maio, na.rm = TRUE),
              m_temp_junho = mean(temp_junho, na.rm = TRUE),
              m_temp_julho = mean(temp_julho, na.rm = TRUE),
              m_temp_agosto = mean(temp_agosto, na.rm = TRUE),
              m_temp_setembro = mean(temp_setembro, na.rm = TRUE),
              m_temp_outubro = mean(temp_outubro, na.rm = TRUE),
              m_temp_novembro = mean(temp_novembro, na.rm = TRUE),
              m_temp_dezembro = mean(temp_dezembro, na.rm = TRUE))
  
  temperatura <- pivot_longer(temperatura,
                              cols = c(m_temp_janeiro,
                                       m_temp_fevereiro,
                                       m_temp_marco,
                                       m_temp_abril,
                                       m_temp_maio,
                                       m_temp_junho,
                                       m_temp_julho,
                                       m_temp_agosto,
                                       m_temp_setembro,
                                       m_temp_outubro,
                                       m_temp_novembro,
                                       m_temp_dezembro),
                              names_to = "mes",
                              values_to = "temperatura")
  temperatura$mes <- gsub("m_temp_", "", temperatura$mes)
  
  clima <- left_join(pluviosidade, temperatura, by = c("ano","mes","localidade"))
  clima$mes <- str_to_upper(clima$mes)
  clima$mes <- substr(clima$mes,1,3)
  
  meses_ord <- c("JAN", "FEV","MAR",
                 "ABR","MAI","JUN",
                 "JUL","AGO","SET",
                 "OUT","NOV","DEZ")
  clima$mes <- factor(clima$mes, levels = meses_ord)
  clima <- clima %>%
    mutate_all(~replace(., is.nan(.), NA))
  
  
  return(clima)
}

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
