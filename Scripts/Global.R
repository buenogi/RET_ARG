################################ Script Global #################################
##### Análises confirmatórias e modularização para construção do dashboard #####
################################################################################

#  Dados -----------------------------------------------------------------------
dados <- read.csv("Dados/Dados_processados/RET_ARG.csv")

# Pré-procesamento -------------------------------------------------------------

dados$rep_v <- NULL
dados <- dados %>% filter(!is.na(rep_i))
dados$grupo__qualidade <- gsub("GRUPO DE CALIDAD 1", "Grupo de qualidade 1",
                               dados$grupo__qualidade)
dados$grupo__qualidade <- gsub("GRUPO DE CALIDAD 2", "Grupo de qualidade 2",
                               dados$grupo__qualidade)
dados$grupo__qualidade <- gsub("GRUPO DE CALIDAD 3", "Grupo de qualidade 3",
                               dados$grupo__qualidade)
dados$grupo__qualidade <- ifelse(is.na(dados$grupo__qualidade), "Não informado", dados$grupo__qualidade)


# Tema para os gráficos --------------------------------------------------------

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

# Extração das coordenadas ------------------------------------------------------
caminho_arquivo <- "Dados/mapa_subregiones_formato_gis_2021-06-19/Subregiones 2021.shp"

extracao_coordenadas <- function(caminho){
  dados_shapefile <- read_sf(dsn = caminho_arquivo)
  dados_shapefile <- st_make_valid(dados_shapefile)
  dados_shapefile <- st_transform(dados_shapefile, crs = 4326)
  
  dados_shapefile <- dados_shapefile %>%
    st_centroid()
  coords <- st_coordinates(dados_shapefile)
  
  dados_shapefile$Longitude <- coords[, "X"]
  dados_shapefile$Latitude <- coords[, "Y"]
  # 
  # coords <- st_coordinates(dados_shapefile)
  # 
  # dados_shapefile <- st_transform(dados_shapefile, crs = 4326)
  # 
  # dados_sf_simplificado$centroid <-
  #   sf::st_centroid(dados_sf_simplificado$geometry)
  # 
  # coord <- dados_sf_simplificado%>% 
  #   mutate(centroid = gsub('[()°]', '', centroid )) %>% 
  #   separate(col = centroid  , into = c('Latitude', 'Longitude'), sep = '\\,')
  # coord$Latitude <- gsub("c", "", coord$Latitude)
  return(dados_shapefile)
}
coordenadas <- extracao_coordenadas(caminho_arquivo)
# Inclusão das coordenadas na tabela principal ---------------------------------

dados <- dados%>%
  mutate(subregiao_abrev = str_extract(subregiao_abrev, "^.{1,3}"))
dados <- left_join(dados, coordenadas, by = c("subregiao_abrev"="SUB_ABR"))

# Cálculo do rendimento médio --------------------------------------------------

CalcRendimento <- function(dados) {
  
  for (i in 1:nrow(dados)) {
    dados$rendimento_medio[i] <- mean(c(dados$rep_i[i], dados$rep_ii[i], dados$rep_iii[i], dados$rep_iv[i]), na.rm = TRUE)
  }
  
  return(dados)
}

# Filtragem para avaliação de desempenho ---------------------------------------
desempenho <- function(dados, ANO, CICLO, GRUPO_DE_QUALIDADE, FUNGICIDA){
  #filtragem dos selecionados
  selecionados<- dados%>%
    filter(ano == ANO &
             grupo__qualidade == GRUPO_DE_QUALIDADE &
             ciclo == CICLO &
             fungicida == FUNGICIDA)
}

desempenhoR <- function(dados, ANO, GRUPO_DE_QUALIDADE, SUBREGIAO, FUNGICIDA){
  #filtragem dos selecionados
  selecionados<- dados%>%
    filter(ano == ANO &
             grupo__qualidade == GRUPO_DE_QUALIDADE &
             subregiao_abrev == SUBREGIAO &
             fungicida == FUNGICIDA)
}

# TESTE ------------------------------------------------------------------------

ANO <- "2023"
CICLO <- "LARGO"
GRUPO_DE_QUALIDADE <- "GRUPO DE CALIDAD 1"
FUNGICIDA <- "SIN FUNGICIDA"
DADOS <- CalcRendimento(dados)
DADOS <- desempenho(DADOS, ANO, CICLO, GRUPO_DE_QUALIDADE, FUNGICIDA)

# FILTRANDO POR ANO, SUBREGIAO, GRUPO DE QUALIDADE E FUNGICIDA

selecionados<- dados%>%
  filter(ano == ANO &
           grupo__qualidade == GRUPO_DE_QUALIDADE &
           subregiao_abrev == "CHS NEA" &
           fungicida == FUNGICIDA)
selecionados <- CalcRendimento(selecionados)
# ANOVA ------------------------------------------------------------------------
modelo_anova <- aov(rendimento_medio ~ cultivar*subregiao_abrev, data = selecionados)
modelo_anova <- aov(rendimento_medio ~ cultivar, data = selecionados)
summary(modelo_anova)
posthoc_tukey<- TukeyHSD(modelo_anova)

# Preparação do mapa -----------------------------------------------------------------
dados <- CalcRendimento(dados)

addLegendCustom <- function(map, position = "bottomright", title = "Tamanho das Bolhas", sizes, labels) {
  colorAdditions <- paste0(sizes, "px solid #00525b")
  labelAdditions <- labels
  
  legendHTML <- paste0("<div style='background-color: white; padding: 10px;'><h4>", title, "</h4><div>")
  
  for(i in 1:length(sizes)){
    legendHTML <- paste0(legendHTML, "<div style='display: flex; align-items: center;'><div style='background-color: #00525b; height: ", sizes[i], "px; width: ", sizes[i], "px; border-radius: 50%; margin-right: 10px;'></div>", labelAdditions[i], "</div>")
  }
  
  legendHTML <- paste0(legendHTML, "</div></div>")
  
  map %>% addControl(html = legendHTML, position = position)
}

mapa <- function(caminho_arquivo, dados, empresa) {
  library(leaflet)
  library(sf)
  library(dplyr)
  
  # Carregar dados do shapefile
  dados_shapefile <- read_sf(dsn = caminho_arquivo)
  
  # Converter Latitude e Longitude para numérico
  dados$Latitude <- as.numeric(dados$Latitude)
  dados$Longitude <- as.numeric(dados$Longitude)
  
  # Definir cor e empresa
  cor <- "#00525b"
  empresa <- empresa
  
  # Garantir que 'empresa' seja um fator
  dados$empresa <- factor(dados$empresa)
  
  # Definir tamanhos para a legenda
  sizes <- c(sqrt(fivenum(dados$media)[1]),
             sqrt(fivenum(dados$media)[2]),
             sqrt(fivenum(dados$media)[3]),
             sqrt(fivenum(dados$media)[5]))
  
  # Calcular os intervalos para a legenda com base nos tamanhos
  media_max <- max(dados$media)
  intervalos <- seq(0, max(dados$media), length.out = length(sizes) + 1)
  labels <- paste0(round(intervalos[-length(intervalos)]), "-", round(intervalos[-1]))
  
  # Adicionar um pequeno deslocamento aleatório às coordenadas
  set.seed(123)  
  dados <- dados %>%
    mutate(
      Latitude_jitter = Latitude + runif(n(), min = -0.05, max = 0.05),
      Longitude_jitter = Longitude + runif(n(), min = -0.05, max = 0.05)
    )
  
  # Criar o mapa com os marcadores de bolhas e a legenda personalizada
  mapa_leaflet <- leaflet(data = dados_shapefile) %>%
    addProviderTiles("Esri.WorldTopoMap") %>%
    addPolygons(fillColor = "#392502", color = "#392502", stroke = TRUE, fillOpacity = 0.5) %>%
    addCircleMarkers(data = dados, lat = ~Latitude_jitter, lng = ~Longitude_jitter,
                     radius = ~sqrt(media) / 3,
                     color = ~cores[as.integer(dados$empresa)],
                     fillOpacity = 0.7,
                     label = ~paste("Rendimento:", round(media, 2),"kg/ha"),
                     labelOptions = labelOptions(noHide = F, direction = "top", textsize = "15px")) %>%
    # addLegendCustom(position = "bottomright",
    #                 sizes = sizes,
    #                 labels = labels) %>%
    addLegend(position = "bottomright",
              colors = cor,
              labels = empresa)
  
  return(mapa_leaflet)
}


teste <- dados %>%
  select(empresa, subregiao_abrev, Latitude, Longitude, rendimento_medio) %>%
  filter(empresa == "CRIADERO KLEIN S.A.")%>%
  group_by(empresa, subregiao_abrev, Latitude, Longitude) %>%
  summarise(media = mean(rendimento_medio, na.rm = TRUE), .groups = 'drop')

mapa(caminho_arquivo, teste, "CRIADERO KLEIN S.A.")


selecionados<- dados%>%
  filter(ano == "2021" &
           grupo__qualidade == "Grupo de qualidade 1" &
           empresa == "CRIADERO KLEIN S.A.")

selecionados  <- CalcRendimento(selecionados)
# Análise de variancia por localidade
modelo_anova <- aov(rendimento_medio ~ cultivar*subregiao_abrev, data = selecionados)

summary(modelo_anova)

# CATERPILAR PLOT POR CULTIVAR

media_geral <- mean(selecionados$rendimento_medio, na.rm = T)

# 2. Calcular a média do rendimento para cada cultivar
media_por_cultivar <- aggregate(rendimento_medio ~ cultivar, data = selecionados, FUN = mean)

# 3. Calcular a diferença entre a média do rendimento de cada cultivar e a média geral
media_por_cultivar$diferenca_media <- media_por_cultivar$rendimento_medio - media_geral

# 4. Criar o Caterpillar plot
library(ggplot2)

caterpillar_plot <- ggplot(media_por_cultivar, aes(x = reorder(cultivar, diferenca_media), y = rendimento_medio)) +
  geom_point(size = 3, color = "#392502") +  # Pontos representando as diferenças de média
  geom_errorbar(aes(ymin = rendimento_medio-diferenca_media, ymax = rendimento_medio+diferenca_media), width = 0.2, color = "#392502") +  # Barras de erro (opcional)
  coord_flip() +  # Inverter eixo x e y para criar um gráfico horizontal
  labs(x = "Cultivar", y = "Média de Rendimento (Kg/ha)") +  # Rótulos dos eixos
  ggtitle("Diferença de Média de Rendimento por Cultivar") +  # Título do gráfico
  meutema()  # Estilo do tema do gráfico

# Exibir o Caterpillar plot
print(caterpillar_plot)


# Boxplot por ciclo

ggplot(selecionados) +
  aes(x = ciclo, y = rendimento_medio) +
  geom_boxplot(fill = "#557962") +
  meutema()

# Tabela com informações edafoclimaticas
library(tidyverse)
pluviosidade <- selecionados%>%
  mutate_at(vars(starts_with("chuvas_")),as.numeric)%>%
  select(chuvas_janeiro,chuvas_fevereiro,chuvas_marco,chuvas_abril,
         chuvas_maio,chuvas_junho, chuvas_julho, chuvas_agosto, 
         chuvas_setembro, chuvas_outubro, chuvas_novembro, chuvas_dezembro,
         ano, subregiao_abrev)%>%
  group_by(ano,subregiao_abrev)%>%
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

temperatura <- selecionados%>%
  mutate_at(vars(starts_with("temp_")),as.numeric)%>%
  select(temp_janeiro,temp_fevereiro,temp_marco,temp_abril,
         temp_maio,temp_junho, temp_julho, temp_agosto, 
         temp_setembro, temp_outubro, temp_novembro, temp_dezembro,
         ano, subregiao_abrev)%>%
  group_by(ano,subregiao_abrev)%>%
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

clima <- left_join(pluviosidade, temperatura, by = c("ano","mes","subregiao_abrev"))


