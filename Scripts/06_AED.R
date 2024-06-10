################################################################################
####################### Análise exploratória de dados ##########################
################################################################################
library(tidyverse)

dados <- read.csv("Dados/Dados_processados/RET_ARG.csv")

# PERGUNTA DE NEGÓCIO: Quais cultivares tiveram a melhor performance e em quais 
# condições de cultivo?

# 1. Nº de cultivares
# 2. Nº de experimentos para cada um dos cultivares (por ano)(barras)
# 3. Nº de condições avaliadas(árvore)
# 4. Nº de experimentos por região
# 5. Rendimento médio e variação do rendimento (boxplot)
# 6. Duração média das etapas do ciclo para cada condição
# 7. Gráfico de linhas com a pluviosidade e a tempetatura com pontos de rendimento
# para cada cultivar, para cada ano
# Modelo: ANOVA para identificação de diferença dos cultivares em cada condição
# 8. Teste de Tukey apra deonstrar entre quais deles há diferença

#Tratamento inicial ----
# Remoção de valores ausentes
dados <- dados[complete.cases(dados$rep_i, dados$rep_ii, dados$rep_iii), ]


# Nomes cultivares 
dados$cultivar<- gsub("[Ã]", "A", dados$cultivar)
dados$cultivar<- gsub("[Á]", "A", dados$cultivar)
dados$cultivar<- gsub("[Ñ]", "N", dados$cultivar)
dados$cultivar<- gsub("NEO 30T 23", "NEO30T23", dados$cultivar)
dados$cultivar<- gsub("MS INTA B 221", "MS INTA221", dados$cultivar)
dados$cultivar<- gsub("MS INTA B 122", "MS INTA MDA BONAERENSE 122", dados$cultivar)
dados$cultivar<- gsub("MS INTA B 119", "MS INTA119", dados$cultivar)
dados$cultivar<- gsub("MS 817", "MS INTA BONAERENSE 817", dados$cultivar)
dados$cultivar<- gsub("MA INTA 122", "MS INTA MDA BONAERENSE 122", dados$cultivar)
dados$cultivar<- gsub("K. PROMETEO", "KLEINPROMETEO", dados$cultivar)
dados$cultivar<- gsub("KLEINPROMETEO", "KLEIN PROMETEO", dados$cultivar)
dados$cultivar<- gsub("HORNERO IS", "IS HORNERO", dados$cultivar)
dados$cultivar<- gsub("BOINTA 1006", "BIOINTA 1006", dados$cultivar)
dados$cultivar<- gsub("B AIMARA", "BUCK AIMARA", dados$cultivar)
dados$cultivar<- gsub("B PRETAL", "BUCK PRETAL", dados$cultivar)
dados$cultivar<- gsub("B 525", "BAGUETTE 525", dados$cultivar)
dados$cultivar<- gsub("610 BAGUETTE", "BAGUETTE 610", dados$cultivar)
dados$cultivar<- gsub("921", "ACA 921", dados$cultivar)
dados$cultivar<- gsub("920", "ACA 920", dados$cultivar)
dados$cultivar<- gsub("370502", "BUCK 370502", dados$cultivar)
dados$cultivar<- gsub("BUCK BUCK 370502", "BUCK 370502", dados$cultivar)
dados$cultivar<- gsub("603", "ACA 603", dados$cultivar)
dados$cultivar<- gsub("ACA ACA", "ACA", dados$cultivar)

#dados$cultivar<- gsub("460", "ACA 460", dados$cultivar)
# Remoção de dados  sem expecificação de cultivar
dados$cultivar<- trimws(dados$cultivar)
dados <- dados[complete.cases(dados$cultivar), ]

# Tipos de plantio 
dados$sistema_de_plantio <- str_to_upper(dados$sistema_de_plantio)
dados$sistema_de_plantio <- gsub("A MANO", "MANUAL", dados$sistema_de_plantio)

for(i in 1:nrow(dados)) {
  if (!is.na(dados$sistema_de_plantio[i]) && dados$sistema_de_plantio[i] != "MANUAL") {
    dados$sistema_de_plantio[i] <- "MECANICA"
  }
}

print(levels(as.factor(dados$sistema_de_plantio)))
# Correção Fungicida 
dados$fungicida <- gsub("CON FUNG.", "CON FUNGICIDA", dados$fungicida)
dados$fungicida <- gsub("CON FUNGICIDACIDA", "CON FUNGICIDA", dados$fungicida)
# Empresas
dados$empresa <- trimws(dados$empresa)
print(levels(as.factor(dados$empresa)))
dados$empresa <- str_to_upper(dados$empresa)

dados$empresa <- gsub("ASOCIADOS DON MARIO S.A.", "ASOCIADOS DON MARIO",dados$empresa)
dados$empresa <- gsub("ADM" , "ASOCIADOS DON MARIO",dados$empresa)
dados$empresa <- gsub("BIIOSEMINIS" , "BIOSEMINIS",dados$empresa)
dados$empresa <- gsub("LCD SEMILLAS" , "LDC SEMILLAS",dados$empresa)
dados$empresa <- gsub("LIMAGRAIN S.A.", "LIMAGRAIN",dados$empresa)

for(i in 1:nrow(dados)) {
  if(dados$empresa[i] == "" | is.na(dados$empresa[i])) {
    dados$empresa[i] <- "Não informado"
  }
}

for(i in 1:nrow(dados)) {
  if(dados$cultivo_antecesor[i] == "" | is.na(dados$cultivo_antecesor[i])) {
    dados$cultivo_antecesor[i] <- "Não informado"
  }
}

# Calculo rendimento médio
dados$rep_v <- NULL

for (i in 1:nrow(dados)) {
  if (!is.na(dados$rep_iv[i])) {
    dados$rendimento_medio[i] <- dados$rep_i[i] + dados$rep_ii[i] + dados$rep_iii[i] + dados$rep_iv[i]
  } else {
    dados$rendimento_medio[i] <- dados$rep_i[i] + dados$rep_ii[i] + dados$rep_iii[i]
  }
}

# Clima

pluviosidade <- dados%>%
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

temperatura <- dados%>%
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


#teste <- dados %>%
# select(-starts_with("chuvas_"), -starts_with("temp_")) 


#teste2 <-  left_join(teste,clima, by = c("ano", "subregiao_abrev"))
# Nº de observações por ano ----

frequencia_ano <- table(dados$ano)
quebras_y <- seq(0, max(frequencia_ano), by = 500)
barplot(frequencia_ano,
        col = "#ead693",
        ylim = c(0,2000),
        main = "Frequencia de observações\npor ano de plantio",
        xlab = "Ano",
        ylab = "Frequencia",
        border = "NA",
        yaxp = c(0, 2000, length.out = length(quebras_y))
)

# Análise univariada SEM separação por ano----
# Nº de cultivares
dados|>
  count(cultivar)|>
  nrow()

# Nº de empresas
dados|>
  count(empresa)|>
  nrow()


# Epoca 
frequencia_epoca <- table(dados$epoca)
quebras_y <- seq(0, max(frequencia_epoca), by = 250)
barplot(frequencia_epoca,
        col = "#ead693",
        ylim = c(0,1800),
        main = "Frequencia de observações por época de plantio",
        xlab = "Época",
        ylab = "Frequencia",
        border = "NA",
        yaxp = c(0, 1800, length.out = length(quebras_y)))
# Ciclo 
frequencia_ciclo <- table(dados$ciclo)
quebras_y <- seq(0, max(frequencia_ciclo), by = 250)
barplot(frequencia_ciclo,
        col = "#ead693",
        ylim = c(0,1800),
        main = "Frequencia de observações por ciclo de plantio",
        xlab = "Ciclo",
        ylab = "Frequencia",
        border = "NA",
        yaxp = c(0, 1800, length.out = length(quebras_y)))

# Fungicida 
frequencia_fungicida <- table(dados$fungicida)
quebras_y <- seq(0, max(frequencia_fungicida), by = 500)
barplot(frequencia_fungicida,
        col = "#ead693",
        ylim = c(0,3000),
        main = "Frequencia de observações por fungicida de plantio",
        xlab = "Fungicida",
        ylab = "Frequencia",
        border = "NA",
        yaxp = c(0, 3000, length.out = length(quebras_y))
        )
# Grupo de qualidade

frequencia_grupo_qualidade <- table(dados$grupo__qualidade)
quebras_y <- seq(0, max(frequencia_grupo_qualidade), by = 250)
barplot(frequencia_grupo_qualidade,
        col = "#ead693",
        ylim = c(0,1800),
        main = "Frequencia de observações \n por grupo de qualidade",
        xlab = "Grupo de qualidade",
        ylab = "Frequencia",
        border = "NA",
        yaxp = c(0, 1800, length.out = length(quebras_y))
)

# Regiao
frequencia_subregiao_abrev <- table(dados$subregiao_abrev)
quebras_y <- seq(0, max(frequencia_subregiao_abrev), by = 200)

barplot(frequencia_subregiao_abrev,
        col = "#ead693",
        ylim = c(0,1800),
        main = "Frequencia de observações por Sub-região de plantio",
        xlab = "Sub-região",
        ylab = "Frequencia",
        border = "NA",
        yaxp = c(0, 1800, length.out = length(quebras_y)))
        
# Sistema de plantio
frequencia_sistema_de_plantio <- table(dados$sistema_de_plantio)
quebras_y <- seq(0, max(frequencia_sistema_de_plantio), by = 500)
barplot(frequencia_sistema_de_plantio,
        col = "#ead693",
        ylim = c(0,5000),
        main = "Frequencia de observações por sistema de plantio",
        xlab = "Sistema de plantio",
        ylab = "Frequencia",
        border = "NA",
        yaxp = c(0, 5000, length.out = length(quebras_y)))

# Tipo de solo
dados$solo <- trimws(dados$solo)
dados$solo <- gsub("Típico","",dados$solo)
frequencia_solo <- table(dados$solo)
quebras_y <- seq(0, max(frequencia_solo), by = 200)
barplot(frequencia_solo,
        col = "#ead693",
        ylim = c(0,1800),
        main = "Frequencia de observações por solo de plantio",
        xlab = "Solo",
        ylab = "Frequencia",
        border = "NA",
        yaxp = c(0, 1800, length.out = length(quebras_y)))


# Análise univariada COM separação por ano----
# Empresas

contagens_empresa <- table(dados$empresa)

dados %>%
  mutate(empresa = factor(empresa, levels = names(sort(contagens_empresa, decreasing = F))))%>%
  ggplot() +
  aes(x = empresa) +
  geom_bar(fill = "#9d793a") +
  coord_flip() +
  meutema() +
  facet_wrap(vars(ano))


# Epoca
ggplot(dados) +
  aes(x = epoca) +
  geom_bar(fill = "#9d793a") +
  meutema() +
  facet_wrap(vars(ano), ncol = 3, nrow = 1)+
  labs(x = "Época",y = "Frequência")
# Ciclo
ggplot(dados) +
  aes(x = ciclo) +
  geom_bar(fill = "#9d793a") +
  meutema() +
  facet_wrap(vars(ano), ncol = 3, nrow = 1)+
  labs(x = "Ciclo",y = "Frequência")+
  theme(legend.text = element_text(angle = 45, hjust = 1))
# Fungicida
ggplot(dados) +
  aes(x = fungicida) +
  geom_bar(fill = "#9d793a") +
  meutema() +
  facet_wrap(vars(ano), ncol = 3, nrow = 1)+
  labs(x = "Fungicida",y = "Frequência")
# Grupo de qualidade
ggplot(dados) +
  aes(x = grupo__qualidade) +
  geom_bar(fill = "#9d793a") +
  meutema() +
  facet_wrap(vars(ano), ncol = 3, nrow = 1)+
  labs(x = "Grupo de qualidade",y = "Frequência")
# Regiao
ggplot(dados) +
  aes(x = subregiao_abrev) +
  geom_bar(fill = "#9d793a") +
  meutema() +
  facet_wrap(vars(ano), ncol = 1, nrow = 3)+
  labs(x = "Sub-região",y = "Frequência")
# Localidade
ggplot(dados) +
  aes(x = localidade) +
  geom_bar(fill = "#9d793a") +
  coord_flip() +
  meutema() +
  facet_wrap(vars(ano), ncol = 3, nrow = 1)+
  labs(x = "Localidade",y = "Frequência")
# Sistema de plantio
ggplot(dados) +
  aes(x = sistema_de_plantio) +
  geom_bar(fill = "#9d793a") +
  meutema() +
  facet_wrap(vars(ano), ncol = 3, nrow = 1)+
  labs(x = "Sistema de plantio",y = "Frequência")
# Cultivo antecessor

contagens_cultivo_antecessor <- table(dados$cultivo_antecesor)

dados%>%
  mutate(cultivo_antecesor = factor(cultivo_antecesor,
                                    levels = names(sort(contagens_cultivo_antecessor , decreasing = F))))%>%
  ggplot() +
  aes(x = cultivo_antecesor) +
  geom_bar(fill = "#9d793a") +
  coord_flip() +
  meutema() +
  facet_wrap(vars(ano), ncol = 3, nrow = 1)+
  labs(x = "Cultivo antecessor",y = "Frequência")


# Bivariada ------

#Variação do clima por ano

clima$mes <- str_to_upper(clima$mes)
clima$mes <- substr(clima$mes,1,3)

meses_ord <- c("JAN", "FEV","MAR",
               "ABR","MAI","JUN",
               "JUL","AGO","SET",
               "OUT","NOV","DEZ")
clima$mes <- factor(clima$mes, levels = meses_ord)

ggplot(clima) +
 aes(x = mes, y = pluviosidade, group = ano, color = "#9cc9d7") +
 geom_line() +
 theme_minimal() +
 facet_wrap(~subregiao_abrev+ano)

ggplot(clima) +
  aes(x = mes, y = temperatura, group = ano, color = "red") +
  geom_line() +
  theme_minimal() +
  facet_wrap(~subregiao_abrev+ano)


# ggplot(clima) +
#   geom_line(aes(x = mes, y = pluviosidade, group = ano, color = "Pluviosidade")) +
#   geom_line(aes(x = mes, y = temperatura, group = ano, color = "Temperatura")) +
#   scale_color_manual(values = c("Pluviosidade" = "#9cc9d7", "Temperatura" = "red")) +
#   scale_y_continuous(
#     name = "Pluviosidade", 
#     sec.axis = sec_axis(~., name = "Temperatura", 
#                         labels = function(x) paste0(x, "°C"))
#   ) +
#   theme_minimal() +
#   facet_wrap(~subregiao_abrev + ano)


# Variação do rendimento médio por empresa
ggplot(dados) +
  aes(x = empresa, y = rendimento_medio) +
  geom_boxplot(fill = "#9cc9d7") +
  coord_flip()+
  meutema() +
  facet_wrap(~ ano, nrow = 1)

# Tempo médio de espigamentos
# Tempo médio de maturação





```{r}


ggplot(dados) +
  aes(x = empresa, fill = grupo__qualidade, weight = ano) +
  geom_bar() +
  scale_fill_hue(direction = 1) +
  coord_flip() +
  theme_minimal()
dados%>%
  group_by(grupo__qualidade,subregiao_nome, ano)%>%
  summarise(media_rend = mean(rendimento_medio))%>%
  ggplot() +
  aes(x = grupo__qualidade, y = media_rend, colour = factor(ano)) +
  geom_point(shape = "circle", size = 1.5) +
  scale_color_discrete() +
  theme_minimal() +
  facet_wrap(vars(subregiao_nome))
```
# Tempo médio de espigamento
```{r}
duracao <- dados%>%
  group_by(ano, subregiao_abrev, empresa, grupo__qualidade)%>%
  summarise(media_dur_esp = mean(dias_do_plantio_a_espigacao),
            media_dur_maturacao = mean(dias_do_plantio_a_maturacao))

duracao <- duracao[duracao$media_dur_esp >= 0, , drop = FALSE]
duracao <- duracao[!is.na(duracao$media_dur_esp), , drop = FALSE]

ggplot(duracao) +
  aes(y = media_dur_esp, group = grupo__qualidade, fill = grupo__qualidade) +
  geom_boxplot(alpha = 0.5) +
  meutema() +
  facet_wrap(vars(ano),ncol = 1, nrow = 4)



```


# Tempo médio de maturação

```{r}

duracao <- dados%>%
  group_by(ano, subregiao_abrev, empresa, grupo__qualidade)%>%
  summarise(media_dur_esp = mean(dias_do_plantio_a_espigacao),
            media_dur_maturacao = mean(dias_do_plantio_a_maturacao))

duracao <- duracao[duracao$media_dur_maturacao >= 0, , drop = FALSE]
duracao <- duracao[!is.na(duracao$media_dur_maturacao), , drop = FALSE]
ggplot(duracao) +
  aes(y = media_dur_maturacao, group = grupo__qualidade, fill = grupo__qualidade) +
  geom_boxplot(alpha = 0.5) +
  meutema() +
  facet_wrap(vars(ano),ncol = 1, nrow = 4)
```


# Análise dos experimentos

```{r, ANOVA - Rendimento~Cultivar}
pesos <- 1 / table(dados$cultivar)[dados$cultivar]

modelo_anova <- aov(rendimento_medio ~ cultivar, data = dados, weights = pesos)

summary(modelo_anova)
```
```{r, ANOVA - Rendimento~Cultivar*Localidade}
pesos <- 1 / table(interaction(dados$cultivar, dados$localidade))[interaction(dados$cultivar, dados$localidade)]

modelo_anova <- aov(rendimento_medio ~ cultivar+localidade, data = dados, weights = pesos)

summary(modelo_anova)
```
```{r, ANOVA - Rendimento~Cultivar*Localidade*Ano}

pesos <- 1 / table(interaction(dados$cultivar, dados$localidade, dados$ano))[interaction(dados$cultivar, dados$localidade, dados$ano)]

modelo_anova <- aov(rendimento_medio ~ cultivar + localidade + ano, data = dados, weights = pesos)

summary(modelo_anova)

```


```{r, 2021}
dados_ano_2021 <- dados%>%
  filter(ano == "2021")

dados_ano_2022 <- dados%>%
  filter(ano == "2022")

dados_ano_2023 <- dados%>%
  filter(ano == "2023")

dados_2021_CORTOS <- dados_ano_2021%>%
  filter(ciclo == "CORTOS")

dados_2021_CORTOS_INTERMEDIOS <- dados_ano_2021%>%
  filter(ciclo == "CORTOS E INTERMEDIOS")

dados_2021_LARGOS_INTERMEDIOS <- dados_ano_2021%>%
  filter(ciclo == "LARGOS E INTERMEDIOS")

dados_2021_LARGOS <- dados_ano_2021%>%
  filter(ciclo == "LARGOS")

dados_2022_CORTOS <- dados_ano_2022%>%
  filter(ciclo == "CORTOS")

dados_2022_CORTOS_INTERMEDIOS <- dados_ano_2022%>%
  filter(ciclo == "CORTOS E INTERMEDIOS")

dados_2022_LARGOS_INTERMEDIOS <- dados_ano_2022%>%
  filter(ciclo == "LARGOS E INTERMEDIOS")

dados_2022_LARGOS <- dados_ano_2022%>%
  filter(ciclo == "LARGOS")

dados_2023_CORTOS <- dados_ano_2023%>%
  filter(ciclo == "CORTOS")

dados_2023_CORTOS_INTERMEDIOS <- dados_ano_2023%>%
  filter(ciclo == "CORTOS E INTERMEDIOS")

dados_2023_LARGOS_INTERMEDIOS <- dados_ano_2023%>%
  filter(ciclo == "LARGOS E INTERMEDIOS")

dados_2023_LARGOS <- dados_ano_2023%>%
  filter(ciclo == "LARGOS")

```


```{r}
pesos <- 1 / table(dados_2021_CORTOS$cultivar)[dados_2021_CORTOS$cultivar]

modelo_anova <- aov(rendimento_medio ~ cultivar, data = dados_2021_CORTOS, weights = pesos)

summary(modelo_anova)
```

]```{r}
dados_sf <- read_sf(dsn = "../Dados/mapa_subregiones_formato_gis_2021-06-19/Subregiones 2021.shp")

dados_sf |>
  dplyr::select(1:SUB_ANTER) |>
  sf::st_drop_geometry() |>
  dplyr::arrange(SUB_NUM)

ggplot(data = dados_sf) +
  geom_sf() +
  theme_minimal()

```


