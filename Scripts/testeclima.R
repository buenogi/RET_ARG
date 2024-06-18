library(fmsb)
dados

# Clima
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

selecionado <- clima%>%
  filter( localidade =="Miramar")

matriz_clima <- t(as.matrix(selecionado[,c("temperatura")]))

# colnames(matriz_clima) <- matriz_clima[1,]
# matriz_clima <- matriz_clima[-1,]
matriz_clima <- matrix(matriz_clima, ncol = 12, byrow = TRUE)

colnames(matriz_clima) <- c("Janeiro", "Fevereiro","Março",
                            "Abril", "Maio", "Junho", "Julho",
                            "Agosto", "Setembro","Outubro","Novembro","Dezembro")
dados_clima <- as.data.frame(matriz_clima)
dados_clima <- rbind(rep(max(dados_clima),12),rep(min(dados_clima),12),dados_clima)
colnames(dados_clima) <- c("Janeiro", "Fevereiro","Março",
                            "Abril", "Maio", "Junho", "Julho",
                            "Agosto", "Setembro","Outubro","Novembro","Dezembro")
library(fmsb)
radarchart(dados_clima)

