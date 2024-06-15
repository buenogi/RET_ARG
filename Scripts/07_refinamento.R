# Refinamento

dados <- read.csv("Dados/Dados_processados/RET_ARG.csv")

# Remoção de dados nulos


dados$rep_v <- NULL
dados <- dados %>% filter(!is.na(rep_i))

# TRadução dos grupos de qualidade
dados$grupo__qualidade <- gsub("GRUPO DE CALIDAD 1", "Grupo de qualidade 1",
                               dados$grupo__qualidade)
dados$grupo__qualidade <- gsub("GRUPO DE CALIDAD 2", "Grupo de qualidade 2",
                               dados$grupo__qualidade)
dados$grupo__qualidade <- gsub("GRUPO DE CALIDAD 3", "Grupo de qualidade 3",
                               dados$grupo__qualidade)
dados$grupo__qualidade <- ifelse(is.na(dados$grupo__qualidade), "Não informado", dados$grupo__qualidade)


# Padronização da nomenclatura das empresas 

dados$empresa <- gsub("S.A.", "", dados$empresa)
dados$empresa <- trimws(dados$empresa)
dados$empresa <- str_to_upper(dados$empresa)
dados$empresa <- gsub("LCD", "LDC", dados$empresa)
dados$empresa <- ifelse(is.na(dados$empresa), "Não informado", dados$empresa)
dados$empresa <- ifelse(dados$empresa == "", "Não informado", dados$empresa)

write.csv(dados, file = "Dados/Dados_processados/RET_ARG_PT.csv")
