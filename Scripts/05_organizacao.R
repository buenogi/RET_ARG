################################################################################
######################### Limpeza e organização ################################
################################################################################
library(tidyverse)

# Leitura dos dados extraídos --------------------------------------------------

local <- readxl::read_xlsx("Dados/Dados_Brutos/extraidos/trigo_metadatos.xlsx", sheet = "local", n_max = 51)
desenho <- readxl::read_xlsx("Dados/Dados_Brutos/extraidos/trigo_metadatos.xlsx", sheet = "diseño", n_max = 50)
solo <- readxl::read_xlsx("Dados/Dados_Brutos/extraidos/trigo_metadatos.xlsx", sheet = "suelo", n_max = 51)
antecessor <- readxl::read_xlsx("Dados/Dados_Brutos/extraidos/trigo_metadatos.xlsx", sheet = "antecesor", n_max = 51)
semeadura <- readxl::read_xlsx("Dados/Dados_Brutos/extraidos/trigo_metadatos.xlsx", sheet = "siembra", n_max = 51)
manejo <- readxl::read_xlsx("Dados/Dados_Brutos/extraidos/trigo_metadatos.xlsx", sheet = "manejo", n_max = 51)
chuva <- readxl::read_xlsx("Dados/Dados_Brutos/extraidos/trigo_metadatos.xlsx", sheet = "lluvia", n_max = 51)
temperatura <- readxl::read_xlsx("Dados/Dados_Brutos/extraidos/trigo_metadatos.xlsx", sheet = "temperatura", n_max = 51)
rendimento <- readxl::read_xlsx("Dados/Dados_Brutos/extraidos/trigo_rendimiento.xlsx", n_max = 5670)
datas <- readxl::read_xlsx("Dados/Dados_Brutos/extraidos/trigo_datas.xlsx", n_max =  8139)
grupos <- read.csv("Dados/Dados_Brutos/grupos_qualidade.csv")
de_para <- read_csv("Dados/Dados_Brutos/de_para.csv", show_col_types = FALSE)
de_para <- unique(de_para)
de_para$Para <- trimws(de_para$Para)

# Padronização da nomenclatura de cultivares em rendimento -------------

rendimento$cultivar <- trimws(rendimento$cultivar)|>
  str_to_upper()

rendimento <- left_join(rendimento, de_para, by = c("cultivar" = "De"))

for(i in 1:nrow(rendimento)){
  if( !is.na(rendimento$Para[i])){
    rendimento$cultivar[i] <- rendimento$Para[i]
  }
}

rendimento$cultivar <- gsub("SY109", "SY109", rendimento$cultivar)
rendimento$cultivar <- gsub("SY200 ", "SY200", rendimento$cultivar)
rendimento$cultivar <- gsub("DM.*", "", rendimento$cultivar)
rendimento$cultivar <- gsub("\\b\\w+\\K\\r\\nCL", "", rendimento$cultivar, perl = TRUE)


remove_last_repeated_word <- function(text) {
  gsub("\\b(\\w+)\\s+\\1\\b", "\\1", text, perl = TRUE)
}

rendimento$cultivar <- sapply(rendimento$cultivar, remove_last_repeated_word)

unique(rendimento$cultivar)

# Organização nomenclatura dos cultivares por grupo --------------------

grupos$ano_class_qualidade <- NULL
grupos$titular <- NULL
grupos$NºRNC <- NULL
grupos$variedade <- trimws(grupos$variedade)
grupos$variedade <- gsub("ACA([0-9]+)", "ACA \\1", grupos$variedade)
grupos$variedade <- gsub("MSINTA", "MS INTA", grupos$variedade)
grupos$variedade <- str_to_upper(grupos$variedade)
grupos$variedade <- gsub("SY109", "SY109", grupos$variedade)
grupos$variedade <- gsub("SY200 ", "SY200", grupos$variedade)
grupos$cultivar <- gsub("DM.*", "", grupos$variedade)
grupos <- unique(grupos)
# Identificação dos cultivares que precisam de correção:
# dados <- left_join(rendimento, grupos, by = c("cultivar" = "variedade"))
# a <- dados %>%
#   filter(is.na(empresa)) %>%
#   group_by(filename)
# sem_empresa <- unique(a$cultivar)
# print(sem_empresa)
# sem_empresa <- as.data.frame(sem_empresa)
# corrigido <- as.data.frame(sem_empresa)
# write_csv(corrigido, "Dados/Dados_Brutos/corrigido.csv")
# corrigido <- read.csv("Dados/Dados_Brutos/corrigido_de_para.csv")
# corrigido <- corrigido[-c(43),]
# corrigido <- as.data.frame(corrigido)
# corrigido <- corrigido[-c(94),]
# corrigido <- as.data.frame(corrigido)
# corrigido <- corrigido[-c(93),]
# corrigido <- as.data.frame(corrigido)
# corrigido <- corrigido[-c(95), ]
# sem_empresa<- as.data.frame(sem_empresa)
# 
# #sem_empresa <- sem_empresa[-1,]
# 
# empresas_corrigidas <- as.data.frame(cbind(sem_empresa,corrigido))
# 
# write_csv( empresas_corrigidas,"Dados/Dados_Brutos/de_para_empresas2.csv")
# Correção
dados  <- left_join(rendimento, grupos, by = c("cultivar" = "variedade" ))

unique(dados$cultivar)

sem_empresa <- dados%>%
  filter(is.na(empresa))%>%
  # group_by(cultivar)%>%
  # count()



# for(i in 1:nrow(dados)){
#   if( !is.na(dados$CORRIGIDO[i])){
#     dados$cultivar[i] <- dados$CORRIGIDO[i]
#   }
# }
# 
# dados$CORRIGIDO <- NULL
# dados$Para <- NULL

# Identificação da empresa

dados <- left_join(dados, grupos, by = c("cultivar" = "variedade"))

sem_empresa <- dados%>%
  filter(is.na(empresa))%>%
  group_by(cultivar)%>%
  count()

sem_empresa2 <- dados%>%
  filter(is.na(empresa))%>%
  group_by(cultivar)%>%
  count()

unique(sem_empresa$cultivar)
# Remoção de cultivares sem identificação e de extração inadequada:
for(i in nrow(dados):1) {
  if(is.na(dados$cultivar[i]) || dados$cultivar[i] == "CV%" ||
     dados$cultivar[i] == "PROMEDIO" || dados$cultivar[i] == "MAXIMO" ||
     dados$cultivar[i] == "MINIMO" || dados$cultivar[i] == "") {
    dados <- dados[-i, ]
  }
}

dados$Para <- NULL
dados$cultivar.y <- NULL

# Correção na planilha de local --------------------------------------------------
local <- local %>%
  mutate(filename = str_to_lower(filename)) %>%
  mutate(filename = str_replace(filename, "brutos", "Brutos"))%>%
  mutate(filename = str_replace(filename, "dados", "Dados"))%>%
  mutate(filename = str_replace(filename, "/dados", "/Dados"))

# Correção da nomenclatura dos cultivares em Datas ------------------

datas$cultivar <- trimws(datas$cultivar)|>
  str_to_upper()

datas <- left_join(datas, de_para, by = c("cultivar" = "De"))

for(i in 1:nrow(datas)){
  if( !is.na(datas$Para[i])){
    datas$cultivar[i] <- datas$Para[i]
  }
}

datas$cultivar <- gsub("SY109", "SY109", datas$cultivar)
datas$cultivar <- gsub("SY200 ", "SY200", datas$cultivar)
datas$cultivar <- gsub("DM.*", "", datas$cultivar)
datas$cultivar <- gsub("\\b\\w+\\K\\r\\nCL", "", datas$cultivar, perl = TRUE)


remove_last_repeated_word <- function(text) {
  gsub("\\b(\\w+)\\s+\\1\\b", "\\1", text, perl = TRUE)
}

datas$cultivar <- sapply(datas$cultivar, remove_last_repeated_word)

datas <- unique(datas)
datas$Para <- NULL
dados$cultivar.y <- NULL
dados <- unique(dados)
datas <- unique(datas)

dados <- right_join(dados, datas, by = c("filename", "cultivar", "epoca", "ciclo", "fungicida"))

# União dos dados e metadados -------------------------------------------------
metadados <- left_join(local, desenho)
metadados <- left_join(metadados, solo)
metadados <- left_join(metadados,antecessor)
metadados <- left_join(metadados,semeadura)
metadados <- left_join(metadados,manejo)
metadados <- left_join(metadados,chuva)
metadados <- left_join(metadados,temperatura, by = "filename")
metadados <- metadados%>%
  mutate(filename = str_remove(filename, ".xlsx"))
dados <- left_join(dados, metadados, by = "filename")

# Padronização dos valores das tuplas de utilização de fungicida ---------------

for(i in 1:nrow(dados)){
  if(dados$fungicida[i] == "SIN FUNG."){
    dados$fungicida[i] <- "SIN FUNGICIDA"
  }
}

# Padronização da nomenclatura de desenho experimental -------------------------

dados$diseno_estadistico_del_ensayos <- ifelse(dados$diseno_estadistico_del_ensayos != "Latice", "DBCA", dados$diseno_estadistico_del_ensayos)
dados$Para <- NULL
dados$subregiao <- NULL
# Padronização da nomenclatura do solo ----------------------
dados$suelo <- str_to_title(dados$suelo) 
levels(as.factor(dados$suelo))

for (i in 1:nrow(dados)) {
  if (!is.na(dados$suelo[i])) {
    if (dados$suelo[i] == "Argiudol Tipico" | 
        dados$suelo[i] == "Clase 1") {
      dados$suelo[i] <- "Argiudol Típico"
    } else if (dados$suelo[i] == "Haplustol Tipico") {
      dados$suelo[i] <- "Haplustol Típico"
    }
  }
}

# Padronização do tipo de solo ----------------
dados$tipo <- str_to_title(dados$tipo)
dados$tipo <- str_replace(dados$tipo,"Tipico", "Típico")
dados$tipo <- str_replace(dados$tipo,"Pintos.", "Pintos")


for (i in 1:nrow(dados)) {
  if (!is.na(dados$tipo[i])) {
    if (dados$tipo[i] == "Argiudol  Típico (50% Suelo Principal. Limitante: Somero)" | 
        dados$tipo[i] == "Argiudol Típico, Serie Maciel") {
      dados$tipo[i] <- "Argiudol Típico"
}}}

# Padronização da textura -----------------

dados$textura <- str_to_title(dados$textura)
dados$textura <-str_remove_all(dados$textura, "-")

for (i in 1:nrow(dados)) {
  if (!is.na(dados$textura[i])) {
    if (dados$textura[i] == "Fraco" | 
        dados$textura[i] == "Franca") {
      dados$textura[i] <- "Franco"
    }}}

for (i in 1:nrow(dados)) {
  if (!is.na(dados$textura[i])) {
    if (dados$textura[i] == "ArenosoFranco" | 
        dados$textura[i] == "Franco Arenosa") {
      dados$textura[i] <- "Franco Arenoso"
    }}}

for (i in 1:nrow(dados)) {
  if (!is.na(dados$textura[i])) {
    if (dados$textura[i] == "Fraco Limoso" ||
        dados$textura[i] == "FrancoLimosa"|| 
        dados$textura[i] == "Franco  Limosp" ) {
      dados$textura[i] <- "Franco Limoso"
    }}}

for (i in 1:nrow(dados)) {
  if (!is.na(dados$nitrogeno_ppm[i])) {
    if (dados$nitrogeno_ppm[i] == "0-20: 5,8") {
      dados$nitrogeno_ppm[i] <- "5,8"}
    else if(dados$nitrogeno_ppm[i] == "0,11; 39"){
      dados$nitrogeno_ppm[i] <- "39"}
    else if(dados$nitrogeno_ppm[i] == "11,9 (No3)"){
      dados$nitrogeno_ppm[i] <-  "11,9"
    }}}

# Padronização do cultivo antecessor -------------

dados$cultivo_antecesor <- str_replace(dados$cultivo_antecesor,"Maiz", "Maíz")
dados$cultivo_antecesor <- str_replace(dados$cultivo_antecesor,"Soja 1°", "Soja")

# Extração do ano---------------------------------------------------------------
dados$ano <- NA
for(i in 1:nrow(dados)){
  dados$ano[i] <- as.numeric(gsub("^.*\\/(\\d{4})-\\d{4}\\/.*$", "\\1", dados$filename[i]))

}

# Organização nomenclatura -----------------------------------------------------
nomes <- c("nome_doc",
           "epoca",
           "ciclo",
           "fungicida",
           "cultivar",
           "rep_i",
           "rep_ii",
           "rep_iii",
           "rep_iv",
           "rep_v",
           "empresa",
           "tipo_do_ciclo",
           "inicio_ret",
           "grupo__qualidade",
           "data_de_plantio",
           "data_de_espigacao",
           "data_de_maturacao",
           "dias_do_plantio_a_espigacao",
           "dias_da_espigacao_a_maturacao",
           "dias_do_plantio_a_maturacao",
           "data_de_emergencia",
           "subregiao",
           "localidade",
           "coordenador_a",
           "colaborador_a",
           "subregiao_abrev",
           "subregiao_nome",
           "desenho_experimental",
           "num_total_cultivares_testados",
           "num_total_de_parcelas_por_ensaio",
           "comprimento_medio_m",
           "largura_m",
           "distancia_entre_fileras_cm",
           "numero_de_fileras",
           "solo",
           "materia_organica_percentual",
           "nitrogeno_ppm",
           "fosforo_ppm",
           "potasio_ppm",
           "tipo",
           "textura",
           "nan_mg_kg",
           "cultivo_antecesor",
           "densidad_sementes_m2",
           "densidad_sementes_kg_ha",
           "sistema_de_plantio",
           "semeadura",
           "uso_de_fertilizante",
           "uso_de_irrigacao",
           "uso_de_herbicida",
           "uso_de_fungicida",
           "uso_de_insecticida",
           "uso_de_outros_produtos",
           "chuvas_janeiro",
           "chuvas_fevereiro",
           "chuvas_marco",
           "chuvas_abril",
           "chuvas_maio",
           "chuvas_junho",
           "chuvas_julho",
           "chuvas_agosto",
           "chuvas_setembro",
           "chuvas_outubro",
           "chuvas_novembro",
           "chuvas_dezembro",
           "temp_janeiro",
           "temp_fevereiro",
           "temp_marco",
           "temp_abril",
           "temp_maio",
           "temp_junho",
           "temp_julho",
           "temp_agosto",
           "temp_setembro",
           "temp_outubro",
           "temp_novembro",
           "temp_dezembro",
           "ano")
colnames(dados) <- nomes
dados$semeadura <- NULL
dados$tipo_do_ciclo <- NULL
dados$data_de_emergencia <- NULL
dados$nome_doc <- NULL
dados$coordenador_a <- NULL
dados$colaborador_a <- NULL
dados$subregiao <- NULL

# Formatação das datas e cálculos temporais
dados$data_de_plantio <- ymd(dados$data_de_plantio)
dados$data_de_espigacao <- ymd(dados$data_de_espigacao)
dados$data_de_maturacao <- ymd(dados$data_de_maturacao)

dados <- dados |> 
  mutate(data_de_plantio = format(data_de_plantio, "%d-%m-%Y"))|> 
  mutate(data_de_espigacao = format(data_de_espigacao, "%d-%m-%Y")) |> 
  mutate(data_de_maturacao = format(data_de_maturacao, "%d-%m-%Y"))

# Contagem dos dias

for (i in 1:nrow(dados)) {
  dados$dias_do_plantio_a_espigacao[i] <- time_length(as.period(interval(dmy(dados$data_de_plantio[i]), dmy(dados$data_de_espigacao[i]))),unit="days")
  dados$dias_da_espigacao_a_maturacao[i] <- time_length(as.period(interval(dmy(dados$data_de_espigacao[i]), dmy(dados$data_de_maturacao[i]))),unit="days")
  dados$dias_do_plantio_a_maturacao[i] <- time_length(as.period(interval(dmy(dados$data_de_plantio[i]), dmy(dados$data_de_maturacao[i]))),unit="days")
}

write.csv(dados, file = "Dados/Dados_processados/RET_ARG.csv")
 