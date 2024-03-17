################################################################################
######################### Limpeza e organização ################################
################################################################################
library(tidyverse)

# Leitura dos dados extraídos --------------------------------------------------

local <- readxl::read_xlsx("Dados/Dados_Brutos/trigo_metadatos.xlsx", sheet = "local", n_max = 51)
desenho <- readxl::read_xlsx("Dados/Dados_Brutos/trigo_metadatos.xlsx", sheet = "diseño", n_max = 50)
solo <- readxl::read_xlsx("Dados/Dados_Brutos/trigo_metadatos.xlsx", sheet = "suelo", n_max = 51)
antecessor <- readxl::read_xlsx("Dados/Dados_Brutos/trigo_metadatos.xlsx", sheet = "antecesor", n_max = 51)
semeadura <- readxl::read_xlsx("Dados/Dados_Brutos/trigo_metadatos.xlsx", sheet = "siembra", n_max = 51)
manejo <- readxl::read_xlsx("Dados/Dados_Brutos/trigo_metadatos.xlsx", sheet = "manejo", n_max = 51)
chuva <- readxl::read_xlsx("Dados/Dados_Brutos/trigo_metadatos.xlsx", sheet = "lluvia", n_max = 51)
temperatura <- readxl::read_xlsx("Dados/Dados_Brutos/trigo_metadatos.xlsx", sheet = "temperatura", n_max = 51)
rendimento <- readxl::read_xlsx("Dados/Dados_Brutos/trigo_rendimiento.xlsx", n_max = 3078)
datas <- readxl::read_xlsx("Dados/Dados_Brutos/trigo_datas.xlsx", n_max =  8139)
grupos <- read.csv("Dados/Dados_Brutos/grupos_qualidade.csv")
de_para <- read_csv("Dados/Dados_Brutos/de_para.csv")
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
unique(rendimento$cultivar)

#Organização nomenclatura dos cultivares por grupo --------------------

grupos$ano_class_qualidade <- NULL
grupos$titular <- NULL
grupos$NºRNC <- NULL
grupos$variedade <- trimws(grupos$variedade)
grupos$variedade <- gsub("ACA([0-9]+)", "ACA \\1", grupos$variedade)
grupos$variedade <- gsub("MSINTA", "MS INTA", grupos$variedade)
grupos$variedade <- str_to_upper(grupos$variedade)
grupos$variedade <- gsub("SY109", "SY109", grupos$variedade)
grupos$variedade <- gsub("SY200 ", "SY200", grupos$variedade)
grupos$cultivar <- gsub("DM.*", "", grupos$cultivar)
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
  group_by(cultivar)%>%
  count()



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
  filter(is.na(empresa))

unique(sem_empresa$cultivar)

#rendimento$cultivar <- gsub("ACA([0-9]+)", "ACA \\1", rendimento$cultivar)

# -------------------------------------------------------------------------
local <- local %>%
  mutate(filename = str_to_lower(filename)) %>%
  mutate(filename = str_replace(filename, "brutos", "Brutos"))%>%
  mutate(filename = str_replace(filename, "dados", "Dados"))%>%
  mutate(filename = str_replace(filename, "/dados", "/Dados"))


# for (i in nrow(datas):1) {
#   if (is.na(datas$fecha_de_siembra[i])) {
#     datas <- datas[-i, ]
#   }
# }
# grupos|>
#   writexl::write_xlsx("Dados/Dados_Brutos/trigo_grupos.xlsx")

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
dados <- left_join(rendimento, metadados, by = "filename")
dados <- right_join(dados, datas, by = c("filename", "cultivar", "epoca", "ciclo", "fungicida"))
# Organização da nomenclatura dos cultivares para identificação da empresa
# grupos$variedade <- gsub("MS INTA119", "MS INTA 119", grupos$variedade)
# grupos$variedade <- gsub("ACA 360 ", "ACA 360", grupos$variedade)
# dados$cultivar <- gsub("362", "ACA 362", dados$cultivar)
# dados$cultivar <- gsub("ACA 360 ", "ACA 360", dados$cultivar)
#dados$cultivar <- gsub("K.*", "KLEIN", dados$cultivar)
# dados$cultivar <- gsub("K *", "KLEIN", dados$cultivar)
# dados$cultivar <- gsub("KL *", "KLEIN", dados$cultivar)
# dados$cultivar <- gsub("KL.*", "KLEIN ", dados$cultivar)
# # dados$cultivar <- gsub("GINGKLEIN ", "GINGK", dados$cultivar)
# # dados$cultivar <- gsub("BUCKLEIN ", "BUCK", dados$cultivar)
# dados$cultivar <- gsub("LG ARLASKLEIN ", "LG ARLASK", dados$cultivar)
# dados$cultivar <- gsub("LAPACHO ", "LAPACHO", dados$cultivar)
#dados$cultivar <- gsub("B.", "BUCK", dados$cultivar)


# Organização nomenclatura -----------------------------------------------------
# nomes <- c("nome_doc",
#            "epoca",
#            "ciclo",                                 
#            "fungicida",                             
#            "cultivar",                              
#            "rep_i",                                 
#            "rep_ii",                                
#            "rep_iii",                               
#            "rep_iv",                                
#            "rep_v",                                 
#            "subregiao",             
#            "localidade",                             
#            "coordenador_a",                         
#            "colaborador_a",                         
#            "subregiao_abrev",                               
#            "subregiao_nome",                              
#            "desenho_experimental",        
#            "num_total_cultivares_intervinientes",
#            "num_total_de_parcelas_por_ensaio",   
#            "comprimento_medio_m",                      
#            "largura_m",                               
#            "distancia_entre_fileras_cm",            
#            "numero_de_fileras",                     
#            "solo",                                 
#            "materia_organica_percentual",              
#            "nitrogeno_ppm",                         
#            "fosforo_ppm",                           
#            "potasio_ppm",                           
#            "tipo",                                  
#            "textura",                               
#            "nan_mg_kg",                             
#            "cultivo_antecesor",                     
#            "densidad_sementes_m2",                  
#            "densidad_sementes_kg_ha",               
#            "sistema",                               
#            "semeadura",                               
#            "uso_de_fertilizante",                   
#            "uso_de_irrigacao",                          
#            "uso_de_herbicida",                      
#            "uso_de_fungicida",                      
#            "uso_de_insecticida",                    
#            "uso_de_outros_produtos",                
#            "chuvas_janeiro",                               
#            "chuvas_fevereiro",                             
#            "chuvas_marco",                               
#            "chuvas_abril",                               
#            "chuvas_maio",                                
#            "chuvas_junho",                               
#            "chuvas_julho",                               
#            "chuvas_agosto",                              
#            "chuvas_setembro",                           
#            "chuvas_outubro",                             
#            "chuvas_novembro",                           
#            "chuvas_dezembro",                           
#            "temp_janeiro",                               
#            "temp_fevereiro",                             
#            "temp_marco",                               
#            "temp_abril",                               
#            "temp_maio",                                
#            "temp_junho",                               
#            "temp_julho",                             
#            "temp_agosto",                           
#            "temp_setembro",                           
#            "temp_outubro",                           
#            "temp_novembro",                          
#            "temp_dezembro") 
# colnames(dados) <- nomes
# dados$semeadura <- NULL

# Padronização dos valores das tuplas de utilização de fungicida ---------------

for(i in 1:nrow(dados)){
  if(dados$fungicida[i] == "SIN FUNG."){
    dados$fungicida[i] <- "SIN FUNGICIDA"
  }
}

# Dados de-para nomes de cultivares --------------------------------------------
dados$cultivar <- gsub("\\b(\\w{1,3}|\\d)\\s", "\\1", dados$cultivar, perl = TRUE)

de_para <- data.frame("De" = c("B450", "BAG450",
                               "B550", "BAG550",
                               "B620", "BAG620",
                               "B680","BAG680",
                               "B750", "BAG750",
                               "B820","BAG820",
                               "BBRAVIOCL2",
                               "BCOLIH",
                               "BCUME" ,
                               "BDEST",
                               "BG620" ,
                               "BGTTE 620",
                               "BG680" ,
                               "BG750",
                               "BIOINTA 1006\r\n1006",
                               "BIOINTA 1008\r\n1008",
                               "BMET" ,
                               "BMUT",
                               "BPEREGR",
                               "BUCK AMANCAY\r\nAMANCAY",
                               "BUCK BRAVIO",
                               "BUCK FULGOOR" , "BUCK FULGOR\r\nFULGOR",
                               "DMCEIBO",
                               "DMTBIO AUDAZ",
                               "GINGKO", 
                               "GUAYAVO","GUYABO", 
                               "ISHORNERO\r\nHORNERO",
                               "K.100ANOS", "K.CIEN ANOS", "KLEIN 100ANOS", "KLEIN CIEN AA'OS" ,"KLEIN CIEN ANOS",
                               "KLFAVORI" ,
                               "KLLIEBRE",
                               "KLNUTR",
                               "KLNUTRIA",
                               "KLPROME",
                               "KLTITANCL",
                               "LGWA-11-01" ,"LGWA11" ,"LGWA11 (PAM)", "LGWA11 PAMPERO", "LGWA11-0169" ,"LGWA11-0169 (PAMPERO)",
                               "MSINTA B. 817" , "MSINTA B817", "MSINTA BON817",
                               "MSINTA MDABONAERENSE 122", "MSMB122","MSINTA MDABONAERENSE 221",
                               "TBIOAUDAZ",
                               "TUCELITE 17" ,
                               "TUCELITE 43"),
           
           "Para" = c("BAGUETTE 450","BAGUETTE 450",
                      "BAGUETTE 550","BAGUETTE 550",
                      "BAGUETTE 620","BAGUETTE 620",
                      "BAGUETTE 680","BAGUETTE 680",
                      "BAGUETTE 750","BAGUETTE 750",
                      "BAGUETTE 820","BAGUETTE 820",
                      "BBRAVIO CL2",
                      "BCOLIHUE" ,
                      "BCUMELEN",
                      "BDESTELLO",
                      "BAGUETTE 620","BAGUETTE 620",
                      "BAGUETTE 680",
                      "BAGUETTE 750",
                      "BIOINTA 1006",
                      "BIOINTA 1008",
                      "BMETEORO", 
                      "BMUTISIA", "BPEREGRINO","BUCK AMANCAY", "BUCK BRAVIO CL2", "BUCK FULGOR" , "BUCK FULGOR","CEIBO","DMTBIO\r\nAUDAZ", "GINKO", "GUAYABO","GUAYABO","ISHORNERO", "K. CIEN ANOS" ,"K. CIEN ANOS" ,"K. CIEN ANOS", "K. CIEN ANOS" ,"K. CIEN ANOS" , "KLFAVORITO", "KLIEBRE",  "KNUTRIA"  , "KNUTRIA" , "KLPROMETEO","KLTITAN CL", 
                      "LGWA11 PAMPERO","LGWA11 PAMPERO","LGWA11 PAMPERO","LGWA11 PAMPERO","LGWA11 PAMPERO","LGWA11 PAMPERO", "MSINTA 817" ,"MSINTA 817" ,"MSINTA 817" ,"MSINTA BONAERENSE 122","MSINTA BONAERENSE 122","MSINTA BONAERENSE 221" ,"TBIO AUDAZ" , "TUCELITTE 17", "TUCELITTE 43"))
dados <- left_join(dados, de_para, by = c("cultivar" = "De"))

for(i in 1:nrow(dados)){
  if( !is.na(dados$Para[i])){
    dados$cultivar[i] <- dados$Para[i]
  }
}

# Padronização da nomenclatura de desenho experimental -------------------------

dados$desenho_experimental <- ifelse(dados$desenho_experimental != "Latice", "DBCA", dados$desenho_experimental)
dados$Para <- NULL
dados$subregiao <- NULL
# Padronização da nomenclatura do solo
dados$solo <- str_to_title(dados$solo) 
levels(as.factor(dados$solo))

for (i in 1:nrow(dados)) {
  if (!is.na(dados$solo[i])) {
    if (dados$solo[i] == "Argiudol Tipico" | 
        dados$solo[i] == "Clase 1") {
      dados$solo[i] <- "Argiudol Típico"
    } else if (dados$solo[i] == "Haplustol Tipico") {
      dados$solo[i] <- "Haplustol Típico"
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
    if (dados$textura[i] == "Fraco Limoso" | 
        dados$textura[i] == "FrancoLimosa") {
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

# FINALIZAR
#Verificações ---------------------
#levels(as.factor(dados$densidad_sementes_m2))
#levels(as.factor(dados$densidad_sementes_kg_ha))
#levels(as.factor(dados$sistema))
# Extração do ano

write.csv(dados, file = "Dados/Dados_processados/RET_ARG.csv")
