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

local <- local %>%
  mutate(filename = str_to_lower(filename)) %>%
  mutate(filename = str_replace(filename, "brutos", "Brutos"))%>%
  mutate(filename = str_replace(filename, "dados", "Dados"))%>%
  mutate(filename = str_replace(filename, "/dados", "/Dados"))

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
           "subregiao",             
           "localidade",                             
           "coordenador_a",                         
           "colaborador_a",                         
           "subregiao_abrev",                               
           "subregiao_nome",                              
           "desenho_experimental",        
           "num_total_cultivares_intervinientes",
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
           "sistema",                               
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
           "temp_dezembro") 
colnames(dados) <- nomes
dados$semeadura <- NULL

# Padronização dos valores das tuplas

for(i in 1:nrow(dados)){
  if(dados$fungicida[i] == "SIN FUNG."){
    dados$fungicida[i] <- "SIN FUNGICIDA"
  }
}

dados$cultivar <- gsub("\\b(\\w{1,3}|\\d)\\s", "\\1", dados$cultivar, perl = TRUE)
#-----------
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

# -----------------------
dados <- left_join(dados, de_para, by = c("cultivar" = "De"))

for(i in 1:nrow(dados)){
  if( !is.na(dados$Para[i])){
    dados$cultivar[i] <- dados$Para[i]
  }
}

dados$Para <- NULL
dados$subregiao <- NULL

levels(as.factor(dados$desenho_experimental))

write.csv(dados, file = "Dados/Dados_processados/RET_ARG.csv")
