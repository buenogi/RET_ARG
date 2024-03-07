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


# Organização nomenclarura -----------------------------------------------------
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
           "largo_promedio_m",                      
           "ancho_m",                               
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
