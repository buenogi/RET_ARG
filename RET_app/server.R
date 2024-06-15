library(leaflet)
library(sf)
library(tidyverse)
library(dplyr)
source("Global.R")

dados <- read.csv("RET_ARG_PT.csv")

# Extração de coordenadas 

caminho_arquivo <- "Subregiones 2021.shp"
dados_completos <- extracao_coordenadas(dados, caminho_arquivo)

server <- function(input, output) {
  
  output$empresa_ui <- renderUI({
    selectizeInput(
      inputId = "empresa_selecionada", 
      label = "Selecione a empresa:", 
      choices = get_opcoes_ano_empresa(dados_completos, input$ano, input$grupo_qualidade),
      options = list(
        searchField = TRUE, # Habilitar busca instantânea
        filterOptions = list(
          casesensitive = FALSE # Filtro não case-sensitive
        ),
        multiple = F
    ))
  })
  
  output$cultivar_ui <- renderUI({
    selectizeInput(
      inputId = "cultivar_selecionado", 
      label = "Selecione o cultivar:", 
      choices = get_opcoes_cultivar(dados_completos,input$ano, input$grupo_qualidade, input$selecionar_empresa, input$selecionar_localidade, input$empresa_selecionada, input$localidade_selecionada),
      options = list(
        searchField = TRUE, # Habilitar busca instantânea
        filterOptions = list(
          casesensitive = FALSE # Filtro não case-sensitive
        ),
        multiple = F
      ))
  })
  
  output$localidade_ui <- renderUI({
    selectizeInput(
      inputId = "localidade_selecionada", 
      label = "Selecione a localidade:", 
      choices = get_opcoes_localidade(dados_completos,input$ano, input$grupo_qalidade,input$selecionar_localidade, input$empresa_selecionada),
      options = list(
        searchField = TRUE, # Habilitar busca instantânea
        filterOptions = list(
          casesensitive = FALSE # Filtro não case-sensitive
        ),
        multiple = F
      ))
  })
  
  
  observe({
    input$ano
    input$grupo_qualidade
    
    #opcoes_empresas <-  get_opcoes_ano_empresa(dados_completos, input$ano, input$grupo_qualidade)
    #opcoes_cultivares <- get_opcoes_cultivar(dados_completos,input$ano, input$grupo_qualidade, input$selecionar_empresa, input$selecionar_localidade, input$empresa_selecionada, input$localidade_selecionada)
    #opcoes_localidade <- get_opcoes_localidade(dados_completos,input$ano, input$grupo_qalidade,input$selecionar_localidade, input$empresa_selecionada)
    #updateSelectizeInput(inputId = "empresa_selecionada", choices = opcoes_empresas)
    #updateSelectizeInput(inputId = "cultivar_selecionado", choices = opcoes_cultivares)
    #updateSelectizeInput(inputId = "localidade_selecionada", choices = opcoes_localidade)
    
    #dados, ano, grupo__qualidade, selecionar_empresa, selecionar_localidade, empresa_selecionada, localidade_selecionada
  })
  
  
  dados_filtrados <- eventReactive(input$filtrar, {
    dados_filtrados <- dados_completos %>%
      filter(
        ano == input$ano &
          grupo__qualidade == input$grupo_qualidade &
          empresa == input$empresa_selecionada  &
          cultivar == input$cultivar_selecionado &
          subregiao_nome == input$localidade_selecionada)
      
  })
  
  output$table <- renderDataTable({
    dados_filtrados()
  })
  
  
 

}
