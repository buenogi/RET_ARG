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
  
  
# Filtragem  
  
  observe(input$filtro)
  
  dados_filtrados <- eventReactive(input$filtrar, {
    cat(file = stderr(), input$selecao, "\n")
    if (input$filtro == "empresa") {
      
      dados_filtrados <- filtroSelecao(dados_completos, input$filtro, input$selecao_empresa)
    } else if (input$filtro == "localidade") {
      dados_filtrados <- filtroSelecao(dados_completos, input$filtro, input$selecao_localidade)
    } else {
      dados_filtrados <- filtroSelecao(dados_completos, input$filtro, input$selecao_cultivar)
    }
    return(dados_filtrados)
  })
  
  
  output$table <- renderDataTable({
    dados_filtrados()
  })
}
