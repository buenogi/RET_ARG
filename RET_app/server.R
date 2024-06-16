library(leaflet)
library(sf)
library(tidyverse)
library(dplyr)
library(plotly)
source("Global.R")

dados <- read.csv("RET_ARG_PT.csv")

# Extração de coordenadas  -----------------------------------------------------

caminho_arquivo <- "Subregiones 2021.shp"
dados_completos <- extracao_coordenadas(dados, caminho_arquivo)
shapefile <- read_sf(dsn = caminho_arquivo)

server <- function(input, output,session) {
  
  
# Filtragem  -------------------------------------------------------------------
  
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
    dados_filtrados <- CalcRendimento(dados_filtrados)
    return(dados_filtrados)
  })

  #observe(input$selecao_empresa)
  #observe(input$selecao_localidade)
  #observe(input$selecao_cultivar)
  

  
# Outputs -----------------
  output$table <- renderDataTable({
    dados_filtrados()
  })
  
  output$plot1 <- renderPlotly({
    cat(file = stderr(), input$filtro, "\n")
    
    if(input$filtro == "cultivar"){
      caterpilar_plot <- dados_filtrados() %>%
        select(ano, fungicida, localidade, rendimento_medio, ciclo, grupo__qualidade) %>%
        group_by(localidade,grupo__qualidade) %>%
        summarise(
          mean_rendimento = mean(rendimento_medio, na.rm = TRUE),
          sd_rendimento = sd(rendimento_medio, na.rm = TRUE)
        ) %>%
        ungroup() %>%
        ggplot(aes(x = reorder(localidade, mean_rendimento, decreasing = F), y = mean_rendimento,
                   color = grupo__qualidade)) +
        geom_point() +
        geom_errorbar(aes(ymin = mean_rendimento - sd_rendimento, ymax = mean_rendimento + sd_rendimento), width = 0.5) +
        coord_flip() +
        labs(x = "Localidade", y = "Rendimento médio (ton/ha)", color = "Grupo de qualidade") +
        meutema()+
        theme(legend.position = "bottom", legend.justification = c(0, 0))
      # } else  if(input$filtro == "localidade"){
      #     caterpilar_plot <- dados_filtrados() %>%
      #       select(ano, fungicida, cultivar, rendimento_medio, ciclo, grupo__qualidade) %>%
      #       group_by(cultivar) %>%
      #       summarise(
      #         mean_rendimento = mean(rendimento_medio, na.rm = TRUE),
      #         sd_rendimento = sd(rendimento_medio, na.rm = TRUE)
      #       ) %>%
      #       ungroup() %>%
      #       ggplot(aes(x = reorder(cultivar, mean_rendimento, decreasing = F), y = mean_rendimento)) +
      #       geom_point() +
      #       geom_errorbar(aes(ymin = mean_rendimento - sd_rendimento, ymax = mean_rendimento + sd_rendimento), width = 0.2) +
      #       coord_flip() +
      #       labs(x = "Cultivar", y = "Rendimento médio (ton/ha)") +
      #       meutema()+
      #     theme(legend.position = "bottom", legend.justification = c(0, 0))
    }else{
      caterpilar_plot <- dados_filtrados() %>%
        select(ano, fungicida, cultivar, rendimento_medio, ciclo, grupo__qualidade) %>%
        group_by(cultivar,grupo__qualidade) %>%
        summarise(
          mean_rendimento = mean(rendimento_medio, na.rm = TRUE),
          sd_rendimento = sd(rendimento_medio, na.rm = TRUE)
        ) %>%
        ungroup() %>%
        ggplot(aes(x = reorder(cultivar, mean_rendimento, decreasing = F), y = mean_rendimento,
                   color = grupo__qualidade)) +
        geom_point() +
        geom_errorbar(aes(ymin = mean_rendimento - sd_rendimento, ymax = mean_rendimento + sd_rendimento), width = 0.5) +
        coord_flip() +
        labs(x = "Cultivar", y = "Rendimento médio (ton/ha)", color = "Grupo") +
        meutema()+
        theme(legend.position = "bottom", legend.justification = c(3, 0))
    }
    ggplotly(caterpilar_plot)
    #return(caterpilar_plot)
  })
  
  output$plot2 <- renderLeaflet(
    mapa(shapefile, dados_filtrados(), input$filtro)
  )
}



