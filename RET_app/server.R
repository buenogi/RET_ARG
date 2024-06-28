library(leaflet)
library(sf)
library(shinydashboard)
library(tidyverse)
library(dplyr)
library(plotly)
library(bsicons)
source("Global.R")

#dados <- read.csv("../RET_app/RET_ARG_PT.csv")


# Extração de coordenadas  -----------------------------------------------------

caminho_arquivo <- "Subregiones 2021.shp"
dados_completos <- extracao_coordenadas(dados, caminho_arquivo)
shapefile <- read_sf(dsn = caminho_arquivo)

server <- function(input, output,session) {
  
  
  # Filtragem  -------------------------------------------------------------------
  
  observe(input$filtro)
  
  output$ano_selecionado <- renderUI({
    radioButtons(
      inputId = "ano",
      label = "Ano:",
      choices = list("2021" = "2021",
                     "2022" = "2022",
                     "2023" = "2023")
    )
  })
  
  output$clima <- renderUI({
    radioButtons(
      inputId = "informacoes_clima",
      label = "Informações climáticas:",
      choices = list("Pluviosidade" = "pluviosidade",
                     "Temperatura" = "temperatura")
    )
  })
  
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
    
    # if(input$selecionar_ano == T){
    #   dados_filtrados%>%
    #     filter(ano == input$ano)
    # } else{
    #   dados_filtrados
    # }
    return(dados_filtrados)
  })
  # observe(input$selecionar_ano)
  dados_filtrados_ano <- reactive(
    dados_filtrados_ano <- dados_filtrados()%>%
                filter(ano == input$ano))
  # dados_filtrados_ano <- eventReactive(input$selecionar_ano,{
  #   dados_filtrados_ano <- dados_filtrados()%>%
  #     filter(ano == input$ano)
  # })
  
  #observe(input$selecao_empresa)
  #observe(input$selecao_localidade)
  #observe(input$selecao_cultivar)
  
  
  
  # Outputs -----------------
  output$table <- renderDataTable({
    if(input$selecionar_ano == FALSE){
      if(input$filtro =="empresa"| input$filtro == "localidade"){
        dados_filtrados <- dados_filtrados() %>%
            select(ciclo, fungicida, cultivar, ano, grupo__qualidade, localidade,
                   subregiao_nome, solo, cultivo_antecesor, rendimento_medio)%>%
          group_by(cultivar, ciclo)%>%
          summarise("produtividade_media" =  round(mean(rendimento_medio),2),
                    "sd_produtividade" =  round(sd(rendimento_medio),2))
        colnames(dados_filtrados) <- c("Cultivar",  "Ciclo", "Produtividade média (ton/ha)", 
                                       "Desvio padrão")
          
      } else{
        dados_filtrados <- dados_filtrados() %>%
          select(ciclo, fungicida, cultivar, ano, grupo__qualidade, localidade,
                 subregiao_nome, solo, cultivo_antecesor, rendimento_medio)%>%
          group_by(localidade, ciclo)%>%
          summarise("produtividade_media" =  round(mean(rendimento_medio),2),
                    "sd_produtividade" =  round(sd(rendimento_medio),2))
        colnames(dados_filtrados) <- c("Localidade",  "Ciclo", "Produtividade média (ton/ha)", 
                                       "Desvio padrão")
      }
      dados_filtrados
    } else {
   
    if(input$filtro =="empresa"| input$filtro == "localidade"){
      dados_filtrados_ano <- dados_filtrados_ano() %>%
        select(ciclo, fungicida, cultivar,ano, grupo__qualidade, localidade,
               subregiao_nome, solo, cultivo_antecesor, rendimento_medio)%>%
        group_by(cultivar,grupo__qualidade, ciclo)%>%
        summarise("produtividade_media" =  round(mean(rendimento_medio),2),
                  "sd_produtividade" =  round(sd(rendimento_medio),2))
      colnames(dados_filtrados_ano) <- c("Cultivar", "Grupo de qualidade", "Ciclo", "Produtividade média (ton/ha)", 
                                     "Desvio padrão")
      dados_filtrados_ano
      
    } else{
      dados_filtrados_ano <- dados_filtrados_ano() %>%
        select(ciclo, fungicida, cultivar,ano, grupo__qualidade, localidade,
               subregiao_nome, solo, cultivo_antecesor, rendimento_medio)%>%
        group_by(localidade,grupo__qualidade, ciclo)%>%
        summarise("produtividade_media" =  round(mean(rendimento_medio),2),
                  "sd_produtividade" =  round(sd(rendimento_medio),2))
      colnames(dados_filtrados_ano) <- c("Localidade", "Grupo de qualidade", "Ciclo", "Produtividade média (ton/ha)", 
                                     "Desvio padrão")
      dados_filtrados_ano
    }
    }
  })
  
  
  
  output$plot1 <- renderPlotly({
    cat(file = stderr(), input$filtro, "\n")
    if(input$selecionar_ano){
      dados_usados <- dados_filtrados_ano()
    }
    else{
      dados_usados <- dados_filtrados()
    }
    if(input$filtro == "cultivar"){
      caterpilar_plot <- dados_usados %>%
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
        scale_color_manual(values = c("#00525b", "#639876", "#392502", "#ead693"))+
        labs(x = "Localidade", y = "Rendimento médio (ton/ha)", color = "Grupo de qualidade") +
        theme_minimal()+
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
      caterpilar_plot <- dados_usados %>%
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
        scale_color_manual(values = c("#00525b", "#368352", "#392502", "#ead693"))+
        labs(x = "Cultivar", y = "Rendimento médio (ton/ha)", color = "Grupo") +
        theme_minimal()+
        theme(legend.position = "bottom", legend.justification = c(3, 0))
    }
    ggplotly(caterpilar_plot)
    #return(caterpilar_plot)
  })
  
  output$plot2 <- renderLeaflet(
    if(input$selecionar_ano == F){
      dados_agrupados <- dados_filtrados()%>%
        group_by(localidade,input$filtro, Latitude, Longitude)%>%
        summarise( rendimento_medio = mean(rendimento_medio))
      
      mapa(shapefile, dados_agrupados, input$filtro)
    }
    else{
      dados_agrupados <- dados_filtrados_ano()%>%
        group_by(localidade,input$filtro, Latitude, Longitude, input$ano)%>%
        summarise( rendimento_medio = mean(rendimento_medio))
      
      mapa(shapefile, dados_agrupados, input$filtro)
    }
    
  )
  
  dados_usados2 <- reactive(
    if(input$selecionar_ano) {
      dados_usados <- dados_filtrados_ano()
    } else {
      dados_usados <- dados_filtrados()
    }
  )
  
  output$plot3 <- renderPlotly(

    if(input$filtro == "empresa"|input$filtro == "cultivar"|input$filtro == "localidade") {
      valor_limite_sup <- 250
      valor_limite_inf <- 25
      plot3 <- dados_usados2()%>%
        mutate(
          dias_do_plantio_a_maturacao = ifelse(dias_do_plantio_a_maturacao > valor_limite_sup, valor_limite_sup, dias_do_plantio_a_maturacao),
          dias_do_plantio_a_maturacao = ifelse(dias_do_plantio_a_maturacao < valor_limite_inf, valor_limite_inf, dias_do_plantio_a_maturacao)
        ) %>%
        ggplot() +
        aes(x = as.factor(ano), y = dias_do_plantio_a_maturacao) +
        geom_boxplot(fill = "#00525b") +
        geom_point(data = . %>% filter(dias_do_plantio_a_maturacao == valor_limite_sup|
                                         dias_do_plantio_a_maturacao == valor_limite_inf ),
                   aes(x = as.factor(ano), y = dias_do_plantio_a_maturacao),
                   color = "red", shape = 8, size = 3) +
        facet_grid(~grupo__qualidade) +
        labs(x = "Ano", y = "Dias do Plantio à Maturação") +
        theme_minimal() +
        theme(legend.position = "bottom", legend.justification = c(0, 0))
      
      ggplotly(plot3)
    }
  )
  
  # Caixas de valores
  # media_vb1 <- reactive(
  #   dados_usados2()%>%
  #     group_by(empresa)%>%
  #     summarise(media = mean(rendimento_medio))
  
  output$vb1 <- renderText({
    if(!is.null(input$filtro)){
      mediavb1 <- dados_usados2()%>%
        group_by(input$filtro)%>%
        summarise(media = mean(rendimento_medio))
      paste0(round(mediavb1[1,2], 2))
    }
  })
  
    clima <- reactive(
    dados_clima(dados_usados2())
  )
  
    output$plot4 <- renderPlotly({
      if(input$filtro == "localidade"){
        if(input$informacoes_clima == "pluviosidade"){
          if(input$selecionar_ano){
          pluviosidade <- ggplot(clima()) +
            aes(x = mes, y = pluviosidade, group = localidade, color = localidade) +
            geom_line() +
            labs(x = "Mês", y = "Pluviosidade (mm)", color = "Localidade")+
            theme_minimal()}
          else{
            pluviosidade <- clima()%>%
              group_by(ano, mes, localidade) %>%
              summarise(pl_media = mean(pluviosidade, na.rm = TRUE)) %>%
              ggplot() +
              aes(x = mes, y = pl_media, group = interaction(localidade, ano), 
                  color = localidade, linetype = factor(ano)) +
              geom_line() +
              labs(x = "Mês", y = "Pluviosidade (mm)", color = "Localidade/ano")+
              theme_minimal()}
          ggplotly(pluviosidade)}else{
            if(input$selecionar_ano){
              temperatura <- ggplot(clima()) +
                aes(x = mes, y = temperatura, group = localidade, color = localidade) +
                geom_line() +
                labs(x = "Mês", y = "Temperatura (ºC)", color = "Localidade")+
                theme_minimal()}
            else{
              temperatura <- clima()%>%
                group_by(ano, mes, localidade) %>%
                summarise(pl_media = mean(temperatura, na.rm = TRUE)) %>%
                ggplot() +
                aes(x = mes, y = pl_media, group = interaction(localidade, ano), 
                    color = localidade, linetype = factor(ano)) +
                geom_line() +
                labs(x = "Mês", y = "Temperatura (ºC)", color = "Localidade/ano")+
                theme_minimal()}
            ggplotly(temperatura)
          }
        
      }
        })
    
    indicador_vb2 <- reactive({
      dados_usados2()%>%
      count()})

    output$vb2 <- renderUI({
      value_box(
        title = "Nº de experimentos realizados",
        value = paste0(indicador_vb2()),
        showcase = bsicons::bs_icon("file-ruled"),
        theme = value_box_theme(bg = "#392502", fg = "#ead693")
      )
    })
      
    output$texto <- renderPrint({
     
      if(input$filtro == "localidade") {
        modelo_anova <- aov(rendimento_medio ~ cultivar, data = dados_usados2())
      } else if(input$filtro == "cultivar"){
        modelo_anova <- aov(rendimento_medio ~ localidade, data = dados_usados2())
      }else{
        modelo_anova <- aov(rendimento_medio ~ cultivar * localidade, data = dados_usados2())
      }
      
      print(summary(modelo_anova))
    })
    
    output$texto2 <-  renderDataTable({
      if(input$filtro == "localidade") {
        modelo_anova <- aov(rendimento_medio ~ cultivar, data = dados_usados2())
        comparacao <- emmeans(modelo_anova, specs = ~cultivar) |>
          multcomp::cld(Letters = letters, adjust = "fdr") |>
          dplyr::mutate(.group = trimws(.group)) |>
          as.data.frame()
        comparacao%>%
          select(cultivar, emmean,SE,.group)%>%
          mutate(emmean = round(emmean,2),
                 SE = round(SE,2))
      } else if(input$filtro == "cultivar"){
        modelo_anova <- aov(rendimento_medio ~ localidade, data = dados_usados2())
        comparacao <-emmeans(modelo_anova, specs = ~localidade) |>
          multcomp::cld(Letters = letters, adjust = "fdr") |>
          dplyr::mutate(.group = trimws(.group)) |>
          as.data.frame()
        comparacao%>%
          select(cultivar, emmean,SE,.group)%>%
          mutate(emmean = round(emmean,2),
                 SE = round(SE,2))
      }else{
        modelo_anova <- aov(rendimento_medio ~ cultivar * localidade, data = dados_usados2())
        comparacao <- emmeans(modelo_anova, specs = ~cultivar|localidade) |>
          multcomp::cld(Letters = letters, adjust = "fdr") |>
          dplyr::mutate(.group = trimws(.group)) |>
          as.data.frame()
        comparacao%>%
          select(cultivar,localidade,emmean,SE,.group)%>%
          mutate(emmean = round(emmean,2),
                 SE = round(SE,2))
              }
      #posthoc_tukey <- TukeyHSD(modelo_anova)
      #emmeans::emmeans(modelo_anova)
    })
    
     
  
}
