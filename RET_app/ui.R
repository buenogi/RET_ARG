library(shiny)
library(plotly)
library(gridlayout)
library(bslib)
library(leaflet)
library(DT)

ui <- grid_page(
  layout = c(
    "header  header   header header header",
    "sidebar bluePlot area8  area8  area6 ",
    "sidebar bluePlot area8  area8  area10",
    "table table    table  area7  area7 ",
    "table   table    table  area7  area7 "
  ),
  row_sizes = c(
    "45px",
    "1fr",
    "1fr",
    "1.29fr",
    "0.71fr"
  ),
  col_sizes = c(
    "0.5fr",
    "1.5fr",
    "0.75fr",
    "0.75fr",
    "1fr"
  ),
  gap_size = "1rem",
  grid_card(
    area = "sidebar",
    card_header("Filtros"),
    card_body(
      radioButtons(inputId = "filtro",
                   label = "Selecione um filtro:",
                   choiceValues = c("empresa", "localidade", "cultivar"),
                   choiceNames = c("Empresa", "Localidade", "Cultivar")),
      # Inserir um hoverinfo para que o usuário saiba qual é o efeito de cada uma das seleções
      conditionalPanel(
        condition = "input.filtro == 'empresa'",
        selectInput(
          inputId = "selecao_empresa",
          label = "Selecione a empresa:",
          choices = dados$empresa
        ),
        #uiOutput("empresa_ui")
      ),
      conditionalPanel(
        condition = "input.filtro == 'localidade'",
        selectInput(
          inputId = "selecao_localidade",
          label = "Selecione a localidade:",
          choices = dados$localidade
        )
        #uiOutput("localidade_ui")
      ),
      conditionalPanel(
        condition = "input.filtro == 'cultivar'",
        selectInput(
          inputId = "selecao_cultivar",
          label = "Selecione a cultivar:",
          choices = dados$cultivar
        #uiOutput("cultivar_ui")
      )
    )),
    card_footer(
      actionButton(inputId = "filtrar", label = "Filtrar"),
      align = "center"
    )
  ),
  grid_card_text(
    area = "header",
    content = "Painel de Produtividade de Trigo - RETA",
    alignment = "center",
    is_title = TRUE
  ),
  grid_card(
    area = "table",
    card_header("Table"),
    card_body(DTOutput(outputId = "table", width = "100%"))
  ),
  grid_card(
    area = "bluePlot",
    card_body(
      leafletOutput(outputId = "plot2"))
    
  ),
  grid_card(
    area = "area7",
    card_body(
      plotlyOutput(
        outputId = "distPlot",
        width = "100%",
        height = "100%"
      )
    )
  ),
  grid_card(
    area = "area8",
    card_body(
      #plotOutput("plot1")
      plotlyOutput("plot1")
    )
  ),
  grid_card(
    area = "area6",
    card_body(
      # value_box(
      #   title = "Look at me!",
      #   showcase = bsicons::bs_icon("database")
      # )
    )
  ),
  grid_card(
    area = "area10",
    card_body(
      # value_box(
      #   title = "Look at me!",
      #   showcase = bsicons::bs_icon("database")
      # )
    )
  )
)
