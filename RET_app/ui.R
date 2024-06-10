library(shiny)
library(plotly)
library(gridlayout)
library(bslib)
library(DT)


ui <- grid_page(
  layout = c(
    "header  header   header header header",
    "sidebar bluePlot area8  area8  area6 ",
    "sidebar bluePlot area8  area8  area10",
    "sidebar table    table  area7  area7 ",
    ".       table    table  area7  area7 "
  ),
  row_sizes = c(
    "45px",
    "1fr",
    "1fr",
    "1.29fr",
    "0.71fr"
  ),
  col_sizes = c(
    "210px",
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
      radioButtons(
        inputId = "ano",
        label = "Ano:",
        choices = list("2021" = "2021",
                       "2022" = "2022",
                       "2023" = "2023"),
        width = "100%"
      ),
      checkboxInput(
        inputId = "selecionar_empresa",
        label = "Selecionar empresa",
        value = FALSE
      ),
      selectInput(
        inputId = "empresa",
        label = "Empresa: ",
        choices = list("choice a" = "a", "choice b" = "b")
      ),
      checkboxInput(
        inputId = "selecionar_cultivar",
        label = "Selecionar cultivar",
        value = FALSE
      ),
      selectInput(
        inputId = "cultivar",
        label = "Cultivar: ",
        choices = list("choice a" = "a", "choice b" = "b")
      ),
      checkboxInput(
        inputId = "selecionar_localidade",
        label = "Selecionar localidade",
        value = FALSE
      ),
      selectInput(
        inputId = "localidade",
        label = "Localidade: ",
        choices = list("choice a" = "a", "choice b" = "b")
      )
    ),
    card_footer(
      actionButton(inputId = "filtrar", label = "Filtrar")
    )
  ),
  grid_card_text(
    area = "header",
    content = "Painel de Produtividade de Trigo - RETA",
    alignment = "center",
    is_title = FALSE
  ),
  grid_card(
    area = "table",
    card_header("Table"),
    card_body(DTOutput(outputId = "myTable", width = "100%"))
  ),
  grid_card_plot(area = "bluePlot"),
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
    card_body(plotOutput(outputId = "plot"))
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