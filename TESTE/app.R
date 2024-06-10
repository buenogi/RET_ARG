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
    "sidebar table    table  area7  area7 "
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
        choices = list("choice a" = "a", "choice b" = "b"),
        width = "100%"
      ),
      checkboxInput(
        inputId = "localidade",
        label = "Selecionar localidade",
        value = FALSE
      ),
      selectInput(
        inputId = "mySelectInput",
        label = "Empresa: ",
        choices = list("choice a" = "a", "choice b" = "b")
      ),
      checkboxInput(
        inputId = "myCheckboxInput",
        label = "Selecionar cultivar",
        value = FALSE
      ),
      selectInput(
        inputId = "mySelectInput",
        label = "Cultivar: ",
        choices = list("choice a" = "a", "choice b" = "b")
      ),
      checkboxInput(
        inputId = "myCheckboxInput",
        label = "Selecionar localidade",
        value = FALSE
      ),
      selectInput(
        inputId = "mySelectInput",
        label = "Localidade: ",
        choices = list("choice a" = "a", "choice b" = "b")
      )
    ),
    card_footer(
      actionButton(inputId = "myButton", label = "My Button")
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
      value_box(
        title = "Look at me!",
        showcase = bsicons::bs_icon("database")
      )
    )
  ),
  grid_card(
    area = "area10",
    card_body(
      value_box(
        title = "Look at me!",
        showcase = bsicons::bs_icon("database")
      )
    )
  )
)


server <- function(input, output) {
  
  output$distPlot <- renderPlotly({
    # generate bins based on input$bins from ui.R
    plot_ly(x = ~ faithful[, 2], type = "histogram")
  })
  
  output$bluePlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = "steelblue", border = "white")
  })
  
  output$myTable <- renderDT({
    head(faithful, input$numRows)
  })
}

shinyApp(ui, server)