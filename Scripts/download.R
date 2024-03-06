#-----------------------------------------------------------------------
# Pacotes.

library(xml2)

#-----------------------------------------------------------------------

# Dados dos experimentos de trigo na Argentina.

url <- "https://www.argentina.gob.ar/inase/red-de-ensayos-comparativos-de-variedades-de-trigo/campana-20232024"
url <- "https://www.argentina.gob.ar/inase/red-de-ensayos-comparativos-de-variedades-de-trigo/campana-20222023"
url <- "https://www.argentina.gob.ar/inase/red-de-ensayos-comparativos-de-variedades-de-trigo/campana-20212022"

doc <- read_html(url)

urls <- doc |>
  xml_find_all("//div[@class = 'pane-content']/div/ul/li//a") |>
  xml_attr("href")

urls <- urls |>
    sub(pattern = "^.*#", replacement = "") |>
    grep(pattern = "\\.xlsx$", value = TRUE)

caminho <- "Dados/Dados_Brutos/2021-2022"

for (i in 1:length(urls)) {
    cat("Downloading\n  ", urls[i], "\n", sep = "")
    Sys.sleep(2)
    pasta_dest <- file.path(caminho, basename(urls[i]))
    download.file(urls[i], destfile = pasta_dest)
}

#-----------------------------------------------------------------------
