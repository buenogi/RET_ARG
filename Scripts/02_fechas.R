#-----------------------------------------------------------------------
# Pacotes.

library(tidyverse)

#-----------------------------------------------------------------------
# Importação de todas as tabelas.

fs::dir_tree(".")

fls <- list.files(path = "Dados/Dados_Brutos", pattern = "^ret.*\\.xlsx$", recursive = TRUE, full.names = TRUE)

read_and_prepare <- function(path) {
  tb <- readxl::read_xlsx(path, sheet = "Fechas") |>
    suppressMessages() |>
    select(3:9)
  nms <- tb[[1]] |>
    grep(pattern = "ciclo", value = TRUE, ignore.case = TRUE)
  tb$split <- tb[[1]] |>
    grepl(pattern = "cultivar", x = _, ignore.case = TRUE) |>
    cumsum()
  tb <- tb |>
    filter(split >= 1)
  
  tb <- tb |>
    filter(!is.na(tb[[1]]), !tb[[1]] %in% c("", "0")) |>
    group_split(split) |>
    map(function(tbi) {
      tbi[, c(1:8)] |>
        write_csv("tb.csv", col_names = FALSE)
      read_csv("tb.csv") |>
        suppressMessages() |>
        janitor::clean_names()
    })
  
  names(tb) <- nms
  
  tb <- tb |>
    discard(~nrow(.) == 0L) |>
    bind_rows(.id = "experimento") |>
    add_column(
      filename = tools::file_path_sans_ext(path),
      .before = 1) 
  #|>janitor::remove_empty(which = "cols")
  tb[[4]] <- openxlsx::convertToDate(tb[[4]])
  tb[[5]] <- openxlsx::convertToDate(tb[[5]])
  tb[[6]] <- openxlsx::convertToDate(tb[[6]])
    return(tb)
}

fix_text <- function(x) {
    x |>
        iconv(to = "ASCII//TRANSLIT") |>
        str_to_upper() |>
        str_replace_all(pattern = " +", replacement = " ")
}


resultados <- list()


posicoes_erro <- c()

for (i in seq_along(fls)) {
  
  tryCatch({
    tb <- read_and_prepare(fls[[i]])
    
    resultados[[i]] <- tb
  }, error = function(e) {
    
    mensagem_de_erro <- conditionMessage(e)
    print(paste("Ocorreu um erro na posição", i, ":", mensagem_de_erro))
    posicoes_erro <- c(posicoes_erro, i)
  })
}


#resultados <- resultados[-posicoes_erro]
resultados[[21]] <- NULL
resultados[[40]] <- NULL
resultados[[42]] <- NULL
resultados_sem_x <- map(resultados, ~ select(.x, -starts_with("x")))

tb <- bind_rows(resultados_sem_x)

tb1 <- tb |>
    mutate(cultivar = fix_text(cultivar))

tb1 |>
    count(filename)

tb1 |>
    count(filename, experimento)

# Remoção experimentos com fungicida -------------------

read_and_prepare2 <- function(path) {
  tb <- readxl::read_xlsx(fls[1], sheet = "Fechas") |>
    suppressMessages() |>
    select(11:17)
  nms <- tb[[1]] |>
    grep(pattern = "ciclo", value = TRUE, ignore.case = TRUE)
  tb$split <- tb[[1]] |>
    grepl(pattern = "cultivar", x = _, ignore.case = TRUE) |>
    cumsum()
  tb <- tb |>
    filter(split >= 1)
  tb <- tb |>
    filter(!is.na(tb[[2]]), !tb[[2]] %in% c("", "0")) |>
    group_split(split) |>
    map(function(tbi) {
      tbi[, c(1:8)] |>
        write_csv("tb.csv", col_names = FALSE)
      read_csv("tb.csv") |>
        suppressMessages() |>
        janitor::clean_names()
    })
  names(tb) <- nms
  tb <- tb |>
    discard(~nrow(.) == 0L) |>
    bind_rows(.id = "experimento") |>
    add_column(
      filename = tools::file_path_sans_ext(fls[1]),
      .before = 1
    ) |>
    janitor::remove_empty(which = "cols")
  tb <- openxlsx::convertToDate(tb[[4]])
  tb <- openxlsx::convertToDate(tb[[5]])
  tb <- openxlsx::convertToDate(tb[[6]])
  tb <- openxlsx::convertToDate(tb[[7]])
  return(tb)
}

fix_text <- function(x) {
  x |>
    iconv(to = "ASCII//TRANSLIT") |>
    str_to_upper() |>
    str_replace_all(pattern = " +", replacement = " ")
}

tb <- map(fls, read_and_prepare) |>
  bind_rows()
str(tb)

tb2 <- tb |>
  mutate(cultivar = fix_text(cultivar))

tb2 |>
  count(filename)

tb2 |>
  count(filename, experimento)

tb <- rbind(tb1, tb2)
tb |>
  writexl::write_xlsx("Dados/Dados_Brutos/trigo_rendimiento.xlsx")
