#-----------------------------------------------------------------------
# Pacotes.

library(tidyverse)

# Importação de todas as tabelas.-----------------------------------------------

fs::dir_tree(".")

fls <- list.files(path = "Dados/Dados_Brutos", pattern = "^ret.*\\.xlsx$", recursive = TRUE, full.names = TRUE)

# Extração dos experimentos sem fungicida --------------------------------------
read_and_prepare <- function(path) {
  tb <- readxl::read_xlsx(path, sheet = "Rendimiento") |>
    suppressMessages() |>
    select(1:13)
  nms <- tb[[3]] |>
    grep(pattern = "ciclo", value = TRUE, ignore.case = TRUE)
  tb$split <- tb[[2]] |>
    grepl(pattern = "cultivar", x = _, ignore.case = TRUE) |>
    cumsum()
  tb <- tb |>
    filter(split >= 1)
  tb <- tb |>
    filter(!is.na(tb[[2]]), !tb[[2]] %in% c("", "0")) |>
    group_split(split) |>
    map(function(tbi) {
      tbi[, c(2:6)] |>
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
      .before = 1
    ) |>
    janitor::remove_empty(which = "cols")
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

tb <- tb |>
  mutate(cultivar = fix_text(cultivar))

tb |>
  count(filename)

tb |>
  count(filename, experimento)

# FIXME: Quebrar `experimento` em `época`, `ciclo` e `fungicida`.
tb$experimento <-
  tb$experimento |>
  # unique() |>
  str_replace("º ÉPOCA: +", "_") |>
  str_replace("CICLOS? +", "") |>
  str_replace("(.*) (\\w+ FUNG.*)", "\\1_\\2")
tb <- tb |>
  separate(experimento,
           into = c("epoca", "ciclo", "fungicida"),
           sep = "_")

tb |>
  count(epoca, ciclo, fungicida, sort = TRUE)

tb |>
  count(cultivar, sort = TRUE) |>
  print(n = Inf)

# FIXME: Padronizar o nome dos cultivares.
tb |>
  mutate(cultivar = recode(cultivar,
                           "QUIRIKO" = "QUIRICO",
                           "NEO50T23" = "DM NEO 50T23",
                           "NEO 30T23" = "DM NEO 30T23")) |>
  count(cultivar, sort = TRUE) |>
  print(n = Inf)

# Trocar 0 por NA.
tb <- tb |>
  mutate(across(.cols = starts_with("rep_"),
                .fns = ~replace(., . == 0, NA)))

# Excluir linhas com todos NA.
all_na <- tb |>
  select(starts_with("rep_")) |>
  apply(MARGIN = 1, function(x) {
    all(is.na(x))
  })

tb1 <- tb[!all_na, ]
# Extração de experimentos com fungicida ----------

read_and_prepare2 <- function(path) {
  tb <- readxl::read_xlsx(path, sheet = "Rendimiento") |>
    suppressMessages() |>
    select(1:13)
  nms <- tb[[8]] |>
    grep(pattern = "ciclo", value = TRUE, ignore.case = TRUE)
  tb$split <- tb[[2]] |>
    grepl(pattern = "cultivar", x = _, ignore.case = TRUE) |>
    cumsum()
  tb <- tb |>
    filter(split >= 1)
  tb <- tb |>
    filter(!is.na(tb[[2]]), !tb[[2]] %in% c("", "0")) |>
    group_split(split) |>
    map(function(tbi) {
      tbi[, c(2, 8:11)] |>
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
      .before = 1
    ) |>
    janitor::remove_empty(which = "cols")
  return(tb)
}




tb <- map(fls, read_and_prepare2) |>
  bind_rows()
str(tb)

tb <- tb |>
  mutate(cultivar = fix_text(cultivar))

tb |>
  count(filename)

tb |>
  count(filename, experimento)

# FIXME: Quebrar `experimento` em `época`, `ciclo` e `fungicida`.
tb$experimento <-
  tb$experimento |>
  # unique() |>
  str_replace("º ÉPOCA: +", "_") |>
  str_replace("CICLOS? +", "") |>
  str_replace("(.*) (\\w+ FUNG.*)", "\\1_\\2")
tb <- tb |>
  separate(experimento,
           into = c("epoca", "ciclo", "fungicida"),
           sep = "_")

tb |>
  count(epoca, ciclo, fungicida, sort = TRUE)

tb |>
  count(cultivar, sort = TRUE) |>
  print(n = Inf)

# FIXME: Padronizar o nome dos cultivares.
tb |>
  mutate(cultivar = recode(cultivar,
                           "QUIRIKO" = "QUIRICO",
                           "NEO50T23" = "DM NEO 50T23",
                           "NEO 30T23" = "DM NEO 30T23")) |>
  count(cultivar, sort = TRUE) |>
  print(n = Inf)

# Trocar 0 por NA.
tb2 <- tb |>
  mutate(across(.cols = starts_with("rep_"),
                .fns = ~replace(., . == 0, NA)))

# Excluir linhas com todos NA.
all_na <- tb |>
  select(starts_with("rep_")) |>
  apply(MARGIN = 1, function(x) {
    all(is.na(x))
  })

tb2$na_2 <- NULL
tb2$na_3 <- NULL
tb2$na_4 <- NULL
tb2 <- tb2 %>%
  rename(rep_v = rep_vi)
tb2 <- tb2[!all_na, ]

# União e consolidação dos dados de rendimento----------------------------------
tb <- rbind(tb1, tb2)
tb |>
  writexl::write_xlsx("Dados/Dados_Brutos/extraidos/trigo_rendimiento.xlsx")

