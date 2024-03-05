#-----------------------------------------------------------------------
# Pacotes.

library(tidyverse)

#-----------------------------------------------------------------------
# Importação de todas as tabelas.

fs::dir_tree(".")
fls <- dir(pattern = "ret.*\\.xlsx")

# Função que lê e organiza os dados.
read_and_prepare <- function(path) {
    tb <- readxl::read_xlsx(path, sheet = "Datos", n_max = 76) |>
        suppressMessages()
    tb <-
        tb |>
        select(1:2) |>
        rename(nombre = 1, valor = 2) |>
        mutate(categoria = ifelse(grepl(pattern = "^[[:upper:]]+\\b", nombre),
                                  nombre,
                                  NA_character_
                                  )) |>
        fill(categoria, .direction = "down") |>
        relocate(categoria, .before = nombre) |>
        mutate(across(.cols = where(is.character), .fns = trimws)) |>
        drop_na(valor) |>
        add_column(
            # filename = tools::file_path_sans_ext(path),
            filename = path,
            .before = 1)
    return(tb)
}

tb <- map(fls, read_and_prepare) |>
    bind_rows()
str(tb)

#-----------------------------------------------------------------------
# Extração das informações.

sheets <- list()

# Local e responsável.
sheets[["local"]] <-
    tb |>
    filter(is.na(categoria)) |>
    pivot_wider(names_from = nombre, values_from = valor) |>
    select(-categoria) |>
    janitor::clean_names()
sheets[["local"]][[2]]
sheets[["local"]][["SUB_ABR"]] <- c("VSU",
                                    "CHS",
                                    "VSU",
                                    "VSU",
                                    "VSU",
                                    "VSU",
                                    "PME")
sheets[["local"]][["SUB_NOM"]] <- c("Valles subandinos",
                                    "Chaco húmedo sur",
                                    "Valles subandinos",
                                    "Valles subandinos",
                                    "Valles subandinos",
                                    "Valles subandinos",
                                    "Pampa mesopotámica")
sheets[["local"]]

# Detalhes do experimento.
sheets[["diseño"]] <-
    tb |>
    filter(str_detect(categoria, "CARACTERISTICAS .* ENSAYOS")) |>
    pivot_wider(names_from = nombre, values_from = valor) |>
    mutate(across(.cols = -c(filename, categoria),
                  .fns = parse_guess)) |>
    select(-categoria) |>
    rename_with(.fn = ~str_remove(.x, "^[a-z]+\\)")) |>
    janitor::clean_names()
sheets[["diseño"]]

# Solo.
sheets[["suelo"]] <-
    tb |>
    filter(str_detect(categoria, "SUELO")) |>
    pivot_wider(names_from = nombre, values_from = valor) |>
    mutate(across(.cols = -c(filename, categoria),
                  .fns = parse_guess)) |>
    select(-categoria) |>
    janitor::clean_names()
sheets[["suelo"]]

# Cultura anterior.
sheets[["antecesor"]] <-
    tb |>
    filter(str_detect(nombre, "ANTECESOR")) |>
    pivot_wider(names_from = nombre, values_from = valor) |>
    mutate(across(.cols = -c(filename, categoria),
                  .fns = str_to_title)) |>
    select(-categoria) |>
    janitor::clean_names()
sheets[["antecesor"]]

# Semeadura.
sheets[["siembra"]] <-
    tb |>
    filter(str_detect(categoria, "SIEMBRA")) |>
    pivot_wider(names_from = nombre, values_from = valor) |>
    mutate(across(.cols = -c(filename, categoria), .fns = parse_guess)) |>
    select(-categoria) |>
    rename_with(.fn = ~str_remove(.x, ":.*")) |>
    janitor::clean_names()
sheets[["siembra"]]

# Aplicações de insumos de defensivos.
sheets[["manejo"]] <-
tb |>
    filter(str_detect(categoria, "MANEJO")) |>
    pivot_wider(names_from = nombre, values_from = valor) |>
    mutate(across(.cols = -c(filename, categoria),
                  .fns = parse_guess)) |>
    select(-categoria) |>
    rename_with(.fn = ~str_remove(.x, "\\(.*")) |>
    janitor::clean_names()
sheets[["manejo"]]

# Precipitação.
sheets[["lluvia"]] <-
    tb |>
    filter(str_detect(categoria, "LLUVIAS")) |>
    pivot_wider(names_from = nombre, values_from = valor) |>
    select(-categoria) |>
    mutate(across(.cols = `Enero:`:`Diciembre:`, .fns = parse_guess)) |>
    select(1:`Diciembre:`) |>
    janitor::clean_names()
sheets[["lluvia"]]

# Temperatura.
sheets[["temperatura"]] <-
    tb |>
    filter(str_detect(categoria, "TEMPERATURA")) |>
    pivot_wider(names_from = nombre, values_from = valor) |>
    select(-categoria) |>
    mutate(across(.cols = `Enero:`:`Diciembre:`, .fns = parse_guess)) |>
    select(1:`Diciembre:`) |>
    janitor::clean_names()
sheets[["temperatura"]]

writexl::write_xlsx(sheets, "inase_trigo_metadatos.xlsx")

#-----------------------------------------------------------------------
