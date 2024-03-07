#-----------------------------------------------------------------------
# Pacotes.

library(tidyverse)

#-----------------------------------------------------------------------
# Importação de todas as tabelas.

fs::dir_tree(".")

fls <- list.files(path = "Dados/Dados_Brutos", pattern = "^ret.*\\.xlsx$", recursive = TRUE, full.names = TRUE)
fls_existem <- file.exists(fls)
fls <- fls[fls_existem]

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
#-------------------------------------------------------------------------------
# Tabela auxiliar de classificação de-para para localidade e subregião

nomes <- list(sheets[["local"]][[2]])
nomes <- as.data.frame(nomes)
names(nomes) <- "De"
nomes <- sapply(nomes,str_to_title)
nomes <- unique(nomes)
row.names(nomes) <- NULL

para <- data.frame("SUB_ABR" = c( "VPU NOA",
                                  "CSN NEA-NOA",
                                  "PME III",
                                  "PON I-IIN",
                                  "POS IIN-IIS",
                                  "PSN VN",
                                  "PSE IV",
                                  "PSE IV",
                                  "PSE IV",
                                  "PBN IIN-VN",
                                  "POS IIN-IIS",
                                  "PBS IIS-IV",
                                  NA,
                                  "PSS VS",
                                  "PSE IV",
                                  "CHS NEA",
                                  "VSU NOA",
                                  "PME III",
                                  "PSS VS",
                                  "PSE IV",
                                  "CHS NEA",
                                  "VSU NOA"),
                   
                   "SUB_NOME" = c("Valles puneños",
                                  "Chaco seco norte",
                                  "Pampa mesopotâmica",
                                  "Pampa ondulada norte",
                                  "Pampa ondulada sur",
                                  "Pampa semiárida norte",
                                  "Pampa serrana",
                                  "Pampa serrana",
                                  "Pampa serrana",
                                  "Pampa subúmeda norte",
                                  "Pampa ondulada sur",
                                  "Pampa subúmeda sur",
                                  NA, 
                                  "Pampa semiárida sur",
                                  "Pampa serrana",
                                  "Chaco húmedo sur",
                                  "Valles subandinos",
                                  "Pampa mesopotâmica",
                                  "Pampa semiárida sur",
                                  "Pampa serrana",
                                  "Chaco húmedo sur",
                                  "Valles subandinos"))
de_para <- cbind(nomes, para)

#-------------------------------------------------------------------------------
sheets[["local"]][[2]]
sheets[["local"]]<- sapply(sheets[["local"]],str_to_title)

class(sheets[["local"]])

sheets[["local"]] <- as.data.frame(sheets[["local"]])

sheets[["local"]] <- left_join(sheets[["local"]], de_para, 
                               join_by("subregion_segun_mapa_2021" == "De"))
View(sheets[["local"]])
# sheets[["local"]][["SUB_ABR"]] <-   ("VSU",
#                                     "CHS",
#                                     "VSU",
#                                     "VSU",
#                                     "VSU")
# sheets[["local"]][["SUB_NOM"]] <- c("Valles subandinos",
#                                     "Chaco húmedo sur",
#                                     "Valles subandinos",
#                                     "Valles subandinos",
#                                     "Valles subandinos",
#                                     "Valles subandinos",
#                                     "Pampa mesopotámica")

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

View(sheets[["diseño"]])

# Solo.
sheets[["suelo"]] <-
    tb |>
    filter(str_detect(categoria, "SUELO")) |>
    pivot_wider(names_from = nombre, values_from = valor) |>
    mutate(across(.cols = -c(filename, categoria),
                  .fns = parse_guess)) |>
    select(-categoria) |>
    janitor::clean_names()
View(sheets[["suelo"]])

# Cultura anterior.
sheets[["antecesor"]] <-
    tb |>
    filter(str_detect(nombre, "ANTECESOR")) |>
    pivot_wider(names_from = nombre, values_from = valor) |>
    mutate(across(.cols = -c(filename, categoria),
                  .fns = str_to_title)) |>
    select(-categoria) |>
    janitor::clean_names()
View(sheets[["antecesor"]])


# Semeadura.
sheets[["siembra"]] <-
    tb |>
    filter(str_detect(categoria, "SIEMBRA")) |>
    pivot_wider(names_from = nombre, values_from = valor) |>
    mutate(across(.cols = -c(filename, categoria), .fns = parse_guess)) |>
    select(-categoria) |>
    rename_with(.fn = ~str_remove(.x, ":.*")) |>
    janitor::clean_names()
View(sheets[["siembra"]])

# Aplicações de insumos de defensivos.
sheets[["manejo"]] <-
tb |>
    filter(str_detect(categoria, "MANEJO")) |>
    pivot_wider(names_from = nombre, values_from = valor) |>
    mutate(across(.cols = -c(filename, categoria),
                  .fns = parse_guess))|>
    select(-categoria) |>
    rename_with(.fn = ~str_remove(.x, "\\(.*")) |>
    janitor::clean_names()
View(sheets[["manejo"]])

# Precipitação.
sheets[["lluvia"]] <-
    tb |>
    filter(str_detect(categoria, "LLUVIAS")) |>
    pivot_wider(names_from = nombre, values_from = valor) |>
    select(-categoria) |>
    mutate(across(.cols = `Enero:`:`Diciembre:`, .fns = parse_guess)) |>
    select(1:`Diciembre:`) |>
    janitor::clean_names()
View(sheets[["lluvia"]])

# Temperatura.
sheets[["temperatura"]] <-
    tb |>
    filter(str_detect(categoria, "TEMPERATURA")) |>
    pivot_wider(names_from = nombre, values_from = valor) |>
    select(-categoria) |>
    mutate(across(.cols = `Enero:`:`Diciembre:`, .fns = parse_guess)) |>
    select(1:`Diciembre:`) |>
    janitor::clean_names()
View(sheets[["temperatura"]])

writexl::write_xlsx(sheets, "Dados/Dados_Brutos/trigo_metadatos.xlsx")

#-----------------------------------------------------------------------
