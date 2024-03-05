#-----------------------------------------------------------------------
# Pacotes.

library(tidyverse)

#-----------------------------------------------------------------------
# Importação de todas as tabelas.

fs::dir_tree(".")
fls <- dir(pattern = "ret.*\\.xlsx")

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

tb <- tb[!all_na, ]
tb |>
    writexl::write_xlsx("inase_trigo_rendimiento.xlsx")

#-----------------------------------------------------------------------
# Análise de cada experimento.

tb_long <- tb |>
    pivot_longer(cols = starts_with("rep_"),
                 names_to = "bloc",
                 values_to = "rendimiento")

tb_nest <- tb_long |>
    drop_na(rendimiento) |>
    group_by(filename, epoca, ciclo, fungicida) |>
    nest()
tb_nest$id <- 1:nrow(tb_nest)

# walk(tb_nest$data,
#      function(.x) {
#          # nrow(.x)
#          # n_cult <- n_distinct(.x$cultivar)
#          # n_cult
#          # n_bloc <- n_distinct(.x$bloc)
#          # n_bloc
#          .x |>
#              transform(rendimiento = rendimiento/1000) |>
#              lm(rendimiento ~ bloc + cultivar, data = _) |>
#              # anova() |>
#              car::Anova() |>
#              broom::tidy() |>
#              print()
#         })

tb_emm <- tb_nest |>
    mutate(emmeans = map(data, function(.x) {
               # print(str(.x))
               m0 <- .x |>
                   transform(rendimiento = rendimiento/1000) |>
                   lm(rendimiento ~ bloc + cultivar, data = _)
               # print(anova(m0))
               emmeans::emmeans(m0, spec = ~cultivar) |>
                   multcomp::cld(Letters = letters,
                                 adjust = "bonferroni",
                                 sort = TRUE,
                                 reverse = TRUE) |>
                   as.data.frame()
           })) |>
    select(-data) |>
    unnest(emmeans) |>
    ungroup() |>
    janitor::clean_names() |>
    mutate(group = trimws(group))

# Ordenar por `emmean`.
tb_emm <- tb_emm |>
    arrange(id, emmean) |>
    mutate(id_cultivar = str_c(id, cultivar, sep = "_"),
           id_cultivar = factor(id_cultivar, levels = unique(id_cultivar)))
str(tb_emm)

ggplot(data = tb_emm,
       mapping = aes(x = id_cultivar,
                     # color = epoca,
                     color = ciclo,
                     y = emmean)) +
    # facet_wrap(facets = ~id, scales = "free") +
    # facet_wrap(facets = ~id, scales = "free_y") +
    facet_wrap(facets = ~interaction(filename, epoca, sep = " - "),
               scales = "free") +
    # geom_point() +
    geom_pointrange(mapping = aes(ymin = lower_cl,
                                  ymax = upper_cl),
                    # show.legend = FALSE,
                    cex = 0.25) +
    geom_text(mapping = aes(y = -Inf, label = group),
              hjust = 0,
              show.legend = FALSE) +
    coord_flip() +
    scale_x_discrete(labels = function(x) sub(".*_", "", x)) +
    labs(title = "Rendimiento por cultivar",
         x = "Cultivar",
         y = "Rendimiento (t/ha)",
         color = "Ciclo")

svg("rendimiento_cultivares.svg", width = 1.4 * 16, height = 1.4 * 9)
print(last_plot())
dev.off()

#-----------------------------------------------------------------------
