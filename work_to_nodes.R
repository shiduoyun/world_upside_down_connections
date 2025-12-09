library(janitor)
library(dplyr)
library(stringr)
library(stringi)
library(jsonlite)

# Simple slug function (avoid make_clean_names numbering duplicates)
slugify <- function(x) {
    x %>%
        stri_trans_general("Latin-ASCII") %>%
        str_to_lower() %>%
        str_replace_all("[^a-z0-9]+", "_") %>%
        str_replace_all("_+", "_") %>%
        str_remove("^_") %>%
        str_remove("_$")
}

# Read and tidy the CSV
work <- read.csv("./data/work.csv", stringsAsFactors = FALSE) %>%
    clean_names() %>%
    as_tibble() %>%
    rename(level = level_1_institution_2_group_3_body) %>%
    mutate(
        year = suppressWarnings(as.integer(time)),
        level = suppressWarnings(as.integer(level))
    )

# Build a stable id and a compact shortTitle
work_nodes <- work %>%
    mutate(
        title = work,
        shortTitle = if_else(
            str_length(title) > 40,
            str_c(str_sub(title, 1, 37), "..."),
            title
        ),
        id = str_c(
            slugify(author),
            year,
            slugify(title),
            sep = "_"
        ),
        concepts = key_concepts,
        summary = thesis_summary
    ) %>%
    select(id, year, author, shortTitle, title, level, concepts, summary)

# Serialize to a JS file
json_body <- toJSON(work_nodes, auto_unbox = TRUE, pretty = TRUE)
js_output <- str_c("const bookNodes = ", json_body, ";\n")
writeLines(js_output, "bookNodes.js")
