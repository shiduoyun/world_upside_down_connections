library(janitor)
library(dplyr)
library(stringr)
library(stringi)
library(jsonlite)

# Simple slug to keep IDs stable (no automatic de-dup numbering)
slugify <- function(x) {
  x %>%
    stri_trans_general("Latin-ASCII") %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]+", "_") %>%
    str_replace_all("_+", "_") %>%
    str_remove("^_") %>%
    str_remove("_$")
}

make_id <- function(author, title, year) {
  str_c(slugify(author), year, slugify(title), sep = "_")
}

# Load works to build ID lookup
works <- read.csv("./data/work.csv", stringsAsFactors = FALSE) %>%
  clean_names() %>%
  as_tibble() %>%
  rename(level = level_1_institution_2_group_3_body) %>%
  mutate(
    year = suppressWarnings(as.integer(time)),
    id = make_id(author, work, year),
    key = str_c(slugify(author), "|", slugify(work))
  ) %>%
  select(key, id)

work_lookup <- setNames(works$id, works$key)

# Load connections
conn <- read.csv("./data/connection.csv", stringsAsFactors = FALSE) %>%
  clean_names()

# Normalize work titles that redundantly include the author name
strip_author_prefix <- function(work_title, author) {
  work_slug <- slugify(work_title)
  author_slug <- slugify(author)
  prefix <- str_c(author_slug, "_")
  needs_strip <- str_starts(work_slug, prefix) %in% TRUE
  ifelse(needs_strip, str_remove(work_slug, stringr::fixed(prefix)), work_slug)
}

# Build source/target keys to map to IDs
conn_links <- conn %>%
  mutate(
    from_work_slug = strip_author_prefix(from_work_if_applicable, from_author),
    to_work_slug = strip_author_prefix(to_work, to_author),
    source_key = str_c(slugify(from_author), "|", from_work_slug),
    target_key = str_c(slugify(to_author), "|", to_work_slug),
    source = unname(work_lookup[source_key]),
    target = unname(work_lookup[target_key]),
    relation = explanation
  )

# Warn on unmatched rows
missing <- conn_links %>% filter(is.na(source) | is.na(target))
if (nrow(missing) > 0) {
  message("Unmatched connections (dropped):")
  print(missing %>% select(from_author, from_work_if_applicable, to_work, to_author))
}

conn_links <- conn_links %>%
  filter(!is.na(source), !is.na(target)) %>%
  select(source, target, relation)

# Serialize to JS
json_body <- toJSON(conn_links, auto_unbox = TRUE, pretty = TRUE)
js_output <- str_c("const links = ", json_body, ";\n")
writeLines(js_output, "links.js")
