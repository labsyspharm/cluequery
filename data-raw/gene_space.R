library(tidyverse)

# Downloaded from https://clue.io/command?q=/gene-space on 2019-07-02
gene_space_raw <- read_tsv(
  "data-raw/clue_gene_space.txt.xz",
  col_types = "cc--f--"
)

gene_space <- gene_space_raw %>%
  select(entrez_id = `Entrez ID`, symbol = Symbol, type = Type) %>%
  mutate(bing = type %in% c("landmark", "best inferred"))

usethis::use_data(gene_space, compress = "xz", overwrite = TRUE)
