MIN_N_QUERY = 10
MAX_N_QUERY = 150
MAX_N_SETS_QUERY = 25

#' Check if genes are in BING space
#'
#' @export
clue_check_bing <- function(genes) {
  is_bing <- genes %in% gene_space[gene_space$bing, ]$entrez_id
  if (sum(is_bing) < length(genes)) {
    warning("Some genes not in BING space")
  }
  genes[is_bing]
}

#' Prepare a DESeq2 result for Clue
#'
#' @export
clue_prepare_deseq2 <- function(result_df, gene_set_name, alpha = 0.05) {
  gene_set_df <- result_df %>%
    dplyr::filter(padj <= alpha) %>%
    dplyr::arrange(log2FoldChange) %>%
    dplyr::mutate(
      direction = ifelse(log2FoldChange <= 0, "down", "up"),
      gene_set = gene_set_name
    )
  gene_set_df
}

clue_check_gene_set_df <- function(gs, d) {
  valid <- TRUE
  bing_genes <- clue_check_bing(d$gene_id)
  n_bing <- length(unique(bing_genes))
  n_in <- length(unique(d$gene_id))
  if (n_bing != n_in) {
    warning(
      "In gene set ", gs, " of ", n_in, " genes, ", n_in - n_bing, " are not BING genes. ",
      "Excluding them from analysis. ", n_bing, " genes left."
    )
  }
  d_bing <- d %>%
    dplyr::filter(gene_id %in% bing_genes)
  dir_counts <- as.list(table(d_bing$direction))
  for (dir in c("up", "down")) {
    n <- if (is.null(dir_counts[[dir]])) 0 else dir_counts[[dir]]
    if (n < MIN_N_QUERY) {
      warning(
        "In gene set ", gs, " only ", n, " are in the ", dir,
        "-regulated list. Minimum required is ", MIN_N_QUERY, ".",
        "Excluding gene set from analysis."
      )
      valid <- FALSE
    }
    if (n > MAX_N_QUERY) {
      warning(
        "In gene set ", gs, ", ", n, " are in the ", dir,
        "-regulated list. Maximum is ", MAX_N_QUERY, ". ",
        "Only keeping the ", MAX_N_QUERY, " with highest absolute fold-change."
      )
    }
  }
  if (!valid)
    return(NULL)
  d_bing %>%
    dplyr::group_by(direction) %>%
    dplyr::filter(dplyr::n() >= MIN_N_QUERY) %>%
    dplyr::arrange(dplyr::desc(abs(log2FoldChange))) %>%
    dplyr::slice(1:min(MAX_N_QUERY, dplyr::n())) %>%
    dplyr::ungroup()
}

#' Prepare gmt files from data frames
#'
#' @export
clue_gmt_from_df <- function(gene_set_df, drop_invalid = FALSE) {
  if (!is.character(gene_set_df$gene_id)) {
    warning("Coercing gene_id to `character`.")
    gene_set_df <- gene_set_df %>%
      dplyr::mutate(gene_id = as.character(gene_id))
  }
  n_gene_sets <- length(unique(gene_set_df$gene_set))
  if (n_gene_sets > MAX_N_SETS_QUERY) {
    warning(
      "Can't submit more than ", MAX_N_SETS_QUERY, " gene sets per query. ",
      "Continue with the first ", MAX_N_QUERY, " gene sets."
    )
  }
  gene_sets <- dplyr::group_nest(gene_set_df, gene_set) %>%
    dplyr::slice(seq_len(min(MAX_N_QUERY, dplyr::n()))) %>%
    dplyr::mutate(
      data = purrr::map2(
        gene_set, data,
        clue_check_gene_set_df
      )
    ) %>%
    dplyr::filter(!purrr::map_lgl(data, is.null)) %>%
    tidyr::unnest(data) %>%
    dplyr::group_by(gene_set, direction) %>%
    dplyr::summarize(
      gmt = list(
        list(
          head = gene_set[1],
          desc = paste(gene_set[1], direction[1], sep = "_"),
          len = dplyr::n(),
          entry = gene_id
        )
      )
    ) %>%
    dplyr::ungroup()

  gene_sets_by_dir <- gene_sets %>%
    dplyr::group_by(direction) %>%
    dplyr::summarize(gmt = list(gmt)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      tmp_file = purrr::map_chr(seq_len(dplyr::n()), function(x) tempfile(fileext = ".gmt"))
    )

  purrr::walk2(
    gene_sets_by_dir$gmt,
    gene_sets_by_dir$tmp_file,
    function(gmt, f) {
      cmapR::write.gmt(gmt, f)
    }
  )
  browser
  purrr::set_names(gene_sets_by_dir$tmp_file, gene_sets_by_dir$direction)
}
