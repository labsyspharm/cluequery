MIN_N_QUERY = 10
MAX_N_QUERY = 150
MAX_N_SETS_QUERY = 25

#' Check if genes are in BING space
#'
#' The connectivity map only contains expression information about a set of
#' genes called Best INFerred Genes (BING), see
#' \href{https://clue.io/connectopedia/category/Concept}{Connectopedia}.
#' This function subsets the input vector of gene ids to only contain BING
#' genes.
#'
#' @param genes Vector of gene IDs
#' @return Vector of gene IDs subsetted to only contain BING genes.
#' @export
clue_check_bing <- function(genes) {
  genes <- unique(genes)
  is_bing <- genes %in% gene_space[gene_space$bing, ]$entrez_id
  n_not_bing <- length(genes) - sum(is_bing)
  if (n_not_bing > 0) {
    warning(
      "Of ", length(genes), " genes, ", n_not_bing, " are not in BING space",
      call. = FALSE
    )
  }
  genes[is_bing]
}

#' Prepare GMT functions
#'
#' @return Named vector with paths to the GMT files for the up-regulated
#' and the down-regulated gene sets.
#' @name clue_prepare_funs
NULL

#' @describeIn clue_prepare_funs Prepare a DESeq2 result for Clue
#' @param result_df Data frame returned by \code{\link[DESeq2]{results}}
#' function
#' @param alpha Significance cutoff set during DESeq2 analysis.
#' @export
clue_gmt_from_deseq2 <- function(result_df, name, alpha = 0.05) {
  result_df %>%
    tibble::as_tibble() %>%
    dplyr::filter(padj <= alpha) %>%
    dplyr::arrange(log2FoldChange) %>%
    dplyr::mutate(
      direction = ifelse(log2FoldChange <= 0, "down", "up"),
      gene_set = name
    ) %>%
    clue_gmt_from_df()
}

#' @describeIn clue_prepare_funs Prepare gmt files from a data frame
#' @param gene_set_df Data frame of gene sets. See Details for format.
#' @param drop_invalid If TRUE, drop invalid gene sets with warning. Otherwise
#' an error is raised.
#' @export
clue_gmt_from_df <- function(gene_set_df, drop_invalid = FALSE) {
  if (!is.character(gene_set_df$gene_id)) {
    warning("Coercing gene_id to `character`.", call. = FALSE)
    gene_set_df <- gene_set_df %>%
      dplyr::mutate(gene_id = as.character(gene_id))
  }
  n_gene_sets <- length(unique(gene_set_df$gene_set))
  if (n_gene_sets > MAX_N_SETS_QUERY) {
    warning(
      "Can't submit more than ", MAX_N_SETS_QUERY, " gene sets per query. ",
      "Continue with the first ", MAX_N_QUERY, " gene sets.",
      call. = FALSE
    )
  }

  gene_sets_raw <- dplyr::group_nest(gene_set_df, gene_set) %>%
    dplyr::mutate(
      data = purrr::map2(
        gene_set, data,
        clue_check_gene_set_df
      )
    )

  invalid_sets <- purrr::map_lgl(gene_sets_raw$data, is.null)
  if (any(invalid_sets)) {
    invalid_names <- paste(gene_sets_raw[invalid_sets, ][["gene_set"]], sep = ", ")
    if (!drop_invalid)
      stop("Invalid gene sets: ", invalid_names)
    else
      warning("Dropping invalid gene sets: ", invalid_names, call. = FALSE)
  }

  gene_sets_raw <- gene_sets_raw %>%
    filter(!invalid_sets)
  if(nrow(gene_sets_raw) == 0)
    return(NULL)

  gene_sets <- gene_sets_raw %>%
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
      cmapR::write_gmt(gmt, f)
    }
  )
  purrr::set_names(gene_sets_by_dir$tmp_file, gene_sets_by_dir$direction)
}

#' @describeIn clue_prepare_funs Prepare gmt files from a list of genes
#' @param up,down Vectors of up- and down-regulated gene IDs.
#' @param name Name of gene set.
#' @export
clue_gmt_from_list <- function(
  up, down, name, drop_invalid = FALSE
) {
  dplyr::bind_rows(
    tibble::tibble(direction = "up", gene_id = up),
    tibble::tibble(direction = "down", gene_id = down)
  ) %>%
    dplyr::mutate(gene_set = name) %>%
    clue_gmt_from_df(drop_invalid = drop_invalid)
}

clue_check_gene_set_df <- function(gs, d) {
  valid <- TRUE
  bing_genes <- clue_check_bing(d$gene_id)
  n_bing <- length(unique(bing_genes))
  n_in <- length(unique(d$gene_id))
  if (n_bing != n_in) {
    warning(
      "In gene set ", gs, " of ", n_in, " genes, ", n_in - n_bing, " are not BING genes. ",
      "Excluding them from analysis. ", n_bing, " genes left.",
      call. = FALSE
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
        "Excluding gene set from analysis.",
        call. = FALSE
      )
      valid <- FALSE
    }
    if (n > MAX_N_QUERY) {
      warning(
        "In gene set ", gs, ", ", n, " are in the ", dir,
        "-regulated list. Maximum is ", MAX_N_QUERY, ". ",
        "Only keeping the first ", MAX_N_QUERY,
        call. = FALSE
      )
    }
  }
  if (!valid)
    return(NULL)
  d_bing %>%
    dplyr::group_by(direction) %>%
    dplyr::filter(dplyr::n() >= MIN_N_QUERY) %>%
    dplyr::slice(1:min(MAX_N_QUERY, dplyr::n())) %>%
    dplyr::ungroup()
}
