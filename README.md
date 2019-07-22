
<!-- README.md is generated from README.Rmd. Please edit that file -->

# clueR

<!-- badges: start -->

<!-- badges: end -->

The goal of clueR is to provide an easy R interface to query
Connectivity Map data from [Clue](https://clue.io).

## Installation

You can install the current version of clueR from
[Github](https://github.com/labsyspharm/clueR) with:

``` r
if (!requireNamespace("remotes", quietly = TRUE))
  install.packages("remotes")
remotes::install_github("labsyspharm/clueR")
```

## API key

In order to use this package an API key from Clue is required. For
academice purposes, they are freely available at
[Clue](https://clue.io).

The API key can be automatically retrieved by all `clueR` functions if
it is added to the `~/.Renviron` file:

    CLUE_API_KEY=xxx

## Example

This is a basic example how to query Clue for a gene signature of
interest. Gene signatures can come from any source, but must contain a
set of upregulated genes and a set of downregulated genes.

The gene signatures must be converted into [GMT
format](https://software.broadinstitute.org/cancer/software/genepattern/file-formats-guide#GMT)
for use with Clue.

### Gene signature derived from differential expression with DESeq2

In this example, we generate a gene set compatible with Clue from a
[DESeq2](https://bioconductor.org/packages/release/bioc/html/DESeq2.html)
differential expression experiment.

First, we load our example DESeq2 result:

``` r
library(clueR)
deseq2_res_path <- system.file("extdata", "example_deseq2_result.csv.xz", package = "clueR")
deseq2_res <- read.csv(deseq2_res_path)
head(deseq2_res)
#>    baseMean log2FoldChange     lfcSE     pvalue      padj gene_id
#> 1 4.3724771    0.738529493 0.6278196 0.01665139 0.1310497       1
#> 2 0.2845104   -0.048028976 0.3494750 0.55657836        NA       2
#> 3 5.4377290   -0.004095729 0.2902083 0.97906352 0.9971952    8086
#> 4 7.9978254    0.498179500 0.3916543 0.03767202 0.2267155   65985
#> 5 3.7455513   -0.167951135 0.3488111 0.29332608 0.6829520   79719
#> 6 6.6533538   -0.264704759 0.3473422 0.18680678 0.5510845   22848
```

Note that gene IDs need to be
[Entrez](https://www.ncbi.nlm.nih.gov/gene/) IDs.

We need to choose a name for the gene set and decide on a [significance
cutoff
(alpha)](https://bioconductor.org/packages/release/bioc/vignettes/DESeq2/inst/doc/DESeq2.html#p-values-and-adjusted-p-values)
for our differentially expressed
genes.

``` r
deseq2_gmt <- clue_gmt_from_deseq2(deseq2_res, name = "treatment_drug_x", alpha = 0.05)
#> Warning: Coercing gene_id to `character`.
#> Warning: Of 477 genes, 70 are not in BING space
#> Warning: In gene set treatment_drug_x of 477 genes, 70 are not BING genes.
#> Excluding them from analysis. 407 genes left.
#> Warning: In gene set treatment_drug_x, 264 are in the up-regulated list.
#> Maximum is 150. Only keeping the first 150
str(deseq2_gmt)
#>  Named chr [1:2] "/var/folders/_h/wpz1qzm12t5687lbdm87lsh00000gp/T//RtmpbwGbaz/file65a41ca924d.gmt" ...
#>  - attr(*, "names")= chr [1:2] "down" "up"
```

A number of warnings are raised, indicating that some gene IDs are not
part of the [BING gene
space](https://clue.io/connectopedia/category/Concept). These genes are
removed from the gene sets and not considered by Clue.

`clue_prepare_deseq2` returns a named vector with paths to the GMT files
for the up- and the down-regulated genes.

### Gene signature derived from pre-existing lists

We can also convert an pre-existing set of genes from any source to GMT
files:

``` r
up_genes <- c(
  "10365", "1831", "9314", "4846", "678", "22992", "3397", "26136", 
  "79637", "5551", "7056", "79888", "1032", "51278", "64866", "29775", 
  "994", "51696", "81839", "23580", "219654", "57178", "7014", 
  "57513", "51599", "55818", "4005", "4130", "4851", "2050", "50650", 
  "9469", "54438", "3628", "54922", "3691", "65981", "54820", "2261", 
  "2591", "7133", "162427", "10912", "8581", "2523", "25807", "9922", 
  "30850", "4862", "8567", "79686", "55615", "51283", "3337", "2887", 
  "3223", "6915", "6907", "26056", "259217", "6574", "23097", "5164", 
  "57493", "7071", "5450", "113146", "8650"
)
down_genes <- c(
  "5128", "5046", "956", "10426", "9188", "23403", "7204", "1827", 
  "3491", "9076", "330", "8540", "22800", "10687", "19", "63875", 
  "10979", "51154", "10370", "50628", "7128", "6617", "7187", "22916", 
  "81034", "58516", "3096", "4794", "5202", "26511", "8767", "2355", 
  "22943", "1490", "133", "11010", "51025", "23160", "56902", "3981", 
  "5209", "6347", "5806", "7357", "9425", "3399", "6446", "64328", 
  "6722", "8545", "688", "861", "390", "23034", "51330", "51474", 
  "2633", "4609"
)

pre_gmt <- clue_gmt_from_list(up_genes, down_genes, "my_favourite_genes")
str(pre_gmt)
#>  Named chr [1:2] "/var/folders/_h/wpz1qzm12t5687lbdm87lsh00000gp/T//RtmpbwGbaz/file65a5c31574.gmt" ...
#>  - attr(*, "names")= chr [1:2] "down" "up"
```

### Querying Clue

Now that we have GMT files, we can query Clue. Here we use the GMT files
derived from the DESeq2 result, but we could also use the `pre_gmt`
files generated above.

``` r
submission_result <- clue_query_submit(
  deseq2_gmt[["up"]], deseq2_gmt[["down"]], name = "deseq2_query_job"
)
submission_result$result$job_id
#> [1] "5d36096d12e15519133e87fd"
```

`clue_query_submit` returns a nested list containing information about
the submitted job, including the job id.

Now we have to wait for the job to finish. We can use the function
`clue_query_wait` to pause our script until the results are ready.

``` r
clue_query_wait(submission_result, interval = 60, timeout = 600)
#> Job not completed yet, waiting for: 5d36096d12e15519133e87fd
#> Job not completed yet, waiting for: 5d36096d12e15519133e87fd
#> Job not completed yet, waiting for: 5d36096d12e15519133e87fd
#> Job completed: 5d36096d12e15519133e87fd
```

`clue_query_wait` will automatically continue execution of the script
once results are ready. Now we can download and parse the results:

``` r
result_path <- clue_query_download(submission_result)
result_df <- clue_parse_result(result_path)
#> reading /var/folders/_h/wpz1qzm12t5687lbdm87lsh00000gp/T//RtmpbwGbaz/clueR-65a4facb13e/my_analysis.sig_fastgutc_tool.5d36096d12e15519133e87fd/matrices/gutc/ns_pert_summary.gctx
#> done
```

| pert\_id      | pert\_type | pert\_iname                  | gene\_set          |          ns |
| :------------ | :--------- | :--------------------------- | :----------------- | ----------: |
| BRD-A09094913 | trt\_cp    | strychnine                   | treatment\_drug\_x |   0.0000000 |
| BRD-A55393291 | trt\_cp    | testosterone                 | treatment\_drug\_x |   0.5971081 |
| BRD-A93255169 | trt\_cp    | thalidomide                  | treatment\_drug\_x |   0.1799593 |
| BRD-K41731458 | trt\_cp    | triclosan                    | treatment\_drug\_x |   0.1040271 |
| BRD-K63675182 | trt\_cp    | triflupromazine              | treatment\_drug\_x | \-0.7581666 |
| BRD-A19195498 | trt\_cp    | trimipramine                 | treatment\_drug\_x | \-0.5755145 |
| BRD-K99621550 | trt\_cp    | tubocurarine                 | treatment\_drug\_x |   0.0000000 |
| BRD-A51714012 | trt\_cp    | venlafaxine                  | treatment\_drug\_x |   0.3056950 |
| BRD-K45117373 | trt\_cp    | Y-26763                      | treatment\_drug\_x |   0.0000000 |
| BRD-K42095107 | trt\_cp    | daidzein                     | treatment\_drug\_x |   0.0000000 |

## Funding

We gratefully acknowledge support by NIH Grant 1U54CA225088-01: Systems
Pharmacology of Therapeutic and Adverse Responses to Immune Checkpoint
and Small Molecule Drugs.
