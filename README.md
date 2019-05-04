NLP Analysis of My Scientific Jounral Library
================
Joshua Cook
May 4, 2019

In this analysis, I use ‘Pubtator’ to analysis my own biomedical article
library.

## Data

I began by exporting the data for all of my journal articles in Mendeley
as a ‘.bib’ file format. This is available in
“data/mendeley\_library.bib”. The
[‘bibtex’](https://cran.r-project.org/web/packages/bibtex/index.html)
package was used to read in the file as a `bibentry` object.

``` r
bib <- bibtex::read.bib("data/mendeley_library.bib")
class(bib)
```

    #> [1] "bibentry"

After ‘bibtex’ automatically ignores incomplete references, there were
780 entries. I took a look at the first few entries to get an idea of
what I was dealing with.

    #> Zou J, Lei T, Guo P, Yu J, Xu Q, Luo Y, Ke R, Huang D (2019).
    #> "Mechanisms shaping the role of ERK1/2 in cellular senescence
    #> (Review)." _Molecular medicine reports_, *19*(2), 759-770. ISSN
    #> 1791-3004, doi: 10.3892/mmr.2018.9712 (URL:
    #> http://doi.org/10.3892/mmr.2018.9712), <URL:
    #> http://www.ncbi.nlm.nih.gov/pubmed/30535440
    #> http://www.pubmedcentral.nih.gov/articlerender.fcgi?artid=PMC6323238>.
    #> 
    #> Tosi D, Pérez-Gracia E, Atis S, Vié N, Combès E, Gabanou M,
    #> Larbouret C, Jarlier M, Mollevi C, Torro A, Del Rio M, Martineau
    #> P, Gongora C (2018). "Rational development of synergistic
    #> combinations of chemotherapy and molecular targeted agents for
    #> colorectal cancer treatment." _BMC cancer_, *18*(1), 812. ISSN
    #> 1471-2407, doi: 10.1186/s12885-018-4712-z (URL:
    #> http://doi.org/10.1186/s12885-018-4712-z), <URL:
    #> https://doi.org/10.1186/s12885-018-4712-z
    #> http://www.ncbi.nlm.nih.gov/pubmed/30103709
    #> http://www.pubmedcentral.nih.gov/articlerender.fcgi?artid=PMC6090616>.

The data was turned into a tibble for easier use.

``` r
bib <- RefManageR::as.BibEntry(bib) %>%
    as.data.frame() %>% 
    as_tibble()
```

The data was now in a more managable format for data analysis.

    #> # A tibble: 780 x 28
    #>    bibtype abstract author doi   file  issn  journal keywords month number
    #>    <chr>   <chr>    <chr>  <chr> <chr> <chr> <chr>   <chr>    <chr> <chr> 
    #>  1 Article Senesce… Junro… 10.3… ":Us… 1791… Molecu… Dual ro… feb   2     
    #>  2 Article BACKGRO… Diego… 10.1… :Use… 1471… BMC ca… Colorec… aug   1     
    #>  3 Article The acq… Annet… 10.1… :Use… 1937… Scienc… <NA>     mar   166   
    #>  4 Article Kinase … James… 10.1… :Use… 1097… Cell    <NA>     apr   2     
    #>  5 Article The RAS… Susan… 10.1… :Use… 1538… Cancer… <NA>     may   10    
    #>  6 Article Activat… Chris… 10.1… :Use… 1465… Nature… <NA>     may   5     
    #>  7 Article "Lung c… Guill… 10.1… :Use… 1932… PloS o… <NA>     <NA>  1     
    #>  8 Article Mutatio… Alyss… 10.1… :Use… 1097… Molecu… proj:co… apr   1     
    #>  9 Article "BACKGR… Chris… 10.1… :Use… 1474… The La… <NA>     oct   13    
    #> 10 Article <NA>     Asher… 10.1… :Use… 1474… Nature… <NA>     oct   10    
    #> # … with 770 more rows, and 18 more variables: pages <chr>, pmid <chr>,
    #> #   title <chr>, url <chr>, volume <chr>, year <chr>, annote <chr>,
    #> #   isbn <chr>, `mendeley-tags` <chr>, publisher <chr>,
    #> #   archiveprefix <chr>, arxivid <chr>, eprint <chr>, booktitle <chr>,
    #> #   address <chr>, editor <chr>, primaryclass <chr>, urldate <chr>

## Accessing Pubtator

I used the
[’pubmed.mineR](https://cran.r-project.org/web/packages/pubmed.mineR/index.html)
package (here is a link to the original publication [Rani *et al.*,
(2015)](https://www.ncbi.nlm.nih.gov/pubmed/26564970)) to pull
information on the pubications from Pubtator.

The information can be gathered for a publication using
`pubtator_function(pmid)`.

``` r
# turns the pubtator results into a tidy tibble, indexed by PMID
pubtator_to_tibble <- function(pubtato) {
    pubtato <- unlist(pubtato)
    if (!"PMID" %in% names(pubtato)) return(NULL)
    pmid <- pubtato[["PMID"]]  # extract the PMID
    # create a tibble with a row for each field and value, indexed by PMID
    tib <- tibble(PMID = pmid,
                  field = names(pubtato),
                  val = pubtato) %>% 
        filter(field != "PMID") %>%  # remove PMID
        mutate(field = str_remove_all(field, "[:digit:]+"))
    return(tib)
}


pubtatoes <- purrr::map(bib$pmid, pubmed.mineR::pubtator_function) %>% 
    purrr::map(pubtator_to_tibble) %>% 
    bind_rows()
```

    #> # A tibble: 3,717 x 3
    #>    PMID     field     val              
    #>    <chr>    <chr>     <chr>            
    #>  1 30535440 Genes     ERK1/2           
    #>  2 30535440 Diseases  cancer           
    #>  3 30103709 Genes     AKT              
    #>  4 30103709 Genes     MEK1             
    #>  5 30103709 Genes     MEK              
    #>  6 30103709 Genes     ERK              
    #>  7 30103709 Diseases  colorectal cancer
    #>  8 30103709 Diseases  CRC              
    #>  9 30103709 Diseases  tumour           
    #> 10 30103709 Chemicals irinotecan       
    #> # … with 3,707 more rows

There were a few cleaning steps needed. These were completed in the
following pipeline - I tried to add comments wherever I could.

``` r
replace_synonyms <- function(x) purrr::map_chr(x, replace_synonym)
replace_synonym <- function(x) {
    if (x %in% c("epidermal growth factor receptor",
                 "Epidermal growth factor receptor")) {
        return("EGFR")
    } else if (x == "beta-catenin") {
        return("CTNNB1")
    } else if (x %in% c("K-ras", "K-Ras", "Kras", "KRas")) {
        return("KRAS")
    } else if (x %in% c("N-ras", "N-Ras", "Nras", "NRas")) {
        return("NRAS")
    } else if (x %in% c("H-ras", "H-Ras", "Hras", "HRas")) {
        return("HRAS")
    } else if (x %in% c("p53")) {
        return("TP53")
    } else if (x %in% c("Akt")) {
        return("AKT")
    } else if (x %in% c("Raf")) {
        return("RAF")
    } else if (x == "E-cadherin") {
        return("CDH1")
    }
    return(x)
}

pubtatoes %<>% 
    mutate(val = str_remove_all(val, "\\(|\\)")) %>%  # remove parantheses
    mutate(val = replace_synonyms(val)) %>%
    unique()
```

## Simple Analysis

### Which genes do I read about most frequently?

Since I study the genetics of *KRAS*, I expect there to be a lot of
papers about its interactors and other oncogenes in *RAS*-driven cancers
(such as *APC* in CRC and *STK11* in NSCLC).

``` r
pubtatoes %>% 
    filter(field == "Genes") %>%
    mutate(val = forcats::fct_lump_min(val, min = 3)) %>%
    filter(val != "Other") %>%
    mutate(val = forcats::fct_infreq(val)) %>%
    ggplot() +
    geom_bar(aes(x = val)) +
    theme_classic() +
    scale_y_continuous(expand = expand_scale(mult = c(0, 0.05))) +
    theme(axis.text.x = element_text(angle = 65, hjust = 1),
          axis.title.x = element_blank()) +
    labs(y = "count",
         title = "Frequency of genes in articles")
```

![](README_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

As expected, there are a lot of genes in the MAPK pathway,
PI3Kalpha-Akt, oncogenes, and tumor supressors. Funny enough, a small
project that I am working on concerning Polycomb repressive complex 2
(PRC2) has made quite the impact as several of the subunits are in the
list, including *JARID2*, *EZH2*, and *SUZ12*. Interestingly, though,
PRC1 is on the list, but the term PRC2, is not. I wonder if this is just
due to the differences in how people discuss the two related complexes
in papers.
