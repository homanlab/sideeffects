# Side effects of antipsychotics in schizophrenia spectrum disorders

## Authors

Maria S. Neumeier, Stephanie Homan, Stefan Vetter, Erich Seifritz, John
M. Kane, Maximilian Huhn, Stefan Leucht, Philipp Homan

## Getting Started
This repository contains all the data and analysis code to reproduce the
manuscript about side effects of antipsychotics in schizophrenia
spectrum disorders. These instructions describe how to obtain a copy of
the project up and running on your local machine for reproducing the
analysis described in the manuscript.

### Prerequisites
All analyses were conducted with the R software R version 3.6.1
(2019-07-05). The full session info under R can be found at the end of
this file

## Installing
Clone the repository or download the zip file.

## Producing the manuscript
Change to the src directory and run
'rmarkdown::render("sideeffects_ms.Rmd').

## Built With
Ubuntu 18.04.2 on emacs 25.2.2 and org-mode 9.1.7.

## Session info
```
R version 3.6.1 (2019-07-05)
Platform: x86_64-pc-linux-gnu (64-bit)
Running under: Ubuntu 18.04.2 LTS

Matrix products: default
BLAS:   /usr/lib/x86_64-linux-gnu/blas/libblas.so.3.7.1
LAPACK: /usr/lib/x86_64-linux-gnu/lapack/liblapack.so.3.7.1

locale:
 [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
 [3] LC_TIME=de_CH.UTF-8        LC_COLLATE=en_US.UTF-8    
 [5] LC_MONETARY=de_CH.UTF-8    LC_MESSAGES=en_US.UTF-8   
 [7] LC_PAPER=de_CH.UTF-8       LC_NAME=C                 
 [9] LC_ADDRESS=C               LC_TELEPHONE=C            
[11] LC_MEASUREMENT=de_CH.UTF-8 LC_IDENTIFICATION=C       

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] kableExtra_1.1.0       papaja_0.1.0.9942      rmarkdown_2.1         
 [4] knitr_1.26             represearch_0.0.0.9000 metafor_2.1-0         
 [7] Matrix_1.2-17          cowplot_0.9.4          forcats_0.4.0         
[10] stringr_1.4.0          purrr_0.3.2            tibble_2.1.3          
[13] tidyverse_1.2.1        ggplot2_3.2.0          broom_0.5.2           
[16] sendmailR_1.2-1        readxl_1.3.1           readr_1.3.1           
[19] devtools_2.1.0         usethis_1.5.1          dplyr_0.8.3           
[22] tidyr_0.8.3            pacman_0.5.1          
```
