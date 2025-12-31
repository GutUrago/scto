
# scto: Modern Data Pipelines for 'SurveyCTO'

<!-- badges: start -->
[![R-CMD-check](https://github.com/GutUrago/scto/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/GutUrago/scto/actions/workflows/R-CMD-check.yaml)
[![DOI](https://zenodo.org/badge/1121002963.svg)](https://doi.org/10.5281/zenodo.18107568)
<!-- badges: end -->

An R client for the [SurveyCTO REST API](https://developer.surveycto.com/), a mobile and offline data collection 
platform, providing a modern and consistent interface for programmatic 
access to server resources. Built on top of the [httr2 package](https://httr2.r-lib.org/), it 
enables secure and efficient data retrieval and returns analysis-ready 
outputs through optional tidying. The package also generates clean variable and 
value labels compatible with 'Stata'. Additional functionality includes 
downloading survey metadata, form definitions, and associated attachments. 
Robust authentication and request handling make the package suitable for 
automated survey monitoring and downstream analysis.

## Installation

You can install the development version of scto like so:

``` r
# devtools::install_github("guturago/scto")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(scto)
## basic example code
```

