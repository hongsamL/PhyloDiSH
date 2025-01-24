# PhyloDiSH
Phylogenetic Downsampling in Shiny

## Overview
PhyloDiSH (Phylogenetic Downsampling in Shiny) is an application designed to downsample phylogenetic trees to facilitate genomic epidemiological analyses. This tool is particularly useful for researchers and scientists working in the field of genomics and epidemiology.

PhyloDiSH is developed in R and deployed using Shiny with `webr` and `shinylive`, ensuring it runs entirely on the client side. This guarantees that any files you upload never leave your computer, thereby protecting your privacy.

## Features
Currently, PhyloDiSH implements the downsampling method by monophyletic cluster as described in Hong et al. 2020. This method is aimed at reducing the size of phylogenetic trees when performing discrete state phylogeographic analyses. For more details on the downsampling method, please refer to the original publication:
Hong et al. 2020. ([https://doi.org/10.3390/v12020182](https://doi.org/10.3390/v12020182))
