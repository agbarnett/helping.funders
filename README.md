# helping.funders
Helping funders and researchers by creating a list of a researcher's publication output using R and shiny. It can also be used to create a list for a group of reserachers by uploading a file with multiple ORCID IDs (see `example.file.txt`).

The main file is `orcid.R`, which takes an ORCID ID and creates the publiction data. See `global.R` for a list of required R packages. The key R package is `rorcid`.

My wishlist of additions:
* Get the Google Scholar version working on a publicly available shiny server (see https://github.com/agbarnett/scholar.shiny)
* Add in data from Mendeley on what papers are being read
