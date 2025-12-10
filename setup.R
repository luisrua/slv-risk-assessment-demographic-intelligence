# This script is for functionality that is very commonly used across
# the actual analysis scripts 

#' Convenience function for loading a package and installing if necessaryt
#'
#' @param package package to be installed, must be a character string
#' @param quietly passed through to \code{require}
#' @param github a github user and repo, passed through to install_github. If
#'   blank, package is installed from CRAN
#' @param ... passed through to \code{require}    
#' @details if package cannot be loaded, this function will try to install it.
#'   If github is not provided then it will try to install from CRAN, otherwise
#'   it will install from the GitHub repo provided
library2 <- function(package, quietly = TRUE, github = NULL, ...){
  if(!require(package, character.only = TRUE, quietly = quietly, ...)){
    if(is.null(github)){
      install.packages(package)
    } else {
      remotes::install_github(github)
    }
    require(package, character.only = TRUE, quietly = quietly, ...)
  }
  
}



# counter for checking if we have run setup.R before
if(!exists(".setup_counter")){
  .setup_counter <- 0
  quiet <- FALSE
} else {
  quiet <- TRUE
}

# LIBRARIES 
# General tools
library2("dplyr")
library2("tidyr")
library2("tidyverse")
library2("readxl")
library2("lubridate")
library2("janitor")
library2("tictoc") # processing time
library2("haven") # to read stata datasets
library2("tmap") # to map resups
library2("openxlsx")

# For spatial analysis
library2("terra")
library2("sp")
library2("sf")
library2("tidyterra")
library2("conflicted")
library2("raster")
# install.packages("sfdct")
# library2("sfdct")

# Mapping
library2("leaflet")
library2("leaflet.extras")
library2("htmlwidgets")


# library("ISOcodes")
# library("patchwork")   # layout multiple charts in one image
# library("ggrepel")     # add tet labels to points without overlapping

# CONFLICT WITH FUNCTIONS 
conflict_prefer("select", "dplyr", quiet = quiet)
conflict_prefer("filter", "dplyr", quiet = quiet)
conflict_prefer("year", "lubridate", quiet = quiet)
conflict_prefer("first", "dplyr", quiet = quiet)
conflict_prefer("lag", "dplyr", quiet = quiet)
conflict_prefer("intersect","base", quiet=quiet)

# silence a ubiquitous and annoying message
options(dplyr.summarise.inform = FALSE)

## Function to get labels from stata datasets
get_labels <- function(d){
  tibble(short = names(d),
         long = as.character(sapply(d, function(x){attributes(x)$label}))
  ) |>
    mutate(column_number = 1:n())
}