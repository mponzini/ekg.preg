# Installation
-  install.packages("pak")
-  pak::pkg_install("mponzini/ekg.preg")

# To run locally
Create `inst/extdata/` directory and upload the raw data files  
Run `data-raw/import_raw_data.R` to process the normal data  
Document the package to have the processed data available within the package locally (`devtools::document()`)  
Report and analysis subfiles can be found in the `inst/analyses/` directory
