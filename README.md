# Installation
-  install.packages("pak")
-  pak::pkg_install("mponzini/ekg.preg")

# In RStudio
Create New Project from Version Control  
Create `inst/extdata/` directory and copy the raw data files **LOCALLY**  
Run `data-raw/import_raw_data.R` to process the normal data  
Document the package to have the processed data available within the package locally (`devtools::document()`)  
Report and analysis subfiles can be found in the `inst/analyses/` directory
