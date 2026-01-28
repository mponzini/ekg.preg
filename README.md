# Installation
-  install.packages("pak")
-  pak::pkg_install("mponzini/ekg.preg")

# In RStudio
Create New Project from Version Control  
Create `inst/extdata/` directory and copy the raw data files **LOCALLY**  
Run `data-raw/import_raw_data.R` to process the normal data  
Document the package to have the processed data available within the package locally (`devtools::document()`)  
Report and analysis subfiles can be found in the `inst/analyses/` directory

# Features

## Trend Testing
The package includes functions to test for trends in EKG features across pregnancy trimesters:

- `test_ekg_trend()`: Test for trends in a single EKG feature using both non-parametric (Jonckheere-Terpstra) and parametric (polynomial contrasts) methods
- `process_ekg_trends()`: Process multiple EKG features at once and return a formatted summary table

See `inst/TREND_TEST_GUIDE.md` for detailed documentation and examples.
