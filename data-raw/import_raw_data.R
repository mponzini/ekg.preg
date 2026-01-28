## process raw data files ##
ekg_normal_raw <- openxlsx::read.xlsx(
  "./inst/extdata/merged_cleaned_tmp_1_subset_wks_relabelled_normals_3 2.xlsx"
)

# list of ekg continuous variables
ekg_vars <- c(
  "QRS_Duration", "QT_Interval", "QTc_Bazett", "PR_Interval", "VentRate",
  "AvgRRInterval"
)
# list of ekg factor variables
ekg_factors <- c(
  "InjClsRepolarization", "InjClsConduction", "InjClsInfarction",
  "InjClsHypertrophy", "InjClsRhythm", "InjClsWallMotion", "InjClsValve",
  "InjClsMass", "InjClsAnatomical", "InjClsHemodynamic", "InjClsProsthetic"
)

date_vars <- c(
  "ECGAcquisition_Date", "PregfAcquisitionDate",
  "PregEstStartDate", "PregEstEndDate",
  "AcquistionDate_ShiftedDate", "PregEstStartDate_ShiftedDate",
  "PregEstEndDate_ShiftedDate",
  "AcquisitionDateTime_DT_Char", "EpiStartDate"
)


ekg_normal <- ekg_normal_raw |>
  dplyr::filter(
    AnyDxInSelectedGroupers == 0,
    VentRate >= 35
  ) |>
  dplyr::mutate(
    # clean dates
    dplyr::across(
      .cols = dplyr::all_of(date_vars),
      ~ janitor::excel_numeric_to_date(.x)
    ),
    # split tri4 in two: tri4 if <= 14 days postpartum (2wks), otherwise tri5
    Diff = lubridate::time_length(
      lubridate::interval(
        start = PregEstEndDate,
        end = PregfAcquisitionDate
      ),
      unit = "weeks"
    ),
    Trimester = dplyr::case_when(
      (PregECGTrimester_New == 4 & (Diff <= 2)) ~ 4,
      (PregECGTrimester_New == 4 & (Diff > 2)) ~ 5,
      .default = PregECGTrimester_New
    ),
    # create preg/not preg indicator
    Pregnant = ifelse(Trimester == 0 | Trimester == 5, "Not Pregnant", "Pregnant") |>
      factor(levels = c("Not Pregnant", "Pregnant")),
    # replace EKG features of 0 with missing
    dplyr::across(
      .cols = dplyr::all_of(ekg_vars),
      ~ dplyr::na_if(.x, 0)
    ),
    # convert EKG factors to factors
    dplyr::across(
      .cols = dplyr::all_of(ekg_factors),
      ~ factor(.x, levels = c('0', '1'))
    ),
    # create factor variable for Trimester
    Trimester = factor(
      Trimester, levels = c('0', '1', '2', '3', '4', '5')
    ),
    # scale select EKG variables by factor of 10 for analysis
    dplyr::across(
      .cols = c(QRS_Duration, PR_Interval, VentRate, QTc_Bazett),
      ~ .x / 10
    )
  )

usethis::use_data(ekg_normal, overwrite = TRUE)
