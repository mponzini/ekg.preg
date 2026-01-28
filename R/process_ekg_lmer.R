#' @export

process_ekg_lmer <- function(object){
  if(!(class(object) %in% c('lmerModLmerTest'))){
    stop("object must have class 'lmerModLmerTest'")
  }

  # summary table
  tmp <- summary(object)$coefficients |>
    tibble::as_tibble(rownames = NA) |>
    tibble::rownames_to_column(var = "Variable")
  # get conf ints
  ci <- confint(object) |> suppressMessages() |>
    tibble::as_tibble(rownames = NA) |>
    tibble::rownames_to_column(var = "Variable")
  # combine estimate and CI, extract
  est_ci <- merge(
    tmp, ci, by = "Variable"
  ) |>
    dplyr::mutate(
      Est_ci = paste0(
        round(Estimate, 2),
        " (",
        round(`2.5 %`, 2),
        ", ",
        round(`97.5 %`, 2),
        ")"
      )
    ) |>
    dplyr::pull(Est_ci)

  data.frame(
    Predictor = c(paste0("Trimester [", c(0:5), "]")),
    Estimate = est_ci
  )
}
