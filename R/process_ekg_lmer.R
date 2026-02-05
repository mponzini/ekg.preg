#' @export

process_ekg_lmer <- function(object, se = TRUE, p.value = TRUE){
  if(!(class(object) %in% c('lmerModLmerTest'))){
    stop("object must have class 'lmerModLmerTest'")
  }

  # summary table
  tmp <- summary(object)$coefficients |>
    tibble::as_tibble(rownames = NA) |>
    tibble::rownames_to_column(var = "Variable")

  if (se) {
    est_se <- tmp |>
      dplyr::mutate(
        dplyr::across(
          .cols = c(Estimate, `Std. Error`),
          ~ .x |> round(2) |> format(nsmall = 2)
        )
      ) |>
      tidyr::unite("est_se", Estimate, `Std. Error`, sep = " (") |>
      dplyr::mutate(est_se = paste0(est_se, ")")) |>
      dplyr::pull(est_se)

    ans <- data.frame(
      Predictor = c(paste0("Trimester [", c(0:5), "]")),
      Estimate = est_se
    )
  } else {
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

    ans <- data.frame(
      Predictor = c(paste0("Trimester [", c(0:5), "]")),
      Estimate = est_ci
    )
  }

  if (p.value) {
    ans <- ans |>
      dplyr::mutate(
        p.value = tmp$`Pr(>|t|)` |> round(3)
      ) |>
      dplyr::mutate(
        p.value = ifelse(p.value == 0.000, "<0.001", format(p.value, nsmall = 3))
      ) |>
      dplyr::mutate(
        p.value = ifelse(Predictor == "Trimester [0]", "", p.value)
      )
  }

  return(ans)

}
