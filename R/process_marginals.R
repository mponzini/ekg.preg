#' @export

process_marginals <- function(object, outcome, se = TRUE) {
  if (se) {
    ans <- object |>
      tibble::as_tibble() |>
      # round to 2 decimals
      dplyr::mutate(
        dplyr::across(
          .cols = where(is.numeric),
          ~ .x |> round(2)
        )
      ) |>
      # combine est and se into single column
      tidyr::unite(
        col = "tmp",
        predicted, std.error,
        sep = " ("
      ) |>
      dplyr::mutate(
        tmp = paste0(tmp, ")")
      ) |>
      # rename columns
      dplyr::rename(
        "Trimester" = x,
        !!outcome := tmp
      ) |>
      # remove unnecessary columns
      dplyr::select(-c(conf.low, conf.high, group))
  } else {
    ans <- object |>
      tibble::as_tibble() |>
      # round to 2 decimals
      dplyr::mutate(
        dplyr::across(
          .cols = where(is.numeric),
          ~ .x |> round(2)
        )
      ) |>
      # combine ci into single column
      tidyr::unite("ci", conf.low, conf.high, sep = ", ") |>
      # combine est and ci into single column
      tidyr::unite(
        col = "tmp",
        predicted, ci,
        sep = " ("
      ) |>
      dplyr::mutate(
        tmp = paste0(tmp, ")")
      ) |>
      # rename columns
      dplyr::rename(
        "Trimester" = x,
        !!outcome := tmp
      ) |>
      # remove unnecessary columns
      dplyr::select(-c(std.error, group))
  }

  return(ans)
}
