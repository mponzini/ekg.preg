#' @export

extract_n_subj <- function(
    dataset, trimester = NULL
){
  if(is.null(trimester)){
    dataset |>
      dplyr::pull(PatMRN) |>
      unique() |>
      length()
  } else{
    dataset |>
      dplyr::filter(Trimester == trimester) |>
      dplyr::pull(PatMRN) |>
      unique() |>
      length()
  }
}
