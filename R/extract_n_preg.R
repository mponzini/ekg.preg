#' @export

extract_n_preg <- function(
    dataset, trimester = NULL
){
  if(is.null(trimester)){
    dataset |>
      dplyr::pull(PregKey) |>
      unique() |>
      length()
  } else{
    dataset |>
      dplyr::filter(Trimester == trimester) |>
      dplyr::pull(PregKey) |>
      unique() |>
      length()
  }

}
