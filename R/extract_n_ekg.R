#' @export

extract_n_ekg <- function(
    dataset, trimester = NULL
){
  if(is.null(trimester)){
    dataset |>
      nrow()
  } else{
    dataset |>
      dplyr::filter(Trimester == trimester) |>
      nrow()
  }

}
