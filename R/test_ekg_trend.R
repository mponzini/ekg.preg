#' Test for Trends in EKG Features Across Trimesters
#'
#' @description
#' Performs trend tests to determine if EKG features change systematically
#' across pregnancy trimesters. Since trends are unlikely to be linear, this
#' function provides both non-parametric (Jonckheere-Terpstra) and parametric
#' (polynomial contrasts) trend tests.
#'
#' @param data A data frame containing EKG measurements with a Trimester variable
#' @param ekg_var Character string specifying the EKG feature to test (e.g.,
#'   "QRS_Duration", "QTc_Bazett", "PR_Interval", "VentRate")
#' @param trimesters Vector of trimester values to include in the trend test.
#'   Default is c(0, 1, 2, 3, 4) which tests from pre-pregnancy through early
#'   postpartum. Can be customized to test specific periods (e.g., c(1, 2, 3)
#'   for pregnancy only).
#'
#' @return A list with components:
#'   \item{jt_test}{Results from Jonckheere-Terpstra test (non-parametric)}
#'   \item{polynomial_tests}{Results from polynomial contrast tests}
#'   \item{summary}{Formatted summary of test results}
#'
#' @details
#' The Jonckheere-Terpstra test is a non-parametric test that detects monotonic
#' trends without assuming linearity. It's particularly useful when the trend
#' may be non-linear or when data distributions are non-normal.
#'
#' Polynomial contrasts test for linear, quadratic, and cubic trends, which can
#' help characterize the shape of the trend (e.g., accelerating, decelerating).
#'
#' @examples
#' \dontrun{
#' # Test for trend in QTc interval across all periods
#' qtc_trend <- test_ekg_trend(ekg_normal, "QTc_Bazett")
#'
#' # Test for trend in heart rate during pregnancy only
#' hr_trend <- test_ekg_trend(ekg_normal, "VentRate", trimesters = c(1, 2, 3))
#' }
#'
#' @export
test_ekg_trend <- function(data, ekg_var, trimesters = c(0, 1, 2, 3, 4)) {
  # Input validation
  if (!ekg_var %in% names(data)) {
    stop(paste0("Variable '", ekg_var, "' not found in data"))
  }
  if (!"Trimester" %in% names(data)) {
    stop("Data must contain a 'Trimester' variable")
  }
  
  # Filter data to specified trimesters and remove missing values
  dat_filtered <- data |>
    dplyr::filter(Trimester %in% trimesters) |>
    dplyr::filter(!is.na(.data[[ekg_var]])) |>
    dplyr::mutate(
      Trimester_num = as.numeric(as.character(Trimester))
    )
  
  if (nrow(dat_filtered) == 0) {
    stop("No data available after filtering")
  }
  
  # Jonckheere-Terpstra test for monotonic trend
  jt_result <- DescTools::JonckheereTerpstraTest(
    x = dat_filtered[[ekg_var]],
    g = dat_filtered$Trimester_num
  )
  
  # Polynomial contrast tests using linear model
  # Convert Trimester to ordered factor for polynomial contrasts
  dat_filtered$Trimester_ord <- factor(
    dat_filtered$Trimester_num,
    ordered = TRUE
  )
  
  # Fit model with polynomial contrasts
  lm_model <- lm(
    formula = as.formula(paste0(ekg_var, " ~ Trimester_ord")),
    data = dat_filtered,
    contrasts = list(Trimester_ord = "contr.poly")
  )
  
  # Extract polynomial contrast results
  lm_summary <- summary(lm_model)
  poly_coefs <- lm_summary$coefficients
  
  # Get degrees of freedom for polynomial tests
  n_trimesters <- length(unique(dat_filtered$Trimester_num))
  max_poly_degree <- min(n_trimesters - 1, 3)  # Up to cubic
  
  poly_results <- list()
  poly_names <- c("Linear", "Quadratic", "Cubic")
  
  for (i in 1:max_poly_degree) {
    row_name <- paste0("Trimester_ord", c(".L", ".Q", ".C")[i])
    if (row_name %in% rownames(poly_coefs)) {
      poly_results[[poly_names[i]]] <- list(
        estimate = poly_coefs[row_name, "Estimate"],
        statistic = poly_coefs[row_name, "t value"],
        p_value = poly_coefs[row_name, "Pr(>|t|)"]
      )
    }
  }
  
  # Create formatted summary
  summary_text <- paste0(
    "Trend Test Results for ", ekg_var, "\n",
    "Trimesters tested: ", paste(trimesters, collapse = ", "), "\n",
    "N observations: ", nrow(dat_filtered), "\n\n",
    "Jonckheere-Terpstra Test (non-parametric):\n",
    "  Statistic: ", round(jt_result$statistic, 2), "\n",
    "  p-value: ", format.pval(jt_result$p.value, digits = 3), "\n\n",
    "Polynomial Trend Tests:\n"
  )
  
  for (trend_type in names(poly_results)) {
    summary_text <- paste0(
      summary_text,
      "  ", trend_type, " trend: p-value = ",
      format.pval(poly_results[[trend_type]]$p_value, digits = 3), "\n"
    )
  }
  
  # Return results
  result <- list(
    jt_test = jt_result,
    polynomial_tests = poly_results,
    summary = summary_text,
    n_obs = nrow(dat_filtered),
    trimesters_tested = trimesters
  )
  
  class(result) <- "ekg_trend_test"
  return(result)
}


#' Process Multiple EKG Trend Tests
#'
#' @description
#' Convenience function to test trends for multiple EKG features at once
#' and return a formatted summary table.
#'
#' @param data A data frame containing EKG measurements
#' @param ekg_vars Character vector of EKG feature names to test
#' @param trimesters Vector of trimester values to include in the trend test
#'
#' @return A data frame with trend test results for each EKG feature
#'
#' @examples
#' \dontrun{
#' # Test trends for all main EKG features
#' all_trends <- process_ekg_trends(
#'   ekg_normal,
#'   c("PR_Interval", "QRS_Duration", "QTc_Bazett", "VentRate")
#' )
#' }
#'
#' @export
process_ekg_trends <- function(data, ekg_vars, trimesters = c(0, 1, 2, 3, 4)) {
  
  results <- lapply(ekg_vars, function(var) {
    tryCatch({
      trend_result <- test_ekg_trend(data, var, trimesters)
      
      # Extract key statistics
      jt_pval <- trend_result$jt_test$p.value
      linear_pval <- if (!is.null(trend_result$polynomial_tests$Linear)) {
        trend_result$polynomial_tests$Linear$p_value
      } else {
        NA
      }
      quadratic_pval <- if (!is.null(trend_result$polynomial_tests$Quadratic)) {
        trend_result$polynomial_tests$Quadratic$p_value
      } else {
        NA
      }
      
      data.frame(
        EKG_Feature = var,
        N = trend_result$n_obs,
        JT_p_value = gtsummary::style_pvalue(jt_pval),
        Linear_p = gtsummary::style_pvalue(linear_pval),
        Quadratic_p = gtsummary::style_pvalue(quadratic_pval),
        Interpretation = interpret_trend(
          jt_pval, linear_pval, quadratic_pval
        ),
        stringsAsFactors = FALSE
      )
    }, error = function(e) {
      data.frame(
        EKG_Feature = var,
        N = NA,
        JT_p_value = "Error",
        Linear_p = "Error",
        Quadratic_p = "Error",
        Interpretation = paste0("Error: ", e$message),
        stringsAsFactors = FALSE
      )
    })
  })
  
  do.call(rbind, results)
}


#' Interpret Trend Test Results
#'
#' @description
#' Helper function to provide interpretation of trend test results
#'
#' @param jt_p p-value from Jonckheere-Terpstra test
#' @param linear_p p-value from linear trend test
#' @param quadratic_p p-value from quadratic trend test
#' @param alpha Significance level (default 0.05)
#'
#' @return Character string with interpretation
#'
#' @keywords internal
interpret_trend <- function(jt_p, linear_p, quadratic_p, alpha = 0.05) {
  if (is.na(jt_p)) {
    return("Insufficient data")
  }
  
  if (jt_p >= alpha) {
    return("No significant trend")
  }
  
  # There is a significant monotonic trend
  if (!is.na(linear_p) && linear_p < alpha) {
    if (!is.na(quadratic_p) && quadratic_p < alpha) {
      return("Significant non-linear trend")
    } else {
      return("Significant linear trend")
    }
  } else if (!is.na(quadratic_p) && quadratic_p < alpha) {
    return("Significant non-linear trend")
  } else {
    return("Significant monotonic trend")
  }
}


#' Print Method for EKG Trend Test
#'
#' @param x An object of class "ekg_trend_test"
#' @param ... Additional arguments (not used)
#'
#' @export
print.ekg_trend_test <- function(x, ...) {
  cat(x$summary)
  invisible(x)
}
