# EKG Trend Test Documentation

## Overview

The trend test functionality provides methods to test whether EKG features change systematically across pregnancy trimesters. Since physiological changes during pregnancy are often non-linear, this implementation provides both non-parametric and parametric trend tests.

## Functions

### `test_ekg_trend()`

Tests for trends in a single EKG feature across specified trimesters.

**Parameters:**
- `data`: Data frame with EKG measurements and a Trimester variable
- `ekg_var`: Name of the EKG feature to test (e.g., "QTc_Bazett", "VentRate")
- `trimesters`: Vector of trimester values to include (default: c(0,1,2,3,4))

**Returns:**
A list containing:
- `jt_test`: Jonckheere-Terpstra test results (non-parametric)
- `polynomial_tests`: Polynomial contrast test results (linear, quadratic, cubic)
- `summary`: Formatted text summary
- `n_obs`: Number of observations
- `trimesters_tested`: Trimesters included in the analysis

**Example:**
```r
# Test for trend in QTc interval across all periods
qtc_trend <- test_ekg_trend(ekg_normal, "QTc_Bazett")
print(qtc_trend)

# Test for trend in heart rate during pregnancy only
hr_trend <- test_ekg_trend(ekg_normal, "VentRate", trimesters = c(1, 2, 3))
```

### `process_ekg_trends()`

Convenience function to test trends for multiple EKG features simultaneously.

**Parameters:**
- `data`: Data frame with EKG measurements
- `ekg_vars`: Character vector of EKG feature names to test
- `trimesters`: Vector of trimester values to include

**Returns:**
A data frame with columns:
- `EKG_Feature`: Name of the EKG variable
- `N`: Number of observations
- `JT_p_value`: p-value from Jonckheere-Terpstra test
- `Linear_p`: p-value from linear trend contrast
- `Quadratic_p`: p-value from quadratic trend contrast
- `Interpretation`: Text interpretation of results

**Example:**
```r
# Test all main EKG features
all_trends <- process_ekg_trends(
  ekg_normal,
  c("PR_Interval", "QRS_Duration", "QTc_Bazett", "VentRate")
)
print(all_trends)
```

## Statistical Methods

### Jonckheere-Terpstra Test
- **Type**: Non-parametric test for ordered alternatives
- **Purpose**: Detects monotonic trends without assuming linearity
- **Advantages**: 
  - Does not assume normal distribution
  - Does not require linear relationship
  - Robust to outliers
- **Use when**: Testing if values tend to increase (or decrease) across ordered groups

### Polynomial Contrasts
- **Type**: Parametric trend analysis
- **Components**:
  - **Linear**: Tests for constant rate of change
  - **Quadratic**: Tests for acceleration/deceleration (curved trend)
  - **Cubic**: Tests for more complex S-shaped curves
- **Advantages**:
  - Can characterize the shape of the trend
  - Provides effect size estimates
- **Use when**: Want to understand the specific pattern of change

## Interpretation Guide

The `Interpretation` column in results provides automatic interpretation:

- **"No significant trend"**: No evidence of systematic change (JT p ≥ 0.05)
- **"Significant linear trend"**: Evidence of constant rate of change
- **"Significant non-linear trend"**: Evidence of accelerating/decelerating change
- **"Significant monotonic trend"**: Evidence of consistent increase or decrease, but pattern unclear

## Example Workflow

```r
# Load the package
devtools::load_all()

# Load data (ekg_normal is the cleaned dataset available after processing)
# Use your actual dataset name if different
data(ekg_normal)

# 1. Test single feature with detailed output
qtc_result <- test_ekg_trend(ekg_normal, "QTc_Bazett", trimesters = c(0, 1, 2, 3, 4))
print(qtc_result)

# 2. Test multiple features for summary table
ekg_vars <- c("PR_Interval", "QRS_Duration", "QTc_Bazett", "VentRate")
trend_summary <- process_ekg_trends(ekg_normal, ekg_vars)

# 3. Create formatted table
library(flextable)
flextable(trend_summary) |>
  theme_vanilla() |>
  autofit()

# 4. Test pregnancy-only period
preg_trends <- process_ekg_trends(
  ekg_normal, 
  ekg_vars, 
  trimesters = c(1, 2, 3)  # Only pregnancy trimesters
)
```

## Technical Notes

1. **Missing Data**: Observations with missing values in the EKG variable are automatically excluded
2. **Trimester Coding**: Trimesters should be coded as:
   - 0: Pre-pregnancy
   - 1-3: Pregnancy trimesters
   - 4: Early postpartum (≤2 weeks)
   - 5: Later postpartum (>2 weeks)
3. **Sample Size**: Requires at least 3 trimester groups for meaningful trend analysis
4. **Dependencies**: Requires the `DescTools` package for Jonckheere-Terpstra test

## References

- Jonckheere, A.R. (1954). A distribution-free k-sample test against ordered alternatives. Biometrika, 41(1/2), 133-145.
- Terpstra, T.J. (1952). The asymptotic normality and consistency of Kendall's test against trend, when ties are present in one ranking. Indagationes Mathematicae, 14, 327-333.
