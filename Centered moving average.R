library(dplyr)

# Define a function to calculate three-year centered moving average
centered_three_year_average <- function(data, year_col = "Year", group_col = "Group", factor_col = "Factor") {
  data %>%
    arrange(!!sym(group_col), !!sym(year_col)) %>%  # Ensure data is ordered by group and year
    group_by(!!sym(group_col)) %>%
    mutate("{factor_col}_3YearAvg" := {
      # Capture values for the previous, current, and next years using lag and lead
      values <- c(lag(.data[[factor_col]], 1), .data[[factor_col]], lead(.data[[factor_col]], 1))
      # Calculate the mean of available values, ignoring NA
      mean(values, na.rm = TRUE)
    }) %>%
    ungroup()
}

# Example of usage with a dataset (replace `your_data` with the actual dataset name)
# new_data <- calculate_three_year_average(your_data, year_col = "Year", group_col = "Group", factor_col = "Factor")
