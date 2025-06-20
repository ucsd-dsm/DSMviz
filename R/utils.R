convert_to_factor <- function(data, vars) {
  not_fct <- purrr::map_lgl(
    vars,
    ~ !is.factor(data[[.x]])
  )

  vars_not_fct <- vars[not_fct]

  data |>
    mutate(
      across(
        all_of(vars_not_fct),
        as.factor
      )
    )
}
