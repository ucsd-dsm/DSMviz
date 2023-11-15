#   ____________________________________________________________________________
#   universal high-level function                                           ####

#' Compare categorical variables between groups (data)
#'
#' High-level function that prepares the data needed to compare the frequencies
#'   of the levels of a categorical variables between two data frames (i.e.,
#'   data from two groups). Is used internally by `DSMviz::plot_comp_categ()`.
#'   Allows to choose to prepare data for three different visualizations:
#'   dumbbell plot; deviation lollipop plot (deviation from 0); side-by-side
#'   lollipop plot.
#'
#' @param data_1 tbl. Data frame with data from the first group.
#' @param data_2 tbl. Data frame with data from the second group.
#' @param label_1 character. Label/name for the first group.
#' @param label_2 character. Label/name for the second group.
#' @param var character. Variable/column to compare between the groups. The
#'   column is expected to be a factor variable. If it isn't, it is converted to
#'   factor.
#' @param type character. Type of visualization for which the data is being
#'   created (One of: "dumbbell" (Default), "deviation", or "side").
#'
#' @return tbl. The data frame needed to create the selected visualization.
#' @export
#'
#' @examples
#' \dontrun{
#' # prepare dataset to compare
#' data_main <- data_recruitments
#' data_comp <- data_recruitments |>
#'   slice_sample(
#'     n = 1000
#'   )
#'
#' # create data frame for dumbbell plot of `ethnicity` variable
#' prep_data_comp_categ(
#'   data_1  = data_main,
#'   data_2  = data_comp,
#'   label_1 = "full dataset",
#'   label_2 = "sampled dataset",
#'   vars    = "ethnicity",
#'   type    = "dumbbell"
#' )
#'
#' # create data frame for deviation plot of `urban` variable
#' prep_data_comp_categ(
#'   data_1  = data_main,
#'   data_2  = data_comp,
#'   label_1 = "group 1",
#'   label_2 = "group 2",
#'   vars    = "urban",
#'   type    = "deviation"
#' )
#' }
#'
prep_data_comp_categ <- function(data_1,
                                 data_2,
                                 label_1,
                                 label_2,
                                 var,
                                 type = c("dumbbell", "deviation", "side")) {
  type <- match.arg(type)

  # checks
  data_1 <- convert_to_factor(data_1, var)
  data_2 <- convert_to_factor(data_2, var)

  if (type == "dumbbell") {
    prep_data_comp_dumbbell(
      data_1,
      data_2,
      label_1,
      label_2,
      var
    ) |>
      add_hovertext_dumbbell(
        var,
        label_1,
        label_2
      )
  } else if (type == "deviation") {
    prep_data_comp_deviation(
      data_1,
      data_2,
      label_1,
      label_2,
      var
    ) |>
      add_hovertext_deviation(
        var,
        label_1,
        label_2
      )
  } else if (type == "side") {
    prep_data_comp_side(
      data_1,
      data_2,
      label_1,
      label_2,
      var
    ) |>
      add_hovertext_side(
        var
      )
  }
}


#   ____________________________________________________________________________
#   lower-level data functions                                              ####

##  ............................................................................
##  side-by-side lollipop                                                   ####

prep_data_comp_side <- function(data_1, data_2, label_1, label_2, var) {
  data_group_1 <- data_1 |>
    compute_freqs(var) |>
    mutate(
      group = label_1
    )

  data_group_2 <- data_2 |>
    compute_freqs(var) |>
    mutate(
      group = label_2
    )

  bind_rows(
    data_group_1,
    data_group_2
  ) |>
    mutate(
      group = factor(
        group,
        levels = c(label_1, label_2)
      )
    )
}

add_hovertext_side <- function(data, var) {
  group_levels <- levels(data$group)

  data |>
    mutate(
      color = if_else(
        group == levels(data$group)[[1]],
        "black",
        "#FF7F00"
      ),
      text  = glue::glue(
        "
        <span style='color:#737373;font-size:110%'><i><b>{.data[[var]]}</b></i></span>
        <span style='font-size:90%;color:{color}'>\U2B24</span> <b>{group}:</b> {round_perc(percent)}% [{count}/{total}]
        "
      )
    )
}


##  ............................................................................
##  deviation lollipop                                                      ####

prep_data_comp_deviation <- function(data_1, data_2, label_1, label_2, var) {
  prep_data_comp_side(
    data_1,
    data_2,
    label_1,
    label_2,
    var
  ) |>
    tidyr::pivot_wider(
      names_from = group,
      values_from = c(count, total, percent)
    ) |>
    mutate(
      diff_count     = .data[[paste0("count_", label_2)]] - .data[[paste0("count_", label_1)]],
      diff_percent   = .data[[paste0("percent_", label_2)]] - .data[[paste0("percent_", label_1)]],
      diff_dir       = if_else(
        diff_percent < 0,
        "neg",
        "pos"
      ) |>
        forcats::fct_relevel(
          "neg",
          "pos"
        )
    )
}

add_hovertext_deviation <- function(data, var, label_1, label_2) {
  data |>
    mutate(
      color = if_else(
        diff_dir == "neg",
        "#CB181D",
        "#08519C"
      ),
      text  = glue::glue(
        "
        <span style='color:#737373;font-size:110%'><i><b>{.data[[var]]}</b></i></span>
        <b>{label_1}:</b> {round_perc(.data[[paste0('percent_', label_1)]])}%{format_counts(.data[[paste0('count_', label_1)]], .data[[paste0('total_', label_1)]])}
        <b>{label_2}:</b> {round_perc(.data[[paste0('percent_', label_2)]])}%{format_counts(.data[[paste0('count_', label_2)]], .data[[paste0('total_', label_2)]])}
        <span style='font-size:90%;color:{color}'>\U2B24</span> <span style='color:{color}'><i><b>Difference: {format_diff(diff_percent)}%</b></i></span>
        "
      )
    )
}


##  ............................................................................
##  dumbbell                                                                ####

prep_data_comp_dumbbell <- function(data_1, data_2, label_1, label_2, var) {
  prep_data_comp_deviation(
    data_1,
    data_2,
    label_1,
    label_2,
    var
  ) |>
    mutate(paired = 1:n()) |>
    tidyr::pivot_longer(
      cols = matches("^count_|^total_|^percent_"),
      names_to = c(".value", "group"),
      names_sep="_"
    ) |>
    mutate(
      group = factor(
        group,
        levels = c(label_1, label_2)
      )
    )
}

add_hovertext_dumbbell <- function(data, var, label_1, label_2) {
  data |>
    mutate(
      color_dot  = case_when(
        group == label_1 ~ "#000000",
        group == label_2 & diff_dir == "neg" ~ "#CB181D",
        group == label_2 & diff_dir == "pos" ~ "#08519C"
      ),
      color_diff = case_when(
        diff_dir == "neg" ~ "#CB181D",
        diff_dir == "pos" ~ "#08519C"
      ),
      text_1 = glue::glue(
        "
        <span style='font-size:90%;color:{color_dot}'>\U2B24</span> <b>{group}:</b> {round_perc(percent)}%{format_counts(count, total)}
        "
      )
    ) |>
    group_by(
      across(all_of(var))
    ) |>
    mutate(
      text_2 = paste0(text_1, collapse = '\n'),
      text = glue::glue(
        "
        <span style='color:#737373;font-size: 110%;'><i><b>{.data[[var]]}</b></i></span>
        {text_2}
        <span style='color:{color_diff}'><i><b>Difference: {format_diff(diff_percent)}%</b></i></span>
        "
      )
    ) |>
    ungroup() |>
    select(
      -c(
        text_1,
        text_2
      )
    )
}


#   ____________________________________________________________________________
#   utility functions                                                       ####

compute_freqs <- function(data, var) {
  data |>
    group_by(
      across(all_of(var)),
      .drop = FALSE
    ) |>
    summarise(
      count = n(),
      .groups = "drop_last"
    ) |>
    mutate(
      total   = sum(count),
      percent = count / total) |>
    ungroup()
}

round_perc <- function(perc) {
  round(perc * 100, 2)
}

format_diff <- function(diff) {
  ifelse(
    round_perc(diff) > 0,
    paste0("+", round_perc(diff)),
    as.character(round_perc(diff))
  )
}

format_counts <- function(count, total) {
  ifelse(
    !is.na(count) & !is.na(total),
    paste0(" [", count, "/", total, "]"),
    ""
  )
}

#' Round any
#'
#' Helper function from the `{plyr}` package to ease using rounding functions
#'   (round, floor, ceiling) with specified accuracy.
#'
#' @param x numeric (vector). (Vector of) number(s) to round.
#' @param accuracy numeric. The accuracy to round to (e.g., specifying 25 will
#'   round to the next increment of 25)
#' @param f function. Rounding function to use (Default: round)
#'
#' @return numeric (vector). (Vector of) rounded number(s).
round_any <- function(x, accuracy, f=round){
  f(x/ accuracy) * accuracy
}

count_zeros <- function(x, tol = .Machine$double.eps ^ 0.5) {
  x <- abs(x)
  y <- -log10(x - floor(x))
  floor(y) - (y %% 1 < tol)
}

set_accuracy <- function(num) {
  if (num > 1) {
    10 ^ nchar(floor(num))
  } else {
    10 ^ -count_zeros(num)
  }
}

round_limits <- function(range) {
  accuracy <- set_accuracy(
    (range[[2]] - range[[1]]) / 10
  )

  c(
    round_any(range[[1]], accuracy, floor),
    round_any(range[[2]], accuracy, ceiling)
  )
}

compute_limits <- function(data, type = c("dumbbell", "deviation", "side")) {
  type = match.arg(type)

  if (type %in% c("dumbbell", "deviation")) {

    if (type == "dumbbell") {
      var_x <- "percent"
    } else if (type == "deviation") {
      var_x <- "diff_percent"
    }

    data |>
      mutate(
        limit_min = purrr::map_dbl(
          data,
          ~ min(.x[[var_x]], na.rm = TRUE)
        ),
        limit_max = purrr::map_dbl(
          data,
          ~ max(.x[[var_x]], na.rm = TRUE)
        ),
        limits = list(
          c(min(limit_min), max(limit_max)) |>
            round_limits()
        )
      ) |>
      select(
        -limit_min,
        -limit_max
      )

  } else {
    var_x <- "percent"

    data |>
      mutate(
        limit_max = purrr::map_dbl(
          data,
          ~ max(.x[[var_x]])
        ),
        limits = list(
          c(0, max(limit_max)) |>
            round_limits()
        )
      ) |>
      select(
        -limit_max
      )
  }
}

compute_heights <- function(data, n_vars) {
  space_prop <- case_when(
    n_vars <= 2 ~ .95,
    n_vars == 3 ~ .9,
    n_vars == 4 ~ .8,
    n_vars == 5 ~ .75,
    n_vars == 6 ~ .7,
    n_vars == 7 ~ .65,
    n_vars >= 8 ~ .6
  )
  space_same <- 1 - space_prop

  data |>
    mutate(
      heights = (
        n_levels / sum(n_levels) * space_prop +
          c(rep(space_same / (n_vars - 1), (n_vars - 1)), 0)
      )
    )
}

