#   ____________________________________________________________________________
#   universal high-level function                                           ####

#' Compare categorical variables between groups (plot)
#'
#' High-level function that compares the frequencies of the levels of a
#'   selection of categorical variables between two data frames (i.e., data
#'   from two groups). Allows to choose between three different visualizations:
#'   dumbbell plot; deviation lollipop plot (deviation from 0); side-by-side
#'   lollipop plot. Visualizations are created using `ggplot2` and made
#'   interactive using `plotly`.
#'
#' @param data_1 tbl. Data frame with data from the first group.
#' @param data_2 tbl. Data frame with data from the second group.
#' @param label_1 character. Label/name for the first group.
#' @param label_2 character. Label/name for the second group.
#' @param vars character (vector). One or several variables/columns to compare
#'   between the groups. The columns are expected to be factor variables. If
#'   they aren't, they are converted to factor.
#' @param data_dict tbl. Data frame with columns `var` and `label` to assign
#'   more descriptive names to the variables in the plot (Optional. If not
#'   provided, the column name will be used).
#' @param type character. Type of visualization to create (One of: "dumbbell"
#' (Default), "deviation", or "side")
#'
#' @return plotly object. The resulting visualization.
#' @export
#'
#' @examples
#' # prepare dataset to compare
#' data_main <- data_recruitments
#' data_comp <- data_recruitments |>
#'   slice_sample(
#'     n = 1000
#'   )
#'
#' # compare
#' plot_comp_categ(
#'   data_1  = data_main,
#'   data_2  = data_comp,
#'   label_1 = "full dataset",
#'   label_2 = "sampled dataset",
#'
#'   # try out different combinations (and orders) of variables
#'   # vars   = c("race", "urban", "ruca", "education"),
#'   vars    = c("ethnicity", "education", "substance"),
#'
#'   # try out different plot types
#'   type    = "dumbbell"
#'   # type   = "deviation"
#'   # type   = "side"
#' )
#'
#' # use data dictionary to label variables
#' data_dict <- tribble(
#'   ~var,         ~label,
#'   "race",       "Race",
#'   "ethnicity",  "Ethnicity",
#'   "substance",  "Substance User",
#'   "education",  "Highest Education",
#'   "poverty",    "Poverty Level",
#'   "ruca",       "RUCA",
#'   "urban",      "Urbanicity"
#' )
#'
#' plot_comp_categ(
#'   data_1    = data_recruitments,
#'   data_2    = data_recruitments |>
#'     slice_sample(
#'       n = 1000
#'     ),
#'   label_1   = "Group 1",
#'   label_2   = "Group 2",
#'   data_dict = data_dict,
#'
#'   # try out different combinations (and orders) of variables
#'   vars      = c("race", "urban", "ruca", "education"),
#'   # vars     = c("ethnicity", "education", "substance"),
#'
#'   # try out different plot types
#'   # type     = "dumbbell"
#'   type      = "deviation"
#'   # type     = "side"
#' )
#'
plot_comp_categ <- function(data_1,
                            data_2,
                            label_1,
                            label_2,
                            vars,
                            data_dict = NULL,
                            type = c("dumbbell", "deviation", "side")) {
  type   <- match.arg(type)

  # checks
  data_1 <- convert_to_factor(data_1, vars)
  data_2 <- convert_to_factor(data_2, vars)

  # select lower level functions
  n_vars <- length(vars)
  if (type == "dumbbell") {
    fun_plot    <- plot_comp_dumbbell
    hide_legend <- rep(TRUE, n_vars)
  } else if (type == "deviation") {
    fun_plot    <- plot_comp_deviation
    hide_legend <- rep(TRUE, n_vars)
  } else if (type == "side") {
    fun_plot    <- plot_comp_side
    hide_legend <- c(FALSE, rep(TRUE, n_vars - 1))
  }

  # create nested tibble with data and plot objects
  if (is.null(data_dict)) {
    config <- tibble(
      var   = vars,
      label = vars
    )
  } else {
    config <- data_dict |>
      filter(var %in% vars) |>
      arrange(match(var, vars))
  }

  config <- config |>
    # plot data
    mutate(
      data = purrr::map(
        var,
        ~ prep_data_comp_categ(
          data_1  = data_1,
          data_2  = data_2,
          label_1 = label_1,
          label_2 = label_2,
          var     = .x,
          type    = type
        )
      )
    ) |>

    # single plots and parameters for combined plot
    compute_limits(type = type) |>
    mutate(
      hide_legend = hide_legend,
      plot = purrr::pmap(
        list(
          data        = data,
          var         = var,
          limits      = limits,
          hide_legend = hide_legend
        ),
        fun_plot
      ),
      n_levels = unlist(
        purrr::map2(
          data,
          var,
          ~ .x[[.y]] |>
            levels() |>
            length()
        )
      )
    ) |>
    compute_heights(n_vars = n_vars) |>
    mutate(
      y_pos        = 1 - lag(cumsum(heights), default = 0),
      facet_labels = purrr::map2(
        label,
        y_pos,
        ~ create_facet_label(.x, .y)
      )
    )

  # create combined plot
  subplot(
    config$plot,
    nrows   = nrow(config),
    heights = config$heights,
    shareX  = TRUE,
    margin  = c(0, 0, 0, 0.06)
  ) |>
    layout(
      annotations = config$facet_labels,
      margin = list(
        l   = 20,
        r   = 20,
        b   = 0,
        t   = 50,
        pad = 4
      )
    )
}

#   ____________________________________________________________________________
#   lower-level functions                                                   ####

##  ............................................................................
##  side-by-side lollipop                                                   ####

plot_comp_side <- function(data,
                           var,
                           limits,
                           hide_legend = FALSE) {
  # create a jittered x variable to dodge the lines
  x_var_levels <- forcats::fct_rev(data[[var]])
  levels_group <- levels(data$group)

  data <- data |>
    mutate(
      x_var = if_else(
        group == levels_group[[1]],
        as.numeric(x_var_levels) + 0.15,
        as.numeric(x_var_levels) - 0.15
      )
    )

  p <- data |>
    ggplot(
      mapping = aes(
        x     = x_var,
        y     = percent,
        color = forcats::fct_rev(group),
        text  = text
      )
    ) +
    geom_point(
      size     = 3
    ) +
    geom_line(
      data     = create_data_line(data, "percent", "x_var"),
      mapping  = aes(
        group = group_line
      ),
      size     = 1
    ) +
    scale_y_continuous(
      labels = scales::percent,
      limits   = limits,
      n.breaks = 10
    ) +
    scale_x_continuous(
      breaks = seq_along(x_var_levels),
      labels = function(x) rev(x_var_levels)[x]
    ) +
    scale_fill_manual(
      values =c("#FF7F00", "black")
    ) +
    scale_color_manual(
      values =c("#FF7F00", "black")
    ) +
    theme_minimal() +
    coord_flip() +
    theme(
      panel.grid.major.y = element_blank(),
      axis.title         = element_blank()
    )

  pp <- ggplotly(p, tooltip = "text") |>
    plotly_layout() |>
    layout(
      legend = list(
        orientation = "h",
        xanchor     = "center",
        yanchor     = "bottom",
        x           = 0.5,
        y           = 1.03,
        title       = list(text = ""),
        font        = list(
          family = "Inter",
          size = 13
        ),
        traceorder  = "reversed"
      )
    ) |>
    style(
      name   = levels_group[[2]],
      traces = c(1, 3)
    ) |>
    style(
      name   = levels_group[[1]],
      traces = c(2, 4)
    )

  if (hide_legend) {
    pp <- pp |>
      style(
        showlegend = FALSE
      )
  }

  pp
}


##  ............................................................................
##  deviation lollipop                                                      ####

plot_comp_deviation <- function(data,
                                var,
                                limits,
                                hide_legend = FALSE) {
  p <- data |>
    ggplot(
      aes(
        x    = diff_percent,
        y    = forcats::fct_rev(.data[[var]]),
        fill = diff_dir,
        text = text
      )
    ) +
    geom_vline(
      xintercept = 0,
      color      = "gray",
      size       = 1
    ) +
    geom_line(
      data = create_data_line(data, "diff_percent", var),
      mapping = aes(
        group = group_line,
        color = diff_dir
      ),
      size = 1
    ) +
    geom_point(
      aes(
        color = diff_dir
      ),
      size     = 4
    ) +
    scale_x_continuous(
      labels = scales::percent,
      limits   = limits,
      n.breaks = 10
    ) +
    scale_fill_manual(
      values =c("#CB181D", "#08519C")
    ) +
    scale_color_manual(
      values =c("#CB181D", "#08519C")
    ) +
    theme_minimal() +
    theme(
      axis.title = element_blank()
    )

  pp <- ggplotly(p, tooltip = "text") |>
    plotly_layout()

  if (hide_legend) {
    pp <- pp |>
      style(
        showlegend = FALSE
      )
  }

  pp
}


##  ............................................................................
##  dumbbell                                                                ####

plot_comp_dumbbell <- function(data,
                               var,
                               limits,
                               hide_legend = FALSE) {
  label_1 <- levels(data$group)[[1]]
  label_2 <- levels(data$group)[[2]]

  p <- data |>
    ggplot(
      aes(
        x    = percent,
        y    = forcats::fct_rev(.data[[var]]),
        text = text
      )
    ) +
    geom_line(
      mapping = aes(
        group = paired,
        color = diff_dir
      ),
      size = 1
    ) +
    geom_point(
      mapping = aes(
        color = diff_dir
      ),
      data    = filter(data, group == label_2),
      size    = 4
    ) +
    geom_point(
      data    = filter(data, group == label_1),
      color = "black",
      size=4
    ) +
    scale_x_continuous(
      labels   = scales::percent,
      limits   = limits,
      n.breaks = 10
    ) +
    scale_color_manual(
      values =c("#CB181D", "#08519C")
    ) +
    theme_minimal() +
    theme(
      axis.title = element_blank()
    )

  pp <- ggplotly(p, tooltip = "text") |>
    plotly_layout()

  if (hide_legend) {
    pp <- pp |>
      style(
        showlegend = FALSE
      )
  }

  pp
}


#   ____________________________________________________________________________
#   utility functions                                                       ####

create_data_line <- function(data, x, var) {
  data_new <- data |>
    mutate(
      {{ x }} := 0
    )

  data |>
    bind_rows(
      data_new
    ) |>
    mutate(
      group_line = .data[[var]]
    )
}

plotly_layout <- function(pp) {
  pp |>
    # layout
    layout(
      hoverlabel = list(
        font = list(
          family = "Inter",
          size = 13
        ),
        bgcolor = "white"
      ),
      xaxis = list(
        tickfont = list(
          color = 'Black',
          family = "Inter",
          size = 13
        )
      ),
      yaxis = list(
        tickfont = list(
          color = 'Black',
          family = "Inter",
          size = 15
        )
      )
    ) |>

    # modify plotly modebar (remove everything but download to PNG)
    # https://plotly-r.com/control-modebar.html
    config(
      displaylogo = FALSE,
      modeBarButtons = list(
        list("toImage")
      )
    )
}

create_facet_label <- function(label, y_pos) {
  list(
    x = 0.02,
    y = y_pos,
    text = paste0(label),
    xref = "paper",
    yref = "paper",
    xanchor = "left",
    yanchor = "bottom",
    showarrow = FALSE,
    font = list(
      color = "Black",
      family = "Poppins",
      size = 22
    )
  )
}
