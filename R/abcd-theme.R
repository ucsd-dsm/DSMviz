#' ABCD Bootstrap theme
#'
#' @return a bootstrap theme
#' @export
abcd_bs_theme <- function() {
  bslib::bs_theme(
    version = 5L,
    bootswatch = "pulse",
    base_font = bslib::font_google(
      "Inter",
      wght = 100 * 2:9,
      local = FALSE
    ),
    code_font = bslib::font_google(
      "Source Code Pro",
      wght = 100 * 2:9,
      local = FALSE
    ),
    heading_font = bslib::font_google(
      "Poppins",
      wght = 100 * 2:9,
      local = FALSE
    ),
    primary   = "#08519C",
    secondary = "#FDBF6F",
    success   = "#33A02C",
    info      = "#DCDCDC",
    warning   = "#FF7F00",
    danger    = "#E31A1C"
  ) |>
    bslib::bs_add_variables(
      "nav-link-font-size" = "1.1rem",
      "nav-link-font-weight" = "500",
      "h5-font-size" = "1.1rem"
    )
}
