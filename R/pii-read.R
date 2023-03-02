#' Read PII data and return a dataframe of given fields for a table.
#'
#' In use in abcd-ra tools
#'
#' @param url The REDCap PII URL.
#' @param token Confidential token.
#' @param table Name of the PII table.
#' @param fields Name of the PII fields.
#' @param filter_field Field from 'fields' to filter with. If NULL, return only top ten records in table and specified fields.
#' @param filter_value Value of the 'filter_field' to filter with. If NULL, return only top ten records in table and specified fields.
#' @param batch_size The maximum number of subject records a single batch should contain.
#'
#' @return tbl. Data frame of PII data.
#' @export
#'
#' @examples
#' pii_read(
#'    token = "XXX",
#'    table = "participants_events_log",
#'    fields = c("pGUID", "eventType", "eventNotes"),
#'    filter_field = "pguid",
#'    filter_value = c("YYY", "ZZZ"),
#'    batch_size = 100
#'    )
#'
#' pii_read(
#'    token = "XXX",
#'    table = "participants_contacts",
#'    fields = c("pguid", "contact_id", "relationship", "eventname", "serverdate", "id")
#'    )
#'

pii_read <- function(
    url = "https://redcap-pii.ucsd.edu/pii/",
    token,
    table,
    fields,
    filter_field = NULL,
    filter_value = NULL,
    batch_size = 100) {

  if (is.null(filter_field) | is.null(filter_value)) {
    message("Returning only top ten records in table and specified fields.")
    url_with_params <- paste0(
      url,
      "api/",
      token, "|",
      table, "/",
      paste(fields, collapse = ","), "/"
    )

    r <- url_with_params |>
      httr::GET()

    if (r$status_code != 200) stop("Error with REDCap connection.")

    return(
      r |>
        httr::content(
          as = "text",
          encoding = "UTF-8"
        ) |>
        jsonlite::fromJSON()
    )
  } else {
    message("Creating batch dataset.")

    ds_glossary <- REDCapR::create_batch_glossary(
      length(filter_value),
      batch_size
    )

    output <- list()

    # Loop through batches
    for (i in ds_glossary$id) {
      selected_index <- seq(
        from = ds_glossary$start_index[i],
        to = ds_glossary$stop_index[i]
      )

      url_with_params <- paste0(
        url,
        "api/",
        token, "|",
        table, "/",
        paste(fields, collapse = ","), "/",
        filter_field, "|",
        paste(filter_value[selected_index], collapse = ","), "/"
      )

      message("Reading batch ", i, " of ", nrow(ds_glossary))

      r <- url_with_params |>
        httr::GET()

      output[[i]] <- list(
        status_code = r$status_code,
        result = r |>
          httr::content(
            as = "text",
            encoding = "UTF-8"
          ) |>
          jsonlite::fromJSON()
      )
    }

    output <- output |>
      purrr::list_transpose() |>
      tibble::as_tibble()

    if (any(output$status_code != 200)) stop("Error with REDCap connection.")

    return(dplyr::bind_rows(output$result))
  }
}

