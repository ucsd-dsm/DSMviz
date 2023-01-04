#' Recruitment data
#'
#' A dataset containing fake recruitment data.
#'
#' @format A data frame with 7500 rows and 15 variables:
#' \describe{
#'   \item{site}{Recruitment site}
#'   \item{date}{Recruitment date}
#'   \item{substance}{Participant is substance user (yes/no)}
#'   \item{race}{Participant's race}
#'   \item{ethnicity}{Participant's ethnicity}
#'   \item{education}{Participant's education level}
#'   \item{poverty}{Participant's poverty level}
#'   \item{ruca}{Participant's RUCA classification}
#'   \item{urban}{Participant's urbanicity classification}
#'   \item{adi}{Participant's adi classification}
#'   \item{opioids}{Participant is opioid user (yes/no)}
#'   \item{marijuana}{Participant is marijuana user (yes/no)}
#'   \item{alcohol}{Participant is alcohol user (yes/no)}
#'   \item{tobacco}{Participant is tobacco user (yes/no)}
#'   \item{stimulants}{Participant is stimulants user (yes/no)}
#' }
"data_recruitments"

#' Assessment data
#'
#' A dataset containing fake assessment data.
#'
#' @format A data frame with 59,260 rows and 7 variables:
#' \describe{
#'   \item{site}{Assessment site}
#'   \item{event}{Event}
#'   \item{visit_type}{Visit type}
#'   \item{visit_start}{Visit start date}
#'   \item{visit_end}{Visit end date}
#'   \item{days_from_midpoint}{Deviation in days from midpoint of window}
#'   \item{completion_status}{Completion status}
#' }
"data_assessments"
