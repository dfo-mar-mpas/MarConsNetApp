#' Get Grade Descriptions for Indicators, Objectives, Ecosystem Health, or Threats
#'
#' Returns a named list of grade explanations for different types of assessments.
#' The function supports four types:
#' \code{"indicator"}, \code{"objective"}, \code{"ecosystem_health"}, and \code{"threats"}.
#'
#' @param type Character. The type of grade descriptions to return.
#'   Defaults to \code{"indicator"}.
#'   - \code{"indicator"}: Standard indicator grades (A–F) with ecosystem health interpretations.
#'   - \code{"objective"}: Grades for conservation objectives.
#'   - \code{"ecosystem_health"}: Grades for overall ecosystem health assessments.
#'   - \code{"threats"}: Grades for threat indicator assessments.
#'
#' @return A named list of character strings. Names are \code{A}, \code{B}, \code{C}, \code{D}, \code{F}.
#'   Each element provides a human-readable description of the corresponding grade.
#'
#' @examples
#' grade_description("indicator")
#' grade_description("objective")
#' grade_description("ecosystem_health")
#' grade_description("threats")
#'
#' @export

grade_description <- function(type='indicator') {
  if (type == 'indicator') {
    grade_explanations <- list(
      A = "Strong ecosystem health; key ecosystem components are thriving, and therefore this shows evidence that there is a strong ecosystem health.",
      B = "Ecosystem generally healthy with minor concerns.",
      C = "Some ecosystem components under stress; mixed condition.",
      D = "Limited ecosystem health; emerging concerns across multiple components.",
      F = "Poor ecosystem health; insufficient data or widespread impacts."
    )
  } else if (type == 'objective') {
    grade_explanations <- list(
      A = "Strong evidence the objective is being met",
      B = "Evidence shows the objective is mostly being met",
      C = "Mixed or uncertain evidence about objective progress",
      D = "Limited evidence that objective is being met / emerging concerns",
      F = "Insufficient evidence / objective not met"
    )
  } else if (type == 'ecosystem_health') {
    grade_explanations <- list(
      A = "Strong ecosystem health; key ecosystem components are thriving",
      B = "Ecosystem generally healthy with minor concerns",
      C = "Some ecosystem components under stress; mixed condition",
      D = "Limited ecosystem health; emerging concerns across multiple components",
      F = "Poor ecosystem health; insufficient data or widespread impacts"
    )

  } else if (type == 'threats') {

    grade_explanations <- list(
      A = "Strong evidence the threat indicator is performing well",
      B = "Evidence indicates the threat indicator is generally positive",
      C = "Mixed or uncertain performance of the threat indicator",
      D = "Limited evidence / emerging concerns for the threat indicator",
      F = "Insufficient data / poor performance of the threat indicator"
    )

  }

  return(grade_explanations)

}
