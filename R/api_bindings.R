#' Construct a segments query object
#'
#' @description Internal helper function to construct a database query for
#' retrieving coded segments.
#'
#' @param private A list-like object containing private fields, including
#'   `.con` (database connection), `.project_id`, `.user_id`, and
#'   a method for checking permissions.
#'
#' @return A `dplyr` query object representing the segments table, filtered
#'   according to the project ID and user permissions.
#'
#' @details
#' Memo segments are excluded from the object.
#'
#' @keywords internal
build_segments_query <- function(private) {
  if (is.null(private$.con)) {
    stop("Database connection is not set.")
  }
  can_view_others <- private$.check_permissions(
    "annotation_other_view"
  )

  # Start building the segments query
  segments_query <- dplyr::tbl(private$.con, "segments") %>%
    dplyr::filter(project_id == !!private$.project_id) %>%
    # Exclude memos-segements
    dplyr::filter(!is.na(code_id))

  if (!can_view_others) {
    segments_query <- segments_query %>%
      dplyr::filter(user_id == !!private$.user_id)
  }
  return(segments_query)
}
