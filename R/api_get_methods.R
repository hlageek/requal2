#' Get segments from the database
#'
#' @description Internal helper to retrieve segments belonging to a project
#' from the database. The function retrieves the appropriate subset of segments
#' based on user permissions and joins them with corresponding codes.
#'
#' @param private A list-like object containing private fields, including
#'   `.con` (database connection), `.project_id`, `.user_id`, and
#'   methods for checking permissions.
#'
#' @return A data frame of segments with the columns:
#'   `project_id`, `user_id`, `doc_id`, `segment_id`, `segment_start`,
#'   `segment_end`, `segment_text`, `code_id`, `code_name`,
#'   `code_description`, and `code_created_by`. Returns `NULL` if the query fails.
#' @keywords internal
get_segments_impl <- function(private, self) {
  tryCatch(
    {
      # Perform the join operation in the database
      data <- dplyr::inner_join(
        self$segments,
        self$codebook %>%
          dplyr::rename(code_user_id = user_id),
        by = "code_id"
      ) %>%
        dplyr::select(
          user_id,
          doc_id,
          segment_id,
          segment_start,
          segment_end,
          segment_text,
          code_id,
          code_name,
          code_description,
          code_user_id
        ) %>%
        dplyr::collect()

      return(data)
    },
    error = function(e) {
      warning("Failed to get segments: ", e$message)
      return(NULL)
    }
  )
}

#' Get documents from the database
#'
#' @description Internal helper to retrieve documents belonging to a project
#' from the database. The function retrieves the appropriate subset of documents
#' based on user permissions.
#'
#' @param private A list-like object containing private fields, including
#'   `.con` (database connection), `.project_id`, `.user_id`
#'
#' @return A data frame of documents with the columns:
#'   `doc_id`, `project_id`, `user_id`, `doc_name`, `doc_description`,
#'   `doc_text`, and `created_at`. Returns `NULL` if the query fails.
#' @keywords internal
get_documents_impl <- function(private, self) {
  tryCatch(
    {
      if (is.null(private$.con)) {
        stop("Database connection is not set.")
      }
      data <- self$documents

      return(data)
    },
    error = function(e) {
      warning("Failed to get documents: ", e$message)
      return(NULL)
    }
  )
}
