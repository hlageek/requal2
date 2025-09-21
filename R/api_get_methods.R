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
get_segments_impl <- function(private) {
  tryCatch(
    {
      can_view_others <- private$.check_permissions(
        "annotation_other_view"
      )
      can_view_others_codes <- private$.check_permissions(
        "codebook_other_view"
      )

      if (is.null(private$.con)) {
        stop("Database connection is not set.")
      }

      # Start building the segments query
      segments_query <- dplyr::tbl(private$.con, "segments") %>%
        dplyr::filter(project_id == !!private$.project_id) %>%
        # Exclude memos
        # TODO: make this a parameter
        dplyr::filter(!is.na(code_id))

      if (!can_view_others) {
        segments_query <- segments_query %>%
          dplyr::filter(user_id == !!private$.user_id)
      }

      # Start building the codes query
      codes_query <- dplyr::tbl(private$.con, "codes") %>%
        dplyr::filter(project_id == !!private$.project_id) %>%
        dplyr::select(
          code_id,
          code_name,
          code_description,
          code_created_by = user_id
        )

      if (!can_view_others_codes) {
        codes_query <- codes_query %>%
          dplyr::filter(created_by == !!private$.user_id)
      }

      # Perform the join operation in the database
      data <- segments_query %>%
        dplyr::inner_join(codes_query, by = "code_id") %>%
        dplyr::select(
          project_id,
          user_id,
          doc_id,
          segment_id,
          segment_start,
          segment_end,
          segment_text,
          code_id,
          code_name,
          code_description,
          code_created_by
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
get_documents_impl <- function(private) {
  tryCatch(
    {
      can_view_others <- private$.check_permissions("data_other_view")

      if (is.null(private$.con)) {
        stop("Database connection is not set.")
      }

      # Build the query
      documents_query <- dplyr::tbl(private$.con, "documents") %>%
        dplyr::filter(project_id == !!private$.project_id) %>%
        dplyr::select(
          doc_id,
          project_id,
          user_id,
          doc_name,
          doc_description,
          doc_text,
          created_at
        )

      if (!can_view_others) {
        documents_query <- documents_query %>%
          dplyr::filter(user_id == !!private$.user_id)
      }

      data <- documents_query %>%
        dplyr::collect()

      return(data)
    },
    error = function(e) {
      warning("Failed to get documents: ", e$message)
      return(NULL)
    }
  )
}
