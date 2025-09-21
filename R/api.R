#' Requal API
#'
#' @description
#' An API interface for Requal extension modules to interact with qualitative data.
#' Provides controlled access to documents, codes, coded segments, and other project data
#' while maintaining data integrity and user permissions.
#' The API can also support mixed-methods analyses when used in interactive sessions.
#'
#' @details
#' The API operates in two modes:
#' \itemize{
#'   \item **Production mode**: Uses the actual database connection from the main app
#'   \item **Development mode**: For `NULL` connections, uses a bundled SQLite database for testing purposes
#' }
#'
#' All methods follow a consistent pattern:
#' \itemize{
#'   \item Work within the authenticated user and project context
#'   \item Return all data the user has permissions to access
#'   \item Return dataframes, vectors, or logical values (quiet methods)
#'   \item Return `NULL` on errors with warnings (`get_*` methods)
#'   \item Return `FALSE` on errors with warnings (`write_*`, `edit_*`, `delete_*` methods)
#' }
#'
#' @field version Character. The API version
#'
#' @seealso
#' For examples, see the documentation for the `RequalAPI$new()` method.
#'
#' @export
RequalAPI <- R6::R6Class(
  "RequalAPI",
  private = list(
    .con = NULL,
    .user_id = NULL,
    .project_id = NULL,
    .development = FALSE,
    # Internal helper to connect to the bundled mock database for testing
    .get_mock_connection = function() {
      .get_mock_connection()
    },

    # Internal helper to verify user has permission for requested operation
    .check_permissions = function(permission) {
      .check_permissions_impl(
        private$.con,
        private$.user_id,
        private$.project_id,
        permission = permission
      )
    },
    finalize = function() {
      if (private$.development && !is.null(private$.con)) {
        # Disconnect from the mock database
        DBI::dbDisconnect(private$.con)
      }
    }
  ),

  public = list(
    version = "0.1.3",

    #' @description
    #' Initialize a new RequalAPI instance
    #'
    #' @param con Database connection (pool object). If `NULL`, will launch a bundled mock database.
    #' @param user_id Integer. The current user's ID.
    #' @param project_id Integer. The current project's ID.
    #'
    #' @return A new instance of RequalAPI R6 class
    #'
    #' @examples
    #' api <- requal::RequalAPI$new()
    #' cat("Using API version:", api$version)
    initialize = function(
      con = NULL,
      user_id = NULL,
      project_id = NULL
    ) {
      if (is.null(con)) {
        private$.con <- private$.get_mock_connection()
        private$.user_id <- 1L
        private$.project_id <- 1L
        private$.development <- TRUE
      } else {
        stopifnot(
          "user_id must be a single number" = is.numeric(user_id) &&
            length(user_id) == 1,
          "project_id must be a single number" = is.numeric(project_id) &&
            length(project_id) == 1
        )
        private$.con <- con
        private$.user_id <- as.integer(user_id)
        private$.project_id <- as.integer(project_id)
      }
    },

    #' @description
    #' Get coded segments for the current user and project
    #'
    #' @return A dataframe containing segments with associated code information, or NULL on error.
    #'   Returns all segments the authenticated user is permitted to view based on their permissions.
    #'   Columns include: segment_id, document_id, segment_start, segment_end,
    #'   segment_text, code_id, code_name, code_description, user_id, project_id
    #'
    #' @examples
    #' api <- requal::RequalAPI$new()
    #' segments <- api$get_segments()  # Gets mock segments
    #' head(segments)
    get_segments = function() {
      get_segments_impl(private)
    },

    #' Get documents for the current user and project
    #'
    #' @description
    #' Retrieves documents for the current user and project from the database.
    #' This function acts as a wrapper around the `get_documents_impl` function.
    #' For detailed implementation, see \code{\link{get_documents_impl}}.
    #'
    #' @return A dataframe containing a subset of documents
    #' corresponding to user permissions, or `NULL` on error.
    #'
    #' @examples
    #' api <- requal::RequalAPI$new()
    #' documents <- api$get_documents()  # Gets mock documents
    #' head(documents)
    get_documents = function() {
      get_documents_impl(private)
    },

    #' @description
    #' Add a new document to the current project
    #'
    #' @param .data Dataframe. A dataframe with columns: doc_name, doc_text, doc_description.
    #'
    #' @return Result of the write operation (logical)
    #'
    #' @examples
    #' \dontrun{
    #' documents_df <- data.frame(
    #'   doc_name = c("doc1", "doc2"),
    #'   doc_text = c("Text for doc1", "Text for doc2"),
    #'   doc_description = c("Description for doc1", "Description for doc2")
    #')
    #' api <- RequalAPI$new()
    #' result <- api$write_documents(documents_df)
    #' }
    write_documents = function(.data) {
      write_documents_impl(.data, private)
    }
  ),

  active = list(
    #' @field user_id Integer. The authenticated user's ID (read-only)
    user_id = function(value) {
      if (missing(value)) {
        return(private$.user_id)
      } else {
        stop("`user_id` is read-only and cannot be changed.", call. = FALSE)
      }
    },

    #' @field project_id Integer. The current project's ID (read-only)
    project_id = function(value) {
      if (missing(value)) {
        return(private$.project_id)
      } else {
        stop(
          "`project_id` is read-only and cannot be changed.",
          call. = FALSE
        )
      }
    }
  )
)
