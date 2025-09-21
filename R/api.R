#' Requal API
#'
#' @description
#' An API interface for Requal extensions to interact with qualitative data.
#' Provides controlled access to documents, codes, coded segments, and other project data
#' while maintaining data integrity and user permissions.
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
#' @examples
#' # Development usage (for extension development/testing)
#' api <- requal::RequalAPI$new()
#' cat("Using API version:", api$version)
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
    .check_permissions = function(
      user_id,
      project_id,
      operation = "view",
      context = "data"
    ) {
      .check_permissions(user_id, project_id, private$.con, operation, context)
    },
    finalize = function() {
      if (private$.development && !is.null(private$.con)) {
        # Disconnect from the mock database
        DBI::dbDisconnect(private$.con)
      }
    }
  ),

  public = list(
    version = "0.1.0",

    #' @description
    #' Initialize a new RequalAPI instance
    #'
    #' @param con Database connection (pool object). If `NULL`, will launch a bundled mock database.
    #' @param user_id Integer. The authenticated user's ID (immutable).
    #' @param project_id Integer. The current project's ID (immutable).
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
    #'   Returns all segments the authenticated user is permitted to see based on their permissions.
    #'   Columns include: segment_id, document_id, start_pos, end_pos,
    #'   segment_text, code_id, code_name, code_description, user_id, project_id
    #'
    #' @examples
    #' api <- requal::RequalAPI$new()
    #' segments <- api$get_segments()  # Gets mock segments
    #' head(segments)
    get_segments = function() {
      tryCatch(
        {
          can_view_others <- private$.check_permissions(
            private$.user_id,
            private$.project_id,
            "other_view",
            "annotation"
          )
          can_view_others_codes <- private$.check_permissions(
            private$.user_id,
            private$.project_id,
            "other_view",
            "codebook"
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
    },

    #' @description
    #' Add a new document to the current project
    #'
    #' @param doc_name Character. Name/title for the document (required).
    #' @param doc_text Character. Full text content of the document (required).
    #' @param doc_description Character. Optional description of the document.
    #'
    #' @return Result from the document creation operation, or NULL on error.
    #'
    #' @examples
    #' \dontrun{
    #' api <- RequalAPI$new()
    #' result <- api$write_document(
    #'   doc_name = "Interview_01",
    #'   doc_text = "Full interview transcript...",
    #'   doc_description = "First participant interview"
    #' )
    #' }
    write_document = function(doc_name, doc_text, doc_description = "") {
      tryCatch(
        {
          if (
            !private$.check_permissions(
              private$.user_id,
              private$.project_id,
              "modify",
              "data"
            )
          ) {
            warning("User lacks permission to create documents in this project")
            return(NULL)
          }

          if (is.null(doc_name) || doc_name == "") {
            stop("doc_name cannot be empty")
          }
          if (is.null(doc_text) || doc_text == "") {
            stop("doc_text cannot be empty")
          }

          result <- add_input_document(
            pool = private$.con,
            project = private$.project_id,
            doc_name = doc_name,
            doc_text = doc_text,
            doc_description = doc_description,
            user_id = private$.user_id
          )

          return(result)
        },
        error = function(e) {
          warning("Failed to write document: ", e$message)
          return(NULL)
        }
      )
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
