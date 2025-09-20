#' Requal API for Extensions
#'
#' @description
#' An API interface for Requal extensions to interact with qualitative data.
#' Provides controlled access to segments, documents, codes, and other project data
#' while maintaining data integrity and user permissions.
#'
#' @details
#' The API operates in two modes:
#' - **Production mode**: Uses the actual database connection from the main app
#' - **Mock mode**: Uses a bundled SQLite database for testing extensions
#'
#' All methods follow a consistent pattern:
#' - Return dataframes, vectors, or logical values (quiet methods)
#' - Return NULL on errors with warnings (`get_*` methods)
#' - Return FALSE on errors with warnings (`write_*`, `edit_*``delete_*` methods)
#' - Respect user permissions for the current project
#' - Default to current user and project context
#'
#' @field user_id Integer. The current user's ID
#' @field project_id Integer. The current project's ID
#'
#' @examples
#' \dontrun{
#' # Mock usage (for extension development/testing)
#' api <- requal::RequalAPI$new(mode = "mock")
#' segments <- api$get_segments()  # Gets mock data
#' }
#'
#' @export
RequalAPI <- R6::R6Class(
  "RequalAPI",
  private = list(
    .con = NULL,
    .mode = "production",
    .validate_ids = .validate_ids,
    .get_mock_connection = .get_mock_connection,
    .check_permissions = .check_permissions
  ),

  public = list(
    #' @description
    #' Initialize a new RequalAPI instance
    #'
    #' @param con Database connection (pool object). Required for production mode.
    #' @param user_id Integer. The current user's ID.
    #' @param project_id Integer. The current project's ID.
    #' @param mode Character. Either "production" (default) or "mock".
    #'
    #' @return A new RequalAPI instance
    initialize = function(
      con = NULL,
      user_id,
      project_id,
      mode = "production"
    ) {
      if (mode == "mock") {
        private$.con <- private$.get_mock_connection()
        # Use fixed test IDs for mock mode
        self$user_id <- 1L
        self$project_id <- 1L
      } else {
        if (is.null(con)) {
          stop("Database connection required in production mode", call. = FALSE)
        }
        private$.con <- con
        self$user_id <- user_id
        self$project_id <- project_id
      }
      private$.mode <- mode
    },

    user_id = NULL,
    project_id = NULL,

    # =============================================================================
    # GET METHODS - Read operations
    # =============================================================================

    #' @description
    #' Get coded segments for a user and project
    #'
    #' @param user_id Integer. User ID to get segments for. Defaults to current user.
    #' @param project_id Integer. Project ID to get segments from. Defaults to current project.
    #'
    #' @return A dataframe containing segments with associated code information, or NULL on error.
    #'   Columns typically include: segment_id, document_id, start_pos, end_pos,
    #'   segment_text, code_id, code_name, code_description, user_id, project_id
    #'
    #' @examples
    #' \dontrun{
    #' api <- RequalAPI$new(mode = "mock")
    #' segments <- api$get_segments()  # Get all segments for current user/project
    #' segments <- api$get_segments(user_id = 2)  # Get segments for specific user
    #' }
    get_segments = function(
      user_id = self$user_id,
      project_id = self$project_id
    ) {
      tryCatch(
        {
          ids <- private$.validate_ids(user_id, project_id)

          # Always allow viewing own annotations
          can_view_own <- TRUE
          can_view_others <- private$.check_permissions(
            ids$user_id,
            ids$project_id,
            "other_view",
            "annotation"
          )
          can_view_others_codes <- private$.check_permissions(
            ids$user_id,
            ids$project_id,
            "other_view",
            "codebook"
          )

          # Ensure connection is available
          if (is.null(private$.con)) {
            stop("Database connection is not set.")
          }

          # Get segments data - filter based on permissions
          segments_query <- dplyr::tbl(private$.con, "segments") %>%
            dplyr::filter(project_id == !!ids$project_id)

          if (!can_view_others) {
            # Only get user's own segments
            segments_query <- segments_query %>%
              dplyr::filter(user_id == !!ids$user_id)
          }

          segments_data <- segments_query %>% dplyr::collect()

          # Get codes data - filter based on codebook permissions
          codes_query <- dplyr::tbl(private$.con, "codes") %>%
            dplyr::filter(project_id == !!ids$project_id)

          if (!can_view_others_codes) {
            # Only get codes created by this user
            codes_query <- codes_query %>%
              dplyr::filter(created_by == !!ids$user_id)
          }

          codes_data <- codes_query %>%
            dplyr::select(code_id, code_name, code_description) %>%
            dplyr::collect()

          # Join and return
          data <- dplyr::inner_join(segments_data, codes_data, by = "code_id")
          return(data)
        },
        error = function(e) {
          warning("Failed to get segments: ", e$message)
          return(NULL)
        }
      )
    },

    # TODO: Placeholder get methods
    # get_documents = function(project_id = self$project_id) { },
    # get_codes = function(project_id = self$project_id) { },
    # get_users = function(project_id = self$project_id) { },

    # =============================================================================
    # WRITE METHODS - Create new records
    # =============================================================================

    #' @description
    #' Add a new document to the project
    #'
    #' @param doc_name Character. Name/title for the document (required).
    #' @param doc_text Character. Full text content of the document (required).
    #' @param doc_description Character. Optional description of the document.
    #' @param project_id Integer. Project to add document to. Defaults to current project.
    #' @param user_id Integer. User adding the document. Defaults to current user.
    #'
    #' @return Result from the document creation operation, or NULL on error.
    #'
    #' @examples
    #' \dontrun{
    #' api <- RequalAPI$new(mode = "mock")
    #' result <- api$write_document(
    #'   doc_name = "Interview_01",
    #'   doc_text = "Full interview transcript...",
    #'   doc_description = "First participant interview"
    #' )
    #' }
    write_document = function(
      doc_name,
      doc_text,
      doc_description = "",
      project_id = self$project_id,
      user_id = self$user_id
    ) {
      tryCatch(
        {
          ids <- private$.validate_ids(user_id, project_id)

          if (
            !private$.check_permissions(
              ids$user_id,
              ids$project_id,
              "modify",
              "data"
            )
          ) {
            warning("User lacks permission to create documents in this project")
            return(NULL)
          }

          # Validate inputs
          if (is.null(doc_name) || doc_name == "") {
            stop("doc_name cannot be empty")
          }
          if (is.null(doc_text) || doc_text == "") {
            stop("doc_text cannot be empty")
          }

          # Call the main app's document creation function
          # Following the pattern from your example
          result <- add_input_document(
            pool = private$.con,
            project = ids$project_id,
            doc_name = doc_name,
            doc_text = doc_text,
            doc_description = doc_description,
            user_id = ids$user_id
          )

          return(result)
        },
        error = function(e) {
          warning("Failed to write document: ", e$message)
          return(NULL)
        }
      )
    },

    # TODO: Placeholder write methods
    # write_code = function(code_name, code_description = "", project_id = self$project_id) { },
    # write_segment = function(document_id, start_pos, end_pos, code_id) { },

    # =============================================================================
    # EDIT METHODS - Modify existing records
    # =============================================================================

    # TODO: Placeholder edit methods
    # edit_document = function(document_id, doc_name = NULL, doc_description = NULL) { },
    # edit_code = function(code_id, code_name = NULL, code_description = NULL) { },
    # edit_segment = function(segment_id, start_pos = NULL, end_pos = NULL, code_id = NULL) { },

    # =============================================================================
    # DELETE METHODS - Remove records
    # =============================================================================

    # TODO: Placeholder delete methods
    # delete_document = function(document_id) { },
    # delete_code = function(code_id) { },
    # delete_segment = function(segment_id) { },

    # =============================================================================
    # HELPER METHODS - Utility functions
    # =============================================================================

    # TODO: Placeholder helper methods
    # helper_get_project_stats = function(project_id = self$project_id) { },
    # helper_export_project = function(format = "csv", project_id = self$project_id) { },
    # helper_validate_document = function(doc_text) { }
  )
)

# =============================================================================
# API HELPER FUNCTIONS - TO BE MOVED TO api_funs.R
# =============================================================================

#' Validate user and project IDs
#'
#' @description Internal helper to ensure IDs are valid integers
#' @param user_id Numeric. User ID to validate
#' @param project_id Numeric. Project ID to validate
#' @return List with validated integer user_id and project_id
#' @keywords internal
.validate_ids <- function(user_id, project_id) {
  if (!is.numeric(user_id) || user_id != as.integer(user_id)) {
    stop("user_id must be an integer.", call. = FALSE)
  }
  if (!is.numeric(project_id) || project_id != as.integer(project_id)) {
    stop("project_id must be an integer.", call. = FALSE)
  }
  list(user_id = as.integer(user_id), project_id = as.integer(project_id))
}

#' Get connection to mock database
#'
#' @description Internal helper to connect to the bundled mock database for testing
#' @return DBI connection object to mock SQLite database
#' @keywords internal
.get_mock_connection <- function() {
  mock_db_path <- system.file("extdata", "mock.requal", package = "requal")

  if (!file.exists(mock_db_path)) {
    stop("Mock database not found in package", call. = FALSE)
  }

  DBI::dbConnect(RSQLite::SQLite(), mock_db_path)
}

#' Check user permissions for database operations
#'
#' @description Internal helper to verify user has permission for requested operation
#' @param user_id Integer. User ID to check permissions for
#' @param project_id Integer. Project ID to check permissions in
#' @param operation Character. Type of operation: "view", "modify", "other_view", "other_modify"
#' @param context Character. Permission context: "data", "attributes", "codebook", "annotation", "memo"
#' @return Logical. TRUE if user has permission, FALSE otherwise
#' @keywords internal
.check_permissions <- function(
  user_id,
  project_id,
  operation = "view",
  context = "data"
) {
  # Get user permissions from database
  user_perms <- dplyr::tbl(private$.con, "user_permissions") %>%
    dplyr::filter(
      user_id == !!user_id,
      project_id == !!project_id
    ) %>%
    dplyr::collect()

  if (nrow(user_perms) == 0) {
    return(FALSE) # No permissions found
  }

  # Build permission column name
  perm_column <- paste0(context, "_", operation)

  # Check if column exists and get permission value
  if (perm_column %in% names(user_perms)) {
    return(as.logical(user_perms[[perm_column]]))
  } else {
    return(FALSE) # Unknown permission column
  }
}
