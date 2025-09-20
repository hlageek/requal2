# =============================================================================
# REQUAL API HELPER FUNCTIONS
# =============================================================================

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
#' @param con Database connection object
#' @return Logical. TRUE if user has permission, FALSE otherwise
#' @keywords internal
.check_permissions <- function(
  user_id,
  project_id,
  con,
  operation = "view",
  context = "data"
) {
  # Get user permissions from database
  user_perms <- dplyr::tbl(con, "user_permissions") %>%
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
