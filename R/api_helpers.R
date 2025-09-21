# =============================================================================
# REQUAL API HELPER FUNCTIONS
# =============================================================================

#' Get connection to mock database
#'
#' @description Internal helper to connect to the bundled mock database for testing
#' @return DBI connection object to mock SQLite database
#' @keywords internal
.get_mock_connection <- function() {
  # Locate the mock database in the package
  mock_db_path <- system.file("extdata", "mock.requal", package = "requal")

  # Create a temporary file for the writable database
  temp_db_path <- tempfile(fileext = ".requal")

  # Copy the mock database to the temporary file
  file.copy(mock_db_path, temp_db_path, overwrite = TRUE)

  # Connect to the temporary database file
  con <- DBI::dbConnect(RSQLite::SQLite(), temp_db_path)

  return(con)
}


#' Check user permissions for database operations
#'
#' @description Internal helper to verify user has permission for requested operation
#' @param con Database connection object
#' @param user_id Integer. User ID to check permissions for
#' @param project_id Integer. Project ID to check permissions in
#' @param permission Character. Type of permission to check for. One of:
#'
#' - **data_modify**: Permission to modify data within the project.
#' - **data_other_modify**: Permission to modify data added by other users.
#' - **data_other_view**: Permission to view data added by other users.
#' - **attributes_modify**: Permission to modify attributes within the project.
#' - **attributes_other_modify**: Permission to modify attributes added by other users.
#' - **attributes_other_view**: Permission to view attributes added by other users.
#' - **codebook_modify**: Permission to modify the codebook within the project.
#' - **codebook_other_modify**: Permission to modify the codebook entries added by other users.
#' - **codebook_other_view**: Permission to view the codebook entries added by other users.
#' - **annotation_modify**: Permission to modify annotations within the project.
#' - **annotation_other_modify**: Permission to modify annotations added by other users.
#' - **annotation_other_view**: Permission to view annotations added by other users.
#' - **analysis_other_view**: Permission to view analyses performed by other users.
#' - **report_other_view**: Permission to view data of other users in reports.
#' - **permissions_modify**: Permission to modify permissions within the project.
#' - **memo_modify**: Permission to modify memos within the project.
#' - **memo_other_modify**: Permission to modify memos added by other users.
#' - **memo_other_view**: Permission to view memos added by other users.
#' - **project_owner**: Permission indicating ownership of the project.
#'
#' @return Logical. TRUE if user has permission, FALSE otherwise
#' @keywords internal
.check_permissions_impl <- function(
  con,
  user_id,
  project_id,
  permission = NULL
) {
  # Early exit if permission is not specified
  if (is.null(permission)) {
    warning("Permission argument is NULL; returning FALSE.")
    return(FALSE)
  }

  # Get user permissions from database
  user_perms <- dplyr::tbl(con, "user_permissions") %>%
    dplyr::filter(
      user_id == !!user_id,
      project_id == !!project_id
    ) %>%
    dplyr::collect()

  # Permission data should be exactly 1 row dataframe, no more, no less
  if (nrow(user_perms) != 1) {
    return(FALSE) # Could not resolve permissions data
  }

  # Check if permission exists and get permission value
  if (permission %in% names(user_perms)) {
    return(as.logical(user_perms[[permission]]))
  } else {
    warning("Unknown permission argument; returning FALSE.")
    return(FALSE) # Unknown permission
  }
}
