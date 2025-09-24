# =============================================================================
# REQUAL API HELPER FUNCTIONS
# =============================================================================

# Connect to mock database ----------------------------

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

# Permission checks ----------------------------

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

# Query the database tables ----------------------------

#' Construct a generic table query object
#'
#' @description Internal helper function to construct a database query for
#' retrieving data from a specified table.
#'
#' @param private A list-like object containing private fields, including
#'   `.con` (database connection), `.project_id`, `.user_id`, and
#'   a method for checking permissions.
#' @param table A character string specifying the table name to query.
#'
#' @return A `dplyr` query object representing the specified table, filtered
#'   according to the project ID and user permissions.
#'
#' @keywords internal
get_table_query <- function(private, table = NULL, collect = FALSE) {
  if (is.null(private$.con)) {
    stop("Database connection is not set.")
  }
  if (is.null(table)) {
    stop("Table name must be provided.")
  }

  # Build the base query
  query <- dplyr::tbl(private$.con, table) %>%
    dplyr::filter(project_id == !!private$.project_id)

  # Apply table-specific modifications
  query <- modify_query(query, table, can_view_others, private)

  if (collect) {
    query <- dplyr::collect(query)
  }

  return(query)
}

# To add new tables
# 1. create new modification function
# 2. add them to the switch statement in modify_query.

#' Modify query based on table-specific logic
#'
#' @param query The base dplyr query object.
#' @param table The table name.
#' @param can_view_others Boolean indicating if the user can view others' data.
#' @param private The private list-like object containing user and project info.
#'
#' @return A modified dplyr query object.
#' @keywords internal
modify_query <- function(query, table, can_view_others, private) {
  modified_query <- switch(
    table,
    "documents" = modify_documents_query(query, private),
    "segments" = modify_segments_query(query, private),
    "codes" = modify_codebook_query(query, private),
    stop("Unknown table name.")
  )
  return(modified_query)
}

#' Modify documents query
modify_documents_query <- function(query, private) {
  query <- query %>%
    dplyr::select(
      doc_id,
      user_id,
      doc_name,
      doc_description,
      doc_text,
      created_at
    )
  if (!private$.check_permissions("data_other_view")) {
    query <- query %>%
      dplyr::filter(user_id == !!private$.user_id)
  }
  return(query)
}

#' Modify segments query
modify_segments_query <- function(query, private) {
  query <- query %>%
    dplyr::filter(!is.na(code_id)) %>% # Exclude memo segments
    dplyr::select(
      segment_id,
      user_id,
      doc_id,
      segment_start,
      segment_end,
      segment_text,
      code_id
    )
  if (!private$.check_permissions("annotation_other_view")) {
    query <- query %>%
      dplyr::filter(user_id == !!private$.user_id)
  }
  return(query)
}

#' Modify codebook query
modify_codebook_query <- function(query, private) {
  query <- query %>%
    dplyr::select(code_id, code_name, code_description, user_id)
  if (!private$.check_permissions("codebook_other_view")) {
    query <- query %>%
      dplyr::filter(user_id == !!private$.user_id)
  }
  return(query)
}

# Get_* methods filters ----------------------------

# get_segments = function(date_range = NULL, user_ids = NULL, keyword = NULL,
#                         page = 1, page_size = 100, sort_by = NULL, descending = FALSE) {
#   query <- get_table_query(private, "segments")

#   # Apply filters using helper functions
#   query <- apply_date_range_filter(query, "segment_date", date_range)
#   query <- apply_user_id_filter(query, "user_id", user_ids)
#   query <- apply_keyword_filter(query, "segment_text", keyword)
#   query <- apply_sorting(query, sort_by, descending)
#   query <- apply_pagination(query, page, page_size)

#   return(dplyr::collect(query))
# }

#' Filter for date range
apply_date_range_filter <- function(query, date_column, date_range) {
  if (!is.null(date_range)) {
    query <- query %>%
      dplyr::filter(
        !!sym(date_column) >= !!date_range[1] &
          !!sym(date_column) <= !!date_range[2]
      )
  }
  return(query)
}

apply_user_id_filter <- function(query, user_id_column, user_ids) {
  if (!is.null(user_ids)) {
    query <- query %>%
      dplyr::filter(!!sym(user_id_column) %in% !!user_ids)
  }
  return(query)
}

apply_keyword_filter <- function(query, text_column, keyword) {
  if (!is.null(keyword)) {
    query <- query %>%
      dplyr::filter(grepl(!!keyword, !!sym(text_column), ignore.case = TRUE))
  }
  return(query)
}

apply_sorting <- function(query, sort_by, descending) {
  if (!is.null(sort_by)) {
    query <- query %>%
      dplyr::arrange(
        if (descending) dplyr::desc(!!sym(sort_by)) else !!sym(sort_by)
      )
  }
  return(query)
}

apply_pagination <- function(query, page, page_size) {
  query <- query %>%
    dplyr::slice((page - 1) * page_size + 1:page_size)
  return(query)
}
