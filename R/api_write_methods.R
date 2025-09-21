#' Write documents from a data frame to the database
#'
#' @description Internal helper to write documents from a data frame to the database.
#' This function performs input validation, a permissions check, and then
#' iterates through the data frame to add each document to the project
#' using an internal `add_input_document()` function which also handles logging.
#'
#' @param .data A data frame containing the documents to be written. Must have
#'   the columns `doc_name`, `doc_text`, and `doc_description`.
#' @param private A list-like object containing private fields, including
#'   `.con` (database connection), `.project_id`, `.user_id`
#'
#' @return A logical value. Returns `TRUE` if all documents are added successfully,
#'   and `FALSE` if any document fails to be added.
#' @keywords internal
write_documents_impl <- function(.data, private) {
  # 1. Input Validation
  if (!is.data.frame(.data)) {
    stop("The input object must be a data frame.")
  }

  expected_cols <- c("doc_name", "doc_text", "doc_description")
  missing_cols <- setdiff(expected_cols, colnames(.data))
  if (length(missing_cols) > 0) {
    stop(
      "The following required columns are missing from the data frame: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  # 2. Ensure the user has permission for the project
  # Here we need a permission check for "data_modify"
  data_modify_permission <- private$.check_permissions(
    permission = "data_modify"
  )
  if (!data_modify_permission) {
    stop("User does not have permission to modify data in this project.")
  }

  # 3. Function execution
  # Sanitize purrr input
  .data <- .data %>%
    dplyr::select(doc_name, doc_text, doc_description) %>%
    dplyr::mutate(row_index = dplyr::row_number())
  results <- purrr::pmap_lgl(
    .data,
    function(doc_name, doc_text, doc_description, row_index) {
      tryCatch(
        {
          add_input_document(
            pool = private$.con,
            project = private$.project_id,
            doc_name = doc_name,
            doc_text = doc_text,
            doc_description = doc_description,
            user_id = private$.user_id
          )
          TRUE # Return TRUE on success
        },
        error = function(e) {
          warning(sprintf(
            "Failed to add document at row %d: %s",
            row_index,
            e$message
          ))
          FALSE # Return FALSE on error
        }
      )
    }
  )

  # 4. Final Result
  return(all(results))
}
