#' @export
RequalAPI <- R6::R6Class(
  "RequalAPI",
  private = list(
    .con = NULL,
    .mode = "production" # Default mode
  ),
  public = list(
    initialize = function(con, user_id, project_id, mode = "production") {
      private$.con <- con
      self$user_id <- user_id
      self$project_id <- project_id
      private$.mode <- mode
    },
    user_id = NULL,
    project_id = NULL,

    get_segments = function(
      user_id = self$user_id,
      project_id = self$project_id
    ) {
      # Ensure user_id is an integer
      if (!is.numeric(user_id) || user_id != as.integer(user_id)) {
        stop("user_id must be an integer.")
      }
      user_id <- as.integer(user_id)

      # Ensure project_id is an integer
      if (!is.numeric(project_id) || project_id != as.integer(project_id)) {
        stop("project_id must be an integer.")
      }
      project_id <- as.integer(project_id)

      if (private$.mode == "mock") {
        # Return mock data
        return(data.frame(
          project_id = project_id,
          user_id = user_id,
          code_id = c(1, 2),
          code_name = c("Mock Code 1", "Mock Code 2"),
          segment_text = c("Mock Segment 1", "Mock Segment 2"),
          stringsAsFactors = FALSE
        ))
      } else {
        # Ensure connection is available
        if (is.null(private$.con)) {
          stop("Database connection is not set.")
        }
        # Collect data with filters
        segments_data <- dplyr::tbl(private$.con, "segments") |>
          dplyr::filter(
            user_id == !!user_id,
            project_id == !!project_id
          ) |>
          dplyr::collect()
        codes_data <- dplyr::tbl(private$.con, "codes") |>
          dplyr::filter(
            project_id == !!project_id
          ) |>
          dplyr::select(code_id, code_name, code_description) |>
          dplyr::collect()
        data <- dplyr::inner_join(segments_data, codes_data, by = "code_id")
        return(data)
      }
    }
  )
)
