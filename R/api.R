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
      if (private$.mode == "mock") {
        # Return mock data
        return(data.frame(
          segment_text = c("Mock Segment 1", "Mock Segment 2"),
          stringsAsFactors = FALSE
        ))
      } else {
        # Ensure connection is available
        if (is.null(private$.con)) {
          stop("Database connection is not set.")
        }
        # Collect data with filters
        data <- dplyr::tbl(private$.con, "segments") |>
          dplyr::filter(
            user_id == !!user_id,
            project_id == !!project_id
          ) |>
          dplyr::select(segment_text) |>
          dplyr::collect()
        return(data)
      }
    }
  )
)
