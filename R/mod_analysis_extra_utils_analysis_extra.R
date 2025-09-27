# Functions to move to R/mod_segment_more_helpers.R
segment_info_block <- function(
  ns,
  segment_df,
  segment_categories,
  segment_documents
) {
  div(
    style = "text-align: left;",
    div(strong("Segment ID:"), segment_df$segment_id),
    div(
      strong("Coder:"),
      segment_df$user_name,
      paste0("(", segment_df$user_login, ")")
    ),
    div(strong("Document:"), segment_documents$doc_name),
    div(
      strong("Segment range:"),
      segment_df$segment_start,
      "-",
      segment_df$segment_end
    ),
    div(
      strong("Segment code:"),
      span(segment_df$code_name, title = segment_df$code_description),
      icon("tag", style = paste0("color:", segment_df$code_color, ";"))
    ),
    div(
      style = if (nrow(segment_categories) > 0) {
        "margin-top: 5px; margin-bottom: 5px;"
      } else {
        NULL
      },
      strong("Segment categories:"),
      purrr::map2(
        segment_categories$category_name,
        segment_categories$category_description,
        ~ span(
          .x,
          title = .y,
          style = "border-style: solid; padding: 3px; border-width: 1px; border-radius: 5px; border-color: gray; margin-right: 5px;"
        )
      )
    )
  )
}

recode_block <- function(ns, code_choices) {
  div(
    style = "margin-top: 10px; text-align: left;",
    radioButtons(
      ns("recode_action"),
      "Recode action:",
      choices = list(
        "Alter" = "alter",
        "Add" = "add",
        "Remove" = "remove"
      ),
      selected = character(0),
      inline = TRUE
    ),

    conditionalPanel(
      condition = "input.recode_action == 'alter' || input.recode_action == 'add'",
      ns = ns,
      wellPanel(
        checkboxInput(ns("create_new_code"), "Create new code", value = FALSE),
        conditionalPanel(
          condition = "!input.create_new_code",
          ns = ns,
          selectInput(
            ns("recode_select"),
            "Select code:",
            choices = c("", code_choices),
            selected = ""
          )
        ),
        conditionalPanel(
          condition = "input.create_new_code",
          ns = ns,
          textInput(
            ns("new_code_name"),
            "Code name:",
            placeholder = "Enter code name"
          ),
          textInput(
            ns("new_code_description"),
            "Code description:",
            placeholder = "Enter description (optional)"
          ),
          colourpicker::colourInput(
            ns("new_code_color"),
            "Code color:",
            value = "#3498db",
            showColour = "background"
          )
        )
      )
    ),

    conditionalPanel(
      condition = "input.recode_action == 'remove'",
      ns = ns,
      div(
        style = "margin-top: 10px; padding: 8px; background-color: #fff3cd; border: 1px solid #ffeaa7; border-radius: 4px;",
        p(
          style = "margin: 0; color: #856404;",
          HTML(
            "<strong>Warning:</strong> This will permanently delete the segment."
          )
        )
      )
    ),

    conditionalPanel(
      condition = "input.recode_action != ''",
      ns = ns,
      div(style = "margin-top: 15px;", uiOutput(ns("recode_button_ui")))
    )
  )
}

adjust_block <- function(ns) {
  div(
    style = "margin-top: 10px; text-align: left;",
    sliderInput(
      ns("adjust_context_window"),
      "Context in characters:",
      min = 0,
      max = 1000,
      value = 0,
      step = 50
    )
  )
}

# Functions to move to R/mod_segment_more_business.R
check_permissions <- function(
  action,
  segment_df,
  user_data,
  is_creating_new_code,
  current_user_id
) {
  if (is.null(user_data) || user_data$annotation_modify != 1) {
    return(list(
      valid = FALSE,
      message = "Insufficient permissions: annotation_modify required"
    ))
  }

  is_other_user <- segment_df$user_id != current_user_id
  needs_other_modify <- (action == "alter")

  if (
    needs_other_modify &&
      is_other_user &&
      (is.null(user_data) || user_data$annotation_other_modify != 1)
  ) {
    return(list(
      valid = FALSE,
      message = "Insufficient permissions: annotation_other_modify required to modify other user's segments"
    ))
  }

  if (
    is_creating_new_code &&
      (is.null(user_data) || user_data$codebook_modify != 1)
  ) {
    return(list(
      valid = FALSE,
      message = "Insufficient permissions: codebook_modify required to create new codes"
    ))
  }

  return(list(valid = TRUE, message = NULL))
}

validate_new_code <- function(code_name, existing_codes, current_code) {
  if (code_name == "") {
    return(list(valid = FALSE, message = "Please enter a code name"))
  }

  existing_names <- c(existing_codes$code_name, current_code)
  if (code_name %in% existing_names) {
    return(list(
      valid = FALSE,
      message = "Code name already exists. Please choose a unique name."
    ))
  }

  return(list(valid = TRUE, message = NULL))
}

create_new_code <- function(code_name, code_description, code_color, glob) {
  codes_input_df <- data.frame(
    code_name = code_name,
    code_description = code_description,
    code_color = paste0(
      "rgb(",
      paste(as.vector(grDevices::col2rgb(code_color)), collapse = ", "),
      ")"
    ),
    project_id = as.integer(glob$active_project),
    user_id = as.integer(glob$user$user_id),
    stringsAsFactors = FALSE
  )

  new_code_id <- add_codes_record(
    pool = glob$pool,
    project_id = glob$active_project,
    codes_df = codes_input_df,
    user_id = glob$user$user_id
  )

  if (length(new_code_id) == 0) {
    stop("Failed to retrieve new code ID after creation")
  }

  return(new_code_id)
}

check_code_overlaps <- function(target_code_id, segment_df, glob) {
  coded_segments <- dplyr::tbl(glob$pool, "segments") %>%
    dplyr::filter(
      .data$project_id == !!as.integer(glob$active_project),
      .data$user_id == !!as.integer(glob$user$user_id),
      .data$doc_id == !!as.integer(segment_df$doc_id),
      .data$code_id == !!as.integer(target_code_id)
    ) %>%
    dplyr::select(
      project_id,
      user_id,
      doc_id,
      code_id,
      segment_id,
      segment_start,
      segment_end
    ) %>%
    dplyr::collect()

  overlap_result <- check_overlap(
    coded_segments,
    segment_df$segment_start,
    segment_df$segment_end
  )
  list(has_overlap = nrow(overlap_result) > 0, overlap_data = overlap_result)
}

execute_direct_update <- function(
  target_code_id,
  segment_id,
  action_name,
  pool
) {
  tryCatch(
    {
      update_sql <- glue::glue_sql(
        "UPDATE segments SET code_id = {target_code_id} WHERE segment_id = {segment_id}",
        target_code_id = as.integer(target_code_id),
        segment_id = as.integer(segment_id),
        .con = pool
      )
      DBI::dbExecute(pool, update_sql)
      showNotification(
        paste0(action_name, " segment ", segment_id),
        type = "message"
      )
      TRUE
    },
    error = function(e) {
      showNotification(
        paste0("Failed to update segment: ", e$message),
        type = "error"
      )
      FALSE
    }
  )
}

execute_write_segment <- function(
  target_code_id,
  segment_df,
  action_name,
  glob
) {
  tryCatch(
    {
      write_segment_db(
        pool = glob$pool,
        active_project = glob$active_project,
        user_id = glob$user$user_id,
        doc_id = segment_df$doc_id,
        code_id = target_code_id,
        startOff = segment_df$segment_start,
        endOff = segment_df$segment_end
      )
      showNotification(
        paste0(action_name, " segment ", segment_df$segment_id),
        type = "message"
      )
      TRUE
    },
    error = function(e) {
      showNotification(
        paste0("Failed to ", tolower(action_name), " segment: ", e$message),
        type = "error"
      )
      FALSE
    }
  )
}
