# UI helpers ------
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
        "Strip" = "strip"
        # TODO: "Split" = "split"
      ),
      selected = character(0),
      inline = TRUE
    ),

    conditionalPanel(
      condition = "input.recode_action == 'alter' || input.recode_action == 'add'",
      ns = ns,

      # Select existing code
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
      # Checkbox to toggle between selecting and creating a new code
      checkboxInput(ns("create_new_code"), "Create new code", value = FALSE),
      # Create new code inputs
      conditionalPanel(
        condition = "input.create_new_code",
        ns = ns,
        textInput(
          ns("new_code_name"),
          "Code name:",
          placeholder = "Enter code name"
        ),
        textAreaInput(
          ns("new_code_description"),
          "Code description:",
          placeholder = "Enter description (optional)",
          rows = 1
        ),
        colourpicker::colourInput(
          ns("new_code_color"),
          "Code color:",
          value = "#ffff00",
          showColour = "background"
        )
      )
    ),

    conditionalPanel(
      condition = "input.recode_action == 'strip'",
      ns = ns,
      div(
        style = "margin-top: 10px; padding: 8px; background-color: #fff3cd; border: 1px solid #ffeaa7; border-radius: 4px;",
        p(
          style = "margin: 0; color: #856404;",
          HTML(
            "<strong>Warning:</strong> Stripping code from segment will delete the segment."
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

mod_analysis_extra_css <- function() {
  "
          .segment_container {
            display: flex;
            flex-direction: row-reverse;
            width: 100%;
            max-width: 1000px;
          }
          .info_box {
            width: 300px;
            min-width: 20vw;
            max-width: 40vw;
            padding-left: 30px;
            scrollbar-width: thin;
          }
          .quoted_segment {
            flex: 1;
            padding: 10px;
            min-width: 40vw;
            max-width: 60vw;
            max-height: 80vh;
            background-color: white;
            text-align: left;
            overflow-y: scroll;
            scrollbar-width: thin;
          }
          .segment_outline {
            white-space: pre-wrap;
            background-color: #FFF8DC;
            padding: 10px;
            border-radius: 5px;
            box-sizing: border-box;
            outline: dashed 2px #FF6347;
          }
        "
}


# check permissions for recode actions --------
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

# Validate if new code has a name and is unique ------
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

# Create new code from recode module ----
create_new_code_on_recode <- function(
  code_name,
  code_description,
  code_color,
  glob
) {
  rql_message(paste0(
    "rgb(",
    paste(as.vector(grDevices::col2rgb(code_color)), collapse = ", "),
    ")"
  ))
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

check_code_overlaps_on_recode <- function(target_code_id, segment_df, glob) {
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

  # use function from document_code_utils
  overlap_result <- check_overlap(
    coded_segments,
    segment_df$segment_start,
    segment_df$segment_end
  )
  list(has_overlap = nrow(overlap_result) > 0, overlap_data = overlap_result)
}

# Update information in segments table ----
# this is the most straightforward recode action
# but cannot be used for situations with overlapping segments
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

# A wrapper around write_segment_db ----
# with some error handling and message display
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
