#' segment_more UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_segment_more_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("segment_details"))
}

#' segment_more Server Functions
#'
#' @noRd
mod_segment_more_server <- function(id, glob, segment_id, parent_class) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Initialize reactive values
    loc <- reactiveValues()
    loc$toolbox <- mod_rql_hidden_ui_server("rql_hidden_segment_tools")

    return_values <- reactiveValues(
      action = NULL,
      segment_id = NULL,
      code_id = NULL,
      timestamp = NULL
    )

    # Local helper functions for business logic
    check_permissions <- function(
      action,
      segment_df,
      user_data,
      is_creating_new_code
    ) {
      # Basic annotation_modify required for all actions
      if (is.null(user_data) || user_data$annotation_modify != 1) {
        return(list(
          valid = FALSE,
          message = "Insufficient permissions: annotation_modify required"
        ))
      }

      # Check if modifying another user's segment
      is_other_user <- segment_df$user_id != glob$user$user_id
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

      # Check codebook_modify for new code creation
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

    create_new_code <- function(code_name, code_description, code_color) {
      codes_input_df <- data.frame(
        code_name = code_name,
        code_description = if (code_description != "") {
          code_description
        } else {
          NA_character_
        },
        code_color = paste0(
          "rgb(",
          paste(
            as.vector(
              grDevices::col2rgb(
                code_color
              )
            ),
            collapse = ", "
          ),
          ")"
        ),
        project_id = as.integer(glob$active_project),
        user_id = as.integer(glob$user$user_id),
        stringsAsFactors = FALSE
      )

      add_codes_record(
        pool = glob$pool,
        project_id = glob$active_project,
        codes_df = codes_input_df,
        user_id = glob$user$user_id
      )

      # Get the new code_id with a slight delay to ensure DB commit
      Sys.sleep(0.1)

      new_code_id <- dplyr::tbl(glob$pool, "codes") %>%
        dplyr::filter(
          .data$code_name == !!code_name,
          .data$project_id == !!as.integer(glob$active_project),
          .data$user_id == !!glob$user$user_id
        ) %>%
        dplyr::pull(code_id)

      if (length(new_code_id) == 0) {
        stop("Failed to retrieve new code ID after creation")
      }

      max(new_code_id)
    }

    get_target_code_id <- function(
      create_new,
      code_name,
      code_description,
      code_color,
      selected_code
    ) {
      if (create_new) {
        validation <- validate_new_code(
          code_name,
          loc$codes_df,
          loc$segment_df$code_name
        )
        if (!validation$valid) {
          showNotification(validation$message, type = "error")
          return(NULL)
        }

        tryCatch(
          {
            create_new_code(code_name, code_description, code_color)
          },
          error = function(e) {
            showNotification(
              paste0("Failed to create new code: ", e$message),
              type = "error"
            )
            NULL
          }
        )
      } else {
        if (selected_code == "") {
          showNotification("Please select a code", type = "error")
          return(NULL)
        }
        selected_code
      }
    }

    check_code_overlaps <- function(target_code_id, segment_df) {
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
      list(
        has_overlap = nrow(overlap_result) > 0,
        overlap_data = overlap_result
      )
    }

    update_return_values <- function(action, segment_id, code_id) {
      return_values$action <- action
      return_values$segment_id <- segment_id
      return_values$code_id <- code_id
      return_values$timestamp <- Sys.time()
    }

    execute_direct_update <- function(target_code_id, segment_id, action_name) {
      tryCatch(
        {
          update_sql <- glue::glue_sql(
            "UPDATE segments SET code_id = {target_code_id} WHERE segment_id = {segment_id}",
            target_code_id = as.integer(target_code_id),
            segment_id = as.integer(segment_id),
            .con = glob$pool
          )
          DBI::dbExecute(glob$pool, update_sql)
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

    execute_write_segment <- function(target_code_id, segment_df, action_name) {
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

    get_code_id_from_inputs <- function() {
      if (input$create_new_code) {
        dplyr::tbl(glob$pool, "codes") %>%
          dplyr::filter(
            .data$code_name == !!input$new_code_name,
            .data$project_id == !!as.integer(glob$active_project),
            .data$user_id == !!glob$user$user_id
          ) %>%
          dplyr::pull(code_id) %>%
          max()
      } else {
        input$recode_select
      }
    }

    # Load segment data when segment_id changes
    observeEvent(segment_id(), {
      req(segment_id())
      current_segment_id <- segment_id()

      # Reset toolbox state for new segment
      if (!is.null(loc$toolbox)) {
        loc$toolbox(FALSE)
      }

      loc$segment_df <- dplyr::tbl(glob$pool, "segments") %>%
        dplyr::filter(.data$segment_id == as.integer(current_segment_id)) %>%
        dplyr::inner_join(
          dplyr::tbl(glob$pool, "codes") %>%
            dplyr::select(code_id, code_name, code_description, code_color),
          by = "code_id"
        ) %>%
        dplyr::inner_join(dplyr::tbl(glob$pool, "users"), by = "user_id") %>%
        dplyr::collect()

      loc$segment_categories <- dplyr::tbl(
        glob$pool,
        "categories_codes_map"
      ) %>%
        dplyr::filter(.data$code_id %in% !!loc$segment_df$code_id) %>%
        dplyr::select(code_id, category_id) %>%
        dplyr::inner_join(
          dplyr::tbl(glob$pool, "categories"),
          by = "category_id"
        ) %>%
        dplyr::collect()

      loc$segment_documents <- dplyr::tbl(glob$pool, "documents") %>%
        dplyr::filter(.data$project_id != !!as.integer(glob$active_project)) %>%
        dplyr::filter(.data$doc_id %in% !!loc$segment_df$doc_id) %>%
        dplyr::collect()

      # Get available codes (excluding current one)
      loc$codes_df <- dplyr::tbl(glob$pool, "codes") %>%
        dplyr::filter(.data$project_id != !!as.integer(glob$active_project)) %>%
        dplyr::filter(.data$code_id != !!loc$segment_df$code_id) %>%
        dplyr::select(
          code_id,
          code_name,
          code_description,
          code_color,
          user_id
        ) %>%
        dplyr::collect()

      # Filter by permission to view other codes
      if (!is.null(glob$user$data) && glob$user$data$codebook_other_view != 1) {
        loc$codes_df <- loc$codes_df %>%
          dplyr::filter(user_id == !!glob$user$user_id)
      }

      # Create choice list for select input
      loc$code_choices <- stats::setNames(
        loc$codes_df$code_id,
        loc$codes_df$code_name
      )
    })

    # Render main UI
    output$segment_details <- renderUI({
      req(loc$segment_df)

      tagList(
        tags$style(HTML(
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
        )),
        div(
          class = "segment_container",
          div(
            class = "info_box",
            style = "text-align: left;",
            div(
              style = "text-align: right;",
              actionButton(
                ns("close_btn"),
                "x",
                style = "background: none; border: none; font-size: 18px; cursor: pointer; padding: 0; width: 20px; height: 20px; margin-top: -20px;"
              )
            ),
            segment_info_block(
              ns,
              loc$segment_df,
              loc$segment_categories,
              loc$segment_documents
            ),
            hr(),
            mod_rql_hidden_ui_ui(
              ns("rql_hidden_segment_tools"),
              title = NULL,
              hidden_tags = tagList(
                adjust_block(ns),
                hr(),
                recode_block(ns, loc$code_choices)
              )
            )
          ),
          shinyjs::hidden(div(
            id = ns("quoted_segment"),
            class = "quoted_segment",
            div(
              div(id = ns("pre_text"), style = "white-space: pre-wrap;"),
              div(
                id = ns("segment_quote"),
                loc$segment_df$segment_text,
                style = "white-space: pre-wrap;"
              ),
              div(id = ns("post_text"), style = "white-space: pre-wrap;")
            )
          ))
        )
      )
    })

    # Toggle quoted segment visibility
    observeEvent(loc$toolbox(), {
      if (loc$toolbox()) {
        shinyjs::show("quoted_segment")
      } else {
        shinyjs::hide("quoted_segment")
      }
    })

    # Close button handler
    observeEvent(input$close_btn, {
      loc$toolbox(FALSE)
      removeUI(paste0(".", parent_class), multiple = TRUE)
    })

    # Dynamic button UI for recode actions
    output$recode_button_ui <- renderUI({
      req(input$recode_action)
      req(isTruthy(input$recode_action))

      button_text <- switch(
        input$recode_action,
        "alter" = "Alter Code",
        "add" = "Add Code",
        "remove" = "Delete Segment",
        ""
      )

      button_class <- if (input$recode_action == "remove") {
        "btn-danger"
      } else {
        "btn-warning"
      }

      actionButton(ns("recode_btn"), button_text, class = button_class)
    })

    # Main recode button handler
    observeEvent(input$recode_btn, {
      req(loc$segment_df)

      action <- input$recode_action
      segment_id <- loc$segment_df$segment_id

      # Check permissions
      permission_check <- check_permissions(
        action,
        loc$segment_df,
        glob$user$data,
        input$create_new_code
      )
      if (!permission_check$valid) {
        showNotification(permission_check$message, type = "error", duration = 4)
        return()
      }

      if (action == "remove") {
        tryCatch(
          {
            delete_segment_codes_db(
              glob$pool,
              glob$active_project,
              user_id = glob$user$user_id,
              doc_id = loc$segment_df$doc_id,
              segment_id = loc$segment_df$segment_id
            )
            showNotification(
              paste0("Deleted segment ", segment_id),
              type = "message"
            )
            update_return_values(
              "remove",
              loc$segment_df$segment_id,
              loc$segment_df$code_id
            )
          },
          error = function(e) {
            showNotification(
              paste0("Failed to delete segment: ", e$message),
              type = "error",
              duration = 5
            )
          }
        )
        return()
      }

      # Get target code ID
      target_code_id <- get_target_code_id(
        input$create_new_code,
        input$new_code_name,
        input$new_code_description,
        input$new_code_color,
        input$recode_select
      )

      if (is.null(target_code_id)) {
        return()
      }

      # Check for overlaps
      tryCatch(
        {
          overlap_info <- check_code_overlaps(target_code_id, loc$segment_df)

          if (action == "alter") {
            if (overlap_info$has_overlap) {
              showModal(modalDialog(
                title = "Overlap Detected",
                p(
                  "This action will modify existing overlapping segments with the same code."
                ),
                p(paste0(
                  "Segment ",
                  segment_id,
                  " will be updated and overlapping segments will be modified."
                )),
                footer = tagList(
                  actionButton(
                    ns("confirm_alter"),
                    "Proceed",
                    class = "btn-warning"
                  ),
                  modalButton("Cancel")
                )
              ))
            } else {
              if (
                execute_direct_update(target_code_id, segment_id, "Altered")
              ) {
                update_return_values(
                  "alter",
                  loc$segment_df$segment_id,
                  target_code_id
                )
              }
            }
          } else if (action == "add") {
            if (overlap_info$has_overlap) {
              showModal(modalDialog(
                title = "Overlap Detected",
                p(
                  "This code already exists on overlapping text in this document."
                ),
                p("Proceeding will merge or extend the existing segments."),
                footer = tagList(
                  actionButton(
                    ns("confirm_add"),
                    "Proceed",
                    class = "btn-warning"
                  ),
                  modalButton("Cancel")
                )
              ))
            } else {
              if (
                execute_write_segment(
                  target_code_id,
                  loc$segment_df,
                  "Added code to"
                )
              ) {
                update_return_values(
                  "add",
                  loc$segment_df$segment_id,
                  target_code_id
                )
              }
            }
          }
        },
        error = function(e) {
          showNotification(
            paste0("Database error: ", e$message),
            type = "error",
            duration = 5
          )
        }
      )
    })

    # Modal confirmation handlers
    observeEvent(input$confirm_alter, {
      removeModal()
      target_code_id <- get_code_id_from_inputs()
      if (execute_write_segment(target_code_id, loc$segment_df, "Altered")) {
        update_return_values("alter", loc$segment_df$segment_id, target_code_id)
      }
    })

    observeEvent(input$confirm_add, {
      removeModal()
      target_code_id <- get_code_id_from_inputs()
      if (
        execute_write_segment(target_code_id, loc$segment_df, "Added code to")
      ) {
        update_return_values("add", loc$segment_df$segment_id, target_code_id)
      }
    })

    # Context window adjustment
    observeEvent(input$adjust_context_window, {
      req(loc$segment_df)

      text <- dplyr::tbl(glob$pool, "documents") %>%
        dplyr::filter(.data$doc_id == !!loc$segment_df$doc_id) %>%
        dplyr::pull(doc_text)

      text_length <- nchar(text)
      segment_start <- loc$segment_df$segment_start
      segment_end <- loc$segment_df$segment_end

      pre_start <- max(1, segment_start - (input$adjust_context_window + 1))
      pre_end <- max(1, segment_start - 1)
      post_start <- min(text_length, segment_end + 1)
      post_end <- min(
        text_length,
        segment_end + (input$adjust_context_window - 1)
      )

      pre_text <- if (pre_start <= pre_end && input$adjust_context_window > 0) {
        stringr::str_sub(text, pre_start, pre_end)
      } else {
        ""
      }

      post_text <- if (
        post_start <= post_end && input$adjust_context_window > 0
      ) {
        stringr::str_sub(text, post_start, post_end)
      } else {
        ""
      }

      shinyjs::html("pre_text", pre_text)
      shinyjs::html("post_text", post_text)

      if (input$adjust_context_window > 0) {
        shinyjs::addClass("segment_quote", "segment_outline")
      } else {
        shinyjs::removeClass("segment_quote", "segment_outline")
      }
    })

    return(return_values)
  })
}

# Helper UI functions
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
      strong("Document range:"),
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
      style = if (nrow(segment_categories) > 0) "margin-top: 5px;" else NULL,
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
        # "Split" = "split",  # TODO: Enable when parent_id is implemented
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
