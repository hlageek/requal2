#' segment_more UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_analysis_extra_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("segment_details"))
}

#' segment_more Server Functions
#'
#' @noRd
# Main module server function
# Main module server function
mod_analysis_extra_server <- function(id, glob, segment_id, parent_class) {
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

    # Locally scoped helper functions - defined early so they can be called later
    .get_target_code_id <- function() {
      if (input$create_new_code) {
        validation <- validate_new_code(
          input$new_code_name,
          loc$codes_df,
          loc$segment_df$code_name
        )
        if (!validation$valid) {
          rql_message(validation$message)
          return(NULL)
        }

        tryCatch(
          {
            new_code_id <- create_new_code(
              input$new_code_name,
              input$new_code_description,
              input$new_code_color,
              glob
            )
            return(new_code_id)
          },
          error = function(e) {
            rql_message(paste0("Failed to create new code: ", e$message))
            NULL
          }
        )
      } else {
        if (input$recode_select == "") {
          rql_message("Please select a code")
          return(NULL)
        }
        input$recode_select
      }
    }

    .get_code_id_from_inputs <- function() {
      if (input$create_new_code) {
        # Since we just created the code, we can get it by name
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

    .update_return_values <- function(action, segment_id, code_id) {
      return_values$action <- action
      return_values$segment_id <- segment_id
      return_values$code_id <- code_id
      return_values$timestamp <- Sys.time()
    }

    # Load segment data when segment_id changes
    observeEvent(segment_id(), {
      req(segment_id())
      current_segment_id <- segment_id()

      # Reset toolbox state for new segment
      if (!is.null(loc$toolbox)) {
        loc$toolbox(FALSE)
      }

      # Load segment data with proper bang-bang operators
      loc$segment_df <- dplyr::tbl(glob$pool, "segments") %>%
        dplyr::filter(.data$segment_id == !!as.integer(current_segment_id)) %>%
        dplyr::inner_join(
          dplyr::tbl(glob$pool, "codes") %>%
            dplyr::select(code_id, code_name, code_description, code_color),
          by = "code_id"
        ) %>%
        dplyr::inner_join(dplyr::tbl(glob$pool, "users"), by = "user_id") %>%
        dplyr::collect()

      # Ensure project filtering for all subsequent queries
      loc$segment_categories <- dplyr::tbl(
        glob$pool,
        "categories_codes_map"
      ) %>%
        dplyr::filter(.data$code_id %in% !!loc$segment_df$code_id) %>%
        dplyr::select(code_id, category_id) %>%
        dplyr::inner_join(
          dplyr::tbl(glob$pool, "categories") %>%
            dplyr::filter(
              .data$project_id == !!as.integer(glob$active_project)
            ),
          by = "category_id"
        ) %>%
        dplyr::collect()

      loc$segment_documents <- dplyr::tbl(glob$pool, "documents") %>%
        dplyr::filter(.data$project_id == !!as.integer(glob$active_project)) %>%
        dplyr::filter(.data$doc_id %in% !!loc$segment_df$doc_id) %>%
        dplyr::collect()

      # Get available codes with proper project filtering
      loc$codes_df <- dplyr::tbl(glob$pool, "codes") %>%
        dplyr::filter(.data$project_id == !!as.integer(glob$active_project)) %>%
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
        input$create_new_code,
        glob$user$user_id
      )
      if (!permission_check$valid) {
        rql_message(permission_check$message)
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
            rql_message(paste0("Deleted segment ", segment_id))
            .update_return_values(
              "remove",
              loc$segment_df$segment_id,
              loc$segment_df$code_id
            )
          },
          error = function(e) {
            rql_message(paste0("Failed to delete segment: ", e$message))
          }
        )
        return()
      }

      # Get target code ID
      target_code_id <- .get_target_code_id()
      if (is.null(target_code_id)) {
        return()
      }

      # Check for overlaps and execute action
      tryCatch(
        {
          overlap_info <- check_code_overlaps(
            target_code_id,
            loc$segment_df,
            glob
          )

          if (action == "alter") {
            if (overlap_info$has_overlap) {
              showModal(modalDialog(
                title = "Overlap Detected",
                p(
                  "This code already exists on overlapping text in this document."
                ),
                p(paste0(
                  "The current segment (",
                  segment_id,
                  ") will be removed and the existing overlapping coded segment will be adjusted to cover this text range."
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
                execute_direct_update(
                  target_code_id,
                  segment_id,
                  "Altered",
                  glob$pool
                )
              ) {
                .update_return_values(
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
                  "Added code to",
                  glob
                )
              ) {
                .update_return_values(
                  "add",
                  loc$segment_df$segment_id,
                  target_code_id
                )
              }
            }
          }
        },
        error = function(e) {
          rql_message(paste0("Database error: ", e$message))
        }
      )
    })

    # Modal confirmation handlers
    observeEvent(input$confirm_alter, {
      removeModal()
      target_code_id <- .get_code_id_from_inputs()

      # For ALTER with overlap: remove current segment, then write new segment to extend overlapping
      tryCatch(
        {
          # First remove the current segment (user doesn't want this code here anymore)
          delete_segment_codes_db(
            glob$pool,
            glob$active_project,
            user_id = glob$user$user_id,
            doc_id = loc$segment_df$doc_id,
            segment_id = loc$segment_df$segment_id
          )

          # Then write the target code segment (will extend/merge with existing overlapping segment)
          if (
            execute_write_segment(
              target_code_id,
              loc$segment_df,
              "Altered",
              glob
            )
          ) {
            .update_return_values(
              "alter",
              loc$segment_df$segment_id,
              target_code_id
            )
          }
        },
        error = function(e) {
          rql_message(paste0("Failed to alter segment: ", e$message))
        }
      )
    })

    observeEvent(input$confirm_add, {
      removeModal()
      target_code_id <- .get_code_id_from_inputs()
      if (
        execute_write_segment(
          target_code_id,
          loc$segment_df,
          "Added code to",
          glob
        )
      ) {
        .update_return_values("add", loc$segment_df$segment_id, target_code_id)
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
