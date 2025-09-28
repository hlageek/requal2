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

    # Start of locally scoped helper function definitions --------

    # Returns either an ID of a new code or the ID of the selected code
    .get_target_code_id <- function() {
      if (input$create_new_code) {
        # Check for duplicate or missing code name
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
            new_code_id <- create_new_code_on_recode(
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

    .update_return_values <- function(action, segment_id, code_id) {
      return_values$action <- action
      return_values$segment_id <- segment_id
      return_values$code_id <- code_id
      return_values$timestamp <- Sys.time()
    }

    .get_segment_df_on_recode <- function() {
      dplyr::tbl(glob$pool, "segments") %>%
        dplyr::filter(
          .data$segment_id == !!as.integer(loc$current_segment_id)
        ) %>%
        dplyr::inner_join(
          dplyr::tbl(glob$pool, "codes") %>%
            dplyr::select(code_id, code_name, code_description, code_color),
          by = "code_id"
        ) %>%
        dplyr::inner_join(dplyr::tbl(glob$pool, "users"), by = "user_id") %>%
        dplyr::collect()
    }

    .get_codes_df_on_recode <- function() {
      dplyr::tbl(glob$pool, "codes") %>%
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
    }

    .get_categories_df_on_recode <- function() {
      dplyr::tbl(
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
    }

    .get_documents_df_on_recode <- function() {
      dplyr::tbl(glob$pool, "documents") %>%
        dplyr::filter(.data$project_id == !!as.integer(glob$active_project)) %>%
        dplyr::filter(.data$doc_id %in% !!loc$segment_df$doc_id) %>%
        dplyr::collect()
    }

    .render_segment_details_ui <- function() {
      tagList(
        tags$style(HTML(
          mod_analysis_extra_css()
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
    }

    # End of locally scoped helper function definitions --------

    # Load segment data when segment_id changes -------
    observeEvent(segment_id(), {
      req(segment_id())
      # Assign the ID value from the reactive value
      loc$current_segment_id <- segment_id()

      # Reset toolbox state for new segment
      # Otherwise, this is toggled by the hide/reveal UI module
      if (!is.null(loc$toolbox)) {
        loc$toolbox(FALSE)
      }

      # Load segment data
      # It seems rlang bang-bang operators are needed to make this work in deployment
      loc$segment_df <- .get_segment_df_on_recode()

      # Query DB for categories, documents, codes
      # so that we have more details about the segment
      loc$segment_categories <- .get_categories_df_on_recode()
      loc$segment_documents <- .get_documents_df_on_recode()
      loc$codes_df <- .get_codes_df_on_recode()

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

    # Render main UI of the module -------
    # populate with data collected when segment_id changes
    output$segment_details <- renderUI({
      req(loc$segment_df)
      .render_segment_details_ui()
    })

    # Toggle quoted segment visibility -----
    # uses the same reactive value as the hide/reveal toggle module
    observeEvent(loc$toolbox(), {
      if (loc$toolbox()) {
        shinyjs::show("quoted_segment")
      } else {
        shinyjs::hide("quoted_segment")
      }
    })

    # Close button handler -----
    observeEvent(input$close_btn, {
      loc$toolbox(FALSE)
      removeUI(paste0(".", parent_class), multiple = TRUE)
    })

    # Dynamic button UI for recode actions -----
    output$recode_button_ui <- renderUI({
      req(input$recode_action)
      req(isTruthy(input$recode_action))

      button_text <- switch(
        input$recode_action,
        "alter" = "Alter code",
        "add" = "Add code",
        "split" = "Split code",
        "strip" = "Strip code"
      )

      button_class <- if (input$recode_action == "strip") {
        "btn-danger"
      } else {
        "btn-warning"
      }

      actionButton(ns("recode_btn"), button_text, class = button_class)
    })

    # Main recode button handler ----
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

      # Strip code logic ------
      if (action == "strip") {
        # Stripping a code from segment is the same as deleting it
        tryCatch(
          {
            delete_segment_codes_db(
              glob$pool,
              glob$active_project,
              user_id = glob$user$user_id,
              doc_id = loc$segment_df$doc_id,
              segment_id = loc$segment_df$segment_id
            )
            rql_message(paste0("Stripped code from segment ", segment_id))
            .update_return_values(
              "strip",
              loc$segment_df$segment_id,
              loc$segment_df$code_id
            )
          },
          error = function(e) {
            rql_message(paste0(
              "Failed to strip code from segment: ",
              e$message
            ))
          }
        )
        return()
      }

      # Alter, Add, Split code logic ------
      # Get target code ID ------
      # if we are recoding, we need to know what is the new code ID
      loc$target_code_id <- .get_target_code_id()
      if (is.null(loc$target_code_id)) {
        return()
      }

      # Check for overlaps
      # if we find overlaps, we need to prompt user because the recode action will be more complex
      tryCatch(
        {
          overlap_info <- check_code_overlaps_on_recode(
            loc$target_code_id,
            loc$segment_df,
            glob
          )
          # Alter code ------
          if (action == "alter") {
            if (overlap_info$has_overlap) {
              showModal(modalDialog(
                title = "Overlap Detected",
                p(
                  "This code already exists in the range of the current segment."
                ),
                p(paste0(
                  "The current segment (",
                  segment_id,
                  ") will be removed and the overlapping coded segment will be adjusted to cover the current segment's text range."
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
              # If no overlap, just write to DB
              if (
                execute_direct_update(
                  loc$target_code_id,
                  segment_id,
                  "Altered",
                  glob$pool
                )
              ) {
                .update_return_values(
                  "alter",
                  loc$segment_df$segment_id,
                  loc$target_code_id
                )
              }
            }
            # Add code ------
          } else if (action == "add") {
            if (overlap_info$has_overlap) {
              showModal(modalDialog(
                title = "Overlap Detected",
                p(
                  "This code already exists in the range of the current segment."
                ),
                p("Proceeding will merge or extend the pre-existing segments."),
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
              # If no overlap, just write to DB
              if (
                execute_write_segment(
                  loc$target_code_id,
                  loc$segment_df,
                  "Added code to",
                  glob
                )
              ) {
                .update_return_values(
                  "add",
                  loc$segment_df$segment_id,
                  loc$target_code_id
                )
              }
            }
          }
          # TODO: Split code logic with else if (action == "split")
        },
        error = function(e) {
          rql_message(paste0("Database error: ", e$message))
        }
      )
    })

    # Modal confirmation handlers ------
    # here we handle the more complicated cases upon user confirmation
    observeEvent(input$confirm_alter, {
      removeModal()

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
              loc$target_code_id,
              loc$segment_df,
              "Altered",
              glob
            )
          ) {
            .update_return_values(
              "alter",
              loc$segment_df$segment_id,
              loc$target_code_id
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
      if (
        execute_write_segment(
          loc$target_code_id,
          loc$segment_df,
          "Added code to",
          glob
        )
      ) {
        .update_return_values(
          "add",
          loc$segment_df$segment_id,
          loc$target_code_id
        )
      }
    })

    # Context window adjustment -----
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
