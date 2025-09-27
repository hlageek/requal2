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

    # Initialize reactiveValues to store data
    loc <- reactiveValues()
    loc$toolbox <- mod_rql_hidden_ui_server("rql_hidden_segment_tools")

    # Use observeEvent to update loc when segment_id changes
    observeEvent(segment_id(), {
      req(segment_id()) # Ensure segment_id is available
      current_segment_id <- segment_id() # Convert reactiveVal to normal variable

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
        dplyr::filter(.data$code_id %in% loc$segment_df$code_id) %>%
        dplyr::select(code_id, category_id) %>%
        dplyr::inner_join(
          dplyr::tbl(glob$pool, "categories"),
          by = "category_id"
        ) %>%
        dplyr::collect()

      loc$segment_documents <- dplyr::tbl(glob$pool, "documents") %>%
        dplyr::filter(.data$doc_id %in% loc$segment_df$doc_id) %>%
        dplyr::collect()

      # Get all available codes (excluding current one)
      loc$codes_df <- dplyr::tbl(glob$pool, "codes") %>%
        dplyr::filter(.data$code_id != loc$segment_df$code_id) %>%
        dplyr::select(
          code_id,
          code_name,
          code_description,
          code_color,
          user_id
        ) %>%
        dplyr::collect()

      # Filter by permission to view other codes
      if (
        !is.null(glob$user$data) &&
          glob$user$data$codebook_other_view != 1
      ) {
        loc$codes_df <- loc$codes_df %>%
          dplyr::filter(user_id == !!glob$user$user_id)
      }

      # Create choice list for select input
      loc$code_choices <- stats::setNames(
        loc$codes_df$code_id,
        loc$codes_df$code_name
      )
    })

    output$segment_details <- renderUI({
      req(loc$segment_df) # Ensure data is available

      tagList(
        tags$style(HTML(
          "
      .segment_container {
        display: flex;
        flex-direction: row-reverse; /* Info box on the right */
        width: 100%;
        max-width: 1000px; /* Overall container width */
      }
      .info_box {
        width: 300px;
        min-width: 20vw;
        max-width: 40vw;
        padding-left: 30px;
        scrollbar-width: thin;
      }
      .quoted_segment {
        flex: 1; /* Allow to grow */
        padding: 10px; /* Space between columns */
        min-width: 40vw; /* Minimum width for expansion */
        max-width: 60vw; /* Maximum width for expansion */
        max-height: 80vh;
        background-color: white;
        text-align: left;
        overflow-y: scroll;
        scrollbar-width: thin;
      }
      .segment_outline {
        white-space: pre-wrap;
        background-color: #FFF8DC; /* Background for readability */
        padding: 10px; /* Padding for content */
        border-radius: 5px; 
        box-sizing: border-box; /* Include padding in width */
        outline: dashed 2px #FF6347; /* Optional: visual outline */
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

    observeEvent(
      loc$toolbox(),
      {
        # Toggle visibility of the quoted segment
        if (loc$toolbox()) {
          shinyjs::show("quoted_segment")
        } else {
          shinyjs::hide("quoted_segment")
        }
      }
    )

    observeEvent(input$close_btn, {
      loc$toolbox(FALSE) # Reset toolbox state before closing
      removeUI(paste0(".", parent_class), multiple = TRUE)
    })

    # Dynamic button UI
    output$recode_button_ui <- renderUI({
      req(input$recode_action)
      req(isTruthy(input$recode_action))

      button_text <- switch(
        input$recode_action,
        "alter" = "Alter Code",
        "add" = "Add Code",
        "split" = "Split Code",
        "remove" = "Delete Segment",
        ""
      )

      button_class <- if (input$recode_action == "remove") {
        "btn-danger"
      } else {
        "btn-warning"
      }

      actionButton(
        ns("recode_btn"),
        button_text,
        class = button_class
      )
    })

    # Handle recode button click
    observeEvent(input$recode_btn, {
      req(loc$segment_df)

      action <- input$recode_action
      segment_id <- loc$segment_df$segment_id
      current_code <- loc$segment_df$code_name

      if (action == "remove") {
        showNotification(
          paste0("Would DELETE segment ", segment_id, " (", current_code, ")"),
          type = "error",
          duration = 3
        )
      } else {
        if (input$create_new_code) {
          req(input$new_code_name)
          if (input$new_code_name == "") {
            showNotification("Please enter a code name", type = "error")
            return()
          }

          new_code_info <- paste0(
            input$new_code_name,
            if (input$new_code_description != "") {
              paste0(" (", input$new_code_description, ")")
            } else {
              ""
            },
            " [",
            input$new_code_color,
            "]"
          )

          message <- switch(
            action,
            "alter" = paste0(
              "Would ALTER segment ",
              segment_id,
              " from '",
              current_code,
              "' to NEW CODE: ",
              new_code_info
            ),
            "add" = paste0(
              "Would ADD NEW CODE: ",
              new_code_info,
              " to segment ",
              segment_id,
              " (keeping '",
              current_code,
              "')"
            ),
            "split" = paste0(
              "Would SPLIT segment ",
              segment_id,
              " with NEW CODE: ",
              new_code_info,
              if (input$keep_parent_on_split) {
                " (keeping parent)"
              } else {
                " (removing parent)"
              }
            )
          )
        } else {
          req(input$recode_select)
          if (input$recode_select == "") {
            showNotification("Please select a code", type = "error")
            return()
          }

          selected_code_name <- names(loc$code_choices)[
            loc$code_choices == input$recode_select
          ]

          message <- switch(
            action,
            "alter" = paste0(
              "Would ALTER segment ",
              segment_id,
              " from '",
              current_code,
              "' to '",
              selected_code_name,
              "'"
            ),
            "add" = paste0(
              "Would ADD '",
              selected_code_name,
              "' to segment ",
              segment_id,
              " (keeping '",
              current_code,
              "')"
            ),
            "split" = paste0(
              "Would SPLIT segment ",
              segment_id,
              " with '",
              selected_code_name,
              "'",
              if (input$keep_parent_on_split) {
                " (keeping parent)"
              } else {
                " (removing parent)"
              }
            )
          )
        }

        showNotification(message, type = "message", duration = 4)
      }
    })

    observeEvent(input$adjust_context_window, {
      req(loc$segment_df) # Ensure data is available

      # Get the document text
      text <- dplyr::tbl(glob$pool, "documents") %>%
        dplyr::filter(.data$doc_id == loc$segment_df$doc_id) %>%
        dplyr::pull(doc_text)

      # Calculate safe indices for pre and post text
      text_length <- nchar(text)
      segment_start <- loc$segment_df$segment_start
      segment_end <- loc$segment_df$segment_end

      # Constrain pre_text indices (adjusted for your original logic)
      pre_start <- max(1, segment_start - (input$adjust_context_window + 1))
      pre_end <- max(1, segment_start - 1)

      # Constrain post_text indices (adjusted for your original logic)
      post_start <- min(text_length, segment_end + 1)
      post_end <- min(
        text_length,
        segment_end + (input$adjust_context_window - 1)
      )

      # Extract pre and post context text with safe indices
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

      # Update the HTML content
      shinyjs::html("pre_text", pre_text)
      shinyjs::html("post_text", post_text)

      # Conditionally add or remove the class
      if (input$adjust_context_window > 0) {
        shinyjs::addClass("segment_quote", "segment_outline")
      } else {
        shinyjs::removeClass("segment_quote", "segment_outline")
      }
    })
  })
}

# Helper functions for this module -----------------------
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
      icon(
        "tag",
        style = paste0("color:", segment_df$code_color, ";")
      )
    ),
    div(
      style = if (nrow(segment_categories) > 0) {
        "margin-top: 5px;"
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
        "Split" = "split",
        "Remove" = "remove"
      ),
      selected = character(0),
      inline = TRUE
    ),

    conditionalPanel(
      condition = "input.recode_action == 'alter' || input.recode_action == 'add' || input.recode_action == 'split'",
      ns = ns,

      wellPanel(
        checkboxInput(
          ns("create_new_code"),
          "Create new code",
          value = FALSE
        ),

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
      ),

      conditionalPanel(
        condition = "input.recode_action == 'split'",
        ns = ns,
        div(
          style = "margin-top: 10px; padding: 8px; background-color: #f8f9fa; border-radius: 4px;",
          strong("Split options:"),
          checkboxInput(
            ns("keep_parent_on_split"),
            "Keep parent code on segment",
            value = TRUE
          ),
          p(
            style = "font-size: 0.9em; color: #666; margin-top: 5px;",
            "Note: Only child codes of the current code will be available for selection."
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
      div(
        style = "margin-top: 15px;",
        uiOutput(ns("recode_button_ui"))
      )
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
