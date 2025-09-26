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

    output$segment_details <- renderUI({
      req(segment_id()) # Ensure segment_id is available
      current_segment_id <- segment_id() # Convert reactiveVal to normal variable

      segment_df <- dplyr::tbl(glob$pool, "segments") %>%
        dplyr::filter(.data$segment_id == as.integer(current_segment_id)) %>%
        dplyr::inner_join(
          dplyr::tbl(glob$pool, "codes") %>%
            dplyr::select(code_id, code_name, code_description, code_color),
          by = "code_id"
        ) %>%
        dplyr::inner_join(dplyr::tbl(glob$pool, "users"), by = "user_id") %>%
        dplyr::collect()

      segment_categories <- dplyr::tbl(glob$pool, "categories_codes_map") %>%
        dplyr::filter(.data$code_id %in% segment_df$code_id) %>%
        dplyr::select(code_id, category_id) %>%
        dplyr::inner_join(
          dplyr::tbl(glob$pool, "categories"),
          by = "category_id"
        ) %>%
        dplyr::collect()

      segment_documents <- dplyr::tbl(glob$pool, "documents") %>%
        dplyr::filter(.data$doc_id %in% segment_df$doc_id) %>%
        dplyr::collect()

      # Get all available codes (excluding current one)
      codes_df <- dplyr::tbl(glob$pool, "codes") %>%
        dplyr::filter(.data$code_id != segment_df$code_id) %>%
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
        codes_df <- codes_df %>%
          dplyr::filter(user_id == !!glob$user$user_id)
      }

      # Create choice list for select input
      code_choices <- stats::setNames(
        codes_df$code_id,
        codes_df$code_name
      )

      tagList(
        fluidRow(
          column(
            width = 8,
            style = "text-align: left;",
            p(segment_df$segment_text, style = "white-space: pre-wrap;")
          ),
          column(
            width = 4,
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
              segment_df,
              segment_categories,
              segment_documents
            ),
            hr(),
            tabsetPanel(
              tabPanel("Recode", recode_block(ns, code_choices)),
              tabPanel("Adjust", adjust_block(ns))
            )
          )
        )
      )
    })

    observeEvent(input$close_btn, {
      removeUI(paste0(".", parent_class), multiple = TRUE)
    })

    observeEvent(input$adjust_context_window, {
      pre_text <- dplyr::tbl(glob$pool, "documents") %>%
        dplyr::filter(.data$doc_id == segment_df$doc_id) %>%
        dplyr::pull(doc_text)
      print(pre_text)
    })
  })
}

# Locally scoped helper functions -------------------
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
        "Add" = "add"
      ),
      selected = "add",
      inline = TRUE
    ),
    selectInput(
      ns("recode_select"),
      "Select new code:",
      choices = c("", code_choices),
      selected = ""
    ),
    actionButton(
      ns("recode_btn"),
      "Recode",
      class = "btn-warning"
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
