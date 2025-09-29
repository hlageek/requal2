#' analysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_analysis_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      class = "module_tools",
      mod_rql_button_ui(
        ns("filter_ui"),
        label = "Filter segments",
        icon = "filter"
      ),
      mod_rql_button_ui(
        ns("sort_ui"),
        label = "Sort segments",
        icon = "sort"
      ),
      mod_rql_button_ui(
        ns("download_ui"),
        label = "Download segments",
        icon = "download"
      )
    ),
    fluidRow(
      class = "module_content",
      uiOutput(ns("segments")) %>%
        tagAppendAttributes(
          class = "scrollable80",
          style = "padding-right: 50px"
        )
    )
  )
}

#' analysis Server Functions
#'
#' @noRd
mod_analysis_server <- function(id, glob) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    loc <- reactiveValues()

    if (golem::get_golem_options(which = "mode") == "server") {
      observeEvent(input$user_filter, {
        loc$user_filter <- input$user_filter
      })
    } else {
      # local version does not have user filter UI
      # so we set the user filter to 1
      # to always select the user in desktop version
      loc$user_filter <- 1 # for local version
    }

    # UI ----

    #--- Filter UI --------------
    mod_rql_button_server(
      id = "filter_ui",
      custom_title = "Filter segments",
      custom_tagList = tagList(
        rql_picker_UI(
          ns("code_filter"),
          label = "Filter by code",
          choices = "",
          multiple = TRUE,
          none = "Off"
        ),
        rql_picker_UI(
          ns("category_filter"),
          label = "Filter by category",
          choices = "",
          multiple = TRUE,
          none = "Off"
        ),
        rql_picker_UI(
          ns("document_filter"),
          label = "Filter by document",
          choices = "",
          multiple = TRUE,
          none = "Off"
        ),
        if (golem::get_golem_options(which = "mode") == "server") {
          rql_picker_UI(
            ns("user_filter"),
            label = "Filter by user",
            choices = "",
            multiple = TRUE,
            none = "Off"
          )
        }
      ),
      glob
    )

    #--- Sort UI --------------
    mod_rql_button_server(
      id = "sort_ui",
      custom_title = "Sort segments",
      custom_tagList = tagList(
        selectInput(
          ns("sort_by"),
          label = "Sort by",
          choices = list(
            "Document" = "document",
            "Code" = "code",
            "Segment" = "segment"
          ),
          selected = "segment"
        ),
        selectInput(
          ns("sort_principle"),
          label = "Sort principle",
          choices = list(
            "Recency (newest first)" = "recency_desc",
            "Recency (oldest first)" = "recency_asc",
            "Alphanumeric (A-Z)" = "alpha_asc",
            "Alphanumeric (Z-A)" = "alpha_desc"
          ),
          selected = "recency_desc"
        ),
        conditionalPanel(
          condition = "input.sort_by != 'document'",
          ns = ns,
          checkboxInput(
            ns("group_by_document"),
            label = "Group by document first",
            value = TRUE
          )
        ),
        tags$small(
          class = "text-muted",
          HTML(
            "Alphanumeric uses names/text, Recency uses creation IDs.<br/>
               When grouping by document, segments are automatically sorted by position within each document."
          )
        )
      ),
      glob
    )

    # Download UI ----
    mod_rql_button_server(
      id = "download_ui",
      custom_title = "Download segments",
      custom_tagList = tagList(
        tags$p(mod_download_csv_ui("download_csv_ui_1", "download_analysis")),
        tags$p(mod_download_html_ui("download_html_ui_1"))
      )
    )

    # Filters ----

    observeEvent(glob$codebook, {
      shinyWidgets::updatePickerInput(
        session = session,
        "code_filter",
        choices = stats::setNames(
          glob$codebook$code_id,
          glob$codebook$code_name
        ),
        selected = glob$codebook$code_id
      )
    })

    observeEvent(glob$documents, {
      shinyWidgets::updatePickerInput(
        session = session,
        "document_filter",
        choices = glob$documents,
        selected = glob$documents
      )
    })
    observeEvent(glob$category, {
      shinyWidgets::updatePickerInput(
        session = session,
        "category_filter",
        choices = glob$category,
        selected = ""
      )
    })

    observeEvent(glob$users_observer, {
      req(golem::get_golem_options(which = "mode") == "server")
      shinyWidgets::updatePickerInput(
        session = session,
        "user_filter",
        choices = get_user_permissions(
          glob$pool,
          glob$active_project
        ) %>%
          dplyr::pull(user_id, name = "user_name"),
        selected = get_user_permissions(
          glob$pool,
          glob$active_project
        ) %>%
          dplyr::pull(user_id)
      )
    })

    # Set default sort options
    observeEvent(
      once = TRUE,
      ignoreNULL = FALSE,
      {
        TRUE
      },
      {
        # Defaults are already set in selectInput, but we can update if needed
      }
    )

    # Update sort principle options based on what field is being sorted
    observeEvent(input$sort_by, {
      req(input$sort_by)

      if (input$sort_by == "segment") {
        # For segment sorting, add segment position options
        choices <- list(
          "Recency (newest first)" = "recency_desc",
          "Recency (oldest first)" = "recency_asc",
          "Alphanumeric (A-Z)" = "alpha_asc",
          "Alphanumeric (Z-A)" = "alpha_desc",
          "Position (start to end)" = "position_asc",
          "Position (end to start)" = "position_desc"
        )
      } else {
        # For document and code sorting, standard options
        choices <- list(
          "Recency (newest first)" = "recency_desc",
          "Recency (oldest first)" = "recency_asc",
          "Alphanumeric (A-Z)" = "alpha_asc",
          "Alphanumeric (Z-A)" = "alpha_desc"
        )
      }

      # Preserve current selection if it's still valid
      current_selection <- input$sort_principle
      if (
        is.null(current_selection) || !current_selection %in% unlist(choices)
      ) {
        current_selection <- if (input$sort_by == "segment") {
          "recency_desc"
        } else {
          "alpha_asc"
        }
      }

      updateSelectInput(
        session = session,
        "sort_principle",
        choices = choices,
        selected = current_selection
      )
    })

    # Segments to display and filter ----
    observeEvent(
      c(
        input$code_filter,
        input$category_filter,
        input$document_filter,
        loc$user_filter,
        glob$segments_observer,
        glob$codebook,
        glob$category,
        glob$documents,
        glob$active_project
      ),
      {
        loc$temp_df <- load_segments_analysis(
          pool = glob$pool,
          active_project = as.integer(glob$active_project),
          selected_codes = as.integer(input$code_filter),
          selected_categories = as.integer(input$category_filter),
          selected_docs = as.integer(input$document_filter),
          selected_users = as.integer(loc$user_filter)
        )

        if (nrow(loc$temp_df) > 0) {
          # handle view permissions
          if (glob$user$data$analysis_other_view != 1) {
            loc$temp_df <- loc$temp_df %>%
              dplyr::filter(user_id == glob$user$user_id)
          }

          loc$segments_df_unsorted <- loc$temp_df %>%
            dplyr::left_join(glob$codebook, by = "code_id") %>%
            dplyr::left_join(
              tibble::enframe(
                glob$documents,
                name = "doc_name",
                value = "doc_id"
              ),
              by = "doc_id"
            )
        } else {
          loc$segments_df_unsorted <- as.data.frame(NULL)
        }
      }
    )

    # Apply sorting ----
    observeEvent(
      c(
        loc$segments_df_unsorted,
        input$sort_by,
        input$sort_principle,
        input$group_by_document
      ),
      {
        if (
          is.null(loc$segments_df_unsorted) ||
            nrow(loc$segments_df_unsorted) == 0
        ) {
          loc$segments_df <- loc$segments_df_unsorted
          return()
        }

        # Get sort parameters with defaults
        sort_by <- if (is.null(input$sort_by)) "segment" else input$sort_by
        sort_principle <- if (is.null(input$sort_principle)) {
          "recency_desc"
        } else {
          input$sort_principle
        }
        group_by_doc <- if (is.null(input$group_by_document)) {
          TRUE
        } else {
          input$group_by_document
        }

        # Determine actual sort field based on sort_by and sort_principle
        if (sort_principle %in% c("position_asc", "position_desc")) {
          # Position sorting: use segment_start
          actual_sort_field <- "segment_start"
        } else if (sort_principle %in% c("recency_asc", "recency_desc")) {
          # Recency: use IDs
          if (sort_by == "document") {
            actual_sort_field <- "doc_id"
          } else if (sort_by == "code") {
            actual_sort_field <- "code_id"
          } else {
            # segment
            actual_sort_field <- "segment_id"
          }
        } else {
          # Alphanumeric: use names/text with cleaning
          if (sort_by == "document") {
            # Clean document names
            loc$segments_df_unsorted <- loc$segments_df_unsorted %>%
              dplyr::mutate(
                doc_name_clean = stringr::str_to_lower(trimws(doc_name))
              )
            actual_sort_field <- "doc_name_clean"
          } else if (sort_by == "code") {
            # Clean code names
            loc$segments_df_unsorted <- loc$segments_df_unsorted %>%
              dplyr::mutate(
                code_name_clean = stringr::str_to_lower(trimws(code_name))
              )
            actual_sort_field <- "code_name_clean"
          } else {
            # segment
            # Clean segment text for better sorting
            loc$segments_df_unsorted <- loc$segments_df_unsorted %>%
              dplyr::mutate(
                segment_text_clean = stringr::str_sub(
                  trimws(segment_text),
                  1,
                  100
                ) %>%
                  stringr::str_to_lower()
              )
            actual_sort_field <- "segment_text_clean"
          }
        }

        use_desc <- sort_principle %in%
          c("recency_desc", "alpha_desc", "position_desc")

        # Apply sorting logic
        if (sort_by == "document" || !group_by_doc) {
          # No document grouping - sort globally
          if (use_desc) {
            loc$segments_df <- loc$segments_df_unsorted %>%
              dplyr::arrange(dplyr::desc(!!dplyr::sym(actual_sort_field)))
          } else {
            loc$segments_df <- loc$segments_df_unsorted %>%
              dplyr::arrange(!!dplyr::sym(actual_sort_field))
          }
        } else {
          # Group by document first, then apply sort within groups
          # Document groups inherit the sorting principle (but always use position within docs)
          doc_sort_field <- if (
            sort_principle %in% c("recency_asc", "recency_desc")
          ) {
            "doc_id"
          } else {
            "doc_name_clean"
          }

          # When grouping by document, automatically add segment position as secondary sort
          if (use_desc) {
            loc$segments_df <- loc$segments_df_unsorted %>%
              dplyr::arrange(
                dplyr::desc(!!dplyr::sym(doc_sort_field)),
                dplyr::desc(!!dplyr::sym(actual_sort_field)),
                segment_start
              ) # Always ascending for natural document order
          } else {
            loc$segments_df <- loc$segments_df_unsorted %>%
              dplyr::arrange(
                !!dplyr::sym(doc_sort_field),
                !!dplyr::sym(actual_sort_field),
                segment_start
              ) # Always ascending for natural document order
          }
        }
      }
    )

    observeEvent(loc$segments_df, {
      if (nrow(loc$segments_df) > 0) {
        loc$segments_taglist <- purrr::pmap(
          list(
            loc$segments_df$segment_id,
            loc$segments_df$segment_text,
            loc$segments_df$doc_id,
            loc$segments_df$doc_name,
            loc$segments_df$code_name,
            loc$segments_df$code_color
          ),
          ~ format_segments(
            ns = ns,
            segment_id = ..1,
            segment_text = ..2,
            segment_document_id = ..3,
            segment_document_name = ..4,
            segment_code = ..5,
            segment_color = ..6
          )
        )
      }
    })

    output$segments <- renderUI({
      if (nrow(req(loc$segments_df)) > 0) {
        loc$segments_taglist
      } else {
        "No coded segments detected."
      }
    })

    # Segments extras menu and recoding --------------------

    # Create reactive value outside observeEvent
    segment_id_rv <- reactiveVal(NULL)
    # Define class for removal of UI
    segment_more_class <- ns("segment_more")
    # Create server module outside observeEvent - pass the reactive value
    inner_values <- mod_analysis_extra_server(
      "segment_more",
      glob = glob,
      segment_id = segment_id_rv,
      parent_class = segment_more_class
    )
    observeEvent(inner_values$timestamp, {
      # Code to execute when action changes
      glob$codebook_observer <- glob$codebook_observer + 1
      glob$segments_observer <- glob$segments_observer + 1
    })
    observeEvent(input$segments_more_btn, {
      # Remove any existing segment_more UI
      removeUI(paste0(".", segment_more_class), multiple = TRUE)

      # Insert new module UI where it was called from
      insertUI(
        selector = paste0(
          "#",
          ns("segments_more_btn"),
          input$segments_more_btn
        ),
        where = "afterEnd",
        ui = div(
          class = paste(segment_more_class, "segment_more"), # see .segment_more in custom.css
          mod_analysis_extra_ui(ns("segment_more"))
        )
      )

      segment_id_rv(input$segments_more_btn) # Triggers new render in the server
    })

    # for download modules ------------------
    observeEvent(
      {
        loc$segments_df
        loc$segments_taglist
        glob$active_project
      },
      {
        if (!is.null(loc$segments_df) && nrow(loc$segments_df) > 0) {
          shinyjs::enable("download_csv_ui_1-download", asis = TRUE)
          shinyjs::enable("download_html_ui_1-report", asis = TRUE)
          glob$segments_df <- loc$segments_df %>%
            dplyr::select(
              doc_name,
              doc_id,
              segment_start,
              segment_end,
              code_name,
              code_id,
              segment_text,
              user_name
            )
          glob$segments_taglist <- loc$segments_taglist
        } else {
          shinyjs::disable("download_csv_ui_1-download", asis = TRUE)
          shinyjs::disable("download_html_ui_1-report", asis = TRUE)
        }
      }
    )
  })
}
