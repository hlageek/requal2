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
mod_segment_more_server <- function(id, segment_id, parent_class) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$segment_details <- renderUI({
      req(segment_id())
      tagList(
        actionButton(
          ns("close_btn"),
          "x",
          style = "background: none; border: none; font-size: 18px; cursor: pointer; padding: 0; width: 20px; height: 20px; margin-top: -20px;"
        ),
        p(paste("test", segment_id()))
      )
    })

    observeEvent(input$close_btn, {
      removeUI(paste0(".", parent_class), multiple = TRUE)
    })
  })
}
