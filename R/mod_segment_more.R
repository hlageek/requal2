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
  div(
    class = "segment_more", # Note: changed to match your removeUI selector
    tags$button(
      "×",
      onclick = "$(this).closest('.segment_more_instance').remove();",
      style = "background: none; border: none; font-size: 18px; cursor: pointer; padding: 0; width: 20px; height: 20px;"
    ),
    uiOutput(ns("segment_details"))
  )
}

#' segment_more Server Functions
#'
#' @noRd
mod_segment_more_server <- function(id, segment_id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    loc <- reactiveValues()
    loc$segment_id <- segment_id()

    output$segment_details <- renderUI({
      div(class = "segment_details", p(paste("test", loc$segment_id)))
    })
  })
}
