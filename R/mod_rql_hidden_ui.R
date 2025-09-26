#' rql_hidden_ui UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_rql_hidden_ui_ui <- function(id, title, hidden_tags, ...) {
  ns <- NS(id)
  tagList(
    shinyjs::hidden(div(id = ns("toolbox"), hidden_tags)),
    actionButton(
      ns("btn_toolbox"),
      shiny::icon("angle-double-down"),
      title = title,
      class = "toolbox-button"
    )
  )
}

#' rql_hidden_ui Server Functions
#'
#' @noRd
mod_rql_hidden_ui_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Create a reactive value to track the visibility state
    toolbox_visible <- reactiveVal(FALSE)

    # Observe advanced toolbox display
    observeEvent(input$btn_toolbox, {
      golem::invoke_js("toggle_icon_angles", list(id = ns("btn_toolbox")))
      shinyjs::toggle(
        id = "toolbox",
        anim = TRUE,
        animType = "slide"
      )

      # Toggle the visibility state
      toolbox_visible(!toolbox_visible())
    })

    # Return the reactive value
    return(toolbox_visible)
  })
}
