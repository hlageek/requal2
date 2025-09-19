#' extensions UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_extensions_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      type = "tabs",
      id = ns("extensions_tabset"),
      tabPanel(
        "Modules manager",
        id = ns("extensions_manager"),
        value = "extensions_manager",
        fluidRow(
          column(
            12,
            h4("Available Extensions"),
            uiOutput(ns("extensions_cards"))
          )
        )
      )
      # Extensions will be dynamically added here via updateTabsetPanel
    )
  )
}

#' extensions Server Functions
#'
#' @noRd
mod_extensions_server <- function(id, api) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    observe(print(api))

    extensions_data <- reactiveValues(
      available = NULL,
      launched = FALSE
    )

    # Auto-discover extensions on module load
    observe({
      if (is.null(extensions_data$available)) {
        extensions_data$available <- tools::dependsOnPkgs("requal")
        print(paste(
          "Auto-discovered extensions:",
          paste(extensions_data$available, collapse = ", ")
        ))
      }
    })

    # Render extension cards
    output$extensions_cards <- renderUI({
      if (is.null(extensions_data$available)) {
        return(div(
          style = "text-align: center; padding: 40px; color: #666;",
          icon(
            "spinner",
            class = "fa-spin",
            style = "font-size: 32px; margin-bottom: 10px;"
          ),
          br(),
          "Loading available extensions..."
        ))
      }

      if (length(extensions_data$available) == 0) {
        return(div(
          style = "text-align: center; padding: 40px; color: #999;",
          icon("info-circle", style = "font-size: 48px; margin-bottom: 10px;"),
          br(),
          "No extensions found that depend on 'requal'"
        ))
      }

      # Create cards for each extension
      cards <- lapply(extensions_data$available, function(ext) {
        div(
          class = "col-md-4 col-sm-6",
          style = "margin-bottom: 15px;",
          div(
            class = "card",
            style = "height: 200px;",
            div(
              class = "card-body d-flex flex-column",
              h5(
                class = "card-title",
                tagList(icon("puzzle-piece"), " ", ext)
              ),
              p(
                class = "card-text flex-grow-1",
                style = "color: #666; font-size: 0.9em;",
                "Extension package for requal"
              ),
              div(
                class = "mt-auto",
                actionButton(
                  ns(paste0("launch_", ext)),
                  "Launch",
                  class = "btn-primary btn-sm",
                  onclick = paste0(
                    "Shiny.setInputValue('",
                    ns("launch_extension"),
                    "', '",
                    ext,
                    "', {priority: 'event'});"
                  )
                )
              )
            )
          )
        )
      })

      div(
        class = "row",
        cards
      )
    })

    # Handle launch extension events
    observeEvent(input$launch_extension, {
      ext <- input$launch_extension
      print(paste("Launching extension:", ext))

      extension_id <- paste0("ext_", ext)

      tryCatch(
        {
          if (requireNamespace(ext, quietly = TRUE)) {
            # Using eval and parse for dynamic namespace calls
            ui_call <- paste0(ext, "::mod_ui")
            ui_func <- eval(parse(text = ui_call))

            if (is.function(ui_func)) {
              # Add the tab to the existing tabsetPanel
              appendTab(
                inputId = "extensions_tabset",
                tab = tabPanel(
                  title = tagList(icon("puzzle-piece"), ext),
                  value = paste0(ext, "_tab"),
                  ui_func(ns(extension_id))
                ),
                session = session
              )

              showNotification(
                paste("Loaded extension:", ext),
                type = "message"
              )

              # Initialize server function
              server_call <- paste0(ext, "::mod_server")
              server_func <- eval(parse(text = server_call))

              if (is.function(server_func)) {
                server_func(extension_id, api = api)
                print(paste("Server function initialized for", ext))
              }
            } else {
              print(paste("mod_ui is not a function in", ext))
              showNotification(
                paste("Error: mod_ui not found in", ext),
                type = "error"
              )
            }
          } else {
            print(paste("Could not load namespace for", ext))
            showNotification(
              paste("Could not load extension:", ext),
              type = "error"
            )
          }
        },
        error = function(e) {
          print(paste("Error loading extension", ext, ":", e$message))
          showNotification(
            paste("Error loading", ext, ":", e$message),
            type = "error"
          )
        }
      )
    })
  })
}
