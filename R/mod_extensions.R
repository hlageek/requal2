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
        actionButton(
          ns("launch_extensions"),
          "Launch",
          class = "btn-primary"
        ),
        br(),
        br(),
        # Add a placeholder for status
        uiOutput(ns("status_message"))
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

    # Status message output
    output$status_message <- renderUI({
      if (!extensions_data$launched) {
        p("Click 'Launch' to load available extensions.")
      } else if (length(extensions_data$available) == 0) {
        p(
          "No extensions found that depend on 'requal'",
          style = "color: orange;"
        )
      } else {
        p(
          paste("Loaded", length(extensions_data$available), "extension(s)"),
          style = "color: green;"
        )
      }
    })

    # Get available extensions when button is clicked
    observeEvent(input$launch_extensions, {
      print("Launch Extensions button clicked")

      # Get available extensions
      extensions_data$available <- tools::dependsOnPkgs("requal")
      print(paste(
        "Found extensions:",
        paste(extensions_data$available, collapse = ", ")
      ))

      if (length(extensions_data$available) == 0) {
        showNotification(
          "No extensions found that depend on 'requal'",
          type = "warning"
        )
        extensions_data$launched <- TRUE
        return()
      }

      # Add tabs for each extension
      for (ext in extensions_data$available) {
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
                    h4(packageDescription(ext)$Title),
                    ui_func(ns(extension_id))
                  ),
                  session = session
                )

                showNotification(
                  paste("Loaded extension:", ext),
                  type = "message"
                )
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
            print(paste("Error loading UI for", ext, ":", e$message))
            showNotification(
              paste("Error loading", ext, ":", e$message),
              type = "error"
            )
          }
        )
      }

      extensions_data$launched <- TRUE
    })

    # Initialize server functions when extensions are launched
    observeEvent(extensions_data$launched, {
      req(extensions_data$available)
      req(length(extensions_data$available) > 0)

      for (ext in extensions_data$available) {
        tryCatch(
          {
            if (requireNamespace(ext, quietly = TRUE)) {
              server_call <- paste0(ext, "::mod_server")
              server_func <- eval(parse(text = server_call))

              if (is.function(server_func)) {
                extension_id <- paste0("ext_", ext)
                # Call the server function with proper namespaced ID
                server_func(
                  extension_id,
                  api = api
                )
                print(paste("Server function initialized for", ext))
              } else {
                print(paste("mod_server is not a function in", ext))
              }
            } else {
              print(paste("Could not load namespace for", ext))
            }
          },
          error = function(e) {
            print(paste("Error loading server for", ext, ":", e$message))
          }
        )
      }
    })
  })
}
