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
    actionButton(
      ns("launch_extensions"),
      "Launch Extensions",
      class = "btn-primary"
    ),
    br(),
    br(),
    uiOutput(ns("extensions"))
  )
}

#' extensions Server Functions
#'
#' @noRd
mod_extensions_server <- function(id, glob) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive value to store extensions
    extensions_data <- reactiveValues(
      available = NULL,
      launched = FALSE
    )

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
        return()
      }

      extensions_data$launched <- TRUE
    })

    # Render UI when extensions are launched
    output$extensions <- renderUI({
      if (!extensions_data$launched || is.null(extensions_data$available)) {
        return(p("Click 'Launch Extensions' to load available extensions"))
      }

      if (length(extensions_data$available) == 0) {
        return(p("No extensions found that depend on 'requal'"))
      }

      print("Rendering extension UIs...")

      # Create UI for each extension
      ui_elements <- list()

      for (ext in extensions_data$available) {
        print(paste("Processing extension:", ext))

        tryCatch(
          {
            if (requireNamespace(ext, quietly = TRUE)) {
              # Using eval and parse for dynamic namespace calls
              ui_call <- paste0(ext, "::mod_ui")
              print(paste("Calling:", ui_call))

              ui_func <- eval(parse(text = ui_call))

              if (is.function(ui_func)) {
                extension_id <- paste0("ext_", ext)
                print(paste("Creating UI for extension with ID:", extension_id))

                ui_elements[[ext]] <- div(
                  h4(paste("Extension:", ext)),
                  ui_func(ns(extension_id))
                )
              } else {
                print(paste("mod_ui is not a function in", ext))
              }
            } else {
              print(paste("Could not load namespace for", ext))
            }
          },
          error = function(e) {
            print(paste("Error loading UI for", ext, ":", e$message))
            ui_elements[[ext]] <- div(
              h4(paste("Extension:", ext)),
              p(paste("Error loading:", e$message), style = "color: red;")
            )
          }
        )
      }

      if (length(ui_elements) > 0) {
        print(paste("Created", length(ui_elements), "UI elements"))
        do.call(tagList, ui_elements)
      } else {
        p("No valid extension UIs found")
      }
    })

    # Initialize server functions when extensions are launched
    observeEvent(extensions_data$launched, {
      if (!extensions_data$launched || is.null(extensions_data$available)) {
        return()
      }

      print("Initializing extension servers...")

      for (ext in extensions_data$available) {
        print(paste("Processing server for extension:", ext))

        tryCatch(
          {
            if (requireNamespace(ext, quietly = TRUE)) {
              server_call <- paste0(ext, "::mod_server")
              print(paste("Calling:", server_call))

              server_func <- eval(parse(text = server_call))

              if (is.function(server_func)) {
                extension_id <- paste0("ext_", ext)
                print(paste(
                  "Calling server function for extension with ID:",
                  extension_id
                ))

                # Call the server function with proper namespaced ID
                server_func(extension_id, glob)
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
