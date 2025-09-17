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
      "Launch",
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

    # remotes::install_local("/Volumes/CardDisk/repos_backup/rql.wordcloud_0.0.0.9000.tar.gz")

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
      if (length(extensions_data$available) == 0) {
        return(p("No extension modules found."))
      }

      # Create UI for each extension
      ui_elements <- list()

      for (ext in extensions_data$available) {
        extension_id <- paste0("ext_", ext)

        tryCatch(
          {
            if (requireNamespace(ext, quietly = TRUE)) {
              # Using eval and parse for dynamic namespace calls
              ui_call <- paste0(ext, "::mod_ui")
              ui_func <- eval(parse(text = ui_call))

              if (is.function(ui_func)) {
                ui_elements[[ext]] <- div(
                  h4(paste("Extension:", ext)),
                  ui_func(ns(extension_id))
                )
                showNotification(
                  paste("Loading:", ext)
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
        do.call(tagList, ui_elements)
      } else {
        p("No valid extension UIs found")
      }
    })

    # Initialize server functions when extensions are launched
    observeEvent(extensions_data$launched, {
      req(extensions_data$available)

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
                  con = glob$pool,
                  user_id = as.integer(glob$user$user_id),
                  project_id = as.integer(glob$active_project)
                )
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
