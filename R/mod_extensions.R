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
        # Get package description info
        pkg_info <- tryCatch(
          {
            desc <- packageDescription(ext)
            list(
              title = if (!is.null(desc$Title) && desc$Title != "NA") {
                desc$Title
              } else {
                ext
              },
              description = if (
                !is.null(desc$Description) && desc$Description != "NA"
              ) {
                desc$Description
              } else {
                "Extension package for requal"
              },
              author = if (!is.null(desc$Author) && desc$Author != "NA") {
                desc$Author
              } else {
                "Unknown"
              },
              version = if (!is.null(desc$Version) && desc$Version != "NA") {
                desc$Version
              } else {
                "Unknown"
              }
            )
          },
          error = function(e) {
            list(
              title = ext,
              description = "Extension package for requal",
              author = "Unknown",
              version = "Unknown"
            )
          }
        )

        div(
          class = "col-md-4 col-sm-6 mb-3",
          div(
            class = "card h-100",
            div(
              class = "card-body d-flex flex-column",
              h5(
                class = "card-title",
                tagList(icon("puzzle-piece"), " ", pkg_info$title)
              ),
              h6(
                class = "card-subtitle mb-2 text-muted",
                paste("Package:", ext)
              ),
              p(
                class = "card-text flex-fill",
                # Truncate long descriptions
                if (nchar(pkg_info$description) > 120) {
                  paste0(substr(pkg_info$description, 1, 117), "...")
                } else {
                  pkg_info$description
                }
              ),
              p(
                class = "text-muted mb-2 small",
                paste(
                  "Version:",
                  pkg_info$version,
                  "| Author:",
                  if (nchar(pkg_info$author) > 30) {
                    paste0(substr(pkg_info$author, 1, 27), "...")
                  } else {
                    pkg_info$author
                  }
                )
              ),
              actionButton(
                ns(paste0("launch_", ext)),
                "Launch",
                class = "btn-primary btn-sm mt-auto",
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
