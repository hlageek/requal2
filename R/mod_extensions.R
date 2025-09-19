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
            div(
              id = ns("search_container"),
              class = "mb-3",
              textInput(
                ns("search_extensions"),
                label = NULL,
                placeholder = "Search extensions...",
                width = "300px"
              )
            ),
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
      launched = c() # Track which extensions are launched
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

      # Show/hide search input based on available extensions
      if (length(extensions_data$available) > 0) {
        shinyjs::show("search_container")
      } else {
        shinyjs::hide("search_container")
      }
    })

    # Render extension cards with search filtering
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
          class = "text-center",
          style = "padding: 60px 20px;",
          div(
            icon(
              "puzzle-piece",
              style = "font-size: 64px; color: #ccc; margin-bottom: 20px;"
            )
          ),
          h4(
            "No Extensions Found",
            style = "color: #666; margin-bottom: 15px;"
          ),
          p(
            "No extension packages were found that depend on 'requal'.",
            style = "color: #888; margin-bottom: 25px; font-size: 16px;"
          ),
          p(
            "Visit ",
            tags$a(
              href = "https://www.requal.app",
              target = "_blank",
              "www.requal.app"
            ),
            " to learn more about available extensions and installation instructions.",
            style = "color: #666; margin-bottom: 20px;"
          ),
          actionButton(
            ns("refresh_extensions"),
            "Refresh",
            class = "btn-outline-primary",
            icon = icon("refresh")
          )
        ))
      }

      # Get search term
      search_term <- input$search_extensions

      # Filter extensions based on search
      filtered_extensions <- extensions_data$available
      if (!is.null(search_term) && nchar(trimws(search_term)) > 0) {
        search_lower <- tolower(trimws(search_term))
        filtered_extensions <- Filter(
          function(ext) {
            pkg_info <- tryCatch(
              {
                desc <- packageDescription(ext)
                title <- if (!is.null(desc$Title) && desc$Title != "NA") {
                  desc$Title
                } else {
                  ext
                }
                # Search in package name and title
                grepl(search_lower, tolower(ext), fixed = TRUE) ||
                  grepl(search_lower, tolower(title), fixed = TRUE)
              },
              error = function(e) {
                # If error getting description, just search package name
                grepl(search_lower, tolower(ext), fixed = TRUE)
              }
            )
          },
          extensions_data$available
        )
      }

      # Show message if no results
      if (length(filtered_extensions) == 0) {
        return(div(
          style = "text-align: center; padding: 40px; color: #999;",
          icon("search", style = "font-size: 48px; margin-bottom: 10px;"),
          br(),
          paste("No extensions found matching '", search_term, "'")
        ))
      }

      # Create cards for filtered extensions
      cards <- lapply(filtered_extensions, function(ext) {
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

        # Check if extension is launched
        is_launched <- ext %in% extensions_data$launched

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
              if (is_launched) {
                div(
                  class = "mt-auto",
                  div(
                    class = "btn-group w-100",
                    actionButton(
                      ns(paste0("goto_", ext)),
                      "Go to",
                      class = "btn-success btn-sm",
                      onclick = paste0(
                        "Shiny.setInputValue('",
                        ns("goto_extension"),
                        "', '",
                        ext,
                        "', {priority: 'event'});"
                      )
                    ),
                    actionButton(
                      ns(paste0("close_", ext)),
                      "Close",
                      class = "btn-outline-danger btn-sm",
                      onclick = paste0(
                        "Shiny.setInputValue('",
                        ns("close_extension"),
                        "', '",
                        ext,
                        "', {priority: 'event'});"
                      )
                    )
                  )
                )
              } else {
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
              }
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

              # Add to launched extensions
              extensions_data$launched <- c(extensions_data$launched, ext)

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

    # Handle goto extension events
    observeEvent(input$goto_extension, {
      ext <- input$goto_extension
      updateTabsetPanel(
        session,
        "extensions_tabset",
        selected = paste0(ext, "_tab")
      )
    })

    # Handle refresh extensions
    observeEvent(input$refresh_extensions, {
      extensions_data$available <- NULL # Reset to trigger re-discovery
      showNotification("Checking for extensions...", type = "message")
    })
    observeEvent(input$close_extension, {
      ext <- input$close_extension
      print(paste("Closing extension:", ext))

      tryCatch(
        {
          # Remove the tab
          removeTab(
            inputId = "extensions_tabset",
            target = paste0(ext, "_tab"),
            session = session
          )

          # Remove from launched extensions
          extensions_data$launched <- extensions_data$launched[
            extensions_data$launched != ext
          ]

          showNotification(
            paste("Closed extension:", ext),
            type = "message"
          )

          # Switch back to modules manager tab
          updateTabsetPanel(
            session,
            "extensions_tabset",
            selected = "extensions_manager"
          )
        },
        error = function(e) {
          print(paste("Error closing extension", ext, ":", e$message))
          showNotification(
            paste("Error closing", ext, ":", e$message),
            type = "error"
          )
        }
      )
    })
  })
}
