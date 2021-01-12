#' Run logTime adding
#'
#' @return Nothing. Called for side effect of displaying gadget
#' @import miniUI
#' @import shiny
#' @import shinyjs
#' @import keyring
#' @export
logToggl <- function() {

  ui <- miniPage(title = "timeR",
                 gadgetTitleBar("Toggl Logger", left = NULL),
                 miniContentPanel(
                   shinyjs::useShinyjs(),
                   textInput("token", HTML("<a href=https://track.toggl.com/profile>User Token</a>")),
                   checkboxInput("save_token",
                                 "Save in keyring",
                                 value = TRUE),
                   selectInput("workspace", "Workspace", NULL),
                   selectInput("project", "Project", NULL),
                   textInput("description", HTML("What are you working on")),
                   actionButton("start", label = "Start", icon = icon("play-circle")),
                   actionButton("stop", label = "Stop", icon = icon("stop-circle"))
                 )
  )

  server <- function(input, output, session) {
    token <- tryCatch({
      key_get("TOGGL_TOKEN")
    }, error = function(e) {
      NULL
    })

    updateCheckboxInput(session, "token", value = token)

    observeEvent(input$token, {
      if(input$save_token) {
        key_set_with_value("TOGGL_TOKEN", password = input$token)
      }
      updateSelectInput(session, "workspace",
                        choices = toggl_get_workspaces(token = input$token))
    })

    observeEvent(input$workspace, {
      updateSelectInput(session, "project",
                        choices = toggl_get_projects(input$workspace,
                                               token = input$token))
      toggl_update_buttons(input, output, session)
    })

    observeEvent(input$start, {
      if(nchar(input$description) == 0) {
        showNotification("Description cannot be blank", type = "error")
      } else {
        toggl_start_timer(input$project,
                    input$description,
                    input$token)
        toggl_update_buttons(input, output, session)
      }
    })

    observeEvent(input$stop, {
      toggl_stop_timer(input$token)
      toggl_update_buttons(input, output, session)
    })

    observeEvent(input$done, {
      stopApp(TRUE)
    })
  }
  runGadget(ui, server)
}

#' GET requests to toggl endpoints
#'
#' Convenience wrapper to fetch data from toggle API GET endpoints
#'
#' @param endpoint desired endpoint to GET (e.g. "/me")
#' @param token access token for desired user. If null, will try to load from
#' environment variable TOGGL_TOKEN
#'
#' @return response code returned by request
#' @import httr
#' @import keyring
#' @export
#'
toggl_get <- function(endpoint, token) {
  if(length(token) == 0)
    token <- key_get("TOGGL_TOKEN")
  if(length(token) == 0)
    stop("No user token provided.")
  endpoint <- gsub("^/", "", endpoint)
  res <- GET(paste0("https://api.track.toggl.com/api/v8/", endpoint),
            add_headers('Authorization' = auth_header(token)))
  content(res)
}


#' Get encoded auth header
#'
#' Convenience wrapper format authorization header value based on token
#'
#' @param token access token for desired user.
#'
#' @return encoded header value suitable for use in httr::add_headers
#' @import base64enc
#' @export
#'
auth_header <- function(token) {
  paste("Basic", base64encode(charToRaw(paste0(token, ":api_token"))))
}


#' POST/PUT requests to toggl endpoints
#'
#' Convenience wrapper to fetch data from toggle API POST/PUT endpoints
#'
#' @param endpoint desired endpoint to GET (e.g. "/workspaces")
#' @param data data to submit with post
#' @param token access token for desired user.
#' @param method POST by default, but call be used for "PUT".
#'
#' @return response code returned by request
#' @import httr
#' @import keyring
#' @export
toggl_post <- function(endpoint, data, token, method = "POST") {
  if(length(token) == 0)
    token <- key_get("TOGGL_TOKEN")
  if(length(token) == 0)
    stop("No user token provided.")
  endpoint <- gsub("^/", "", endpoint)
  if(method == "POST") {
    res <- POST(paste0("https://api.track.toggl.com/api/v8/", endpoint),
                body = data,
                encode = "json",
                add_headers('Authorization' = auth_header(token)))
  } else {
    res <- PUT(paste0("https://api.track.toggl.com/api/v8/", endpoint),
                body = data,
                encode = "json",
                add_headers('Authorization' = auth_header(token)))
  }
  content(res)
}

#' Get workspaces
#'
#' @param token access token for desired user.
#'
#' @return List of workspaces, with name and id for each workspace
#' @export
toggl_get_workspaces <- function(token = NULL) {
  ws <- toggl_get("workspaces", token)
  tryCatch({
    sapply(ws, function(x) {
      w <- list()
      w[x$name] <- x$id
      w
    })
  },
  error = function(e) {
    return(NULL)
  })
}

#' Get projects
#'
#' @param workspace_id id for the workspace from which to load projects
#' @param token access token for desired user.
#'
#' @return List of projects, with name and id for each workspace
#' @export
toggl_get_projects <- function(workspace_id, token = NULL) {
  pr <- toggl_get(paste0("workspaces/", workspace_id, "/projects"),
                     token)
  tryCatch({
    sapply(pr, function(x) {
      p <- list()
      p[x$name] <- x$id
      p
    })
  },
  error = function(e) {
    return(NULL)
  })
}

#' Start the timer
#'
#' @param project_id id for the desired project
#' @param description description for work being done
#' @param token access token for desired user.
#'
#' @return ID for started time entry
#' @export
toggl_start_timer <- function(project_id, description, token=NULL) {
  res <- toggl_post(paste0("time_entries/start"),
                data = list(time_entry = list(description = description,
                                              pid = project_id,
                                              created_with = "RStudio")),
                token = token)
  res$data$id
}


#' Stop the timer
#'
#' @param time_entry_id id for the desired workspace
#' @param token access token for desired user.
#'
#' @return result of API call
#' @export
toggl_stop_timer <- function(token=NULL) {
  toggl_post(paste0("time_entries/",
                              toggl_get_running_task(token)$id,
                              "/stop"),
                      data = NULL,
                      token,
                      method = "PUT")
}


#' Get user
#'
#' @param token access token for desired user.
#'
#' @return ID of authorized user
#' @export
toggl_get_user <- function(token) {
  toggl_get("me", token)$id
}


# GET https://api.track.toggl.com/api/v8/time_entries/current

#' Get running task
#'
#' @param token access token for desired user.
#'
#' @return Named list with
#' @export
toggl_get_running_task <- function(token) {
  tasks <- toggl_get("time_entries/current",
                        token)
  running <- list()
  if(length(tasks)) {
    if("id" %in% names(tasks[[1]])) {
      running <- list(id = tasks[[1]]$id,
                      description = tasks[[1]]$description,
                      start = tasks[[1]]$start)
    }
  }
  running
}


#' Is a time entry currently running
#'
#' @param token access token for desired user.
#'
#' @return  Boolean indicating if a time entry is currently running
#' @export
toggl_is_running <- function(token) {
  length(toggl_get_running_task(token)) > 0
}


#' Toggle button enabled status
#'
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#'
#' @return None. Called for side effect of updating UI elements
#' @import shiny
#' @import shinyjs
#' @export
toggl_update_buttons <- function(input, output, session) {
  if(toggl_is_running(input$token)) {
    rt <- toggl_get_running_task(input$token)
    updateTextInput(session, "description", value=rt$description)
    updateActionButton(session, "stop", label = paste0("Stop (",
                                                       toggl_elapsed(input$token),
                                                       "mins)"))
    shinyjs::disable("start")
    shinyjs::enable("stop")
  } else {
    updateActionButton(session, "stop", label = "Stop")
    shinyjs::disable("stop")
    shinyjs::enable("start")
  }
}

toggl_formatted_time <- function(){
  #2018-06-12T13:48:14.000Z"
  ct <- as.POSIXct(Sys.time())
  attributes(ct)$tzone <- "GMT"
  format(ct, '%Y-%m-%dT%H:%M:%SZ')
}

toggl_elapsed <- function(token) {
  ct <- toggl_formatted_time()
  ct <- as.POSIXlt(ct,format = '%Y-%m-%dT%H:%M:%SZ' )
  st <- as.POSIXlt(toggl_get_running_task(token)$start,
                   format = '%Y-%m-%dT%H:%M:%S+00:00' )
  round(as.numeric(difftime(ct, st, units = "mins")))
}



