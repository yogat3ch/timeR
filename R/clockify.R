
#' Run logTime adding
#'
#' @return Nothing. Called for side effect of displaying gadget
#' @import miniUI
#' @import shiny
#' @import shinyjs
#' @import keyring
#' @export
logClockify <- function() {

  ui <- miniPage(title = "timeR",
                 gadgetTitleBar("Clockify Logger", left = NULL),
                 miniContentPanel(
                   shinyjs::useShinyjs(),
                   textInput("token", HTML("<a href=https://clockify.me/user/settings>User Token</a>")),
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
      key_get("CLOCKIFY_TOKEN")
    }, error = function(e) {
      NULL
    })

    updateCheckboxInput(session, "token", value = token)

    observeEvent(input$token, {
      if(input$save_token) {
        key_set_with_value("CLOCKIFY_TOKEN", password = input$token)
      }
      updateSelectInput(session, "workspace",
                        choices = clockify_get_workspaces(token = input$token))
    })

    observeEvent(input$workspace, {
      updateSelectInput(session, "project",
                        choices = clockify_get_projects(input$workspace,
                                               token = input$token))
      clockify_update_buttons(input, output, session)
    })

    observeEvent(input$start, {
      if(nchar(input$description) == 0) {
        showNotification("Description cannot be blank", type = "error")
      } else {
        clockify_start_timer(input$workspace,
                    input$project,
                    input$description,
                    input$token)
        clockify_update_buttons(input, output, session)
      }
    })

    observeEvent(input$stop, {
      clockify_stop_timer(input$workspace, input$token)
      clockify_update_buttons(input, output, session)
    })

    observeEvent(input$done, {
      if(input$save_token)
        Sys.setenv(CLOCKIFY_TOKEN = input$token)
      stopApp(TRUE)
    })

  }
  runGadget(ui, server)
}


#' GET requests to clockify endpoints
#'
#' Convenience wrapper to fetch data from clockify API GET endpoints
#'
#' @param endpoint desired endpoint to GET (e.g. "/workspaces")
#' @param token access token for desired user. If null, will try to load from
#' environment variable CLOCKIFY_TOKEN
#'
#' @return response code returned by request
#' @import httr
#' @import keyring
#' @export
clockify_get <- function(endpoint, token) {
  if(length(token) == 0)
    token <- key_get("CLOCKIFY_TOKEN")

  if(length(token) == 0)
    stop("No user token provided.")
  endpoint <- gsub("^/", "", endpoint)
  res <- GET(paste0("https://api.clockify.me/api/v1/", endpoint),
            add_headers('X-Api-Key' = token))
  content(res)
}

#' Convenience wrapper to post data to clockify API POST endpoints
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
clockify_post <- function(endpoint, data, token, method = "POST") {
  if(length(token) == 0)
    token <- key_get("CLOCKIFY_TOKEN")
  if(length(token) == 0)
    stop("No user token provided.")
  endpoint <- gsub("^/", "", endpoint)
  if(method == "POST") {
    res <- POST(paste0("https://api.clockify.me/api/v1/", endpoint),
                body = data,
                encode = "json",
                add_headers('X-Api-Key' = token))
  } else {
    res <- PUT(paste0("https://api.clockify.me/api/v1/", endpoint),
                body = data,
                encode = "json",
                add_headers('X-Api-Key' = token))
  }
  content(res)
}

#' Get workspaces
#'
#' @param token access token for desired user.
#'
#' @return List of workspaces, with name and id for each workspace
#' @export
clockify_get_workspaces <- function(token = NULL) {
  ws <- clockify_get("workspaces", token)
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
clockify_get_projects <- function(workspace_id = clockify_get_workspaces()[[1]], token = NULL) {
  pr <- clockify_get(paste0("workspaces/", workspace_id, "/projects"),
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
#' @param workspace_id id for the desired workspace
#' @param project_id id for the desired project
#' @param description description for work being done
#' @param token access token for desired user.
#'
#' @return ID for started time entry
#' @export
clockify_start_timer <- function(workspace_id, project_id, description, token=NULL) {
  ct <- .formatted_time()
  res <- clockify_post(paste0("workspaces/", workspace_id, "/time-entries"),
                data = list(start = ct,
                            description = description,
                            projectId = project_id),
                token = token)
  res$id
}


#' Stop the timer
#'
#' @param workspace_id id for the desired workspace
#' @param token access token for desired user.
#'
#' @return result of API call
#' @export
clockify_stop_timer <- function(workspace_id, token=NULL) {
  ct <- .formatted_time()
  time_entry_id <- clockify_get_running_task(workspace_id, token)$id
  entry <- clockify_get(paste0("workspaces/",
                              workspace_id,
                              "/time-entries/",
                              time_entry_id),
                        token)
  entry$timeInterval$end <- ct
  clockify_post(paste0("workspaces/",
                              workspace_id,
                              "/time-entries/",
                              time_entry_id),
                       data = list(
                         description = entry$description,
                         start = entry$timeInterval$start,
                         end = ct,
                         projectId = entry$projectId),
                       token = token,
                       method = "PUT")
}


#' Get user
#'
#' @param token access token for desired user.
#'
#' @return ID of authorized user
#' @export
clockify_get_user <- function(token) {
  if (missing(token))
    token <- key_get("CLOCKIFY_TOKEN")
  clockify_get("/user", token)$id
}


#' Get running task
#'
#' @param workspace_id id for the desired workspace
#' @param token access token for desired user.
#'
#' @return Named list with
#' @export
clockify_get_running_task <- function(workspace_id, token) {
  user_id <- clockify_get_user(token)
  tasks <- clockify_get(paste0("/workspaces/",
                               workspace_id,
                               "/user/",
                               user_id,
                               "/time-entries?in-progress=true"),
                        token)

  running <- list()
  if(length(tasks)) {
    if("id" %in% names(tasks[[1]])) {
      running <- list(id = tasks[[1]]$id,
                      description = tasks[[1]]$description,
                      start = tasks[[1]]$timeInterval$start)
    }
  }
  running
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
clockify_update_buttons <- function(input, output, session) {
  if(clockify_is_running(input$workspace, input$token)) {
    rt <- clockify_get_running_task(input$workspace, input$token)
    updateTextInput(session, "description", value=rt$description)
    output$timer <- renderText(.elapsed(rt$start))
    updateActionButton(session, "stop", label = paste0("Stop (",
                                                       .elapsed(rt$start),
                                                       "mins)"))
    shinyjs::disable("start")
    shinyjs::enable("stop")
  } else {
    updateActionButton(session, "stop", label = "Stop")
    shinyjs::disable("stop")
    shinyjs::enable("start")
  }
}

clockify_is_running <- function(workspace_id, token) {
  length(clockify_get_running_task(workspace_id, token)) > 0
}

.formatted_time <- function(){
  #2018-06-12T13:48:14.000Z"
  ct <- as.POSIXct(Sys.time())
  attributes(ct)$tzone <- "GMT"
  format(ct, '%Y-%m-%dT%H:%M:%SZ')
}

.elapsed <- function(st) {
  ct <- .formatted_time()
  ct <- as.POSIXlt(ct,format = '%Y-%m-%dT%H:%M:%SZ' )
  st <- as.POSIXlt(st,format = '%Y-%m-%dT%H:%M:%SZ' )
  round(as.numeric(difftime(ct, st, units = "mins")))
}


.reports <- list(url = "https://reports.api.clockify.me/v1",
                 path = rlang::expr(list(ws = "workspaces", wid = workspaceId, rep = "reports",
                                         type = NULL)))

clockify_get_report <-
  function(workspaceId = clockify_get_workspaces()[[1]],
           type = c("summary", "detailed", "weekly")[1],
           dateRangeStart = lubridate::floor_date(Sys.time(), "month"),
           dateRangeEnd = Sys.time(),
           project = clockify_get_projects()[[1]],
           filter,
           exportType = c("CSV", "JSON")[1],
           token,
           ...) {
    if (missing(filter)) {
      filter <- purrr::when(type,
                  . == "summary" ~ list(groups = list("PROJECT")),
                  . == "detailed" ~ list(page = 1, pageSize = 200),
                  . == "weekly" ~ list(group = "USER",
                                       subgroup = "TIME"))

    }
    type <- UU::match_letters(type, c("summary", "detailed", "weekly"))

    .url <- httr::parse_url(.reports$url)
    .url$path <- rlang::eval_bare(.reports$path)
    .url$path$type <- type
    .body <- rlang::dots_list(..., .named = TRUE)
    .body$dateRangeStart <- paste0(lubridate::format_ISO8601(dateRangeStart), ".000Z")
    .body$dateRangeEnd <- paste0(lubridate::format_ISO8601(dateRangeEnd), ".000Z")
    # add appropriate filter type
    if (filter$pageSize > 200) {
      filter$pageSize <- 200
      rlang::warn("pageSize must be 200 or less")
    }

    .body[[paste0(type, "Filter")]] <- filter
    .body$exportType <-
      UU::match_letters(exportType, c("JSON", "CSV", "XLSX", "PDF"))
    env <- environment()
    # Construct filters for simple entries
    purrr::iwalk(.body, ~{
      if (!inherits(.x, "list") && UU::is_legit(.x) && .y %in% c("users", "clients", "projects", "tags")) {
        .body[[.y]] <<- list(ids = list(.x), status = "ALL")
        .body[[.y]][[switch(.y, tags = "containedInTimeentry", clients = , projects = , users = "contains")]] <- "CONTAINS"
      }
    })




    # .body$Projects <- list(
    #   ids = project,
    #   contains = "CONTAINS",
    #   status = "ALL"
    # )
    if(missing(token))
      token <- key_get("CLOCKIFY_TOKEN")
    if(length(token) == 0)
      stop("No user token provided.")

    req <-
      httr::content(httr::POST(
        httr::build_url(.url),
        body = .body,
        encode = "json",
        add_headers('X-Api-Key' = token)
      ), encoding = "UTF-8")

    num_entries <- purrr::when(.body$exportType,
                               . == "JSON" ~ length(req$timeentries),
                               . == "CSV" ~ nrow(req))
    # handle pagination
    if (num_entries == filter$pageSize) {
      pages <- list(req)
      while (num_entries == filter$pageSize) {
        .body$detailedFilter$page <- .body$detailedFilter$page + 1
        pages[[.body$detailedFilter$page]] <-
          httr::content(httr::POST(
            httr::build_url(.url),
            body = .body,
            encode = "json",
            add_headers('X-Api-Key' = token)
          ), encoding = "UTF-8")
      }
      if (.body$exportType == "CSV") {
        out <- dplyr::bind_rows(pages)
      } else {
        out <- pages
      }
    } else {
      out <- req
    }

    out
  }
