plotlyIntervalPlot <- function(data,
                               paramVar,
                               paramLab,
                               timeStartVar,
                               timeStartLab,
                               timeEndVar,
                               timeEndLab,
                               colorVar,
                               colorLab,
                               colorPalette,
                               alpha,
                               timeStartShapeVar,
                               timeEndShapeVar,
                               shapeLab,
                               shapePalette,
                               shapeSize,
                               title,
                               xLab,
                               yLab,
                               caption,
                               margin = list(
                                 l = 50,
                                 r = 50,
                                 b = 100,
                                 t = 50,
                                 pad = 4
                               ),
                               caption_y_shift = -0.12,
                               log_x_axis = NULL,
                               add_vars = NULL) {
  
  # log the x-axis
  if (!is.null(log_x_axis)) {
    if (!log_x_axis %in% c("pos", "neg", "both")) {
      stop("log_x_axis must be either NULL, 'pos', 'neg', or 'both'")
    }
    
    if (log_x_axis == "pos") {
      data <- dplyr::mutate(
        data,
        dplyr::across(
          c(!!rlang::sym(timeStartVar), !!rlang::sym(timeEndVar)),
          ~ dplyr::if_else(. > 0, log(abs(.)), .),
          .names = "{.col}Log"
        )
      )
      footnote <- paste(
        "Note: positive x-axis", 
        "values are plotted on a log scale; however", 
        "values in tooltip reflect actual",
        timeStartLab, "and", timeEndLab,
        "data."
      )
    } else if (log_x_axis == "neg") {
      data <- dplyr::mutate(
        data,
        dplyr::across(
          c(!!rlang::sym(timeStartVar), !!rlang::sym(timeEndVar)),
          ~ dplyr::if_else(. < 0, -1 * log(abs(.)), .),
          .names = "{.col}Log"
        )
      )
      footnote <- paste(
        "Note: negative x-axis", 
        "values are plotted on a log scale; however", 
        "values in tooltip reflect actual",
        timeStartLab, "and", timeEndLab,
        "data."
      )
    } else {
      data <- dplyr::mutate(
        data,
        dplyr::across(
          c(!!rlang::sym(timeStartVar), !!rlang::sym(timeEndVar)),
          ~ dplyr::case_when(
              . > 0 ~ log(abs(.)),
              . < 0 ~ -1 * log(abs(.)),
              TRUE ~ .
          ),
          .names = "{.col}Log"
        )
      )
      footnote <- paste(
        "Note: x-axis", 
        "values are plotted on a log scale; however", 
        "values in tooltip reflect actual",
        timeStartLab, "and", timeEndLab,
        "data."
      )
    }
    caption <- paste0(caption, "<br>", footnote)
  }
  
  # convert shape unicode and ggplot symbols to plotly symbol names
  shapePalettePlotly <- convert_shapes_to_plotly_symbols(shapes = shapePalette)
  
  # treat tooltip hovertemplate column in the data
  if (!is.null(colorVar)) {
    colorVarDat <- data[[colorVar]]
  } else {
    colorVarDat <- NULL
  }
  if (!is.null(add_vars)) {
    add_vars <- purrr::map(add_vars, \(x) data[[x]])
  }
  data <- dplyr::mutate(
    data,
    hovertemplate = intervalPlotHoverTemplate(
      paramVal = !!rlang::sym(paramVar),
      paramLab = paramLab,
      timeStartVal = !!rlang::sym(timeStartVar),
      timeStartLab = timeStartLab,
      timeEndVal = !!rlang::sym(timeEndVar),
      timeEndLab = timeEndLab,
      colorLab = colorLab, 
      colorVal = colorVarDat, 
      add_vars = add_vars
    )
  )
  
  p <- plotly::plot_ly(data = data)
  
  # plot each color so colors can be filtered using the legend
  for (color_num in seq_along(colorPalette)) {
    color <- colorPalette[[color_num]]
    color_val <- names(colorPalette)[color_num]
    
    # subset data for this color
    if (length(colorPalette) == 1) {
      color_dat <- data
    } else {
      color_dat <- dplyr::filter(data, !!rlang::sym(colorVar) == color_val)
    }
    
    # plot AEs where start time is present and end time is missing
    start_dat <- dplyr::filter(
      color_dat,
      !is.na(!!rlang::sym(timeEndShapeVar)) &
        !is.na(!!rlang::sym(timeStartShapeVar)) &
        !!rlang::sym(timeStartShapeVar) == "Complete" & 
        !!rlang::sym(timeEndShapeVar) == "Missing end"
    )
    
    if (nrow(start_dat) > 0) {
      p <- p |>
        plotly::add_trace(
          x = if (is.null(log_x_axis)) {
            start_dat[[timeStartVar]]
            } else {
              start_dat[[paste0(timeStartVar, "Log")]]
            },
          y = start_dat[[paramVar]],
          color = color,
          type = 'scatter',
          mode = 'markers',
          marker = list(
            symbol = shapePalettePlotly[["Missing end"]],
            size = shapeSize,
            opacity = alpha
          ),
          name = paste(color_val, "- Missing end"),
          showlegend = TRUE,
          legendgroup = color_val,
          hovertemplate = start_dat$hovertemplate
        )
    }
    
    # plot AEs where end time is present and start time is missing
    end_dat <- dplyr::filter(
      color_dat,
      !is.na(!!rlang::sym(timeEndShapeVar)) &
        !is.na(!!rlang::sym(timeStartShapeVar)) &
        !!rlang::sym(timeStartShapeVar) == "Missing start" & 
        !!rlang::sym(timeEndShapeVar) == "Complete"
    )
    
    if (nrow(end_dat) > 0) {
      p <- p |>
        plotly::add_trace(
          x = if (is.null(log_x_axis)) {
            end_dat[[timeEndVar]]
          } else {
            end_dat[[paste0(timeEndVar, "Log")]]
          },
          y = end_dat[[paramVar]],
          color = color,
          type = 'scatter',
          mode = 'markers',
          marker = list(
            symbol = shapePalettePlotly[["Missing start"]],
            size = shapeSize,
            opacity = alpha
          ),
          name = paste(color_val, "- Missing start"),
          showlegend = TRUE,
          legendgroup = color_val,
          hovertemplate = end_dat$hovertemplate
        )
    }
    
    # plot AEs that have both start and end dates
    segment_dat <- dplyr::filter(
      color_dat,
      !!rlang::sym(timeStartShapeVar) == "Complete" &
      !!rlang::sym(timeEndShapeVar) == "Complete"
    )
    
    if (nrow(segment_dat) > 0) {
      p <- p |>
        plotly::add_segments(
          mode = 'lines+markers',
          x = if (is.null(log_x_axis)) {
            segment_dat[[timeStartVar]]
          } else {
            segment_dat[[paste0(timeStartVar, "Log")]]
          },
          xend = if (is.null(log_x_axis)) {
            segment_dat[[timeEndVar]]
          } else {
            segment_dat[[paste0(timeEndVar, "Log")]]
          },
          y = segment_dat[[paramVar]],
          yend = segment_dat[[paramVar]],
          marker = list(
            symbol = shapePalettePlotly[["Complete"]],
            size = shapeSize,
            opacity = alpha
          ),
          color = color,
          opacity = alpha,
          line = list(width = 2),
          name = paste(color_val, "- Complete"),
          showlegend = TRUE,
          legendgroup = color_val,
          hovertemplate = segment_dat$hovertemplate
        )
    }
  } # end of color plotting loop
  
  p <- p |>
    plotly::layout(
      title = title,
      xaxis = list(
        title = xLab
      ),
      xaxis = list(
        title = yLab
      ),
      legend = list(
        title = list(
          text = paste0("<b>", colorLab, " - ", shapeLab, "</b>")
        )
      ),
      margin = margin
    ) %>%
    plotly::config(
      displaylogo = FALSE,
      modeBarButtonsToRemove = c(
        'lasso2d',
        'select2d',
        'hoverClosestCartesian',
        'hoverCompareCartesian'
      )
    )
  
  # add caption
  if (!is.null(caption)) {
    p <- plotly::add_annotations(
      p,
      text = caption,
      xref = "paper",
      yref = "paper",
      x = 0,
      y = caption_y_shift,
      showarrow = FALSE,
      align = "left"
    )
  }
  
  return(p)
}



#' Create [plotly] tool tip hover template for an interval plot
#'
#' @param title a string, the tool tip title
#' @inheritParams patientProfilesVis-common-args
#' @inheritParams subjectProfileLinePlot
#' @inheritParams plotlyLinePlot
#' @param lln,uln optional numeric values, the lower and upper limit normal
#'   values, respectively
#'
#' @returns a string with html formatting
#' 
intervalPlotHoverTemplate <- function(paramVal,
                                      paramLab,
                                      timeStartVal,
                                      timeStartLab,
                                      timeEndVal,
                                      timeEndLab,
                                      colorLab = NULL, 
                                      colorVal = NULL, 
                                      add_vars = NULL) {
  
  # title (usually the parameter name) and y value
  ht <- paste0(
    '<b>', paramLab, ':</b><br>',
    '<b>', paramVal, '</b><br><br>',
    '<i>', timeStartLab, '</i>: ', timeStartVal, '<br>',
    '<i>', timeEndLab, '</i>: ', timeEndVal, '<br>'
  )
  
  # optional color variable
  if (!is.null(colorLab) & !is.null(colorVal)) {
    ht <- paste0(ht, '<i>', colorLab, '</i>: ', colorVal, '<br>')
  }
  
  ## additional variables
  if (!is.null(add_vars)) {
    add_vars1 <- purrr::imap(add_vars, \(value, label) {
      purrr::map_chr(value, \(val) {
        if (!is.na(val) & val != "") {
          paste0('<i>', label, '</i>: ', val, '<br>')
        } else {
          ""
        }
      })
    }) %>%
      purrr::discard(is.null) %>%
      purrr::reduce(paste0)
    ht <- paste0(ht, add_vars1)
  }
  
  ht <- paste0(ht, '<extra></extra>')
  return(ht)
}
