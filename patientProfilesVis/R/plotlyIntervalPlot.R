#' Create a [plotly] interval plot for a subject
#' 
#' @details 
#' This function is designed to be called by [subjectProfileIntervalPlot()].
#'
#' @param data a data frame with data for 1 subject only
#' @inheritParams patientProfilesVis-common-args
#' @inheritParams subjectProfileIntervalPlot
#' @param margin see [subjectProfileIntervalPlot()] argument `plotly_args`
#' @param caption_y_shift see [subjectProfileIntervalPlot()] argument
#'   `plotly_caption_y_shift`
#' @param log_x_axis see [subjectProfileIntervalPlot()] argument
#'   `plotly_log_x_axis`
#' @param add_vars see [subjectProfileIntervalPlot()] argument `plotly_add_vars`
#' 
#' @returns a [plotly] object
#' @export
#'
plotlyIntervalPlot <- function(data,
                               paramVar,
                               paramLab,
                               timeStartVar,
                               timeStartLab,
                               timeEndVar,
                               timeEndLab,
                               timeLim = NULL,
                               colorVar = NULL,
                               colorLab = NULL,
                               colorPalette = getColorPalettePatientProfile(n = 1),
                               alpha = 1,
                               timeStartShapeVar,
                               timeEndShapeVar,
                               shapeLab,
                               shapePalette = getShapePalettePatientProfile(n = 1),
                               shapeSize,
                               title,
                               xLab,
                               yLab,
                               caption = NULL,
                               labelVars = NULL,
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
  
  # break up y values if too long
  data$yVar <- gsub("\n", "<br>", data$yVar)
  
  # log the x-axis
  if (!is.null(log_x_axis)) {
    logOut <- logPlotlyXAxis(data = data, 
                             xvars = c(timeStartVar, timeEndVar), 
                             log_x_axis = log_x_axis)
    data <- logOut$data
    caption <- if (!is.null(caption)) {
      paste0(caption, "<br>", logOut$footnote)
    } else {
      logOut$footnote
    }
  }
  
  # convert shape unicode and ggplot symbols to plotly symbol names
  shapePalettePlotly <- convert_shapes_to_plotly_symbols(shapes = shapePalette)
  
  # create tooltip hovertemplate column in the data
  data$hovertemplate <- intervalPlotHoverTemplate(
    data = data,
    yLab = toString(paramLab),
    timeStartVar = timeStartVar,
    timeStartLab = timeStartLab,
    timeEndVar = timeEndVar,
    timeEndLab = timeEndVar,
    colorLab = colorLab, 
    colorVar = colorVar, 
    add_vars = add_vars
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
      color_dat <- data[which(data[[colorVar]] == color_val),]
    }
    
    # plot AEs where start time is present and end time is missing
    start_dat <- color_dat[which(
      !is.na(color_dat[[timeEndShapeVar]]) &
      color_dat[[timeEndShapeVar]] == "Missing end" &
      !is.na(color_dat[[timeStartShapeVar]]) &
      color_dat[[timeStartShapeVar]] == "Complete"
    ),]
    
    if (nrow(start_dat) > 0) {
      p <- p |>
        plotly::add_trace(
          x = if (is.null(log_x_axis)) {
            start_dat[[timeStartVar]]
            } else {
              start_dat[[paste0(timeStartVar, "Log")]]
            },
          y = start_dat$yVar,
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
    end_dat <- color_dat[which(
      !is.na(color_dat[[timeEndShapeVar]]) &
      color_dat[[timeEndShapeVar]] == "Complete" &
      !is.na(color_dat[[timeStartShapeVar]]) &
      color_dat[[timeStartShapeVar]] == "Missing start"
    ),]
    
    if (nrow(end_dat) > 0) {
      p <- p |>
        plotly::add_trace(
          x = if (is.null(log_x_axis)) {
            end_dat[[timeEndVar]]
          } else {
            end_dat[[paste0(timeEndVar, "Log")]]
          },
          y = end_dat$yVar,
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
    segment_dat <- color_dat[which(
      !is.na(color_dat[[timeEndShapeVar]]) &
      color_dat[[timeEndShapeVar]] == "Complete" &
      !is.na(color_dat[[timeStartShapeVar]]) &
      color_dat[[timeStartShapeVar]] == "Complete"
    ),]
    
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
          y = segment_dat$yVar,
          yend = segment_dat$yVar,
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
      xaxis = if (is.null(timeLim)) {
        list(
          title = xLab
        )
      } else {
        list(
          title = xLab,
          range = timeLim,
          constrain = "domain"
        )
      },
      yaxis = list(
        title = yLab
      ),
      legend = list(
        title = list(
          text = if (!is.null(colorVar)) {
            paste0("<b>", colorLab, " -<br>", shapeLab, "</b>")
          } else {
            paste0("<b>", shapeLab, "</b>")
          }
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
#' @inheritParams patientProfilesVis-common-args
#' @inheritParams subjectProfileIntervalPlot
#' @inheritParams plotlyIntervalPlot
#'
#' @returns a character vector of html formatted strings
#' 
intervalPlotHoverTemplate <- function(data,
                                      yLab,
                                      timeStartVar,
                                      timeStartLab,
                                      timeEndVar,
                                      timeEndLab,
                                      colorLab = NULL, 
                                      colorVar = NULL, 
                                      add_vars = NULL) {
  
  make_template <- function(yVal,
                            yLab,
                            timeStartVal,
                            timeStartLab,
                            timeEndVal,
                            timeEndLab,
                            colorLab, 
                            colorVal, 
                            add_vars) {
    
    # title (usually the parameter name) and y value
    ht <- paste0(
      '<b>', yLab, ':</b><br>',
      '<b>', yVal, '</b><br><br>',
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
  
  if (!is.null(colorVar)) {
    colorVarDat <- data[[colorVar]]
  } else {
    colorVarDat <- NULL
  }
  if (!is.null(add_vars)) {
    add_vars <- formatAdditionalPlotlyHoverVars(data = data, 
                                                add_vars = add_vars, 
                                                labelVars = labelVars)
  }
  
  hovertemplates <- make_template(
    yVal = data$yVar,
    yLab = yLab,
    timeStartVal = data[[timeStartVar]],
    timeStartLab = timeStartLab,
    timeEndVal = data[[timeEndVar]],
    timeEndLab = timeEndLab,
    colorLab = colorLab, 
    colorVal = colorVarDat, 
    add_vars = add_vars
  )
  
  return(hovertemplates)
}
