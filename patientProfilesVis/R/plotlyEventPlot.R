#' Create a [plotly] event plot for a subject
#' 
#' @details 
#' This function is designed to be called by [subjectProfileEventPlot()].
#'
#' @param data a data frame with data for 1 subject only
#' @param margin see [subjectProfileEventPlot()] argument `plotly_args`
#' @param log_footnote_y_shift see [subjectProfileEventPlot()] argument
#'   `plotly_args`
#' @param log_x_axis see [subjectProfileEventPlot()] argument `plotly_args`
#' @param add_vars see [subjectProfileEventPlot()] argument `plotly_args`
#' @inheritParams patientProfilesVis-common-args
#' @inheritParams subjectProfileEventPlot
#'
#' @returns a [plotly] object
#' @export
#'
plotlyEventPlot <- function(data,
                            paramLab,
                            colorVar = NULL, 
                            colorLab = NULL,
                            colorPalette = getColorPalettePatientProfile(n = 1),
                            shapeVar = NULL, 
                            shapeLab = NULL,
                            shapePalette = NULL,
                            alpha = 1,
                            timeVar, 
                            timeLab,
                            timeLim = NULL,
                            xLab,
                            yLab,
                            title,
                            labelVars = NULL,
                            margin = list(
                              l = 50,
                              r = 50,
                              b = 50,
                              t = 50,
                              pad = 4
                            ),
                            log_footnote_y_shift = -0.12,
                            log_x_axis = NULL,
                            add_vars = NULL) {
  
  # log the x-axis
  if (!is.null(log_x_axis)) {
    logOut <- logPlotlyXAxis(data = data, 
                             xvars = timeVar, 
                             log_x_axis = log_x_axis)
    data <- logOut$data
    caption <- logOut$footnote
  } else {
    caption <- NULL
  }
  
  # convert shape unicode and ggplot symbols to plotly symbol names
  if (is.null(shapePalette)) {
    shapePalette <- getShapePalettePatientProfile(n = 1)
  }
  shapePalettePlotly <- convert_shapes_to_plotly_symbols(shapes = shapePalette)
  
  # create hovertemplate column in the data
  if (!is.null(colorVar)) {
    colorVarDat <- data[[colorVar]]
  } else {
    colorVarDat <- NULL
  }
  if (!is.null(shapeVar)) {
    shapeVarDat <- data[[shapeVar]]
  } else {
    shapeVarDat <- NULL
  }
  if (!is.null(add_vars)) {
    add_vars <- formatAdditionalPlotlyHoverVars(data = data, 
                                                add_vars = add_vars, 
                                                labelVars = labelVars)
  }
  data <- dplyr::mutate(
    data,
    hovertemplate = eventPlotHoverTemplate(
      paramVal = yVar,
      paramLab = paramLab,
      timeVal = !!rlang::sym(timeVar),
      timeLab = timeLab, 
      colorLab = colorLab, 
      colorVal = colorVarDat, 
      shapeLab = shapeLab, 
      shapeVal = shapeVarDat,
      add_vars = add_vars
    )
  )
  
  # initialize plotting object
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
    
    # plot each shape so they can be filtered using the legend
    for (shape_num in seq_along(shapePalettePlotly)) {
      shape <- shapePalettePlotly[[shape_num]]
      shape_val <- names(shapePalettePlotly)[shape_num]
      
      # subset data for this shape
      if (length(shapePalette) == 1) {
        shape_color_dat <- color_dat
      } else {
        shape_color_dat <- dplyr::filter(color_dat, 
                                         !!rlang::sym(shapeVar) == shape_val)
      }
      
      # legend grouping
      if (is.null(colorVar) & is.null(shapeVar)) {
        legendgroup <- NULL
      } else if (!is.null(colorVar) & is.null(shapeVar)) {
        legendgroup <- color_val
      } else if (is.null(colorVar) & !is.null(shapeVar)) {
        legendgroup <- shape_val
      } else {
        legendgroup <- paste(color_val, "-", shape_val)
      }
      
      if (nrow(shape_color_dat) > 0) {
        p <- p %>%
          plotly::add_markers(
            type = "scatter",
            x = if (is.null(log_x_axis)) {
                shape_color_dat[[timeVar]]
              } else {
                shape_color_dat[[paste0(timeVar, "Log")]]
              },
            y = shape_color_dat$yVar,
            marker = list(
              color = color,
              symbol = shape,
              size = 10,
              opacity = alpha
            ),
            name = legendgroup,
            showlegend = TRUE,
            legendgroup = legendgroup,
            hovertemplate = shape_color_dat$hovertemplate
          )
      }
    } # end of shape for loop
  } # end of color for loop
  
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
          text = if (is.null(colorVar) & is.null(shapeVar)) {
            ""
          } else if (!is.null(colorVar) & is.null(shapeVar)) {
            paste0("<b>", colorLab, "</b>")
          } else if (is.null(colorVar) & !is.null(shapeVar)) {
            paste0("<b>", shapeLab, "</b>")
          } else {
            paste0("<b>", colorLab, " - ", shapeLab, "</b>")
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
      y = log_footnote_y_shift,
      showarrow = FALSE,
      align = "left"
    )
  }
  
  return(p)
}


#' Create [plotly] tool tip hover template for a event plot
#'
#' @param title a string, the tool tip title
#' @param paramValueVal the value of `paramValueVar` for the tooltip
#' @param paramValueValUnits the value of `paramValueVarUnits` for the tooltip
#' @param timeVal the value of `timeVar` for the tooltip
#' @param colorVal optional, the value of `colorVar` for the tooltip
#' @param shapeVal optional, the value of `shapeVar` for the tooltip
#' @param lln,uln optional numeric values, the lower and upper limit normal
#'   values, respectively
#' @inheritParams patientProfilesVis-common-args
#' @inheritParams subjectProfileLinePlot
#' @inheritParams plotlyLinePlot
#'
#' @returns a string with html formatting
#' 
eventPlotHoverTemplate <- function(paramVal,
                                   paramLab,
                                   timeVal,
                                   timeLab, 
                                   colorLab = NULL, 
                                   colorVal = NULL, 
                                   shapeLab = NULL, 
                                   shapeVal = NULL,
                                   add_vars = NULL) {
  
  # x and y values are the minimum required
  ht <- paste0(
    '<b>', paramLab, ':</b><br>',
    '<b>', paramVal, '</b><br><br>',
    '<i>', timeLab, '</i>: ', timeVal, '<br>'
  )
  
  ## optional variables
  # color
  if (!is.null(colorLab) & !is.null(colorVal)) {
    ht <- paste0(ht, '<i>', colorLab, '</i>: ', colorVal, '<br>')
  }
  
  # shape
  if (!is.null(shapeLab) & !is.null(shapeVal)) {
    ht <- paste0(ht, '<i>', shapeLab, '</i>: ', shapeVal, '<br>')
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
