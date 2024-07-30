#' Create a [plotly] line plot facetted by `paramFacetVar` for a subject
#' 
#' @details 
#' This function is designed to be called by [subjectProfileLinePlot()].
#'
#' @param data a data frame with data for 1 subject only
#' @inheritParams patientProfilesVis-common-args
#' @inheritParams subjectProfileLinePlot
#' @param add_vars see [subjectProfileLinePlot()] argument `plotly_args`
#' @param margin see [subjectProfileLinePlot()] argument `plotly_args`
#' @param yaxis_title_shift see [subjectProfileLinePlot()] argument
#'   `plotly_args`
#' @param showspikes see [subjectProfileLinePlot()] argument `plotly_args`
#' @param spikecolor see [subjectProfileLinePlot()] argument `plotly_args`
#' @param log_x_axis see [subjectProfileLinePlot()] argument `plotly_args`
#' @param log_footnote_y_shift see [subjectProfileLinePlot()] argument
#'   `plotly_args`
#' 
#' @returns a [plotly] object
#' @export
#'
plotlyLinePlot <- function(data,
                           paramValueVar,
                           paramValueVarUnits = NULL,
                           paramValueRangeVar = NULL,
                           colorValueRange,
                           colorVar = NULL,
                           colorLab = NULL,
                           colorPalette,
                           alpha = 1,
                           shapeVar = NULL,
                           shapeLab = NULL,
                           shapePalette = NULL,
                           shapeSize = 7,
                           timeVar,
                           timeLab,
                           timeLim = NULL,
                           title,
                           xLab,
                           labelVars = NULL,
                           add_vars = NULL,
                           margin = list(
                             l = 250,
                             r = 50,
                             b = 75,
                             t = 50,
                             pad = 4
                           ),
                           yaxis_title_shift = -0.035,
                           showspikes = TRUE,
                           spikecolor = 'red',
                           log_x_axis = NULL,
                           log_footnote_y_shift = -0.1) {
  
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
  
  # create tool tip column in data
  if (!is.null(paramValueVarUnits)) {
    paramValueVarUnitsDat <- data[[paramValueVarUnits]]
  } else {
    paramValueVarUnitsDat <- NULL
  }
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
  if (!is.null(paramValueRangeVar)) {
    llnDat <- data[[paramValueRangeVar[1]]]
    ulnDat <- data[[paramValueRangeVar[2]]]
  } else {
    llnDat <- NULL
    ulnDat <- NULL
  }
  if (!is.null(add_vars)) {
    add_vars <- formatAdditionalPlotlyHoverVars(data = data, 
                                                add_vars = add_vars, 
                                                labelVars = labelVars)
  }
  data <- dplyr::mutate(
    data,
    hovertemplate = linePlotHoverTemplate(
      title = paramFacetVar,
      paramValueVal = !!rlang::sym(paramValueVar),
      paramValueValUnits = paramValueVarUnitsDat,
      timeLab = timeLab,
      timeVal = !!rlang::sym(timeVar),
      colorLab = colorLab,
      colorVal = colorVarDat,
      shapeLab = shapeLab,
      shapeVal = shapeVarDat,
      lln = llnDat,
      uln = ulnDat,
      add_vars = add_vars
    )
  )
  
  # format ribbon colors
  if (!is.null(colorValueRange)) {
    ribbon_color <- col2rgb(colorValueRange)
    ribbon_color <- paste0("rgba(", 
                           ribbon_color[1,1], ",",
                           ribbon_color[2,1], ",",
                           ribbon_color[3,1], ",",
                           "0.2)")
  }
  
  # convert symbols
  if (!is.null(shapeVar)) {
    shapePalettePlotly <- convert_shapes_to_plotly_symbols(
      shapes = shapePalette
    )
  } else {
    shapePalette <- getShapePalettePatientProfile(n = 1)
    shapePalettePlotly <- convert_shapes_to_plotly_symbols(
      shapes = shapePalette
    )
  }
  
  # plot
  plot_tbl <- data %>%
    dplyr::group_by(paramFacetVar) %>%
    plotly::do(
      p = {
        if (!is.null(log_x_axis)) {
          p <- plotly::plot_ly(
            .,
            x = ~.data[[paste0(timeVar, "Log")]],
            y = ~yVar
          )
        } else {
          p <- plotly::plot_ly(
            .,
            x = ~.data[[timeVar]],
            y = ~yVar
          )
        }
      
        # plot each color so colors can be filtered using the legend
        for (color_num in seq_along(colorPalette)) {
          color <- colorPalette[[color_num]]
          color_val <- names(colorPalette)[color_num]
          
          # subset data for this color
          if (length(colorPalette) == 1) {
            color_dat <- .data
          } else {
            color_dat <- .data[which(.data[[colorVar]] == color_val),]
          }
          
          # plot each shape so they can be filtered using the legend
          for (shape_num in seq_along(shapePalettePlotly)) {
            shape <- shapePalettePlotly[[shape_num]]
            shape_val <- names(shapePalettePlotly)[shape_num]
            
            # subset data for this shape
            if (length(shapePalettePlotly) == 1) {
              shape_color_dat <- color_dat
            } else {
              shape_color_dat <- .data[which(.data[[shapeVar]] == shape_val),]
            }
            
            # legend grouping
            if (is.null(colorVar) & is.null(shapeVar)) {
              legendname <- shape_color_dat$paramFacetVar[1]
              legendgroup <- NULL
            } else if (!is.null(colorVar) & is.null(shapeVar)) {
              legendname <- paste(shape_color_dat$paramFacetVar[1], "-",
                                  color_val)
              legendgroup <- color_val
            } else if (is.null(colorVar) & !is.null(shapeVar)) {
              legendname <- paste(shape_color_dat$paramFacetVar[1], "-",
                                  shape_val)
              legendgroup <- shape_val
            } else {
              legendname <- paste(shape_color_dat$paramFacetVar[1], "-",
                                  color_val, "-",
                                  shape_val)
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
                    size = shapeSize,
                    opacity = alpha
                  ),
                  name = legendname,
                  showlegend = if (is.null(colorVar) & is.null(shapeVar)) {
                    FALSE
                  } else {
                    TRUE
                  },
                  legendgroup = legendgroup,
                  hovertemplate = shape_color_dat$hovertemplate
                )
            }
          } # end of shape for loop
        } # end of color for loop
        
        # legend title
        ltitle <- NULL
        if (!is.null(colorVar) & is.null(shapeVar)) {
          ltitle <- paste0("<b>", colorLab, "</b>")
        } else if (is.null(colorVar) & !is.null(shapeVar)) {
          ltitle <- paste0("<b>", shapeLab, "</b>")
        } else {
          ltitle <- paste0("<b>", colorLab, " -<br>", shapeLab, "</b>")
        }
          
        p <- p %>%
          plotly::add_trace(
            type = "scatter",
            mode = "lines",
            line = list(
              color = "#000000",
              opacity = alpha
            ),
            showlegend = FALSE
          ) %>%
          {
            if (!is.null(paramValueRangeVar)) {
              plotly::add_ribbons(
                .,
                ymin = ~.data[[paramValueRangeVar[1]]],
                ymax = ~.data[[paramValueRangeVar[2]]],
                line = list(
                  color = ribbon_color
                ),
                fillcolor = ribbon_color,
                showlegend = FALSE,
                hoverinfo = 'none'
              )
            } else {
              .
            }
          } %>%
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
              title = NA
            ),
            legend = list(
              title = list(
                text = ltitle
              )
            ),
            margin = margin,
            hoverlabel = list(
              align = "left"
            )
          ) %>%
          plotly::add_annotations(
            text = .data$paramFacetVar,
            xref = "paper",
            yref = "paper",
            x = yaxis_title_shift,
            y = 0.5,
            xanchor = "right",
            yanchor = "bottom",
            showarrow = FALSE
          )
        
        p
      }
    )
  
  plots <- plotly::subplot(
    plot_tbl,
    nrows = length(unique(data$paramFacetVar)),
    shareX = TRUE,
    shareY = TRUE
  ) %>%
    plotly::layout(
      xaxis = list(
        showspikes = FALSE,
        spikemode = 'across',
        spikecolor = spikecolor
      )
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
  
  # add footnote about x-axis log scale
  if (!is.null(log_x_axis)) {
    plots <- plotly::add_annotations(
      plots,
      text = caption,
      xref = "paper",
      yref = "paper",
      x = 0,
      y = log_footnote_y_shift,
      showarrow = FALSE,
      align = "left"
    )
  }
  
  return(plots)
}


#' Create [plotly] tool tip hover template for a line plot
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
linePlotHoverTemplate <- function(title, 
                                  paramValueVal,
                                  paramValueValUnits = NULL,
                                  timeLab, 
                                  timeVal,
                                  colorLab = NULL, 
                                  colorVal = NULL, 
                                  shapeLab = NULL, 
                                  shapeVal = NULL,
                                  lln = NULL,
                                  uln = NULL,
                                  add_vars = NULL) {
    
  # title (usually the parameter name) and y value
  ht <- paste0(
    '<b>', title, '</b><br><br>',
    '<i>Value</i>: ', paramValueVal
  )
  
  # optionally add units for y value
  if (!is.null(paramValueValUnits)) {
    units <- purrr::map_chr(paramValueValUnits, \(x) {
      if (!is.na(x) & x != "") {
        paste0(' ', x, '<br>')
      } else {
        '<br>'
      }
    })
    ht <- paste0(ht, units)
  } else {
    ht <- paste0(ht, '<br>')
  }
  
  # x value (time)
  ht <- paste0(ht, '<i>', timeLab, '</i>: ', timeVal, '<br>')
  
  ## optional variables
  # color
  if (!is.null(colorLab) & !is.null(colorVal)) {
    ht <- paste0(ht, '<i>', colorLab, '</i>: ', colorVal, '<br>')
  }
  
  # shape
  if (!is.null(shapeLab) & !is.null(shapeVal)) {
    ht <- paste0(ht, '<i>', shapeLab, '</i>: ', shapeVal, '<br>')
  }
  
  # ranges
  if (!is.null(lln) & !is.null(uln)) {
    ht <- paste0(
      ht,
      '<i>ULN</i>: ', uln, '<br>',
      '<i>LLN</i>: ', lln, '<br>'
    )
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
