#' Create plotly tool tip hover template for a line plot
#'
#' @details 
#' This function is designed to called within a [plotly::plot_ly()] or
#' [plotly::add_trace()] call for the `hovertemplate` argument.
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
linePlotHoverTemplate <- function(title, 
                                  paramValueVar,
                                  paramValueVarUnits = NULL,
                                  timeLab, 
                                  timeVar,
                                  colorLab = NULL, 
                                  colorVar = NULL, 
                                  shapeLab = NULL, 
                                  shapeVar = NULL,
                                  lln = NULL,
                                  uln = NULL,
                                  add_vars = NULL) {
    
  # title (usually the parameter name) and y value
  ht <- paste0(
    '<b>', title, '</b><br><br>',
    '<i>Value</i>: ', paramValueVar
  )
  
  # optionally add units for y value
  if (!is.null(paramValueVarUnits)) {
    units <- purrr::map_chr(paramValueVarUnits, \(x) {
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
  ht <- paste0(ht, '<i>', timeLab, '</i>: ', timeVar, '<br>')
  
  ## optional variables
  # color
  if (!is.null(colorLab) & !is.null(colorVar)) {
    ht <- paste0(ht, '<i>', colorLab, '</i>: ', colorVar, '<br>')
  }
  
  # shape
  if (!is.null(shapeLab) & !is.null(shapeVar)) {
    ht <- paste0(ht, '<i>', shapeLab, '</i>: ', shapeVar, '<br>')
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


#' Convert [ggplot2] shape to [plotly] symbol
#'
#' @param shapes a numeric or character vector representing [ggplot2] shapes
#'
#' @returns a character vector, the corresponding [plotly] shape name
#'
convert_ggplot_shapes_to_plotly_symbols <- function(shapes) {
  
  convert_shape <- function(shape) {
    
    # convert ggplot shape from character to numeric
    if (!grepl("^[[:digit:]]+L*$", shape)) {
      shape <- ggplot2::translate_shape_string(shape)
    }
    
    shape <- as.character(shape)
    switch(
      shape,
      "0" = "square-open",
      "1" = "circle-open",
      "2" = "triangle-up-open",
      "3" = "cross-thin",
      "4" = "x-thin",
      "5" = "diamond-open",
      "6" = "triangle-down-open",
      "7" = "square-x-open",
      "8" = "asterisk-open",
      "9" = "diamond-cross-open",
      "10" = "circle-cross-open",
      "11" = "hourglass-open",
      "12" = "square-cross-open",
      "13" = "circle-x-open",
      # "14" # no equivalent
      "15" = "square-dot", 
      "16" = "circle-dot", 
      "17" = "triangle-up",
      "18" = "diamond-dot",
      "19" = "circle-dot",
      "20" = "circle-dot",
      "21" = "circle-dot",
      "22" = "square-dot",
      "23" = "diamond-dot",
      "24" = "triangle-up",
      "25" = "triangle-down",
      stop(paste("no plotly shape corresponds to ggplot shape", shape))
    )
  }
  
  purrr::map_chr(shapes, \(shape) convert_shape(shape))
}


#' Convert [ggplot2] shape to unicode character
#'
#' @param shapes a numeric or character vector representing [ggplot2] shapes
#'
#' @returns a character vector of unicode symbols
#'
convert_ggplot_shapes_to_unicode <- function(shapes) {
  
  convert_shape <- function(shape) {
    
    # convert ggplot shape from character to numeric
    if (!grepl("^[[:digit:]]+L*$", shape)) {
      shape <- ggplot2::translate_shape_string(shape)
    }
    
    shape <- as.character(shape)
    switch(
      shape,
      "0" = "\u25A1", # "square-open",
      "1" = "\u2B58", # "circle-open",
      "2" = "\u25B3", # "triangle-up-open",
      "3" = "\U1F7A2", # "cross-thin",
      "4" = "\u2A2F", # "x-thin",
      "5" = "\u25C7", # "diamond-open",
      "6" = "\u25BD", # "triangle-down-open",
      "7" = "\u22A0", # "square-x-open",
      "8" = "\u002A", # "asterisk-open", # no equivalent
      # "9" = "", # "diamond-cross-open",
      "10" = "\u2295", # "circle-cross-open",
      # "11" = "\u29D6", # "hourglass-open", # no equivalent
      "12" = "\u229E", # "square-cross-open",
      "13" = "\u2297", # "circle-x-open",
      # "14" # no equivalent
      "15" = "\u25A0", # "square-dot", 
      "16" = "\u25CF", # "circle-dot", 
      "17" = "\u25B2", # "triangle-up",
      "18" = "\u25C6", # "diamond-dot",
      "19" = "\u25CF", # "circle-dot",
      "20" = "\u25CF", # "circle-dot",
      "21" = "\u25CF", # "circle-dot",
      "22" = "\u25A0", # "square-dot",
      "23" = "\u25C6", # "diamond-dot",
      "24" = "\u25B2", # "triangle-up",
      "25" = "\u25BC", # "triangle-down",
      stop(paste("no unicode equivalent to ggplot shape", shape))
    )
  }
  
  purrr::map_chr(shapes, \(shape) convert_shape(shape))
}


#' Create a plotly line plot facetted by paramFacetVar
#' 
#' @details 
#' This function is designed to be called by [subjectProfileLinePlot()].
#'
#' @param data a data frame with data for 1 subject only
#' @inheritParams patientProfilesVis-common-args
#' @inheritParams subjectProfileLinePlot
#' @param add_vars see [subjectProfileLinePlot()] argument `plotly_args`
#' @param facetVarMaxLength see [subjectProfileLinePlot()] argument
#'   `plotly_args`
#' @param margin see [subjectProfileLinePlot()] argument `plotly_args`
#' @param yaxis_title_shift see [subjectProfileLinePlot()] argument
#'   `plotly_args`
#' @param legend_y_shift see [subjectProfileLinePlot()] argument `plotly_args`
#' @param spikecolor see [subjectProfileLinePlot()] argument `plotly_args`
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
                           title,
                           xLab,
                           add_vars = NULL,
                           facetVarMaxLength = 30,
                           margin = list(
                             l = 250,
                             r = 250,
                             b = 75,
                             t = 50,
                             pad = 4
                           ),
                           yaxis_title_shift = -0.035,
                           legend_x_shift = 1.2,
                           spikecolor = 'red') {
  
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
    add_vars <- purrr::map(add_vars, \(x) data[[x]])
  }
  data <- dplyr::mutate(
    data,
    hovertemplate = linePlotHoverTemplate(
      title = paramFacetVar,
      paramValueVar = !!rlang::sym(paramValueVar),
      paramValueVarUnits = paramValueVarUnitsDat,
      timeLab = timeLab,
      timeVar = !!rlang::sym(timeVar),
      colorLab = colorLab,
      colorVar = colorVarDat,
      shapeLab = shapeLab,
      shapeVar = shapeVarDat,
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
  
  # create a shape symbol column in the data
  if (!is.null(shapeVar)) {
    shapePalettePlotly <- convert_ggplot_shapes_to_plotly_symbols(
      shapes = shapePalette
    )
    shapePaletteUnicode <- convert_ggplot_shapes_to_unicode(
      shapes = shapePalette
    )
  }
  
  # format y axis labels if too long
  paramFacetVarLevels <- levels(data$paramFacetVar)
  paramFacetVarLevels <- purrr::map_chr(paramFacetVarLevels, \(x) {
    if (nchar(x) > facetVarMaxLength) {
      gsub(" ", "<br>", x)
    } else {
      x
    }
  })
  data <- dplyr::mutate(
    data,
    paramFacetVar = as.character(paramFacetVar),
    paramFacetVar = dplyr::if_else(
      nchar(paramFacetVar) > facetVarMaxLength,
      gsub(" ", "<br>", paramFacetVar),
      paramFacetVar
    ),
    paramFacetVar = factor(paramFacetVar,
                           levels = paramFacetVarLevels)
  )
  
  # plot
  plot_tbl <- data %>%
    dplyr::group_by(paramFacetVar) %>%
    plotly::do(
      p = plotly::plot_ly(
        .,
        x = ~.data[[timeVar]],
        y = ~yVar
      ) %>%
        {
          if (!is.null(shapeVar)) {
            plotly::add_trace(
              .,
              type = "scatter",
              mode = "markers",
              symbol = ~shapeVar,
              symbols = shapePalettePlotly,
              marker = list(
                color = if (!is.null(colorVar)) {
                  colorPalette[
                    which(names(colorPalette) == unique(.data[[colorVar]]))
                  ]
                } else {
                  colorPalette # variable is never NULL in subjectProfileLinePlot
                },
                size = shapeSize,
                opacity = alpha
              ),
              showlegend = FALSE,
              hovertemplate = .data$hovertemplate
            )
          } else {
            plotly::add_trace(
              .,
              type = "scatter",
              mode = "markers",
              marker = list(
                color = if (!is.null(colorVar)) {
                  colorPalette[
                    which(names(colorPalette) == unique(.data[[colorVar]]))
                  ]
                } else {
                  colorPalette # variable is never NULL in subjectProfileLinePlot
                },
                size = shapeSize,
                opacity = alpha
              ),
              showlegend = FALSE,
              hovertemplate = .data$hovertemplate
            )
          }
        } %>%
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
          xaxis = list(
            title = xLab
          ),
          yaxis = list(
            title = NA
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
    )
  
  plots <- plotly::subplot(
    plot_tbl,
    nrows = length(unique(data$paramFacetVar)),
    shareX = TRUE,
    shareY = TRUE
  ) %>%
  plotly::layout(
    xaxis = list(
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
  
  # add legend as annotation
  if (!is.null(shapeVar) | !is.null(colorVar)) {
    
    if (!is.null(shapeVar)) {
      shapeLegendText <- purrr::imap(shapePaletteUnicode, ~ {
        paste0(.x, "   ", .y, "<br>")
      }) %>%
        purrr::reduce(paste0) %>%
        paste0("<b>", shapeLab, " Shapes</b><br>", .)
    } else {
      shapeLegendText <- NULL
    }
    
    if (!is.null(colorVar)) {
      colorLegendText <- purrr::imap(colorPalette, ~ {
        paste0("<span style='color: ", .x, ";'>&#x1f534;&#xfe0e;</span>   ", .y, "<br>")
      }) %>%
        purrr::reduce(paste0) %>%
        paste0("<b>", colorLab , " Colors</b><br>", .)
    } else {
      colorLegendText <- NULL
    }
    
    if (!is.null(shapeLegendText) & !is.null(colorLegendText)) {
      legendText <- paste0(shapeLegendText, "<br><br>", colorLegendText)
    } else if (!is.null(shapeLegendText)) {
      legendText <- shapeLegendText
    } else {
      legendText <- colorLegendText
    }
    
    plots <- plotly::add_annotations(
      plots,
      text = legendText,
      xref = "paper",
      yref = "paper",
      x = legend_x_shift,
      y = 0.75,
      xanchor = "right",
      yanchor = "bottom",
      showarrow = FALSE,
      align = "left"
    )
  }
    
  return(plots)
}