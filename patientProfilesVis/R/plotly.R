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


#' Convert [ggplot] shape to [plotly] symbol
#'
#' @param shapes a numeric or character vector representing [ggplot2] shapes
#'
#' @returns a string, the corresponding plotly shape name
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
                             r = 50,
                             b = 75,
                             t = 50,
                             pad = 4
                           ),
                           yaxis_title_shift = -0.035,
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
    
  return(plots)
}