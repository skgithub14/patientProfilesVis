#' Create a line plot for one subject
#'
#' @returns a plotly object
#' @export
#'
plotlyLinePlot <- function(data) {
  
  # inputs
  data <- dataLB
  paramNameVar = "LBTEST"
  paramValueVar = "LBSTRESN"
  data[, "yVar"] <- data[, paramValueVar]
  paramGroupVar = "LBCAT"
  paramValueRangeVar = c("LBSTNRLO", "LBSTNRHI")
  colorValueRange = "lightgreen"
  paramVarSep = "-"
  data[, "paramFacetVar"] <- interactionWithMissing(data = data, vars = paramNameVar, varSep = paramVarSep)
  colorVar = "LBCAT"
  colorPalette <- getColorPalettePatientProfile(x = data[, colorVar])
  colorLab = "Lab Category"
  shapeVar = "LBNRIND"
  shapePalette = c(
    'LOW' = 25, 'NORMAL' = 19, 'HIGH' = 24, 
    'ABNORMAL' = 11
  )
  shapeSize <- 7
  alpha = 1
  timeVar = "LBDY"
  xLab = "Study Day"
  yLab = ""
  title = "Laboratory test measurements: actual value"
  labelVars = labelVarsSDTM
  
  # colors
  ribbon_color <- col2rgb(colorValueRange)
  ribbon_color <- paste0("rgba(", 
                         ribbon_color[1,1], ",",
                         ribbon_color[2,1], ",",
                         ribbon_color[3,1], ",",
                         "0.2)")
  
  # shapes
  shapePalette <- getShapePalettePatientProfile(x = data[, shapeVar])
  data <- dplyr::mutate(
    data,
    shapeVarSymbol = dplyr::recode(!!rlang::sym(shapeVar), !!!shapePalette)
  )
  
  # y axis labels - if too long, break into lines
  facetVarMaxLength <- 30
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
  
  fixer <- function(plt) {  
    
    
    
    plt <- plotly::plotly_build(plt)
    print(plt)
    
    # plt$x$data[[17]]$showlegend <- TRUE
    # 
    # p
  }
  
  # plot
  p <- data %>%
    dplyr::group_by(dplyr::across(tidyselect::all_of(paramNameVar))) %>%
    plotly::do(
      p = plotly::plot_ly(
        .,
        x = ~.data[[timeVar]],
        y = ~yVar
      ) %>%
        plotly::add_trace(
          type = "scatter",
          mode = "markers",
          symbol = ~shapeVarSymbol,
          marker = list(
            color = colorPalette[which(names(colorPalette) == unique(.data[[colorVar]]))],
            size = shapeSize,
            opacity = alpha
          ),
          legendgroup = ~yVar,
          showlegend = FALSE
        ) %>%
        plotly::add_trace(
          type = "scatter",
          mode = "lines",
          line = list(
            color = "#000000",
            opacity = alpha
          ),
          showlegend = FALSE
        ) %>%
        plotly::add_ribbons(
          ymin = ~.data[[paramValueRangeVar[1]]],
          ymax = ~.data[[paramValueRangeVar[2]]],
          line = list(
            color = ribbon_color
          ),
          fillcolor = ribbon_color,
          showlegend = FALSE
        ) %>%
        plotly::layout(
          title = title,
          xaxis = list(
            title = xLab
          ),
          yaxis = list(
            title = NA
          ),
          margin = list(
            l = 250,
            r = 50,
            b = 75,
            t = 50,
            pad = 4
          )
        ) %>%
        plotly::add_annotations(
          text = unique(.data$paramFacetVar),
          xref = "paper",
          yref = "paper",
          x = -0.05,
          y = 0.5,
          xanchor = "right",
          yanchor = "bottom",
          showarrow = FALSE
        )
    ) %>%
    plotly::subplot(
      nrows = length(unique(data[[paramNameVar]])),
      shareX = TRUE,
      shareY = TRUE
    )
    
  
}