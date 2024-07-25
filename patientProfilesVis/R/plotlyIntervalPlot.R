plotlyIntervalPlot <- function(data,
                               timeStartVar,
                               timeEndVar,
                               alpha,
                               shapeSize) {
  
  p <- plotly::plot_ly(
    data = data, 
    y = ~yVar
  )
  
  # add segments that have no missing start/end dates
  not_missing <- dplyr::filter(
    data,
    !is.na(!!rlang::sym(timeStartVar)) & !is.na(!!rlang::sym(timeEndVar))
  )
  p <- p %>%
    plotly::add_trace(
      data = not_missing,
      type = "scatter",
      mode = "lines",
      # line = list(
      #   color = ,
      #   opacity = alpha
      # ),
      # showlegend = FALSE
    ) %>%
    
    # start point
    plotly::add_trace(
      data = not_missing,
      type = "scatter",
      mode = "markers",
      x = ~.data[[timeStartVar]]
      # ,
      # symbol = ~shapeVar,
      # symbols = shapePalettePlotly,
      # marker = list(
      #   color = if (!is.null(colorVar)) {
      #     colorPalette[
      #       which(names(colorPalette) == unique(.data[[colorVar]]))
      #     ]
      #   } else {
      #     colorPalette # variable is never NULL in subjectProfileLinePlot
      #   },
      #   size = shapeSize,
      #   opacity = alpha
      # )
      # ,
      # showlegend = FALSE,
      # hovertemplate = .data$hovertemplate
    ) %>%
    
    # end point
    plotly::add_trace(
      data = not_missing,
      type = "scatter",
      mode = "markers",
      x = ~.data[[timeEndVar]]
      # ,
      # symbol = ~shapeVar,
      # symbols = shapePalettePlotly,
      # marker = list(
      #   color = if (!is.null(colorVar)) {
      #     colorPalette[
      #       which(names(colorPalette) == unique(.data[[colorVar]]))
      #     ]
      #   } else {
      #     colorPalette # variable is never NULL in subjectProfileLinePlot
      #   },
      #   size = shapeSize,
      #   opacity = alpha
      # )
      # ,
      # showlegend = FALSE,
      # hovertemplate = .data$hovertemplate
    )
  
  missing <- dplyr::filter(
    data,
    is.na(!!rlang::sym(timeStartVar)) | is.na(!!rlang::sym(timeEndVar))
  )
  
  return(p)
  
  # dataSubject[[paramVar]] <- forcats::fct_rev(factor(dataSubject[[paramVar]]))
  # 
  # p <- plotly::plot_ly(
  #   color = dataSubject[[colorVar]]
  # )
  # 
  # # We're going to plot each colorVar sequentially, and within it, plot
  # # start/end points + segments, that way we can control all three with "MILD" click
  # 
  # for (colour in levels(dataSubject[[colorVar]])) {
  #   dat <- dataSubject[!is.na(dataSubject[[colorVar]]) & dataSubject[[colorVar]] == colour,]
  #   
  #   # Plot the start points
  #   for (category in unique(dat[[timeStartShapeVar]])) {
  #     
  #     if (!is.na(category)) {
  #       
  #       start_dat <- dat[!is.na(dat[[timeStartShapeVar]]) & dat[[timeStartShapeVar]] == category,]
  #       
  #       hover_texts <- apply(start_dat, 1, generate_plotly_hover_text, 
  #                            timeStartVar, paramVar, colorVar, 
  #                            timeStartShapeVar, plotly_hover_text)
  #       
  #       start_dat$hover_texts <- unname(hover_texts)
  #       
  #       p <- p |>
  #         plotly::add_trace(
  #           x = start_dat[[timeStartVar]],
  #           y = start_dat[[paramVar]],
  #           color = sapply(start_dat[[colorVar]], function(x) color_scale[[x]]),
  #           type = 'scatter',
  #           mode = 'markers',
  #           # If not complete, it's missing start
  #           marker = list(symbol = shape_scale[[category]],
  #                         size = 10),
  #           showlegend = TRUE,
  #           legendgroup = colour, # This is necessary to control multiple traces with one legend.
  #           text = start_dat[["hover_texts"]],
  #           hoverinfo = "text"
  #         )
  #     }
  #   }
  #   
  #   # Plot the end points
  #   for (category in unique(dat[[timeEndShapeVar]])) {
  #     if (!is.na(category)) {
  #       
  #       end_dat <- dat[!is.na(dat[[timeEndShapeVar]]) & dat[[timeEndShapeVar]] == category,]
  #       
  #       hover_texts <- apply(end_dat, 1, generate_plotly_hover_text, 
  #                            timeEndVar, paramVar, colorVar, 
  #                            timeEndShapeVar, plotly_hover_text)
  #       
  #       end_dat$hover_texts <- unname(hover_texts)
  #       
  #       p <- p |>
  #         plotly::add_trace(
  #           x = end_dat[[timeEndVar]],
  #           y = end_dat[[paramVar]],
  #           color = sapply(end_dat[[colorVar]], function(x) color_scale[[x]]),
  #           type = 'scatter',
  #           mode = 'markers',
  #           # If not complete, it's missing end
  #           marker = list(symbol = shape_scale[[category]],
  #                         size = 10),
  #           showlegend = FALSE,
  #           legendgroup = colour,
  #           text = end_dat[["hover_texts"]],
  #           hoverinfo = "text"
  #         )
  #     }
  #   }
  #   
  #   # Plot the connecting lines
  #   segment_df <- dat[!is.na(dat$missingStartPlot) & !is.na(dat$missingEndPlot), ]
  #   
  #   if (nrow(segment_df) > 0) {
  #     p <- p |>
  #       plotly::add_segments(
  #         x = segment_df[[timeStartVar]],
  #         xend = segment_df[[timeEndVar]],
  #         y = segment_df[[paramVar]],
  #         yend = segment_df[[paramVar]],
  #         color = sapply(segment_df[[colorVar]], function(x) color_scale[[x]]),
  #         line = list(width = 2),
  #         showlegend = FALSE,
  #         legendgroup = colour
  #       )
  #   }
  # }
  # 
  # p <- p |>
  #   plotly::layout(
  #     title = title,
  #     xaxis = list(
  #       zeroline = FALSE,
  #       title = xLab
  #     )
  #   )
  # 
  # p
  
  
}



























#' Generate text to include in plotly hover
#' @param dat A row of dataframe
#' @param timeVar A string, that determines time mapping
#' @param paramVar A string, that determines param mapping
#' @param colorVar A string, that determines color mapping
#' @param timeShapeVar A string that determines shape mapping
#' @param plotly_hover_text A named vector. If not null, 
#' `generate_extra_plotly_hover_text` will be called to create extra info.
generate_plotly_hover_text <- function(dat, timeVar, paramVar, colorVar, 
                                       timeShapeVar, plotly_hover_text = NULL) {
  base_text <- paste0(
    '<b>', timeVar, '</b>: ', dat[[timeVar]], '<br>',
    '<b>', paramVar, '</b>: ', dat[[paramVar]], '<br>',
    '<b>', colorVar, '</b>: ', dat[[colorVar]], '<br>',
    '<b>Status</b>: ', dat[[timeShapeVar]], '<br>'
  )
  
  extra_text <- if (!is.null(plotly_hover_text)) {
    generate_extra_plotly_hover_text(plotly_hover_text, dat)
  } else {
    ""
  }
  return(paste0(base_text, extra_text, "<br>"))
}

#' Generate extra text to include in plotly hover
#' @param plotly_hover_text Named vector. Plotly hover will include
#' this information, in form of `<b>Name</b>: value`
#' @param dat Data frame, where column `value` will be pulled to make the text
generate_extra_plotly_hover_text <- function(plotly_hover_text, dat) {
  text_list <- mapply(function(name, value) {
    paste0("<b>", name, "</b>: ", dat[[value]])
  }, names(plotly_hover_text), plotly_hover_text)
  
  collapsed <- paste(text_list, collapse = "<br>")
  res <- paste0(collapsed, "<br>")
  
  return(res)
}
