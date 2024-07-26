plotlyIntervalPlot <- function(data,
                               timeStartVar,
                               timeEndVar,
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
                               caption_y_shift = -0.12) {
  
  # helper function to for legend labels
  get_timeStatusLegendLabel <- function(timeStatusCat) {
    switch(timeStatusCat,
           "Complete" = "(complete start/end)",
           "Missing start" = "(missing start)",
           "Missing end" = "(missing end)")
  }
  
  # convert shape unicode and ggplot symbols to plotly symbol names
  shapePalettePlotly <- convert_shapes_to_plotly_symbols(shapes = shapePalette)
  
  p <- plotly::plot_ly(data = data)

  # We're going to plot each colorVar sequentially, and within it, plot
  # start/end points + segments, that way we can control all three with "MILD" click
  
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
    
    # plot the start points
    for (timeStartCat in unique(color_dat[[timeEndShapeVar]])) {
      if (!is.na(timeStartCat)) {
        
        start_dat <- dplyr::filter(
          color_dat,
          !is.na(!!rlang::sym(timeEndShapeVar)) &
          !!rlang::sym(timeEndShapeVar) == timeStartCat
        )
        
        timeStartCatLegend <- get_timeStatusLegendLabel(timeStartCat)
        
        p <- p |>
          plotly::add_trace(
            x = start_dat[[timeStartVar]],
            y = start_dat[[paramVar]],
            color = color,
            type = 'scatter',
            mode = 'markers',
            marker = list(
              symbol = shapePalettePlotly[[timeStartCat]],
              size = shapeSize,
              opacity = alpha
            ),
            name = paste(color_val, "- Start Point", timeStartCatLegend),
            showlegend = TRUE,
            legendgroup = paste(color_val, "- Start") # This is necessary to control multiple traces with one legend.
            # text = start_dat[["hover_texts"]],
            # hoverinfo = "text"
          )
      }
    }
    
    # plot the end points
    for (timeEndCat in unique(color_dat[[timeStartShapeVar]])) {
      if (!is.na(timeEndCat)) {
        
        end_dat <- dplyr::filter(
          color_dat,
          !is.na(!!rlang::sym(timeStartShapeVar)) &
            !!rlang::sym(timeStartShapeVar) == timeEndCat
        )
        
        timeEndCatLegend <- get_timeStatusLegendLabel(timeEndCat)
        
        p <- p |>
          plotly::add_trace(
            x = end_dat[[timeEndVar]],
            y = end_dat[[paramVar]],
            color = color,
            type = 'scatter',
            mode = 'markers',
            marker = list(
              symbol = shapePalettePlotly[[timeEndCat]],
              size = shapeSize,
              opacity = alpha
            ),
            name = paste(color_val, "- End Point", timeEndCatLegend),
            showlegend = TRUE,
            legendgroup = paste(color_val, "- End") # This is necessary to control multiple traces with one legend
          )
      }
    }
    
    # plot the connecting lines
    segment_df <- dplyr::filter(
      color_dat,
      !!rlang::sym(timeStartShapeVar) == "Complete" &
      !!rlang::sym(timeEndShapeVar) == "Complete"
    )
    
    if (nrow(segment_df) > 0) {
      p <- p |>
        plotly::add_segments(
          x = segment_df[[timeStartVar]],
          xend = segment_df[[timeEndVar]],
          y = segment_df[[paramVar]],
          yend = segment_df[[paramVar]],
          color = color,
          opacity = alpha,
          line = list(width = 2),
          showlegend = FALSE,
          legendgroup = paste(color_val, "- End")
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
