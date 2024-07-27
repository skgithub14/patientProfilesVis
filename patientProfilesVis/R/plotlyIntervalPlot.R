plotlyIntervalPlot <- function(data,
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
                               log_x_axis = NULL) {
  
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
          legendgroup = color_val
          # text = start_dat[["hover_texts"]],
          # hoverinfo = "text"
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
            start_dat[[timeEndVar]]
          } else {
            start_dat[[paste0(timeEndVar, "Log")]]
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
          legendgroup = color_val
        )
    }
    
    # plot AEs that have both start and end dates
    segment_df <- dplyr::filter(
      color_dat,
      !!rlang::sym(timeStartShapeVar) == "Complete" &
      !!rlang::sym(timeEndShapeVar) == "Complete"
    )
    
    if (nrow(segment_df) > 0) {
      p <- p |>
        plotly::add_segments(
          mode = 'lines+markers',
          x = if (is.null(log_x_axis)) {
            segment_df[[timeStartVar]]
          } else {
            segment_df[[paste0(timeStartVar, "Log")]]
          },
          xend = if (is.null(log_x_axis)) {
            segment_df[[timeEndVar]]
          } else {
            segment_df[[paste0(timeEndVar, "Log")]]
          },
          y = segment_df[[paramVar]],
          yend = segment_df[[paramVar]],
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
          legendgroup = color_val
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
