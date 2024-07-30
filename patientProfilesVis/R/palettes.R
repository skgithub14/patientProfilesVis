#' Parameters for all patient profiles visualization palette functions.
#' @param includeNA Logical (TRUE by default), 
#' should NA elements be retained in the palette in case
#' \code{x} is specified?
#' @name patientProfilesVis-palette
#' @return No return value, used for the documentation of 
#' the palette functions of the package.
NULL

#' Get a shape palette for patient profile
#' visualizations.
#' 
#' This is a simple wrapper around 
#' \link[clinUtils]{getShapePalette},
#' with different defaults:
#' \itemize{
#' \item{inclusion of missing values by
#' default (\code{includeNA} set to \code{TRUE})
#' }
#' \item{the extraction of shapes as text
#' by default (\code{asText} set to \code{TRUE})
#' }
#' }
#' @param asText Logical (TRUE by default), should the palette
#' be expressed as integer (base R plot and ggplot2 compatible)
#' or in text format 
#' (e.g. required if combined with unicode symbols in ggplot2)?
#' @inheritParams patientProfilesVis-palette
#' @inheritDotParams clinUtils::getShapePalette
#' @inherit clinUtils::getShapePalette return
#' @author Laure Cougnaud
#' @seealso \link[clinUtils]{getShapePalette}
#' @importFrom clinUtils getShapePalette
#' @export
getShapePalettePatientProfile <- function(
	...,
	includeNA = TRUE, asText = TRUE){

	argsFct <- c(
		list(...),
		list(
			includeNA = includeNA,
			asText = asText
		)
	)
	if(asText)	argsFct$palette <- getOption("patientProfilesVis.shapes")
	palette <- do.call(getShapePalette, argsFct)
	return(palette)
	
}

#' Get a color palette for patient profile
#' visualizations.
#' 
#' This is a simple wrapper around 
#' \link[clinUtils]{getColorPalette},
#' with different defaults:
#' \itemize{
#' \item{inclusion of missing values
#' by default (\code{includeNA} set to \code{TRUE})}
#' }
#' @inheritParams patientProfilesVis-palette
#' @inheritDotParams clinUtils::getColorPalette
#' @inherit clinUtils::getShapePalette return
#' @author Laure Cougnaud
#' @seealso \link[clinUtils]{getColorPalette}
#' @importFrom clinUtils getColorPalette
#' @export
getColorPalettePatientProfile <- function(..., includeNA = TRUE){
	
	palette <- getColorPalette(
		...,
		includeNA = includeNA,
		palette = getOption("patientProfilesVis.colors")
	)
	
	return(palette)
	
}


#' Convert [ggplot2] shapes and some unicode characters to [plotly] symbols
#'
#' @param shapes a numeric or character vector representing [ggplot2] shapes
#'
#' @returns a character vector, the corresponding [plotly] shape name
#'
convert_shapes_to_plotly_symbols <- function(shapes) {
  
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
      
      # add unicode characters used in interval plot defaults
      "\u25A0" = "square",
      "\u25C4" = "triangle-left",
      "\u25BA" = "triangle-right",
      
      stop(paste("no plotly shape corresponds to ggplot shape", shape))
    )
  }
  
  purrr::map_chr(shapes, \(shape) convert_shape(shape))
}
