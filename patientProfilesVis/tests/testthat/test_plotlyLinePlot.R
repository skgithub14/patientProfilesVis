#### plotlyLinePlot() ####

test_that("plotlyLinePlot() generates a line plot", {
  
  ### NOTE: these are visual tests only for the time being
  
  ## data prep
  data("dataSDTMCDISCP01", package = "clinUtils")
  dataSDTM <- dataSDTMCDISCP01
  labelVarsSDTM <- attr(dataSDTM, "labelVars")
  dataLB <- dataSDTM$LB
  dataLB <- dplyr::filter(dataLB, LBTEST %in% unique(LBTEST)[1:5])
  subjectLB <- "01-704-1445"
  dataLB <- dplyr::filter(dataLB, USUBJID == subjectLB)
  data <- dataLB
  colorVar <- "LBCAT"
  colorPalette <- getColorPalettePatientProfile(x = data[, colorVar])
  shapeVar = "LBNRIND"
  shapePalette <- getShapePalettePatientProfile(x = data[, shapeVar])
  paramValueVar <- "LBSTRESN"
  data[, "yVar"] <- data[, paramValueVar]
  paramNameVar <- c("LBTEST", "LBSTRESU")
  paramVarSep <- " - "
  data[, "paramFacetVar"] <- interactionWithMissing(data = data, 
                                                    vars = paramNameVar, 
                                                    varSep = paramVarSep)
  data[, "paramFacetVar"] <- clinUtils::formatVarForPlotLabel(
    data = data, 
    paramVar = "paramFacetVar", 
    paramGroupVar = NULL,
    width = 30
  )
  
  # plot with all elements
  plotlyLinePlot(
    data = data,
    paramValueVar = paramValueVar,
    paramValueVarUnits = "LBSTRESU",
    paramValueRangeVar = c("LBSTNRLO", "LBSTNRHI"),
    colorValueRange = "lightgreen",
    colorVar = colorVar,
    colorLab = "Lab Category",
    colorPalette = colorPalette,
    alpha = 1,
    shapeVar = shapeVar,
    shapeLab = "Normal Range Indicator",
    shapePalette = shapePalette,
    shapeSize = 7,
    timeVar = "LBDY",
    timeLab = "Study Day",
    timeLim = NULL,
    title = "Laboratory test measurements: actual value",
    xLab = "Study Day",
    add_vars = list(
      Date = "LBDTC",
      `Range Indicator` = "LBNRIND"
    ),
    labelVars = labelVarsSDTM
  )
  
  # plot with time limits
  plotlyLinePlot(
    data = data,
    paramValueVar = paramValueVar,
    paramValueVarUnits = "LBSTRESU",
    paramValueRangeVar = c("LBSTNRLO", "LBSTNRHI"),
    colorValueRange = "lightgreen",
    colorVar = colorVar,
    colorLab = "Lab Category",
    colorPalette = colorPalette,
    alpha = 1,
    shapeVar = shapeVar,
    shapeLab = "Normal Range Indicator",
    shapePalette = shapePalette,
    shapeSize = 7,
    timeVar = "LBDY",
    timeLab = "Study Day",
    timeLim = c(0, 60),
    title = "Laboratory test measurements: actual value",
    xLab = "Study Day",
    add_vars = list(
      Date = "LBDTC",
      `Range Indicator` = "LBNRIND"
    ),
    labelVars = labelVarsSDTM
  )
  
  # plot without units, no missing names in add_vars
  plotlyLinePlot(
    data = data,
    paramValueVar = paramValueVar,
    paramValueVarUnits = NULL,
    paramValueRangeVar = c("LBSTNRLO", "LBSTNRHI"),
    colorValueRange = "lightgreen",
    colorVar = colorVar,
    colorLab = "Lab Category",
    colorPalette = colorPalette,
    alpha = 1,
    shapeVar = shapeVar,
    shapeLab = "Normal Range Indicator",
    shapePalette = shapePalette,
    shapeSize = 7,
    timeVar = "LBDY",
    timeLab = "Study Day",
    title = "Laboratory test measurements: actual value",
    xLab = "Study Day",
    add_vars = list(
      Date = "LBDTC",
      `Range Indicator` = "LBNRIND"
    ),
    labelVars = NULL
  )
  
  # plot without colors, some missing names in add_vars
  plotlyLinePlot(
    data = data,
    paramValueVar = paramValueVar,
    paramValueVarUnits = "LBSTRESU",
    paramValueRangeVar = c("LBSTNRLO", "LBSTNRHI"),
    colorValueRange = "lightgreen",
    colorVar = NULL,
    colorLab = NULL,
    colorPalette = getColorPalettePatientProfile(n = 1),
    alpha = 1,
    shapeVar = shapeVar,
    shapeLab = "Normal Range Indicator",
    shapePalette = shapePalette,
    shapeSize = 7,
    timeVar = "LBDY",
    timeLab = "Study Day",
    title = "Laboratory test measurements: actual value",
    xLab = "Study Day",
    add_vars = list(
      Date = "LBDTC",
      "LBNRIND"
    ),
    labelVars = labelVarsSDTM
  )
  
  # plot without shapes, no names in add_vars
  plotlyLinePlot(
    data = data,
    paramValueVar = paramValueVar,
    paramValueVarUnits = "LBSTRESU",
    paramValueRangeVar = c("LBSTNRLO", "LBSTNRHI"),
    colorValueRange = "lightgreen",
    colorVar = colorVar,
    colorLab = "Lab Category",
    colorPalette = colorPalette,
    alpha = 1,
    shapeVar = NULL,
    shapeLab = NULL,
    shapePalette = NULL,
    shapeSize = 7,
    timeVar = "LBDY",
    timeLab = "Study Day",
    title = "Laboratory test measurements: actual value",
    xLab = "Study Day",
    add_vars = list(
      "LBDTC",
      "LBNRIND"
    ),
    labelVars = labelVarsSDTM
  )
  
  # plot without colors or shapes
  plotlyLinePlot(
    data = data,
    paramValueVar = paramValueVar,
    paramValueVarUnits = "LBSTRESU",
    paramValueRangeVar = c("LBSTNRLO", "LBSTNRHI"),
    colorValueRange = "lightgreen",
    colorVar = NULL,
    colorLab = NULL,
    colorPalette = getColorPalettePatientProfile(n = 1),
    alpha = 1,
    shapeVar = NULL,
    shapeLab = NULL,
    shapePalette = NULL,
    shapeSize = 7,
    timeVar = "LBDY",
    timeLab = "Study Day",
    title = "Laboratory test measurements: actual value",
    xLab = "Study Day",
    add_vars = list(
      Date = "LBDTC",
      `Range Indicator` = "LBNRIND"
    )
  )
  
  # plot without ranges
  plotlyLinePlot(
    data = data,
    paramValueVar = paramValueVar,
    paramValueVarUnits = "LBSTRESU",
    paramValueRangeVar = NULL,
    colorValueRange = NULL,
    colorVar = colorVar,
    colorLab = "Lab Category",
    colorPalette = colorPalette,
    alpha = 1,
    shapeVar = shapeVar,
    shapeLab = "Normal Range Indicator",
    shapePalette = shapePalette,
    shapeSize = 7,
    timeVar = "LBDY",
    timeLab = "Study Day",
    title = "Laboratory test measurements: actual value",
    xLab = "Study Day",
    add_vars = list(
      Date = "LBDTC",
      `Range Indicator` = "LBNRIND"
    )
  )
  
  # plot without add_vars
  plotlyLinePlot(
    data = data,
    paramValueVar = paramValueVar,
    paramValueVarUnits = "LBSTRESU",
    paramValueRangeVar = c("LBSTNRLO", "LBSTNRHI"),
    colorValueRange = "lightgreen",
    colorVar = colorVar,
    colorLab = "Lab Category",
    colorPalette = colorPalette,
    alpha = 1,
    shapeVar = shapeVar,
    shapeLab = "Normal Range Indicator",
    shapePalette = shapePalette,
    shapeSize = 7,
    timeVar = "LBDY",
    timeLab = "Study Day",
    title = "Laboratory test measurements: actual value",
    xLab = "Study Day",
    add_vars = NULL
  )
})

test_that("plotlyLinePlot() can log the x-axis and include footnote", {
  
  ## data prep
  data("dataSDTMCDISCP01", package = "clinUtils")
  dataSDTM <- dataSDTMCDISCP01
  dataLB <- dataSDTM$LB
  dataLB <- dplyr::filter(dataLB, LBTEST %in% unique(LBTEST)[1:5])
  subjectLB <- "01-704-1445"
  dataLB <- dplyr::filter(dataLB, USUBJID == subjectLB)
  data <- dataLB
  colorVar <- "LBCAT"
  colorPalette <- getColorPalettePatientProfile(x = data[, colorVar])
  shapeVar = "LBNRIND"
  shapePalette <- getShapePalettePatientProfile(x = data[, shapeVar])
  paramValueVar <- "LBSTRESN"
  data[, "yVar"] <- data[, paramValueVar]
  paramNameVar <- c("LBTEST", "LBSTRESU")
  paramVarSep <- " - "
  data[, "paramFacetVar"] <- interactionWithMissing(data = data, 
                                                    vars = paramNameVar, 
                                                    varSep = paramVarSep)
  data[, "paramFacetVar"] <- clinUtils::formatVarForPlotLabel(
    data = data, 
    paramVar = "paramFacetVar", 
    paramGroupVar = NULL,
    width = 30
  )
  
  # exaggerate negative values
  data <- dplyr::mutate(
    data,
    LBDY = dplyr::if_else(LBDY < 0, LBDY - 100, LBDY)
  )
  
  # log positive x values
  plotlyLinePlot(
    data = data,
    paramValueVar = paramValueVar,
    paramValueVarUnits = "LBSTRESU",
    paramValueRangeVar = c("LBSTNRLO", "LBSTNRHI"),
    colorValueRange = "lightgreen",
    colorVar = colorVar,
    colorPalette = colorPalette,
    timeVar = "LBDY",
    timeLab = "Study Day",
    title = "Laboratory test measurements: actual value",
    xLab = "Study Day",
    log_x_axis = "pos"
  )
  
  # log negative x values
  plotlyLinePlot(
    data = data,
    paramValueVar = paramValueVar,
    paramValueVarUnits = "LBSTRESU",
    paramValueRangeVar = c("LBSTNRLO", "LBSTNRHI"),
    colorValueRange = "lightgreen",
    colorVar = colorVar,
    colorPalette = colorPalette,
    timeVar = "LBDY",
    timeLab = "Study Day",
    title = "Laboratory test measurements: actual value",
    xLab = "Study Day",
    log_x_axis = "neg"
  )
  
  # log all x values
  plotlyLinePlot(
    data = data,
    paramValueVar = paramValueVar,
    paramValueVarUnits = "LBSTRESU",
    paramValueRangeVar = c("LBSTNRLO", "LBSTNRHI"),
    colorValueRange = "lightgreen",
    colorVar = colorVar,
    colorPalette = colorPalette,
    timeVar = "LBDY",
    timeLab = "Study Day",
    title = "Laboratory test measurements: actual value",
    xLab = "Study Day",
    log_x_axis = "both"
  )
  
})


#### subjectProfileLinePlot() ####

test_that("subjectProfileLinePlot() can produce plotly outputs correctly", {
  
  # data prep
  data("dataSDTMCDISCP01", package = "clinUtils")
  dataSDTM <- dataSDTMCDISCP01
  dataLB <- dataSDTM$LB
  dataLB <- dplyr::filter(dataLB, LBTEST %in% unique(LBTEST)[1:5])
  subjectLB <- "01-704-1445"
  dataLB <- dplyr::filter(dataLB, USUBJID == subjectLB)
  data <- dataLB
  labelVarsSDTM <- attr(dataSDTM, "labelVars")
  
  p <- subjectProfileLinePlot(
    data = data,
    paramNameVar = "LBTEST",
    paramValueVar = "LBSTRESN", 
    paramValueRangeVar = c("LBSTNRLO", "LBSTNRHI"), 
    colorVar = "LBCAT", 
    shapeVar = "LBNRIND", 
    paramGroupVar = "LBCAT",
    timeVar = "LBDY",
    title = "Laboratory test measurements: actual value",
    labelVars = labelVarsSDTM,
    shapeSize = 7,
    plotly = TRUE,
    plotly_args = list(
      paramValueVarUnits = "LBSTRESU"
    )
  )
  
  p$`01-704-1445`
  
})


test_that("subjectProfileLinePlot() can produce plotly outputs for more than 1 subject", {
  
  # data prep
  data("dataSDTMCDISCP01", package = "clinUtils")
  dataSDTM <- dataSDTMCDISCP01
  dataLB <- dataSDTM$LB
  dataLB <- dplyr::filter(dataLB, LBTEST %in% unique(LBTEST)[1:5])
  subjectLB <- c("01-704-1445", "01-701-1192")
  dataLB <- dplyr::filter(dataLB, USUBJID %in% subjectLB)
  data <- dataLB
  labelVarsSDTM <- attr(dataSDTM, "labelVars")
  
  p <- subjectProfileLinePlot(
    data = data,
    paramNameVar = "LBTEST",
    paramValueVar = "LBSTRESN", 
    timeVar = "LBDY",
    title = "Laboratory test measurements: actual value",
    labelVars = labelVarsSDTM,
    shapeSize = 7,
    plotly = TRUE
  )
  
  p$`01-701-1192`
  p$`01-704-1445`
  
})
