test_that("intervalPlotHoverTemplate() returns correct string", {
  
  # minimum required arguments
  out <- intervalPlotHoverTemplate(
    paramVal = "Lethargy",
    paramLab = "AE Term",
    timeStartVal = 1,
    timeStartLab = "AE Start Study Dat",
    timeEndVal = 2,
    timeEndLab = "AE End Study Day",
    colorLab = NULL, 
    colorVal = NULL, 
    add_vars = NULL
  )
  
  expect_equal(
    out,
    "<b>Lethargy:</b><br><b>AE Term</b><br><br><i>AE Start Study Dat</i>: 1<br><i>AE End Study Day</i>: 2<br><extra></extra>"
  )
  
  # with color
  out <- intervalPlotHoverTemplate(
    paramVal = "Lethargy",
    paramLab = "AE Term",
    timeStartVal = 1,
    timeStartLab = "AE Start Study Dat",
    timeEndVal = 2,
    timeEndLab = "AE End Study Day",
    colorLab = "Severity", 
    colorVal = "Serious", 
    add_vars = NULL
  )
  
  expect_equal(
    out,
    "<b>Lethargy:</b><br><b>AE Term</b><br><br><i>AE Start Study Dat</i>: 1<br><i>AE End Study Day</i>: 2<br><i>Severity</i>: Serious<br><extra></extra>"
  )
  
  # with additional variables
  out <- intervalPlotHoverTemplate(
    paramVal = "Lethargy",
    paramLab = "AE Term",
    timeStartVal = 1,
    timeStartLab = "AE Start Study Dat",
    timeEndVal = 2,
    timeEndLab = "AE End Study Day",
    colorLab = NULL, 
    colorVal = NULL, 
    add_vars = list(
      Serious = "Y",
      DECOD = "LETHARGY"
    )
  )
  
  expect_equal(
    out,
    "<b>Lethargy:</b><br><b>AE Term</b><br><br><i>AE Start Study Dat</i>: 1<br><i>AE End Study Day</i>: 2<br><i>Serious</i>: Y<br><i>DECOD</i>: LETHARGY<br><extra></extra>"
  )
})


test_that("plotlyIntervalPlot() returns the correct visualization", {
  
  ### NOTE: these are visual tests only for the time being
  
  ## data prep
  data("dataSDTMCDISCP01", package = "clinUtils")
  dataSDTM <- dataSDTMCDISCP01
  dataAE <- dataSDTM$AE
  subjectAE <- "01-718-1427"
  dataAE <- dplyr::filter(dataAE, USUBJID == subjectAE)
  dataAE[, "AESEV"] <- factor(dataAE[, "AESEV"], levels = c("MILD", "MODERATE", "SEVERE"))
  data <- dataAE
  
  paramVar <- "AETERM"
  paramVarSep <- " - "
  paramLab <- "AE Term"
  timeStartVar <- "AESTDY"
  timeStartLab <- "AE Start Study Day"
  timeEndVar <- "AEENDY"
  timeEndLab <- "AE End Study Day"
  colorVar <- "AESEV"
  colorLab <- "AE Severity"
  colorPalette <- getColorPalettePatientProfile(x = data[, colorVar])
  alpha <- 1
  timeStartShapeVar <- "timeStartStatus"
  timeEndShapeVar <- "timeEndStatus"
  data <- dplyr::mutate(
    data,
    timeStartStatus = dplyr::if_else(
      is.na(AESTDY), "Missing start", "Complete"
    ),
    timeEndStatus = dplyr::if_else(
      is.na(AEENDY), "Missing end", "Complete"
    )
  )
  shapePalette <- c(
    Complete = "\u25A0",
    "Missing start" = "\u25C4",
    "Missing end" = "\u25BA"
  )
  shapeSize <- 15
  shapeLab <- "Start/End Time Status"
  title <- "Adverse events"
  xLab <- "Study Day"
  yLab <- "AE Terms"
  caption <- "My caption is short"
  
  data[, colorVar] <- convertAesVar(data, colorVar)
  data[, timeStartShapeVar] <- convertAesVar(data, timeStartShapeVar)
  data[, timeEndShapeVar] <- convertAesVar(data, timeEndShapeVar)
  
  data[, "yVar"] <- interactionWithMissing(data = data, vars = paramVar, varSep = paramVarSep)
  data$yVar <- clinUtils::formatVarForPlotLabel(
    data = data, 
    paramVar = "yVar", 
    paramGroupVar = NULL,
    revert = TRUE, 
    width = 30
  )
  
  # fill missing start/end time and extract time limits
  subjectVar <- "USUBJID"
  resMSED <- formatTimeInterval(
    data = data, 
    timeStartVar = timeStartVar, timeStartLab = timeStartLab,
    timeEndVar = timeEndVar, timeEndLab = timeEndLab,
    timeStartShapeVar = timeStartShapeVar, timeEndShapeVar = timeEndShapeVar,
    subjectVar = subjectVar,
    timeLim = NULL, timeLimData = NULL, 
    timeImpType = "none",
    timeLimStartVar = NULL, timeLimStartLab = NULL,
    timeLimEndVar = NULL, timeLimEndLab = NULL
  )
  data <- resMSED$data
  timeLim <- resMSED$timeLim
  timeLimInit <- resMSED$timeLimSpecified
  timeShapePalette <- resMSED$timeShapePalette
  caption <- if(!missing(caption)){caption}else{resMSED$caption}
  
  # with colorVar specified
  plotlyIntervalPlot(
    data = data,
    paramVar = paramVar,
    paramLab = paramLab,
    timeStartVar = timeStartVar,
    timeStartLab = timeStartLab,
    timeEndVar = timeEndVar,
    timeEndLab = timeEndLab,
    colorVar = colorVar,
    colorLab = colorLab,
    colorPalette = colorPalette,
    alpha = alpha,
    timeStartShapeVar = timeStartShapeVar,
    timeEndShapeVar = timeEndShapeVar,
    shapePalette = shapePalette,
    shapeLab = shapeLab,
    shapeSize = shapeSize,
    title = title,
    xLab = xLab,
    yLab = yLab,
    caption = caption
  )
  
  # without colorVar specified
  plotlyIntervalPlot(
    data = data,
    paramVar = paramVar,
    paramLab = paramLab,
    timeStartVar = timeStartVar,
    timeStartLab = timeStartLab,
    timeEndVar = timeEndVar,
    timeEndLab = timeEndLab,
    colorVar = NULL,
    colorLab = NULL,
    colorPalette =  getColorPalettePatientProfile(n = 1),
    alpha = alpha,
    timeStartShapeVar = timeStartShapeVar,
    timeEndShapeVar = timeEndShapeVar,
    shapePalette = shapePalette,
    shapeLab = shapeLab,
    shapeSize = shapeSize,
    title = title,
    xLab = xLab,
    yLab = yLab,
    caption = caption
  )
  
  # with additional tool tip variables
  plotlyIntervalPlot(
    data = data,
    paramVar = paramVar,
    paramLab = paramLab,
    timeStartVar = timeStartVar,
    timeStartLab = timeStartLab,
    timeEndVar = timeEndVar,
    timeEndLab = timeEndLab,
    colorVar = NULL,
    colorLab = NULL,
    colorPalette =  getColorPalettePatientProfile(n = 1),
    alpha = alpha,
    timeStartShapeVar = timeStartShapeVar,
    timeEndShapeVar = timeEndShapeVar,
    shapePalette = shapePalette,
    shapeLab = shapeLab,
    shapeSize = shapeSize,
    title = title,
    xLab = xLab,
    yLab = yLab,
    caption = caption,
    add_vars = list(
      Subject = "USUBJID"
    )
  )
  
  # with logged pos x-values
  plotlyIntervalPlot(
    data = data,
    paramVar = paramVar,
    paramLab = paramLab,
    timeStartVar = timeStartVar,
    timeStartLab = timeStartLab,
    timeEndVar = timeEndVar,
    timeEndLab = timeEndLab,
    colorVar = NULL,
    colorLab = NULL,
    colorPalette =  getColorPalettePatientProfile(n = 1),
    alpha = alpha,
    timeStartShapeVar = timeStartShapeVar,
    timeEndShapeVar = timeEndShapeVar,
    shapePalette = shapePalette,
    shapeLab = shapeLab,
    shapeSize = shapeSize,
    title = title,
    xLab = xLab,
    yLab = yLab,
    caption = caption,
    log_x_axis = "pos"
  )
  
  # with logged new x-values
  plotlyIntervalPlot(
    data = data,
    paramVar = paramVar,
    paramLab = paramLab,
    timeStartVar = timeStartVar,
    timeStartLab = timeStartLab,
    timeEndVar = timeEndVar,
    timeEndLab = timeEndLab,
    colorVar = NULL,
    colorLab = NULL,
    colorPalette =  getColorPalettePatientProfile(n = 1),
    alpha = alpha,
    timeStartShapeVar = timeStartShapeVar,
    timeEndShapeVar = timeEndShapeVar,
    shapePalette = shapePalette,
    shapeLab = shapeLab,
    shapeSize = shapeSize,
    title = title,
    xLab = xLab,
    yLab = yLab,
    caption = caption,
    log_x_axis = "neg"
  )
  
  # with logged x-values and caption y shift
  plotlyIntervalPlot(
    data = data,
    paramVar = paramVar,
    paramLab = paramLab,
    timeStartVar = timeStartVar,
    timeStartLab = timeStartLab,
    timeEndVar = timeEndVar,
    timeEndLab = timeEndLab,
    colorVar = NULL,
    colorLab = NULL,
    colorPalette =  getColorPalettePatientProfile(n = 1),
    alpha = alpha,
    timeStartShapeVar = timeStartShapeVar,
    timeEndShapeVar = timeEndShapeVar,
    shapePalette = shapePalette,
    shapeLab = shapeLab,
    shapeSize = shapeSize,
    title = title,
    xLab = xLab,
    yLab = yLab,
    caption = caption,
    log_x_axis = "both",
    caption_y_shift = -0.15
  )
  
})



test_that("subjectProfileIntervalPlot() with plotly option can return outputs correctly", {
  
  ## data prep
  data("dataSDTMCDISCP01", package = "clinUtils")
  dataSDTM <- dataSDTMCDISCP01
  dataAE <- dataSDTM$AE
  subjectAE <- "01-718-1427"
  dataAE <- dplyr::filter(dataAE, USUBJID == subjectAE)
  dataAE[, "AESEV"] <- factor(dataAE[, "AESEV"], levels = c("MILD", "MODERATE", "SEVERE"))
  data <- dataAE
  labelVarsSDTM <- attr(dataSDTM, "labelVars")
  
  # for one subject
  aePlots <- subjectProfileIntervalPlot(
    data = data,
    paramVar = "AETERM",
    timeStartVar = "AESTDY",
    timeEndVar = "AEENDY",
    colorVar = "AESEV",
    labelVars = labelVarsSDTM,
    title = "Adverse events",
    shapeSize = 10,
    plotly = T,
    timeImpType = "none"
  )
  
  aePlots$`01-718-1427`
  
  ## data prep
  data("dataSDTMCDISCP01", package = "clinUtils")
  dataSDTM <- dataSDTMCDISCP01
  dataAE <- dataSDTM$AE
  subjectAE <- c("01-718-1427", "01-701-1148")
  dataAE <- dplyr::filter(dataAE, USUBJID %in% subjectAE)
  dataAE[, "AESEV"] <- factor(dataAE[, "AESEV"], levels = c("MILD", "MODERATE", "SEVERE"))
  data <- dataAE
  labelVarsSDTM <- attr(dataSDTM, "labelVars")
  
  # for two subjects
  aePlots <- subjectProfileIntervalPlot(
    data = data,
    paramVar = "AETERM",
    timeStartVar = "AESTDY",
    timeEndVar = "AEENDY",
    colorVar = "AESEV",
    labelVars = labelVarsSDTM,
    title = "Adverse events",
    shapeSize = 10,
    plotly = T,
    timeImpType = "none"
  )
  
  aePlots$`01-718-1427`
  aePlots$`01-701-1148`
  
})
