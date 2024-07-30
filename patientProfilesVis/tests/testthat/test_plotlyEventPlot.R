test_that("plotlyEventPlot() returns correct plotly object", {
  
  ### NOTE: these are visual tests only for the time being
  
  ## data prep
  data("dataSDTMCDISCP01", package = "clinUtils")
  dataSDTM <- dataSDTMCDISCP01
  labelVarsSDTM <- attr(dataSDTM, "labelVars")
  subjectLB <- "01-704-1445"
  lbTests <- c("CHOL", "PHOS", "ANISO", "MCHC", "PLAT", "KETONES")
  dataLB <- subset(dataSDTM$LB, LBTESTCD %in% lbTests)
  dataLB <- subset(dataLB, USUBJID == subjectLB)
  dataLB$LBNRIND <- factor(dataLB$LBNRIND, 
                           levels = c("LOW", "NORMAL", "HIGH", "ABNORMAL"))
  data <- dataLB
  
  paramVar <- "LBTEST"
  paramLab <- "Lab Test"
  paramVarSep <- " - "
  data[, "yVar"] <- interactionWithMissing(data = data, 
                                           vars = paramVar, 
                                           varSep = paramVarSep)
  yLab <- ""
  
  timeVar <- "LBDY"
  timeLab <- "Study Day"
  xLab <- timeLab
  
  colorVar <- "LBCAT"
  colorLab <- "Lab Category"
  data[, colorVar] <- convertAesVar(data, colorVar)
  colorPalette <- getColorPalettePatientProfile(x = data[, colorVar])
  alpha <- 1
  
  shapeVar <- "LBNRIND"
  shapeLab <- "Range Indicator"
  data[, shapeVar] <- convertAesVar(data, var = shapeVar)
  shapePalette <- getShapePalettePatientProfile(x = data[, shapeVar])
  
  title <- "Lab Tests"
  
  # all elements
  plotlyEventPlot(
    data = data,
    paramLab = paramLab,
    colorVar = colorVar, 
    colorLab = colorLab,
    colorPalette = colorPalette,
    shapeVar = shapeVar, 
    shapeLab = shapeLab,
    shapePalette = shapePalette,
    alpha = alpha,
    timeVar = timeVar, 
    timeLab = timeLab,
    timeLim = NULL,
    xLab = xLab,
    yLab = yLab,
    title = title,
    labelVars = labelVarsSDTM,
    log_x_axis = NULL,
    add_vars = NULL
  )
  
  # time limit
  plotlyEventPlot(
    data = data,
    paramLab = paramLab,
    colorVar = colorVar, 
    colorLab = colorLab,
    colorPalette = colorPalette,
    shapeVar = shapeVar, 
    shapeLab = shapeLab,
    shapePalette = shapePalette,
    alpha = alpha,
    timeVar = timeVar, 
    timeLab = timeLab,
    timeLim = c(10, 20),
    xLab = xLab,
    yLab = yLab,
    title = title,
    labelVars = labelVarsSDTM,
    log_x_axis = NULL,
    add_vars = NULL
  )
  
  # no colors
  plotlyEventPlot(
    data = data,
    paramLab = paramLab,
    colorVar = NULL, 
    colorLab = NULL,
    colorPalette = getColorPalettePatientProfile(n = 1),
    shapeVar = shapeVar, 
    shapeLab = shapeLab,
    shapePalette = shapePalette,
    alpha = alpha,
    timeVar = timeVar, 
    timeLab = timeLab,
    # timeLim = NULL,
    xLab = xLab,
    yLab = yLab,
    title = title,
    labelVars = labelVarsSDTM,
    log_x_axis = NULL,
    add_vars = NULL
  )
  
  # no shapes
  plotlyEventPlot(
    data = data,
    paramLab = paramLab,
    colorVar = colorVar, 
    colorLab = colorLab,
    colorPalette = colorPalette,
    shapeVar = NULL, 
    shapeLab = NULL,
    shapePalette = NULL,
    alpha = alpha,
    timeVar = timeVar, 
    timeLab = timeLab,
    # timeLim = NULL,
    xLab = xLab,
    yLab = yLab,
    title = title,
    labelVars = labelVarsSDTM,
    log_x_axis = NULL,
    add_vars = NULL
  )
  
  # no shapes nor colors
  plotlyEventPlot(
    data = data,
    paramLab = paramLab,
    colorVar = NULL, 
    colorLab = NULL,
    colorPalette = getColorPalettePatientProfile(n = 1),
    shapeVar = NULL, 
    shapeLab = NULL,
    shapePalette = NULL,
    alpha = alpha,
    timeVar = timeVar, 
    timeLab = timeLab,
    # timeLim = NULL,
    xLab = xLab,
    yLab = yLab,
    title = title,
    labelVars = labelVarsSDTM,
    log_x_axis = NULL,
    add_vars = NULL
  )
  
})


test_that("subjectProfileEventPlot() works when plotly = TRUE", {
  
  ### NOTE: these are visual tests only for the time being
  
  ## data prep
  data("dataSDTMCDISCP01", package = "clinUtils")
  dataSDTM <- dataSDTMCDISCP01
  labelVarsSDTM <- attr(dataSDTM, "labelVars")
  subjectLB <- "01-704-1445"
  lbTests <- c("CHOL", "PHOS", "ANISO", "MCHC", "PLAT", "KETONES")
  dataLB <- subset(dataSDTM$LB, LBTESTCD %in% lbTests)
  dataLB <- subset(dataLB, USUBJID == subjectLB)
  dataLB$LBNRIND <- factor(dataLB$LBNRIND, 
                           levels = c("LOW", "NORMAL", "HIGH", "ABNORMAL"))
  data <- dataLB
  
  # one subject
  lbPlotsColorShape <- subjectProfileEventPlot(
    data = data,
    paramVar = "LBTEST",
    paramGroupVar = "LBCAT",
    timeVar = "LBDY",
    colorVar = "LBCAT",
    labelVars = labelVarsSDTM,
    shapeVar = "LBNRIND",
    title = "Laboratory test measurements: reference range indicator",
    plotly = TRUE
  )
  
  lbPlotsColorShape$`01-704-1445`
  
  # two subjects
  data("dataSDTMCDISCP01", package = "clinUtils")
  dataSDTM <- dataSDTMCDISCP01
  labelVarsSDTM <- attr(dataSDTM, "labelVars")
  subjectLB <- c("01-704-1445", "01-701-1148")
  lbTests <- c("CHOL", "PHOS", "ANISO", "MCHC", "PLAT", "KETONES")
  dataLB <- subset(dataSDTM$LB, LBTESTCD %in% lbTests)
  dataLB <- subset(dataLB, USUBJID %in% subjectLB)
  dataLB$LBNRIND <- factor(dataLB$LBNRIND, 
                           levels = c("LOW", "NORMAL", "HIGH", "ABNORMAL"))
  data <- dataLB
  
  lbPlotsColorShape <- subjectProfileEventPlot(
    data = data,
    paramVar = "LBTEST",
    paramGroupVar = "LBCAT",
    timeVar = "LBDY",
    colorVar = "LBCAT",
    labelVars = labelVarsSDTM,
    shapeVar = "LBNRIND",
    title = "Laboratory test measurements: reference range indicator",
    plotly = TRUE
  )
  
  lbPlotsColorShape$`01-704-1445`
  lbPlotsColorShape$`01-701-1148`
  
})
