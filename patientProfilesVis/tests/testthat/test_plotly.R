#### linePlotHoverTemplate() ####

test_that("linePlotHoverTemplate() create correct one hover template string with a variety of different NULL inputs", {
  
  out <- linePlotHoverTemplate(
    title = "Albumin", 
    paramValueVar = 1,
    paramValueVarUnits = NULL,
    timeLab = "Study Day", 
    timeVar = 2,
    colorLab = NULL, 
    colorVar = NULL, 
    shapeLab = NULL, 
    shapeVar = NULL,
    uln = NULL,
    lln = NULL,
    add_vars = NULL
  )
  
  expect_equal(
    out,
    "<b>Albumin</b><br><br><i>Value</i>: 1<br><i>Study Day</i>: 2<br><extra></extra>"
  )
  
  out <- linePlotHoverTemplate(
    title = "Albumin", 
    paramValueVar = 1,
    paramValueVarUnits = "g/L",
    timeLab = "Study Day", 
    timeVar = 2,
    colorLab = NULL, 
    colorVar = NULL, 
    shapeLab = NULL, 
    shapeVar = NULL,
    uln = NULL,     
    lln = NULL,
    add_vars = NULL
  )
  
  expect_equal(
    out,
    "<b>Albumin</b><br><br><i>Value</i>: 1 g/L<br><i>Study Day</i>: 2<br><extra></extra>"
  )
  
  out <- linePlotHoverTemplate(
    title = "Albumin", 
    paramValueVar = 1,
    paramValueVarUnits = "g/L",
    timeLab = "Study Day", 
    timeVar = 2,
    colorLab = "Lab Category", 
    colorVar = "Chemistry", 
    shapeLab = NULL, 
    shapeVar = NULL,
    uln = NULL,     
    lln = NULL,
    add_vars = NULL
  )
  
  expect_equal(
    out,
    "<b>Albumin</b><br><br><i>Value</i>: 1 g/L<br><i>Study Day</i>: 2<br><i>Lab Category</i>: Chemistry<br><extra></extra>"
  )
  
  out <- linePlotHoverTemplate(
    title = "Albumin", 
    paramValueVar = 1,
    paramValueVarUnits = "g/L",
    timeLab = "Study Day", 
    timeVar = 2,
    colorLab = "Lab Category", 
    colorVar = "Chemistry", 
    shapeLab = "Range Indicator", 
    shapeVar = "NORMAL",
    uln = NULL,     
    lln = NULL,
    add_vars = NULL
  )
  
  expect_equal(
    out,
    "<b>Albumin</b><br><br><i>Value</i>: 1 g/L<br><i>Study Day</i>: 2<br><i>Lab Category</i>: Chemistry<br><i>Range Indicator</i>: NORMAL<br><extra></extra>"
  )
  
  out <- linePlotHoverTemplate(
    title = "Albumin", 
    paramValueVar = 1,
    paramValueVarUnits = "g/L",
    timeLab = "Study Day", 
    timeVar = 2,
    colorLab = "Lab Category", 
    colorVar = "Chemistry", 
    shapeLab = "Range Indicator", 
    shapeVar = "NORMAL",
    lln = 0,
    uln = 2,
    add_vars = NULL
  )
  
  expect_equal(
    out,
    "<b>Albumin</b><br><br><i>Value</i>: 1 g/L<br><i>Study Day</i>: 2<br><i>Lab Category</i>: Chemistry<br><i>Range Indicator</i>: NORMAL<br><i>ULN</i>: 2<br><i>LLN</i>: 0<br><extra></extra>"
  )
  
  out <- linePlotHoverTemplate(
    title = "Albumin", 
    paramValueVar = 1,
    paramValueVarUnits = "g/L",
    timeLab = "Study Day", 
    timeVar = 2,
    colorLab = "Lab Category", 
    colorVar = "Chemistry", 
    shapeLab = "Range Indicator", 
    shapeVar = "NORMAL",
    lln = 0,
    uln = 2,
    add_vars = list(Date = "2020-01-01", Alert = "ALB >= 1", Grade = NA, Visit = "")
  )
  
  expect_equal(
    out,
    "<b>Albumin</b><br><br><i>Value</i>: 1 g/L<br><i>Study Day</i>: 2<br><i>Lab Category</i>: Chemistry<br><i>Range Indicator</i>: NORMAL<br><i>ULN</i>: 2<br><i>LLN</i>: 0<br><i>Date</i>: 2020-01-01<br><i>Alert</i>: ALB >= 1<br><extra></extra>"
  )
})


test_that("linePlotHoverTemplate() can be vectorized using dplyr::mutate()", {
  
  # minimum required columns
  data <- data.frame(
    param = rep("Albumin", 2),
    aval = c(1, 2),
    ady = c(10, 11)
  )
  
  out <- dplyr::mutate(
    data,
    hovertemplate = linePlotHoverTemplate(
      title = param,
      paramValueVar = aval,
      timeLab = "Study Day",
      timeVar = ady
    )
  )
  
  expect_equal(
    out$hovertemplate,
    c(
      "<b>Albumin</b><br><br><i>Value</i>: 1<br><i>Study Day</i>: 10<br><extra></extra>",
      "<b>Albumin</b><br><br><i>Value</i>: 2<br><i>Study Day</i>: 11<br><extra></extra>"
    )
  )
  
  # with units
  data <- data.frame(
    param = rep("Albumin", 4),
    aval = 1:4,
    avalu = c(rep("g/L", 2), NA_character_, ""),
    ady = 11:14
  )
  
  out <- dplyr::mutate(
    data,
    hovertemplate = linePlotHoverTemplate(
      title = param,
      paramValueVar = aval,
      paramValueVarUnits = avalu,
      timeLab = "Study Day",
      timeVar = ady
    )
  )
  
  expect_equal(
    out$hovertemplate,
    c(
      "<b>Albumin</b><br><br><i>Value</i>: 1 g/L<br><i>Study Day</i>: 11<br><extra></extra>",
      "<b>Albumin</b><br><br><i>Value</i>: 2 g/L<br><i>Study Day</i>: 12<br><extra></extra>",
      "<b>Albumin</b><br><br><i>Value</i>: 3<br><i>Study Day</i>: 13<br><extra></extra>",
      "<b>Albumin</b><br><br><i>Value</i>: 4<br><i>Study Day</i>: 14<br><extra></extra>"
    )
  )
  
  # add colors and shapes
  data <- data.frame(
    param = rep("Albumin", 2),
    aval = 1:2,
    avalu = rep("g/L", 2),
    ady = 11:12,
    lbcat = rep("Chemistry", 2),
    anrind = c("NORMAL", "HIGH")
  )
  
  out <- dplyr::mutate(
    data,
    hovertemplate = linePlotHoverTemplate(
      title = param,
      paramValueVar = aval,
      paramValueVarUnits = avalu,
      timeLab = "Study Day",
      timeVar = ady,
      colorLab = "Lab Category",
      colorVar = lbcat,
      shapeLab = "Range Indicator",
      shapeVar = anrind
    )
  )
  
  expect_equal(
    out$hovertemplate,
    c(
      "<b>Albumin</b><br><br><i>Value</i>: 1 g/L<br><i>Study Day</i>: 11<br><i>Lab Category</i>: Chemistry<br><i>Range Indicator</i>: NORMAL<br><extra></extra>",
      "<b>Albumin</b><br><br><i>Value</i>: 2 g/L<br><i>Study Day</i>: 12<br><i>Lab Category</i>: Chemistry<br><i>Range Indicator</i>: HIGH<br><extra></extra>"
    )
  )
  
  # normal range
  data <- data.frame(
    param = rep("Albumin", 2),
    aval = 1:2,
    ady = 11:12,
    anrlo1 = rep(0, 2),
    anrhi1 = rep(3, 2)
  )
  
  paramValueRangeVar <- c("anrlo1", "anrhi1")
  
  out <- dplyr::mutate(
    data,
    hovertemplate = linePlotHoverTemplate(
      title = param,
      paramValueVar = aval,
      timeLab = "Study Day",
      timeVar = ady,
      uln = !!rlang::sym(paramValueRangeVar[1]),
      lln = !!rlang::sym(paramValueRangeVar[2])
    )
  )
  
  expect_equal(
    out$hovertemplate,
    c(
      "<b>Albumin</b><br><br><i>Value</i>: 1<br><i>Study Day</i>: 11<br><i>ULN</i>: 0<br><i>LLN</i>: 3<br><extra></extra>",
      "<b>Albumin</b><br><br><i>Value</i>: 2<br><i>Study Day</i>: 12<br><i>ULN</i>: 0<br><i>LLN</i>: 3<br><extra></extra>"
    )
  )
  
  # additional variables
  data <- data.frame(
    param = rep("Albumin", 3),
    aval = 1:3,
    ady = 11:13,
    anrlo1 = rep(0, 3),
    anrhi1 = rep(3, 3),
    adt = c("2020-01-01", "2022-02-02", ""),
    avisit = c("Visit 1", "Visit 2", NA_character_)
  )
  
  add_vars <- list(
    Date = "adt",
    Visit = "avisit"
  )
  
  add_vars <- purrr::map(add_vars, \(x) data[[x]])
  
  out <- dplyr::mutate(
    data,
    hovertemplate = linePlotHoverTemplate(
      title = param,
      paramValueVar = aval,
      timeLab = "Study Day",
      timeVar = ady,
      add_vars = add_vars
    )
  )
  
  expect_equal(
    out$hovertemplate,
    c(
      "<b>Albumin</b><br><br><i>Value</i>: 1<br><i>Study Day</i>: 11<br><i>Date</i>: 2020-01-01<br><i>Visit</i>: Visit 1<br><extra></extra>",
      "<b>Albumin</b><br><br><i>Value</i>: 2<br><i>Study Day</i>: 12<br><i>Date</i>: 2022-02-02<br><i>Visit</i>: Visit 2<br><extra></extra>",
      "<b>Albumin</b><br><br><i>Value</i>: 3<br><i>Study Day</i>: 13<br><extra></extra>"
    )
  )
})


#### plotlyLinePlot() ####

test_that("plotlyLinePlot() generates a line plot", {
  
  ### NOTE: these are visual tests only for the time being
  
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
  data[, "paramFacetVar"] <- interactionWithMissing(data = data, vars = paramNameVar, varSep = paramVarSep)
  
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
    title = "Laboratory test measurements: actual value",
    xLab = "Study Day",
    add_vars = list(
      Date = "LBDTC",
      `Range Indicator` = "LBNRIND"
    )
  )
  
  # plot without units
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
    )
  )
  
  # plot without colors
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
      `Range Indicator` = "LBNRIND"
    )
  )
  
  # plot without shapes
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
  
  subjectProfileLinePlot(
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
    shapeSize = rel(1),
    plotly = TRUE,
    plotly_args = list(
      paramValueVarUnits = "LBSTRESU"
    )
  )
  
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
  
  subjectProfileLinePlot(
    data = data,
    paramNameVar = "LBTEST",
    paramValueVar = "LBSTRESN", 
    timeVar = "LBDY",
    title = "Laboratory test measurements: actual value",
    labelVars = labelVarsSDTM,
    shapeSize = 7,
    plotly = TRUE
  )
  
})
