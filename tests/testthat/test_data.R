library(testthat)
library(MMVbase)

# Test for get_IXGDFtoRemove function
test_that("get_IXGDFtoRemove works correctly", {
  data <- data.frame(
    USUBJID = rep(1:3, each = 6),
    NAME = rep("log(Parasitemia Total)", 18),
    TIME = rep((1:6)*72, 3),
    VALUE = c(10, 4, 3, 4, 6, 5, 12, 10, 8, 5, 4, 3, 14, 3, 3, 3, 3, 3),
    LLOQ = rep(5, 18),
    IXGDF = 1:18
  )
  
  expect_equal(get_IXGDFtoRemove(data, obNAME = "log(Parasitemia Total)", removeType = "ToLastOb"), c(6, 12, 18))
  expect_equal(get_IXGDFtoRemove(data, obNAME = "log(Parasitemia Total)", removeType = "RemoveCure"), c(6, 10, 11, 12, 14, 15, 16, 17, 18))
  expect_equal(get_IXGDFtoRemove(data, obNAME = "log(Parasitemia Total)", removeType = "Cure1Week"), c(12, 16, 17, 18))
})

# Test for IQRtableToDataFrame function
test_that("IQRtableToDataFrame works correctly", {
  sample_text <- c(
    "Sample Table Title",
    "Column1|Column2|Column3",
    "Row1Col1|Row1Col2|Row1Col3",
    "Row2Col1|Row2Col2|Row2Col3",
    "Sample Footer"
  )
  
  writeLines(sample_text, "sample_table.txt")
  
  result <- IQRtableToDataFrame("sample_table.txt")
  unlink("sample_table.txt")
  
  expect_equal(result$xTitle, "Sample Table Title")
  expect_equal(result$xFooter, "Sample Footer")
  expect_equal(result$dataFrame, data.frame(Column1 = c("Row1Col1", "Row2Col1"), Column2 = c("Row1Col2", "Row2Col2"), Column3 = c("Row1Col3", "Row2Col3"), stringsAsFactors = FALSE))
})

# Test for Check_MissingDatabyNAME function
test_that("Check_MissingDatabyNAME works correctly", {
  dataMaster <- data.frame(
    USUBJID = rep(1:5, each = 3),
    NAME = rep(c("Var1", "Var2", "Var3"), 5),
    VALUE = c(1, NA, 3, 4, 5, NA, 7, 8, 9, NA, 11, 12, 13, 14, NA),
    VALUETXT = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O")
  )
  
  NAME <- c("Var1", "Var2", "Var3")
  
  result <- Check_MissingDatabyNAME(dataMaster, NAME)
  
  expect_equal(result$NAME, c("Var1", "Var2", "Var3"))
  expect_equal(result$N, c(1, 1, 2))
})

# Test for check_dataGeneralMMV function
test_that("check_dataGeneralMMV works correctly", {
  dataGeneral <- data.frame(
    USUBJID = rep(1:3, each = 4),
    CENTER = c(101, 101, 102, 102, 103, 103, 104, 104, 105, 105, 106, 106),
    CENTERNAME = c("Center A", "Center A", "Center B", "Center B", "Center C", "Center C", "Center D", "Center D", "Center E", "Center E", "Center F", "Center F"),
    VALUE = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120)
  )
  
  CenterList <- list(
    "Center A" = 101,
    "Center B" = 102,
    "Center C" = 103,
    "Center D" = 104,
    "Center E" = 105,
    "Center F" = 106
  )
  
  result <- check_dataGeneralMMV(dataGeneral, CenterList)
  
  expect_equal(result$CENTER, c(101, 101, 102, 102, 103, 103, 104, 104, 105, 105, 106, 106))
  expect_equal(result$CENTERNAME, c("Center A", "Center A", "Center B", "Center B", "Center C", "Center C", "Center D", "Center D", "Center E", "Center E", "Center F", "Center F"))
})

# Test for swapName_MMVnameToName function
test_that("swapName_MMVnameToName works correctly", {
  data <- data.frame(
    USUBJID = rep(1:3, each = 4),
    COMPOUND = c("MMV123", "MMV123", "MMV456", "MMV456", "MMV789", "MMV789", "MMV101", "MMV101", "MMV112", "MMV112", "MMV123", "MMV123"),
    TRTNAME = c("MMV123 12mg", "MMV123 12mg", "MMV456 20mg", "MMV456 20mg", "MMV789 25mg", "MMV789 25mg", "MMV101 30mg", "MMV101 30mg", "MMV112 50mg", "MMV112 50mg", "MMV123 50mg", "MMV123 50mg"),
    NAME = c("Conc MMV123", "Conc MMV123", "Conc MMV456", "Conc MMV456", "Conc MMV789", "Conc MMV789", "Conc MMV101", "Conc MMV101", "Conc MMV112", "Conc MMV112", "Conc MMV123", "Conc MMV123"),
    VALUE = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120)
  )
  
  CompoundList <- list(
    list(MMVname = "MMV123", Name = "CompoundA"),
    list(MMVname = "MMV456", Name = "CompoundB"),
    list(MMVname = "MMV789", Name = "CompoundC"),
    list(MMVname = "MMV101", Name = "CompoundD"),
    list(MMVname = "MMV112", Name = "CompoundE")
  )
  
  result <- swapName_MMVnameToName(data, CompoundList)
  
  expect_equal(result$COMPOUND, c("CompoundA", "CompoundA", "CompoundB", "CompoundB", "CompoundC", "CompoundC", "CompoundD", "CompoundD", "CompoundE", "CompoundE", "CompoundA", "CompoundA"))
})

# Test for swapName_NameToMMVname function
test_that("swapName_NameToMMVname works correctly", {
  data <- data.frame(
    USUBJID = rep(1:3, each = 4),
    COMPOUND = c("CompoundA", "CompoundA", "CompoundB", "CompoundB", "CompoundC", "CompoundC", "CompoundD", "CompoundD", "CompoundE", "CompoundE", "CompoundA", "CompoundA"),
    TRTNAME = c("CompoundA 12mg", "CompoundA 12mg", "CompoundB 20mg", "CompoundB 20mg", "CompoundC 25mg", "CompoundC 25mg", "CompoundD 30mg", "CompoundD 30mg", "CompoundE 50mg", "CompoundE 50mg", "CompoundA 50mg", "CompoundA 50mg"),
    NAME = c("Conc CompoundA", "Conc CompoundA", "Conc CompoundB", "Conc CompoundB", "Conc CompoundC", "Conc CompoundC", "Conc CompoundD", "Conc CompoundD", "Conc CompoundE", "Conc CompoundE", "Conc CompoundA", "Conc CompoundA"),
    VALUE = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120)
  )
  
  CompoundList <- list(
    list(MMVname = "MMV123", Name = "CompoundA"),
    list(MMVname = "MMV456", Name = "CompoundB"),
    list(MMVname = "MMV789", Name = "CompoundC"),
    list(MMVname = "MMV101", Name = "CompoundD"),
    list(MMVname = "MMV112", Name = "CompoundE")
  )
  
  result <- swapName_NameToMMVname(data, CompoundList)
  
  expect_equal(result$COMPOUND, c("MMV123", "MMV123", "MMV456", "MMV456", "MMV789", "MMV789", "MMV101", "MMV101", "MMV112", "MMV112", "MMV123", "MMV123"))
})

# Test for transform_dataFrame_WideToLong function
test_that("transform_dataFrame_WideToLong works correctly", {
  keyPDParams_Static <- data.frame(
    MIC.MEAN = 3.5,
    MIC.MIN = 2.5,
    MIC.MAX = 4.5,
    MIC.N = 10,
    MPC90.MEAN = 8.2,
    MPC90.MIN = 7.3,
    MPC90.MAX = 10.1,
    MPC90.N = 10,
    EC50.MEAN = 6.3,
    EC50.MIN = 4.5,
    EC50.MAX = 8.0,
    EC50.N = 10,
    PRR48.MEAN = 3.2,
    PRR48.MIN = 2.5,
    PRR48.MAX = 4.3,
    PRR48.N = 10
  )
  keyPDParams_Dynamic <- keyPDParams_Static * 2
  
  keyPDParams_Static$PARAMETERTYPE <- "Static"
  keyPDParams_Static$METHODCLASS <- "SCID Modelling"
  keyPDParams_Static$METHOD <- "EMAX"
  keyPDParams_Static$NLMETEM <- "In Vivo"
  keyPDParams_Static$BINDINGCONVERSION <- "-"
  keyPDParams_Static$DOSE <- "-"
  
  keyPDParams_Dynamic$PARAMETERTYPE <- "Dynamic"
  keyPDParams_Dynamic$METHODCLASS <- "SCID Modelling"
  keyPDParams_Dynamic$METHOD <- "EMAX"
  keyPDParams_Dynamic$NLMETEM <- "In Vivo"
  keyPDParams_Dynamic$BINDINGCONVERSION <- "-"
  keyPDParams_Dynamic$DOSE <- "-"
  
  KeyPDpar <- rbind(keyPDParams_Static, keyPDParams_Dynamic)
  
  KeyPDPar_long <- transform_dataFrame_WideToLong(KeyPDpar,
    key = "key",
    value = c("MEAN", "MIN", "MAX", "N"),
    colMaster = c("PARAMETERTYPE", "METHODCLASS", "METHOD", "NLMETEM", "BINDINGCONVERSION", "DOSE")
  )
  
  expect_equal(nrow(KeyPDPar_long), 8)
  expect_equal(ncol(KeyPDPar_long), 11)
})

# Test for transform_dataFrame_LongToWide function
test_that("transform_dataFrame_LongToWide works correctly", {
  KeyPDPar_long <- tidyr::tribble(
    ~PARAMETERTYPE,      ~METHODCLASS,  ~METHOD,  ~NLMETEM, ~BINDINGCONVERSION, ~DOSE,   ~key,   ~MEAN, ~MIN, ~MAX,  ~N,
    "Dynamic", "SCID Modelling",   "EMAX", "In Vivo",               "-",    "-",  "EC50", 12.6,  9.0, 16.0, 20,
    "Dynamic", "SCID Modelling",   "EMAX", "In Vivo",               "-",    "-",   "MIC",  7.0,  5.0,  9.0, 20,
    "Dynamic", "SCID Modelling",   "EMAX", "In Vivo",               "-",    "-", "MPC90", 16.4, 14.6, 20.2, 20,
    "Dynamic", "SCID Modelling",   "EMAX", "In Vivo",               "-",    "-", "PRR48",  6.4,  5.0,  8.6, 20,
    "Static", "SCID Modelling",   "EMAX", "In Vivo",               "-",    "-",  "EC50",  6.3,  4.5,  8.0, 10,
    "Static", "SCID Modelling",   "EMAX", "In Vivo",               "-",    "-",   "MIC",  3.5,  2.5,  4.5, 10,
    "Static", "SCID Modelling",   "EMAX", "In Vivo",               "-",    "-", "MPC90",  8.2,  7.3, 10.1, 10,
    "Static", "SCID Modelling",   "EMAX", "In Vivo",               "-",    "-", "PRR48",  3.2,  2.5,  4.3, 10
  )
  
  KeyPDPar <- transform_dataFrame_LongToWide(
    KeyPDPar_long,
    key = "key",
    value = c("MEAN", "MIN", "MAX", "N"),
    colMaster = c("PARAMETERTYPE", "METHODCLASS", "METHOD", "NLMETEM", "BINDINGCONVERSION", "DOSE")
  )
  
  expect_equal(nrow(KeyPDPar), 2)
  expect_equal(ncol(KeyPDPar), 22)
})
