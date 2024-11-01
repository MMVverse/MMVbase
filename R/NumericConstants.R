#' Get numeric constants as a named list
#'
#' @return a named list.
#' @examples
#'
#' get_NumericConstantsAsList()$MicrosomesDog_mgProteinPergLiver
#'
#' @export
get_NumericConstantsAsList <- function() {
  dfConstants <- get_NumericConstants()
  listConstants <- as.list(dfConstants$Value)
  names(listConstants) <- dfConstants$Name_unit
  listConstants
}

#' Get numeric constants such as growth rates, body weights, and cell numbers
#'
#' @return a data.frame with Parameter, Value, Unit, and Source columns.
#' @examples
#'
#' dfConstants <- get_NumericConstants()
#' dfConstants[
#'   startsWith(dfConstants$Name_unit, "BodyWeight"), c("Parameter", "Value", "Unit", "Source")]
#'
#' @importFrom tibble tribble
#' @export
get_NumericConstants <- function() {
  tribble(
    ~Name_unit,                                      ~Parameter,    ~Value,                     ~Unit,                                                 ~Source,
    "PLGrowthRateInVitro_1PerHour",            "In-vitro ln-parasitemia growth rate", 0.04797052,                   "1/hour",                                                       "",
    "PLGrowthRatePatient_1PerHour",              "Patient  ln-parasitemia growth rate",       0.07,                   "1/hour",                                                       "",
    "BodyWeightMouse_kg",                                            "M body weight",      0.025,                       "kg",                                                       "",
    "BodyWeightRat_kg",                                              "R body weight",       0.25,                       "kg",                                                       "",
    "BodyWeightDog_kg",                                              "D body weight",        9.8,                       "kg",                                                       "",
    "BodyWeighPatient_kg",                                     "Patient body weight",         55,                       "kg",                                                       "",
    
    
    # BEGIN Constants provided in e-mail from Dr. Peter Webborn; Communication/20220901 Peter Webborn Scaling factors in MMVSola.eml
    "HepatocytesMouse_MillionCellsPergLiver",            "M number of hepatic cells",        135,          "10^6/g of liver", "Tess et al, Pharmaceutical Research (2022) 39:1615-1632",
    "HepatocytesRat_MillionCellsPergLiver",              "R number of hepatic cells",        108,          "10^6/g of liver", "Tess et al, Pharmaceutical Research (2022) 39:1615-1632",
    "HepatocytesDog_MillionCellsPergLiver",              "D number of hepatic cells",        170,          "10^6/g of liver", "Tess et al, Pharmaceutical Research (2022) 39:1615-1632",
    "HepatocytesPatient_MillionCellsPergLiver",      "Patient number of hepatic cells",        125,          "10^6/g of liver",      "Liang et al, Drug Metab Dispos (2020) 48:1283-1292",
    "MicrosomesMouse_mgProteinPergLiver",                 "M microsomes per g liver",         48, "mg of protein/g of liver", "Tess et al, Pharmaceutical Research (2022) 39:1615-1632",
    "MicrosomesRat_mgProteinPergLiver",                   "R microsomes per g liver",         46, "mg of protein/g of liver", "Tess et al, Pharmaceutical Research (2022) 39:1615-1632",
    "MicrosomesDog_mgProteinPergLiver",                   "D microsomes per g liver",         41, "mg of protein/g of liver", "Tess et al, Pharmaceutical Research (2022) 39:1615-1632",
    "MicrosomesPatient_mgProteinPergLiver",           "Patient microsomes per g liver",         40, "mg of protein/g of liver", "Vasilogianni et al, Drug Metab Dispos (2021) 49:563-571",
    "LiverMouse_gPerkg",                              "M g liver per kg body weight",       51.4,                     "g/kg", "Tess et al, Pharmaceutical Research (2022) 39:1615-1632",
    "LiverRat_gPerkg",                                "R g liver per kg body weight",         36,                     "g/kg", "Tess et al, Pharmaceutical Research (2022) 39:1615-1632",
    "LiverDog_gPerkg",                                "D g liver per kg body weight",         30,                     "g/kg", "Tess et al, Pharmaceutical Research (2022) 39:1615-1632",
    "LiverPatient_gPerkg",                        "Patient g liver per kg body weight",       25.5,                     "g/kg",      "Liang et al, Drug Metab Dispos (2020) 48:1283-1292",
    "QhMouse_mLPerMinPerkg",                                  "M hepatic blood flow",        126,                "mL/min/kg",                                                        "",
    "QhRat_mLPerMinPerkg",                                    "R hepatic blood flow",         77,                "mL/min/kg",                                                        "",
    "QhDog_mLPerMinPerkg",                                    "D hepatic blood flow",         56,                "mL/min/kg",                                                        "",
    "QhPatient_mLPerMinPerkg",                          "Patient hepatic blood flow",       20.7,                "mL/min/kg",                                                        ""
    # END Constants provided in e-mail from Dr. Peter Webborn; Communication/20220901 Peter Webborn Scaling factors in MMVSola.eml
    
  )
}