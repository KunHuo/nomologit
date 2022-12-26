#' Hepatocellular carcinoma (HCC) data
#'
#' @description
#' Survival in patients with HCC from the the Surveillance, Epidemiology, and End Results (SEER) dataset.
#'
#' @format A data.frame with 15985 rows and 14 variables.
#' * ID: Patient's ID.
#' * Race: Race.
#' * Sex: Gender.
#' * Age: Age at diagnosis (years).
#' * Grade: Histological grading.
#' * AJCC_T: T stage of AJCC.
#' * AJCC_N: N stage of AJCC.
#' * AJCC_M: M stage of AJCC.
#' * Surg_Prim: Whether the primary lesion should be treated by surgery.
#' * tumor_size: Tumor size (mm).
#' * lymph_nodes: Number of lymph nodes.
#' * metastasis: Metastasis.
#' * time: Survival time (month).
#' * status: Censoring status.
#'
#' @examples
#' head(HCC, 10)
"HCC"

