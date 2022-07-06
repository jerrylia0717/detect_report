#' renderFlow
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#' @importFrom tidyr pivot_longer pivot_wider nest
#' @importFrom dplyr mutate left_join case_when
#' @importFrom purrr map_dbl pwalk
#' @importFrom readxl read_xlsx
#'
renderFlow <- function(rmdFile,
                       combineData,
                       saveDir) {
  product_code <-
    str_split(basename(rmdFile), '_')[[1]][1]
  report_format <-
    str_remove(str_split(basename(rmdFile), '_')[[1]][2], ".Rmd")
  ref_file <-
    str_c(
      dirname(rmdFile),"/",
      product_code, "_reference.xlsx", collapse = ""
    )

  inp_file <-
    str_c(
      dirname(rmdFile),"/",
      product_code, "_interpretation.xlsx", collapse = ""
    )

  if (!file.exists(ref_file)) {
    showNotification("Reference file does not exist!", type = "error")
    stop("Reference file does not exist!")
  } else {
    reference <- read_xlsx(ref_file)
  }

  if (product_code %in% c("DX2056", "DX2057", "DX2058", "DX2059")) {

    pwalk(
      .l = list(
        ..1 = combineData$`Sample Name`,
        ..2 = combineData$data,
        ..3 = combineData$sampleInfo
      ),
      .f = ~ .renderSingle(
        rmdFile = rmdFile,
        sampleId = ..1,
        data = ..2,
        sampleInfo = ..3,
        reference = reference,
        interpretation = NA,
        saveDir = saveDir,
        report_format = report_format,
        product_code = product_code)
    )

  } else if (product_code %in% c("DX1597", "DX1683", "DX1710", "DX1736")) {
    if (!file.exists(inp_file)) {
      showNotification("Interpretatioin file does not exist!", type = "error")
      stop("Interpretatioin file does not exist!")
    } else {
      interpretation <- read_xlsx(inp_file)
    }
    pwalk(
      .l = list(
        ..1 = combineData$`Sample Name`,
        ..2 = combineData$data,
        ..3 = combineData$sampleInfo
      ),
      .f = ~ .renderSingle(
        rmdFile = rmdFile,
        sampleId = ..1,
        data = ..2,
        sampleInfo = ..3,
        reference = reference,
        interpretation = interpretation,
        saveDir = saveDir,
        report_format = report_format,
        product_code = product_code)
    )
  } else {
    showNotification("Unsupported product type!", type = "error")
    stop("Unsupported product type!")
  }
  file.remove(list.files(dirname(rmdFile),pattern = ".log|.tex",full.names = T))
}

#' .renderSingle
#' @importFrom stringr str_c
#' @importFrom dplyr case_when
#' @importFrom rmarkdown render
.renderSingle <- function(rmdFile,
                          sampleId,
                          data,
                          sampleInfo,
                          reference,
                          interpretation,
                          saveDir,
                          report_format,
                          product_code) {
  output_file <- case_when(
    report_format == "PDF" ~ str_c(sampleId,"_",product_code, "_正式报告_中文_其他.pdf", sep = ""),
    report_format == "HTML" ~ str_c(sampleId,".html",sep = "")
  )

  showNotification("running...", type = "message")

  render(
    rmdFile,
    params = list(
      sampleId = sampleId,
      data = data,
      sampleInfo = sampleInfo,
      reference = reference,
      interpretation = interpretation
    ),
    output_dir = saveDir,
    output_file = output_file,
    encoding = "UTF-8",
    clean = TRUE
  )
  if (file.exists(str_c(saveDir,output_file,sep = "/"))) {
    showNotification(str_c(sampleId,"succeed!",sep = ""), type = "message")
  } else {
    showNotification(str_c(sampleId,"defeated!",sep = ""), type = "error")
  }
}
