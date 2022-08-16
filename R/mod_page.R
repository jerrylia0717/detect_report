#' page UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyFiles shinyFilesButton shinyDirButton
#' @importFrom shinydashboard sidebarMenu menuItem
mod_page_ui <- function(id) {
  ns <- NS(id)
  tagList(sidebarMenu(
    menuItem(
      text = "Read configuration file",
      startExpanded = TRUE,
      shinyFilesButton(
        id = ns("rmd"),
        label = "Rmd file",
        title = "Rmd file",
        multiple = FALSE,
        class = "btn-danger",
        style = "width:80%"
      )
    ),
    menuItem(
      text = "Read data",
      startExpanded = TRUE,
      shinyFilesButton(
        id = ns("detectdata"),
        label = "detect data",
        title = "detect data",
        multiple = FALSE,
        class = "btn-danger",
        style = "width:80%"
      ),
      shinyFilesButton(
        id = ns("sampleinfo"),
        label = "sample infos",
        title = "sample infos",
        multiple = FALSE,
        style = "width:80%"
      )
    ),
    menuItem(
      text = "Select save dir",
      startExpanded = TRUE,
      shinyDirButton(
        id = ns("save_dir"),
        label = "save dir",
        title = 'configurations',
        class = "btn-danger",
        style = "width:80%"
      )
    )
  ))
}

#' mod_pagebody_ui UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom shinydashboard box
#' @importFrom DT DTOutput
mod_pagebody_ui <- function(id) {
  ns <- NS(id)
  tagList(fluidRow(
    box(
      title = "Input check",
      solidHeader = TRUE,
      width = 2,
      height = "500px",
      collapsible = T,
      collapsed = F,
      status = "primary",
      checkboxInput(
        inputId = ns("check_rmd"),
        label = "Rmd file",
        value = FALSE
      ),
      checkboxInput(
        inputId = ns("check_ref"),
        label = "Reference",
        value = FALSE
      ),
      checkboxInput(
        inputId = ns("check_detectdata"),
        label = "Detect data",
        value = FALSE
      ),
      checkboxInput(
        inputId = ns("check_sampleinfo"),
        label = "Sample info",
        value = FALSE
      ),
      checkboxInput(
        inputId = ns("check_interpre"),
        label = "Interpretation(optional)",
        value = FALSE
      ),
      checkboxInput(
        inputId = ns("check_savedir"),
        label = "Save dir",
        value = FALSE
      )
    ),
    box(
      title = "sample info check",
      solidHeader = TRUE,
      width = 10,
      height = "500px",
      collapsible = T,
      collapsed = F,
      status = "primary",
      DTOutput(ns("sampleInfo"), width = "100%")
    )
  ),
  actionButton(
    inputId = ns("start"),
    label = "Start generating",
    class = "btn-lg btn-success",
    width = "100%"
  ))
}

#' page Server Functions
#'
#' @noRd
#' @importFrom shinyFiles getVolumes shinyDirChoose parseDirPath shinyFileChoose parseFilePaths
#' @importFrom fs path_home
#' @importFrom DT renderDT datatable
#' @importFrom lubridate ymd
#' @importFrom purrr map_chr
#' @importFrom stringr str_split
#' @import tidyr
#' @import dplyr
mod_page_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    volumes <- c(Home = path_home(),
                 "R Installation" = R.home(),
                 getVolumes()())
    # get rmd file
    shinyFileChoose(
      input = input,
      id = "rmd",
      root = volumes,
      session = session,
      filetypes = "Rmd",
      restrictions = system.file(package = "base")
    )

    inputRmdFile <- reactive({
      validate(need(!is.integer(input$rmd),
                    "No file exist!"))
      parseFilePaths(volumes, input$rmd)$datapath
    })

    # get sample info file
    shinyFileChoose(
      input = input,
      id = "sampleinfo",
      root = volumes,
      session = session,
      filetypes = "xlsx",
      restrictions = system.file(package = "base")
    )

    inputSampleInfoFile <- reactive({
      validate(need(!is.integer(input$sampleinfo),
                    "No file exist!"))
      parseFilePaths(volumes, input$sampleinfo)$datapath
    })
    # get detect data file
    shinyFileChoose(
      input = input,
      id = "detectdata",
      root = volumes,
      session = session,
      filetypes = "xlsx",
      restrictions = system.file(package = "base")
    )

    inputDetectDataFile <- reactive({
      validate(need(!is.integer(input$detectdata),
                    "No file exist!"))
      parseFilePaths(volumes, input$detectdata)$datapath
    })
    # get report save dir
    shinyDirChoose(
      input = input,
      id = "save_dir",
      root = volumes,
      allowDirCreate = FALSE,
      session = session,
      restrictions = system.file(package = "base")
    )

    inputSaveDir <- reactive({
      validate(need(!is.integer(input$save_dir),
                    "No dir!"))
      parseDirPath(volumes, input$save_dir)
    })
    # read detect data into r
    detectData <- reactive({
      req(input$detectdata)
      read_xlsx(inputDetectDataFile())
    })
    # read sample info into r
    sampleInfo <- reactive({
      req(input$sampleinfo)
      product_code <-
        str_split(basename(inputRmdFile()), '_')[[1]][1]
      if (product_code %in% c("DX2056", "DX2057", "DX2058", "DX2059")) {
        read_xlsx(inputSampleInfoFile()) %>%
          select(`Sample Name` = `实验室编号(样本编号)`,
                 `姓名`,
                 `采样日期`,
                 `性别`,
                 `入库时间`,
                 `出生日期`,
                 `联系电话`,
                 `送检单位`,
                 `年龄`) %>%
          mutate(`入库时间` = map_chr(.x = `入库时间`, .f = ~ as.character(ymd(.x))))
      } else if (product_code %in% c("DX1597", "DX1683", "DX1710", "DX1736")) {
        read_xlsx(inputSampleInfoFile()) %>%
          select(`Sample Name` = `绑定的样本编码`,
                 `性别`,
                 `年龄`)
      }
    })
    # display sample info using DT
    combineData <- reactive({
      req(detectData)
      req(sampleInfo)
      detectData_longer <- detectData() %>%
        mutate(across(-`Sample Name`,.fns = round,2))
        # pivot_longer(
        #   cols = -"Sample Name",
        #   names_to = "compound",
        #   values_to = "value"
        # ) %>%
        # mutate(value = map_dbl(.x = value, .f = ~ round(as.numeric(.x), 2))) %>%
        # pivot_wider(names_from = compound, values_from = value)

      data_combine <- detectData_longer %>%
        nest(data = -`Sample Name`) %>%
        inner_join(sampleInfo(), by = "Sample Name") %>%
        nest(sampleInfo = c(-`Sample Name`, -data))
    })

    output$sampleInfo <- renderDT({
      req(combineData)
      datatable(
        combineData() %>%
          unnest(data) %>%
          unnest(sampleInfo),
        selection = 'none',
        options = list(pageLength = 20, scrollY = "250px")
      )
    })
    # check input
    observe({
      updateCheckboxInput(
        inputId = "check_rmd",
        value = file.exists(inputRmdFile()),
        label = "Rmd file"
      )
    })
    observe({
      product_code <-
        str_split(basename(inputRmdFile()), '_')[[1]][1]
      ref_file <-
        str_c(
          dirname(inputRmdFile()),"/",
          product_code, "_reference.xlsx", collapse = ""
        )
      updateCheckboxInput(
        inputId = "check_ref",
        value = file.exists(ref_file),
        label = "Reference"
      )
    })
    observe({
      updateCheckboxInput(
        inputId = "check_detectdata",
        value = !is.null(detectData()),
        label = "Detect data"
      )
    })
    observe({
      updateCheckboxInput(
        inputId = "check_sampleinfo",
        value = !is.null(sampleInfo()),
        label = "Sample info"
      )
    })
    observe({
      product_code <-
        str_split(basename(inputRmdFile()), '_')[[1]][1]
      inp_file <-
        str_c(
          dirname(inputRmdFile()),"/",
          product_code, "_interpretation.xlsx", collapse = ""
        )
      updateCheckboxInput(
        inputId = "check_interpre",
        value = file.exists(inp_file),
        label = "Interpretation(optional)"
      )
    })
    observe({
      updateCheckboxInput(
        inputId = "check_savedir",
        value = dir.exists(inputSaveDir()),
        label = "Save dir"
      )
    })
    # observe({
    #   str(combineData())
    # })
    # generate report
    observeEvent(input$start, {
      req(input$rmd)
      req(detectData)
      req(sampleInfo)
      req(combineData)

      renderFlow(
        rmdFile = inputRmdFile(),
        combineData = combineData(),
        saveDir = inputSaveDir()
      )
    })
  })
}

## To be copied in the UI
# mod_page_ui("page_1")

## To be copied in the server
# mod_page_server("page_1")
