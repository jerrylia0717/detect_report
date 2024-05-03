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
mod_cardioSidebar_ui <- function(id) {
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

#' cardioReport UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_cardioReport_ui <- function(id){
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
      DTOutput(ns("sampleInfo"), width = "100%"),
      verbatimTextOutput(ns("failedSample"))
    )
  ),
  actionButton(
    inputId = ns("start"),
    label = "Start",
    class = "btn-lg btn-success",
    width = "100%"
  ))
}

#' cardioReport Server Functions
#'
#' @noRd
mod_cardioReport_server <- function(id){
    moduleServer(id, function(input, output, session) {
      ns <- session$ns
      volumes <- c(Home = path_home(),
                   "R Installation" = R.home(),
                   getVolumes()())
      # Configuration when installing on a new computer
      observeEvent(input$confNewPc,{
        download_fontawesome()
      })
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
        filetypes = c("xlsx","xls"),
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
                      ""))
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
        req(detectData)
        product_code <-
          str_split(basename(inputRmdFile()), '_')[[1]][1]
        # showNotification("Waiting...", type = "message")
        if (product_code %in% c("DX2056", "DX2057", "DX2058", "DX2059")) {
          read_xls(inputSampleInfoFile(),sheet = 2) %>%
            select(`Sample Name` = `样品编号`,
                   `姓名`,
                   `联系电话` = `电话`,
                   `采样日期` = `样品采集日期`,
                   `性别`,
                   `出生日期`,
                   `送检单位`,
                   `到样日期`,
                   `检测日期`) %>%
            filter(`Sample Name` %in% detectData()$`Sample Name`) %>%
            mutate(`年龄` = round(interval(ymd(`出生日期`), ymd(`采样日期`)) / years(1), 0)) %>%
            mutate(across(.cols = `联系电话`,.fns = as.character)) %>%
            mutate(across(.cols = contains("日期"),.fns = ymd)) %>%
            distinct()
        }
        # else if (product_code %in% c("DX1597", "DX1683", "DX1710", "DX1736")) {
        #   read_xlsx(inputSampleInfoFile()) %>%
        #     # select(`Sample Name` = `绑定的样本编码`, `性别`, `年龄`)
        #     select(`Sample Name` = `绑定的样本编码`,`姓名`,`性别`, `证件号`, `采样时间`, `出生日期`, `年龄`) %>%
        #     filter(`Sample Name` %in% detectData()$`Sample Name`) %>%
        #     mutate(`Sample Name` = toupper(`Sample Name`)) %>%
        #     mutate(`年龄` = pmap_dbl(
        #       .l = list(
        #         ..1 = `年龄`,
        #         ..2 = `出生日期`,
        #         ..3 = `采样时间`,
        #         ..4 = `证件号`
        #       ),
        #       .f = ~ case_when(
        #         !is.na(..1) ~ as.numeric(..1),
        #         all(!is.na(..2),!is.na(..3)) ~ interval(ymd(..2), ymd(str_split(..3," ")[[1]][1])) / years(1),
        #         all(!is.na(..4),!is.na(..3)) ~ interval(ymd(substring(
        #           as.character(..4), 7, 14
        #         )), ymd(str_split(..3," ")[[1]][1])) / years(1)
        #       )
        #     )) %>%
        #     mutate(across(.cols = `年龄`, .fns = round, 0))
        # }
      })
      # display sample info using DT
      combineData <- reactive({
        req(detectData)
        req(sampleInfo)
        detectData_longer <- detectData() %>%
          mutate(across(-`Sample Name`,.fns = round,2))

        data_combine <- detectData_longer %>%
          nest(data = -`Sample Name`) %>%
          left_join(sampleInfo(), by = "Sample Name")
        # filter(!is.na(`年龄`),!is.na(`性别`)) %>%
        # nest(sampleInfo = c(-`Sample Name`, -data))
      })

      output$sampleInfo <- renderDT({
        req(combineData)
        datatable(
          combineData() %>% select(-data),
          selection = 'none',
          options = list(pageLength = 20, scrollY = "250px")
        )
      })
      output$failedSample <- renderText({
        failedSamples <- combineData() %>% filter(is.na(`年龄`)) %>% select(`Sample Name`) %>% pull()
        paste0("Failed:",paste0(failedSamples,collapse = ","),sep = " ")
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
      # observe({
      #   product_code <-
      #     str_split(basename(inputRmdFile()), '_')[[1]][1]
      #   inp_file <-
      #     str_c(
      #       dirname(inputRmdFile()),"/",
      #       product_code, "_interpretation.xlsx", collapse = ""
      #     )
      #   updateCheckboxInput(
      #     inputId = "check_interpre",
      #     value = file.exists(inp_file),
      #     label = "Interpretation(optional)"
      #   )
      # })
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
          combineData = combineData() %>%
            filter(!is.na(`年龄`)) %>%
            nest(sampleInfo = c(-`Sample Name`, -data)),
          saveDir = inputSaveDir()
        )
      })
    })
  }

## To be copied in the UI
# mod_cardioReport_ui("cardioReport_1")

## To be copied in the server
# mod_cardioReport_server("cardioReport_1")
