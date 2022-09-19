# data <- read_xlsx("D:/outputs/test_DX1597_detectData.xlsx") %>%
#   slice(1) %>%
#   select(-`Sample Name`)
#
# sampleInfo <- read_xlsx("D:/outputs/test_sampleinfo.xlsx") %>%
#   slice(1) %>%
#   mutate(`检测日期` = ymd("20220711")) %>%
#   mutate(`出生日期` = ymd(`出生日期`))
#
# reference <- read_xlsx("D:/outputs/configFiles/DX1597_reference.xlsx")
#
# interpreta <- read_xlsx("D:/outputs/configFiles/DX1597_interpretation.xlsx")
#
# dateInterval <-
#   interval(sampleInfo$"出生日期", sampleInfo$"检测日期") / years(1)
#
# if (dateInterval >= 1) {
#   reference_filter <- reference %>%
#     filter(
#       timeUnit == "year",
#       dateInterval > age_lower,
#       dateInterval <= age_upper,
#       gender == sampleInfo$"性别"
#     )
# } else if (dateInterval < 1) {
#   reference_filter_year <- reference %>%
#     filter(timeUnit == "year",
#            dateInterval > age_lower,
#            dateInterval <= age_upper,
#            gender == sampleInfo$"性别")
#   reference_filter_month <- reference %>%
#     filter(
#       timeUnit == "month",
#       dateInterval > age_lower,
#       dateInterval <= age_upper,
#       gender == sampleInfo$"性别"
#     )
#   reference_filter <-
#     bind_rows(reference_filter_year, reference_filter_month)
# }
#
# detectData <- temp_data %>%
#   pivot_longer(cols = everything(),
#                names_to = "compound",
#                values_to = "value")
#
#
#
# ch_compound_levels1 = c("11-脱氧皮质醇","11-脱氧皮质酮","21-脱氧皮质醇","氢化可的松","皮质酮","可的松","醛固酮")
# plot1 <- trans2show_nutrition_ggplot(
#   data = detectData %>%
#     left_join(reference_filter,by = "compound") %>%
#     filter(ch_compound %in% ch_compound_levels1) %>%
#     select(compound,value),
#   reference = reference_filter,
#   barWidth = 0.2,
#   textSize = 12,
#   ch_compound_levels = ch_compound_levels1
# )
# ch_compound_levels2 <- c("脱氢表雄酮","雄烯二酮","睾酮")
# plot2 <- trans2show_nutrition_ggplot(
#   data = detectData %>%
#     left_join(reference_filter,by = "compound") %>%
#     filter(ch_compound %in% ch_compound_levels2) %>%
#     select(compound,value),
#   reference = reference_filter,
#   barWidth = 0.2,
#   textSize = 12,
#   ch_compound_levels = ch_compound_levels2
# )
# ch_compound_levels3 <- c("雌酮","雌二醇")
# plot3 <- trans2show_nutrition_ggplot(
#   data = detectData %>%
#     left_join(reference_filter,by = "compound") %>%
#     filter(ch_compound %in% ch_compound_levels3) %>%
#     select(compound,value),
#   reference = reference_filter,
#   barWidth = 0.2,
#   textSize = 12,
#   ch_compound_levels = ch_compound_levels3
# )
# ch_compound_levels4 <- c("孕酮","17-羟孕酮")
# plot4 <- trans2show_nutrition_ggplot(
#   data = detectData %>%
#     left_join(reference_filter,by = "compound") %>%
#     filter(ch_compound %in% ch_compound_levels4) %>%
#     select(compound,value),
#   reference = reference_filter,
#   barWidth = 0.2,
#   textSize = 12,
#   ch_compound_levels = ch_compound_levels4
# )
# render(
#   "D:/outputs/configFiles/DX1597_HTML.Rmd",
#   params = list(
#     sampleId = "sw122",
#     data = temp_data,
#     sampleInfo = sampleInfo,
#     reference = reference,
#     interpretation = interpreta
#   ),
#   output_dir = "D:/outputs/",
#   output_file = "jisu.html",
#   encoding = "UTF-8",
#   clean = TRUE
# )
#
# ch_compound_levels <- c(
#   ch_compound_levels1,
#   ch_compound_levels2,
#   ch_compound_levels3,
#   ch_compound_levels4
# )
#
# classInfo <-
#   tibble(
#     ch_compound = c(
#       ch_compound_levels1,
#       ch_compound_levels2,
#       ch_compound_levels3,
#       ch_compound_levels4
#     ),
#     type = c(rep("免疫调节激素", 7),
#              rep("雄性激素", 3),
#              rep("雌性激素", 2),
#              rep("孕激素", 2))
#   )
#
# transformer <- transformer %>%
#   left_join(classInfo,by = "ch_compound")
#
# fig1 <- fig +
#   facet_wrap(type~.,ncol = 1,scales = "free_y")


# data <- read_xlsx("D:/outputs/test_DX1736_detectData.xlsx") %>%
#   mutate(across(-`Sample Name`,.fns = round,3))
# sampleInfo <- read_xlsx("D:/outputs/test_sampleinfo.xlsx")
# data_combine <- data %>%
#   nest(data = -`Sample Name`) %>%
#   left_join(sampleInfo, by = c("Sample Name" = "实验室编号(样本编号)")) %>%
#   nest(sampleInfo = c(-`Sample Name`, -data))
#
# pwalk(
#   .l = list(
#     ..1 = data_combine$`Sample Name`,
#     ..2 = data_combine$data,
#     ..3 = data_combine$sampleInfo
#   ),
#   .f = ~ render(
#     "D:/outputs/configFiles/DX1736_HTML.Rmd",
#     params = list(
#       sampleId = ..1,
#       data = ..2,
#       sampleInfo = ..3,
#       reference = dx1736_ref,
#       interpretation = dx1736_inp
#     ),
#     output_dir = "D:/outputs/",
#     output_file = paste(..1,".html",sep = ""),
#     encoding = "UTF-8",
#     clean = TRUE
#   )
# )

# data <- read_xlsx("D:/outputs/test_DX1597_detectData.xlsx") %>%
#   mutate(across(-`Sample Name`,.fns = round,3)) %>%
#   slice(1) %>%
#   select(-`Sample Name`) %>%
#   pivot_longer(cols = everything(),names_to = "compound",values_to = "value")
#
#
# dx1507_ref <- read_xlsx("D:/outputs/configFiles/DX1597_reference.xlsx")
# dx1507_inp <- read_xlsx("D:/outputs/configFiles/DX1597_interpretation.xlsx")
# sampleInfo <- read_xlsx("D:/outputs/test_sampleinfo.xlsx") %>%
#   mutate(`检测日期` = ymd("20220714"),
#          `出生日期` = ymd(`出生日期`))
# data_combine <- data %>%
#   nest(data = -`Sample Name`) %>%
#   left_join(sampleInfo, by = c("Sample Name" = "实验室编号(样本编号)")) %>%
#   nest(sampleInfo = c(-`Sample Name`, -data))
#
# pwalk(
#   .l = list(
#     ..1 = data_combine$`Sample Name`,
#     ..2 = data_combine$data,
#     ..3 = data_combine$sampleInfo
#   ),
#   .f = ~ render(
#     "D:/outputs/configFiles/DX1597_HTML.Rmd",
#     params = list(
#       sampleId = ..1,
#       data = ..2,
#       sampleInfo = ..3,
#       reference = dx1507_ref,
#       interpretation = dx1507_inp
#     ),
#     output_dir = "D:/outputs/",
#     output_file = paste("DX1597_",..1,".html",collapse = ""),
#     encoding = "UTF-8",
#     clean = TRUE
#   )
# )


# ===================================20220714员工体检脂溶性维生素==============================================
# data <- read_xlsx("D:/outputs/reports/20220714_employee exam/20220713_J220714008_FSV_239.xlsx") %>%
#   mutate(across(-`Sample Name`,.fns = round,3)) %>%
#   mutate(`Sample Name` = map_chr(.x = `Sample Name`,.f = ~str_split(.x,"-")[[1]][1]))
#
# sampleInfo <-
#   read_xlsx(
#     "D:/outputs/reports/20220714_employee exam/t_sample.xlsx",
#     col_names = c("Sample Name", "年龄", "性别"),
#     skip = 1
#   ) %>%
#   mutate(`Sample Name` = toupper(`Sample Name`)) %>%
#   mutate(`性别` = map2_chr(
#     .x = `Sample Name`,
#     .y = `性别`,
#     .f = ~ case_when(.x == "MS22087" ~ '女',
#                      TRUE ~ .y)
#   ))
#
# data_combine <- data %>%
#   nest(data = -`Sample Name`) %>%
#   left_join(sampleInfo, by = "Sample Name") %>%
#   nest(sampleInfo = c(-`Sample Name`, -data)) %>%
#   filter(`Sample Name` == "MS22082")
#
# pwalk(
#   .l = list(
#     ..1 = data_combine$`Sample Name`,
#     ..2 = data_combine$data,
#     ..3 = data_combine$sampleInfo
#   ),
#   .f = ~ render(
#     "D:/outputs/configFiles/DX1736_HTML.Rmd",
#     params = list(
#       sampleId = ..1,
#       data = ..2,
#       sampleInfo = ..3,
#       reference = dx1736_ref,
#       interpretation = dx1736_inp
#     ),
#     output_dir = "D:/outputs/",
#     output_file = paste("DX1736_",..1,".html",collapse = ""),
#     encoding = "UTF-8",
#     clean = TRUE
#   )
# )
#
# data_combine_check <- data %>%
#   nest(data = -`Sample Name`) %>%
#   left_join(sampleInfo, by = "Sample Name")
# MS22048,MS22801

# =====================================20220715员工体检人体类固醇激素============================================
# data <- read_xlsx("D:/outputs/reports/20220715_employeeExam/20220713_J220713040_HOR_YGTJ_213.xlsx") %>%
#   mutate(across(-`Sample Name`,.fns = round,3)) %>%
#   mutate(`Sample Name` = map_chr(.x = `Sample Name`,.f = ~str_split(.x,"-")[[1]][1])) %>%
#   mutate(`Sample Name` = toupper(`Sample Name`))
#
# data2 <- read_xlsx("D:/20220713_J220713040_HOR_YGTJ_197.xlsx") %>%
#   mutate(across(-`Sample Name`,.fns = round,3)) %>%
#   mutate(`Sample Name` = map_chr(.x = `Sample Name`,.f = ~str_split(.x,"-")[[1]][1])) %>%
#   mutate(`Sample Name` = toupper(`Sample Name`))
#
# data_diff <- data %>%
#   filter(!(`Sample Name` %in% data2$`Sample Name`)) %>%
#   mutate(across(.cols = -`Sample Name`,.fns = round,2))
#
# sampleInfo <-
#   read_xlsx(
#     "D:/outputs/reports/1658220358927营养及内分泌代谢检测数据.xlsx"
#   ) %>%
#   select(`Sample Name` = `绑定的样本编码`,`性别`,`年龄`) %>%
#   mutate(`Sample Name` = toupper(`Sample Name`)) %>%
#   mutate(across(.cols = `年龄`,.fns = as.numeric))
#
#
# data_combine <- data_diff %>%
#   nest(data = -`Sample Name`) %>%
#   left_join(sampleInfo, by = "Sample Name") %>%
#   nest(sampleInfo = c(-`Sample Name`, -data))
# #
# # #
# # data_combine_check <- data_diff %>%
# #   nest(data = -`Sample Name`) %>%
# #   left_join(sampleInfo, by = "Sample Name")
# # # MS22048
# dx1597_ref <- read_xlsx("D:/outputs/configFiles/DX1597_reference.xlsx")
# dx1597_inp <- read_xlsx("D:/outputs/configFiles/DX1597_interpretation.xlsx")
# # # #
# pwalk(
#   .l = list(
#     ..1 = data_combine$`Sample Name`,
#     ..2 = data_combine$data,
#     ..3 = data_combine$sampleInfo
#   ),
#   .f = ~ render(
#     "D:/outputs/configFiles/DX1597_HTML.Rmd",
#     params = list(
#       sampleId = ..1,
#       data = ..2,
#       sampleInfo = ..3,
#       reference = dx1597_ref,
#       interpretation = dx1597_inp
#     ),
#     output_dir = "D:/outputs/reports/20220715_employeeExam/new/",
#     output_file = paste0("DX1597_",..1,".html",collapse = ""),
#     encoding = "UTF-8",
#     clean = TRUE
#   )
# )
# =====================================20220718员工体检脂溶性维生素========================================
# data <-
#   read_xlsx("D:/outputs/reports/20220718employeeExam/20220715_0146_04718_FSV_113.xlsx") %>%
#   mutate(across(-`Sample Name`, .fns = round, 3)) %>%
#   mutate(`Sample Name` = map_chr(.x = `Sample Name`, .f = ~ str_split(.x, "-")[[1]][1]))
#
# sampleInfo <-
#   read_xlsx(
#     "D:/outputs/reports/1658220358927营养及内分泌代谢检测数据.xlsx"
#   ) %>%
#   select(`Sample Name` = `绑定的样本编码`,`性别`,`年龄`) %>%
#   mutate(`Sample Name` = toupper(`Sample Name`)) %>%
#   mutate(across(.cols = `年龄`,.fns = as.numeric)) %>%
#   filter(`Sample Name` == "MS22082")
#
# data_combine <- data %>%
#   nest(data = -`Sample Name`) %>%
#   left_join(sampleInfo, by = "Sample Name") %>%
#   nest(sampleInfo = c(-`Sample Name`, -data)) %>%
#   filter(`Sample Name` == "MS22082")
#
#
# data_combine_check <- data %>%
#   nest(data = -`Sample Name`) %>%
#   left_join(sampleInfo, by = "Sample Name")
#
# pwalk(
#   .l = list(
#     ..1 = data_combine$`Sample Name`,
#     ..2 = data_combine$data,
#     ..3 = data_combine$sampleInfo
#   ),
#   .f = ~ render(
#     "D:/outputs/configFiles/DX1736_HTML.Rmd",
#     params = list(
#       sampleId = ..1,
#       data = ..2,
#       sampleInfo = ..3,
#       reference = dx1736_ref,
#       interpretation = dx1736_inp
#     ),
#     output_dir = "D:/outputs/",
#     output_file = paste("DX1736_",..1,".html",collapse = ""),
#     encoding = "UTF-8",
#     clean = TRUE
#   )
# )

# =============================20220718心血管报告=======================================
# dx2058 <- read_xlsx("D:/outputs/reports/20220715心血管系列报告/20220714_J220714099_Cer_9.xlsx") %>%
#   mutate(across(.cols = -`Sample Name`,.fns = round,2))
#
# sampleInf0 <- read_xlsx("D:/outputs/reports/20220715心血管系列报告/1656295372734.xlsx") %>%
#   select(`实验室编号(样本编号)`,
#          `姓名`,
#          `采样日期`,
#          `性别`,
#          `入库时间`,
#          `出生日期`,
#          `联系电话`,
#          `送检单位`,
#          `年龄`) %>%
#   mutate(`入库时间` = map_chr(.x = `入库时间`, .f = ~ as.character(ymd(.x)))) %>%
#   mutate(`年龄` = round(interval(ymd(`出生日期`),ymd("2022-07-20"))/years(1),0))
#
# dx2058_combine <- dx2058 %>%
#   nest(data = -`Sample Name`) %>%
#   left_join(sampleInf0, by = c("Sample Name" = "实验室编号(样本编号)")) %>%
#   nest(sampleInfo = c(-`Sample Name`, -data)) %>%
#   filter(`Sample Name` == "22S26330725")
#
# dx2058_ref <- read_xlsx("D:/outputs/configFiles/DX2058_reference.xlsx")
#
# pwalk(
#   .l = list(
#     ..1 = dx2058_combine$`Sample Name`,
#     ..2 = dx2058_combine$data,
#     ..3 = dx2058_combine$sampleInfo
#   ),
#   .f = ~ render(
#     "D:/outputs/configFiles/DX2058_PDF.Rmd",
#     params = list(
#       sampleId = ..1,
#       data = ..2,
#       sampleInfo = ..3,
#       reference = dx2058_ref
#     ),
#     output_dir = "D:/outputs/",
#     output_file = paste0(..1,"_DX2058_正式报告_中文_其他",".pdf",collapse = ""),
#     encoding = "UTF-8",
#     clean = TRUE
#   )
# )
#
# dx2059 <- read_xlsx("D:/outputs/reports/20220715心血管系列报告/副本20220714_J220713068_TMAO_10.xlsx") %>%
#   mutate(across(.cols = -`Sample Name`,.fns = round,2))
#
# dx2059_combine <- dx2059 %>%
#   nest(data = -`Sample Name`) %>%
#   left_join(sampleInf0, by = c("Sample Name" = "实验室编号(样本编号)")) %>%
#   nest(sampleInfo = c(-`Sample Name`, -data)) %>%
#   filter(`Sample Name` == "22S26330725")
#
# dx2059_ref <- read_xlsx("D:/outputs/configFiles/DX2059_reference.xlsx")
#
# pwalk(
#   .l = list(
#     ..1 = dx2059_combine$`Sample Name`,
#     ..2 = dx2059_combine$data,
#     ..3 = dx2059_combine$sampleInfo
#   ),
#   .f = ~ render(
#     "D:/outputs/configFiles/DX2059_PDF.Rmd",
#     params = list(
#       sampleId = ..1,
#       data = ..2,
#       sampleInfo = ..3,
#       reference = dx2059_ref
#     ),
#     output_dir = "D:/outputs/",
#     output_file = paste0(..1,"_DX2059_正式报告_中文_其他",".pdf",collapse = ""),
#     encoding = "UTF-8",
#     clean = TRUE
#   )
# )

# =====================================20220719员工体检脂溶性维生素========================================
# library(tidyverse)
# library(readxl)
# data <-
#   read_xlsx("D:/outputs/reports/20220719employeeExam/20220715_0146_04718_FSV_125.xlsx") %>%
#   mutate(across(-`Sample Name`, .fns = round, 2)) %>%
#   mutate(`Sample Name` = map_chr(.x = `Sample Name`, .f = ~ str_split(.x, "-")[[1]][1]))
#
# sampleInfo <-
#   read_xlsx(
#     "D:/outputs/reports/1658220358927营养及内分泌代谢检测数据.xlsx"
#   ) %>%
#   select(`Sample Name` = `绑定的样本编码`,`性别`,`年龄`) %>%
#   mutate(`Sample Name` = toupper(`Sample Name`)) %>%
#   mutate(across(.cols = `年龄`,.fns = as.numeric))
#
# data_combine <- data %>%
#   nest(data = -`Sample Name`) %>%
#   left_join(sampleInfo, by = "Sample Name") %>%
#   nest(sampleInfo = c(-`Sample Name`, -data))
#
# data_combine_check <- data %>%
#   nest(data = -`Sample Name`) %>%
#   left_join(sampleInfo, by = "Sample Name")
#
# pwalk(
#   .l = list(
#     ..1 = data_combine$`Sample Name`,
#     ..2 = data_combine$data,
#     ..3 = data_combine$sampleInfo
#   ),
#   .f = ~ render(
#     "D:/outputs/configFiles/DX1736_HTML.Rmd",
#     params = list(
#       sampleId = ..1,
#       data = ..2,
#       sampleInfo = ..3,
#       reference = dx1736_ref,
#       interpretation = dx1736_inp
#     ),
#     output_dir = "D:/outputs/reports/20220719employeeExam/",
#     output_file = paste0("DX1736_",..1,".html",collapse = ""),
#     encoding = "UTF-8",
#     clean = TRUE
#   )
# )


# ===============================20220721员工体检======================================

# library(tidyverse)
# library(readxl)
# data <-
#   read_xlsx("D:/outputs/reports/20220721employeeExam/20220718_J220718086_FSV_64.xlsx") %>%
#   mutate(across(-`Sample Name`, .fns = round, 2)) %>%
#   mutate(`Sample Name` = map_chr(.x = `Sample Name`, .f = ~ str_split(.x, "-")[[1]][1]))
#
# sampleInfo <-
#   read_xlsx(
#     "D:/outputs/reports/1658220358927营养及内分泌代谢检测数据.xlsx"
#   ) %>%
#   select(`Sample Name` = `绑定的样本编码`,`性别`,`年龄`) %>%
#   mutate(`Sample Name` = toupper(`Sample Name`)) %>%
#   mutate(across(.cols = `年龄`,.fns = as.numeric))
#
# data_combine <- data %>%
#   nest(data = -`Sample Name`) %>%
#   left_join(sampleInfo, by = "Sample Name") %>%
#   nest(sampleInfo = c(-`Sample Name`, -data))
#
# data_combine_check <- data %>%
#   nest(data = -`Sample Name`) %>%
#   left_join(sampleInfo, by = "Sample Name")
#
# pwalk(
#   .l = list(
#     ..1 = data_combine$`Sample Name`,
#     ..2 = data_combine$data,
#     ..3 = data_combine$sampleInfo
#   ),
#   .f = ~ render(
#     "D:/outputs/configFiles/DX1736_HTML.Rmd",
#     params = list(
#       sampleId = ..1,
#       data = ..2,
#       sampleInfo = ..3,
#       reference = dx1736_ref,
#       interpretation = dx1736_inp
#     ),
#     output_dir = "D:/outputs/reports/20220721employeeExam/",
#     output_file = paste0("DX1736_",..1,".html",collapse = ""),
#     encoding = "UTF-8",
#     clean = TRUE
#   )
# )

#=================20220721DX1597_216份========================
# data <- read_xlsx("D:/outputs/reports/20220721_dx1596_216/20220718_0146_04724_HOR_216.xlsx") %>%
#   mutate(across(-`Sample Name`,.fns = round,2)) %>%
#   mutate(`Sample Name` = map_chr(.x = `Sample Name`,.f = ~str_split(.x,"-")[[1]][1])) %>%
#   mutate(`Sample Name` = toupper(`Sample Name`))
#
# sampleInfo <-
#   read_xlsx(
#     "D:/outputs/reports/1658220358927营养及内分泌代谢检测数据.xlsx"
#   ) %>%
#   select(`Sample Name` = `绑定的样本编码`,`性别`,`年龄`) %>%
#   mutate(`Sample Name` = toupper(`Sample Name`)) %>%
#   mutate(across(.cols = `年龄`,.fns = as.numeric))
#
#
# data_combine <- data %>%
#   nest(data = -`Sample Name`) %>%
#   left_join(sampleInfo, by = "Sample Name") %>%
#   nest(sampleInfo = c(-`Sample Name`, -data)) %>%
#   filter(`Sample Name` != "MS22801")
#
#
# data_combine_check <- data %>%
#   nest(data = -`Sample Name`) %>%
#   left_join(sampleInfo, by = "Sample Name")
#
# dx1597_ref <- read_xlsx("D:/outputs/configFiles/DX1597_reference.xlsx")
# dx1597_inp <- read_xlsx("D:/outputs/configFiles/DX1597_interpretation.xlsx")

# pwalk(
#   .l = list(
#     ..1 = data_combine$`Sample Name`,
#     ..2 = data_combine$data,
#     ..3 = data_combine$sampleInfo
#   ),
#   .f = ~ render(
#     "D:/outputs/configFiles/DX1597_HTML.Rmd",
#     params = list(
#       sampleId = ..1,
#       data = ..2,
#       sampleInfo = ..3,
#       reference = dx1597_ref,
#       interpretation = dx1597_inp
#     ),
#     output_dir = "D:/outputs/reports/20220721_dx1596_216/",
#     output_file = paste0("DX1597_",..1,".html",collapse = ""),
#     encoding = "UTF-8",
#     clean = TRUE
#   )
# )


# =============================20220721 心血管=============================================
# dx20561 <- read_xlsx("D:/outputs/reports/20220721心血管报告/20220719_0146_04711_HCY_2_DX2056.xlsx") %>%
#   mutate(across(-`Sample Name`,.fns = round,2))
#
# sampleInfo1 <-
#   read_xlsx("D:\\outputs\\reports\\20220715心血管系列报告\\1656295372734.xlsx") %>%
#   select(`实验室编号(样本编号)`,
#          `姓名`,
#          `采样日期`,
#          `性别`,
#          `入库时间`,
#          `出生日期`,
#          `联系电话`,
#          `送检单位`,
#          `年龄`) %>%
#   mutate(`入库时间` = map_chr(.x = `入库时间`, .f = ~ as.character(ymd(.x)))) %>%
#   mutate(`采样日期` = map_chr(.x = `采样日期`, .f = ~ as.character(ymd(.x)))) %>%
#   mutate(`年龄` = round(interval(ymd(`出生日期`), ymd("2022-07-20")) / years(1), 0)) %>%
#   filter(`实验室编号(样本编号)` == "22B03745323")
#
# sampleInfo2 <- read_xlsx("D:/outputs/reports/20220721心血管报告/MS21162样本信息.xlsx") %>%
#   select(`实验室编号(样本编号)`,
#          `性别`,
#          `年龄`)
#
#
# data_combine <- dx20561 %>%
#   nest(data = -`Sample Name`) %>%
#   left_join(sampleInfo2, by = c("Sample Name" = "实验室编号(样本编号)")) %>%
#   nest(sampleInfo = c(-`Sample Name`, -data))
#
#
#
#
# dx2056_ref <- read_xlsx("D:/outputs/configFiles/DX2056_reference.xlsx")
#
# pwalk(
#   .l = list(
#     ..1 = data_combine$`Sample Name`,
#     ..2 = data_combine$data,
#     ..3 = data_combine$sampleInfo
#   ),
#   .f = ~ render(
#     "D:/outputs/configFiles/DX2056_HTML.Rmd",
#     params = list(
#       sampleId = ..1,
#       data = ..2,
#       sampleInfo = ..3,
#       reference = dx2056_ref
#     ),
#     output_dir = "D:/outputs/reports/20220721心血管报告/",
#     output_file = paste0("DX2056_",..1,".html",collapse = ""),
#     encoding = "UTF-8",
#     clean = TRUE
#   )
# )
#
#
# dx2057_1 <- read_xlsx("D:/outputs/reports/20220721心血管报告/20220711_0146_04697_HCY_3_DX2057.xlsx") %>%
#   mutate(across(-`Sample Name`,.fns = round,2)) %>%
#   filter(str_detect(`Sample Name`,"MS"))
#
# dx2057_2 <- read_xlsx("D:/outputs/reports/20220721心血管报告/20220719_0146_04711_HCY_1_DX2057.xlsx") %>%
#   mutate(across(-`Sample Name`,.fns = round,2))
#
# dx2057<- bind_rows(dx2057_1,dx2057_2)
#
# sampleInfo <-
#   read_xlsx("D:/outputs/reports/20220715心血管系列报告/3例MS编号信息及检测项目_20220711.xlsx") %>%
#   select(`实验室编号(样本编号)`,
#          `性别`,
#          `年龄`)
#
# data_combine <- dx2057 %>%
#   nest(data = -`Sample Name`) %>%
#   left_join(sampleInfo, by = c("Sample Name" = "实验室编号(样本编号)")) %>%
#   nest(sampleInfo = c(-`Sample Name`, -data))
#
# dx2057_ref <- read_xlsx("D:/outputs/configFiles/DX2057_reference.xlsx")

# pwalk(
#   .l = list(
#     ..1 = data_combine$`Sample Name`,
#     ..2 = data_combine$data,
#     ..3 = data_combine$sampleInfo
#   ),
#   .f = ~ render(
#     "D:/outputs/configFiles/DX2057_HTML.Rmd",
#     params = list(
#       sampleId = ..1,
#       data = ..2,
#       sampleInfo = ..3,
#       reference = dx2057_ref
#     ),
#     output_dir = "D:/outputs/reports/20220721心血管报告/",
#     output_file = paste0("DX2057_",..1,".html",collapse = ""),
#     encoding = "UTF-8",
#     clean = TRUE
#   )
# )

# dx2059 <- read_xlsx("D:/outputs/reports/20220721心血管报告/副本20220714_J220713068_TMAO_10.xlsx") %>%
#   mutate(across(-`Sample Name`,.fns = round,2)) %>%
#   filter(str_detect(`Sample Name`,"MS"))
#
# sampleInfo <-
#   read_xlsx("D:/outputs/reports/20220715心血管系列报告/3例MS编号信息及检测项目_20220711.xlsx") %>%
#   select(`实验室编号(样本编号)`,
#          `性别`,
#          `年龄`)
#
# data_combine <- dx2059 %>%
#   nest(data = -`Sample Name`) %>%
#   left_join(sampleInfo, by = c("Sample Name" = "实验室编号(样本编号)")) %>%
#   nest(sampleInfo = c(-`Sample Name`, -data))
#
# dx2059_ref <- read_xlsx("D:/outputs/configFiles/DX2059_reference.xlsx")
#
# pwalk(
#   .l = list(
#     ..1 = data_combine$`Sample Name`,
#     ..2 = data_combine$data,
#     ..3 = data_combine$sampleInfo
#   ),
#   .f = ~ render(
#     "D:/outputs/configFiles/DX2059_HTML.Rmd",
#     params = list(
#       sampleId = ..1,
#       data = ..2,
#       sampleInfo = ..3,
#       reference = dx2059_ref
#     ),
#     output_dir = "D:/outputs/reports/20220721心血管报告/",
#     output_file = paste0("DX2059_",..1,".html",collapse = ""),
#     encoding = "UTF-8",
#     clean = TRUE
#   )
# )


# dx2058 <- read_xlsx("D:/outputs/reports/20220721心血管报告/20220714_J220714099_Cer_9.xlsx") %>%
#   mutate(across(-`Sample Name`,.fns = round,2)) %>%
#   filter(str_detect(`Sample Name`,"MS"))
#
# sampleInfo <-
#   read_xlsx("D:/outputs/reports/20220715心血管系列报告/3例MS编号信息及检测项目_20220711.xlsx") %>%
#   select(`实验室编号(样本编号)`,
#          `性别`,
#          `年龄`)
#
# data_combine <- dx2058 %>%
#   nest(data = -`Sample Name`) %>%
#   left_join(sampleInfo, by = c("Sample Name" = "实验室编号(样本编号)")) %>%
#   nest(sampleInfo = c(-`Sample Name`, -data))
#
# dx2058_ref <- read_xlsx("D:/outputs/configFiles/DX2058_reference.xlsx")
#
# pwalk(
#   .l = list(
#     ..1 = data_combine$`Sample Name`,
#     ..2 = data_combine$data,
#     ..3 = data_combine$sampleInfo
#   ),
#   .f = ~ render(
#     "D:/outputs/configFiles/DX2058_HTML.Rmd",
#     params = list(
#       sampleId = ..1,
#       data = ..2,
#       sampleInfo = ..3,
#       reference = dx2058_ref
#     ),
#     output_dir = "D:/outputs/reports/20220721心血管报告/",
#     output_file = paste0("DX2058_",..1,".html",collapse = ""),
#     encoding = "UTF-8",
#     clean = TRUE
#   )
# )

# data <- read_xlsx("D:/outputs/reports/20220721心血管报告/20220711_0146_04694_WSV_3（员工活动）.xlsx")
#
# sampleInfo <-
#   read_xlsx("D:/outputs/reports/20220715心血管系列报告/3例MS编号信息及检测项目_20220711.xlsx") %>%
#   select(`实验室编号(样本编号)`,
#          `性别`,
#          `年龄`)
#
# data_combine <- data %>%
#   nest(data = -`Sample Name`) %>%
#   left_join(sampleInfo, by = c("Sample Name" = "实验室编号(样本编号)")) %>%
#   nest(sampleInfo = c(-`Sample Name`, -data))
#
# dx1683_ref <- read_xlsx("D:/outputs/configFiles/DX1683_reference.xlsx")
#
# dx1683_inp <- read_xlsx("D:/outputs/configFiles/DX1683_interpretation.xlsx")
#
# pwalk(
#   .l = list(
#     ..1 = data_combine$`Sample Name`,
#     ..2 = data_combine$data,
#     ..3 = data_combine$sampleInfo
#   ),
#   .f = ~ render(
#     "D:/outputs/configFiles/DX1683_HTML.Rmd",
#     params = list(
#       sampleId = ..1,
#       data = ..2,
#       sampleInfo = ..3,
#       reference = dx1683_ref,
#       interpretation = dx1683_inp
#     ),
#     output_dir = "D:/outputs/reports/20220721心血管报告/",
#     output_file = paste0("DX1683_",..1,".html",collapse = ""),
#     encoding = "UTF-8",
#     clean = TRUE
#   )
# )

#
# data <- read_csv("D:/outputs/reports/20220721心血管报告/20220711_0146_04698_MET_2.csv") %>%
#   mutate(across(-`Sample Name`,.fns = round,2))
#
# sampleInfo <-
#   read_xlsx("D:/outputs/reports/20220715心血管系列报告/3例MS编号信息及检测项目_20220711.xlsx") %>%
#   select(`实验室编号(样本编号)`,
#          `性别`,
#          `年龄`)
#
# data_combine <- data %>%
#   nest(data = -`Sample Name`) %>%
#   left_join(sampleInfo, by = c("Sample Name" = "实验室编号(样本编号)")) %>%
#   nest(sampleInfo = c(-`Sample Name`, -data))
#
# dx1710_ref <- read_xlsx("D:/outputs/configFiles/DX1710_reference.xlsx")
#
# dx1710_inp <- read_xlsx("D:/outputs/configFiles/DX1710_interpretation.xlsx")
#
# pwalk(
#   .l = list(
#     ..1 = data_combine$`Sample Name`,
#     ..2 = data_combine$data,
#     ..3 = data_combine$sampleInfo
#   ),
#   .f = ~ render(
#     "D:/outputs/configFiles/DX1710_HTML.Rmd",
#     params = list(
#       sampleId = ..1,
#       data = ..2,
#       sampleInfo = ..3,
#       reference = dx1710_ref,
#       interpretation = dx1710_inp
#     ),
#     output_dir = "D:/outputs/reports/20220721心血管报告/",
#     output_file = paste0("DX1710_",..1,".html",collapse = ""),
#     encoding = "UTF-8",
#     clean = TRUE
#   )
# )

# ==================================20220722FSV========================
# data <-
#   read_xlsx("D:/outputs/reports/20220809_FSV_105/20220806_0146_04774_FSV_105_YGHD .xlsx") %>%
#   mutate(across(-`Sample Name`, .fns = round, 2))
#
# sampleInfo <-
#   read_xlsx("D:/outputs/员工体检数据汇总/1660008808973营养及内分泌代谢检测数据(1).xlsx") %>%
#   select(`Sample Name` = `绑定的样本编码`, `性别`, `证件号`) %>%
#   mutate(`Sample Name` = toupper(`Sample Name`)) %>%
#   mutate(`年龄` = map_dbl(
#     .x = `证件号`,
#     .f = ~ interval(ymd(substring(as.character(.x),7,14)),ymd("20220809")) / years(1)
#   )) %>%
#   mutate(across(.cols = `年龄`, .fns = round,0))
#
# data_combine <- data %>%
#   nest(data = -`Sample Name`) %>%
#   left_join(sampleInfo, by = "Sample Name") %>%
#   filter(!is.na(`年龄`)) %>%
#   nest(sampleInfo = c(-`Sample Name`, -data))
#
# data_combine1 <- data %>%
#   nest(data = -`Sample Name`) %>%
#   left_join(sampleInfo, by = "Sample Name") %>%
#   filter(is.na(`年龄`))
#
# data_combine_check <- data %>%
#   nest(data = -`Sample Name`) %>%
#   left_join(sampleInfo, by = "Sample Name")
#
# pwalk(
#   .l = list(
#     ..1 = data_combine$`Sample Name`,
#     ..2 = data_combine$data,
#     ..3 = data_combine$sampleInfo
#   ),
#   .f = ~ render(
#     "D:/outputs/configFiles/DX1736_HTML.Rmd",
#     params = list(
#       sampleId = ..1,
#       data = ..2,
#       sampleInfo = ..3,
#       reference = dx1736_ref,
#       interpretation = dx1736_inp
#     ),
#     output_dir = "D:/outputs/reports/20220809_FSV_105/",
#     output_file = paste0("DX1736_",..1,".html",collapse = ""),
#     encoding = "UTF-8",
#     clean = TRUE
#   )
# )

# ====================================20220728-hor=============================

# ##
# library(tidyverse)
# library(readxl)
hor <-
  read_xlsx("D:/outputs/reports/20220909_HOR_105/20220906_J220906014_HOR_delivery.xlsx") %>%
  mutate(across(-`Sample Name`, .fns = round, 2))


sampleInfo <-
  read_xlsx("D:/outputs/1662688720142营养及内分泌代谢检测数据.xlsx") %>%
  # select(`Sample Name` = `绑定的样本编码`, `性别`, `年龄`)
  select(`Sample Name` = `绑定的样本编码`, `性别`, `证件号`, `采样时间`, `出生日期`, `年龄`) %>%
  filter(`Sample Name` %in% hor$`Sample Name`) %>%
  mutate(`Sample Name` = toupper(`Sample Name`)) %>%
  mutate(`年龄` = pmap_dbl(
    .l = list(
      ..1 = `年龄`,
      ..2 = `出生日期`,
      ..3 = `采样时间`,
      ..4 = `证件号`
    ),
    .f = ~ case_when(
      !is.na(..1) ~ as.numeric(..1),
      all(!is.na(..2),!is.na(..3)) ~ interval(ymd(..2), ymd(str_split(..3," ")[[1]][1])) / years(1),
      all(!is.na(..4),!is.na(..3)) ~ interval(ymd(substring(
        as.character(..4), 7, 14
      )), ymd(str_split(..3," ")[[1]][1])) / years(1)
    )
  )) %>%
  mutate(across(.cols = `年龄`, .fns = round, 0))

data_combine <- hor %>%
  nest(data = -`Sample Name`) %>%
  left_join(sampleInfo, by = "Sample Name") %>%
  filter(!is.na(`年龄`)) %>%
  # filter(`Sample Name` %in% c("MS13110")) %>%
  # mutate(`年龄` = 29) %>%
  nest(sampleInfo = c(-`Sample Name`, -data))
#
library(openxlsx)
write.xlsx(data_combine %>% select(`Sample Name`),file = "D:/t.xlsx")
dx1597_ref <- read_xlsx("D:/outputs/configFiles/DX1597_reference.xlsx")
dx1597_inp <- read_xlsx("D:/outputs/configFiles/DX1597_interpretation.xlsx")

pwalk(
  .l = list(
    ..1 = data_combine$`Sample Name`,
    ..2 = data_combine$data,
    ..3 = data_combine$sampleInfo
  ),
  .f = ~ render(
    "D:/outputs/configFiles/DX1597_HTML.Rmd",
    params = list(
      sampleId = ..1,
      data = ..2,
      sampleInfo = ..3,
      reference = dx1597_ref,
      interpretation = dx1597_inp
    ),
    output_dir = "D:/outputs/reports/20220909_HOR_105/",
    output_file = paste0("DX1597_",..1,".html",collapse = ""),
    encoding = "UTF-8",
    clean = TRUE
  )
)

# ----------------------------------------------------

# dx20561 <- read_xlsx("D:/outputs/reports/20220721心血管报告/20220719_0146_04711_HCY_2_DX2056.xlsx") %>%
#   mutate(across(-`Sample Name`,.fns = round,2))
#
# sampleInfo1 <-
#   read_xlsx("D:\\outputs\\reports\\20220715心血管系列报告\\1656295372734.xlsx") %>%
#   select(`实验室编号(样本编号)`,
#          `姓名`,
#          `采样日期`,
#          `性别`,
#          `入库时间`,
#          `出生日期`,
#          `联系电话`,
#          `送检单位`,
#          `年龄`) %>%
#   mutate(`入库时间` = map_chr(.x = `入库时间`, .f = ~ as.character(ymd(.x)))) %>%
#   mutate(`采样日期` = map_chr(.x = `采样日期`, .f = ~ as.character(ymd(.x)))) %>%
#   mutate(`年龄` = round(interval(ymd(`出生日期`), ymd("2022-07-20")) / years(1), 0)) %>%
#   filter(`实验室编号(样本编号)` == "22B03745323")
#
# sampleInfo2 <- read_xlsx("D:/outputs/reports/20220721心血管报告/MS21162样本信息.xlsx") %>%
#   select(`实验室编号(样本编号)`,
#          `性别`,
#          `年龄`)
#
#
# data_combine <- dx20561 %>%
#   nest(data = -`Sample Name`) %>%
#   left_join(sampleInfo1, by = c("Sample Name" = "实验室编号(样本编号)")) %>%
#   nest(sampleInfo = c(-`Sample Name`, -data)) %>%
#   filter(`Sample Name` == "22B03745323")
#
#
#
#
# dx2056_ref <- read_xlsx("D:/outputs/configFiles/DX2056_reference.xlsx")
#
# pwalk(
#   .l = list(
#     ..1 = data_combine$`Sample Name`,
#     ..2 = data_combine$data,
#     ..3 = data_combine$sampleInfo
#   ),
#   .f = ~ render(
#     "D:/outputs/configFiles/DX2056_PDF.Rmd",
#     params = list(
#       sampleId = ..1,
#       data = ..2,
#       sampleInfo = ..3,
#       reference = dx2056_ref
#     ),
#     output_dir = "D:/outputs/reports/20220721心血管报告/",
#     output_file = paste0(..1,"_DX2056_正式报告_中文_其他",".pdf",collapse = ""),
#     encoding = "UTF-8",
#     clean = TRUE
#   )
# )

#---------------------------------

fsv <- read_xlsx('D:/outputs/reports/20220809_FSV_105/20220806_0146_04774_FSV_105_YGHD .xlsx') %>%
  mutate(across(-`Sample Name`,.fns = round,2))

sampleInfo <-
  read_xlsx("D:/outputs/营养及内分泌代谢检测数据-20220823.xlsx") %>%
  select(`Sample Name` = `绑定的样本编码`, `性别`, `证件号`) %>%
  mutate(`Sample Name` = toupper(`Sample Name`)) %>%
  mutate(`年龄` = map_dbl(
    .x = `证件号`,
    .f = ~ interval(ymd(substring(as.character(.x),7,14)),ymd("20220821")) / years(1)
  )) %>%
  mutate(across(.cols = `年龄`, .fns = round,0))

# sampleInfo11 <-
#   read_xlsx("D:/outputs/员工体检数据汇总/1660549663589营养及内分泌代谢检测数据.xlsx") %>%
#   select(`Sample Name` = `绑定的样本编码`, `性别`, `出生日期`) %>%
#   filter(`Sample Name` %in% c("MS21955","MS22539")) %>%
#   mutate(`Sample Name` = toupper(`Sample Name`)) %>%
#   mutate(`年龄` = map_dbl(
#     .x = `出生日期`,
#     .f = ~ interval(ymd(as.character(.x)),ymd("20220809")) / years(1)
#   )) %>%
#   mutate(across(.cols = `年龄`, .fns = round,0))

data_combine <- fsv %>%
  nest(data = -`Sample Name`) %>%
  left_join(sampleInfo, by = "Sample Name") %>%
  filter(!is.na(`年龄`)) %>%
  # mutate(`年龄` = 65) %>%
  # filter(`Sample Name` %in% c("MS22888")) %>%
  nest(sampleInfo = c(-`Sample Name`, -data))

pwalk(
  .l = list(
    ..1 = data_combine$`Sample Name`,
    ..2 = data_combine$data,
    ..3 = data_combine$sampleInfo
  ),
  .f = ~ render(
    "D:/outputs/configFiles/DX1736_HTML.Rmd",
    params = list(
      sampleId = ..1,
      data = ..2,
      sampleInfo = ..3,
      reference = dx1736_ref,
      interpretation = dx1736_inp
    ),
    output_dir = "D:/outputs/reports/",
    output_file = paste0("DX1736_",..1,".html",collapse = ""),
    encoding = "UTF-8",
    clean = TRUE
  )
)
# ==============================20220802——FSV--54==========================================
fsv <- read_xlsx('D:/outputs/reports/20220905_FSV_194/20220902_0315_00315_FSV_194.xlsx') %>%
  mutate(across(-`Sample Name`,.fns = round,2))

sampleInfo <-
  read_xlsx("D:/outputs/1662340610326营养及内分泌代谢检测数据.xlsx") %>%
  # select(`Sample Name` = `绑定的样本编码`, `性别`, `年龄`)
  select(`Sample Name` = `绑定的样本编码`, `性别`) %>%
  # filter(`Sample Name` == "MS13036") %>%
  mutate(`Sample Name` = toupper(`Sample Name`)) %>%
  mutate(`年龄` = interval(ymd(19990626),ymd("20220818")) / years(1)
  ) %>%
  mutate(across(.cols = `年龄`, .fns = round,0))

data_combine <- fsv %>%
  nest(data = -`Sample Name`) %>%
  left_join(sampleInfo, by = "Sample Name") %>%
  # filter(`Sample Name` %in% c("MS21955","MS22539")) %>%
  # mutate(`年龄` = as.numeric(`性别`)) %>%

  filter(is.na(`性别`)) %>%
  # filter(!(`Sample Name` %in% c("MS21751","MS21752","MS22498"))) %>%
  nest(sampleInfo = c(-`Sample Name`, -data))

pwalk(
  .l = list(
    ..1 = data_combine$`Sample Name`,
    ..2 = data_combine$data,
    ..3 = data_combine$sampleInfo
  ),
  .f = ~ render(
    "D:/outputs/configFiles/DX1736_HTML.Rmd",
    params = list(
      sampleId = ..1,
      data = ..2,
      sampleInfo = ..3,
      reference = dx1736_ref,
      interpretation = dx1736_inp
    ),
    output_dir = "D:/outputs/reports/20220818_FSV_109/",
    output_file = paste0("DX1736_",..1,".html",collapse = ""),
    encoding = "UTF-8",
    clean = TRUE
  )
)
#
# # ===================================20220804-心脑血管报告==============================
dx2059 <- read_xlsx("D:/outputs/reports/20220914_cardio/20220914_J220914022_TMAO_2_廊坊.xlsx") %>%
  mutate(across(-`Sample Name`,.fns = round,2))
# #
sampleInfo2 <- read_xlsx("D:/outputs/reports/20220804_心脑血管报告/接收日期.xlsx")
#
sampleInfo1 <-
  read_xlsx("D:\\outputs\\reports\\20220817_cardio/廊坊心脑血管产品送检表.xlsx") %>%
  select(`实验室编号(样本编号)`,
         `姓名`,
         `采样日期`,
         `性别`,
         `出生日期`,
         `联系电话`,
         `送检单位`,
         `年龄`) %>%
  # filter(`实验室编号(样本编号)` %in% c("22S26330726","22S26330727")) %>%
  mutate(across(.cols = `联系电话`,.fns = as.character)) %>%
  left_join(sampleInfo2,by = c("实验室编号(样本编号)" = "Sample Name")) %>%
  mutate(`入库时间` = map_chr(.x = `接收日期`, .f = ~ as.character(ymd(.x)))) %>%
  mutate(`采样日期` = map_chr(.x = `采样日期`, .f = ~ as.character(ymd(.x)))) %>%
  mutate(`年龄` = round(interval(ymd(`出生日期`), ymd("2022-07-20")) / years(1), 0))


sampleInfo3 <- read_xls("D:/outputs/reports/20220823_cardio/sample info.xls",sheet = 2) %>%
  select("实验室编号(样本编号)" = "样品编号","姓名","采样日期" = "样品采集日期","性别",
         "出生日期","联系电话" = "电话","送检单位","年龄","入库时间" = "到样日期") %>%
  mutate(`年龄` = round(interval(ymd(`出生日期`), ymd("2022-08-17")) / years(1), 0)) %>%
  distinct()

data_combine <- dx2059 %>%
  nest(data = -`Sample Name`) %>%
  left_join(sampleInfo5, by = c("Sample Name" = "实验室编号(样本编号)")) %>%
  filter(!is.na(`性别`)) %>%
  # select("Sample Name",data,`性别`,`年龄`) %>%
  nest(sampleInfo = c(-`Sample Name`, -data))

dx2059_lack <- data_combine$`Sample Name`
# #
# # data_combine2 <- dx2059 %>%
# #   nest(data = -`Sample Name`) %>%
# #   left_join(sampleInfo7, by = "Sample Name") %>%
# #   filter(!is.na(`性别`)) %>%
# #   nest(sampleInfo = c(-`Sample Name`, -data))
# #
dx2059_ref <- read_xlsx("D:/outputs/configFiles/DX2059_reference.xlsx")

pwalk(
  .l = list(
    ..1 = data_combine$`Sample Name`,
    ..2 = data_combine$data,
    ..3 = data_combine$sampleInfo
  ),
  .f = ~ render(
    "D:/outputs/configFiles/DX2059_HTML.Rmd",
    params = list(
      sampleId = ..1,
      data = ..2,
      sampleInfo = ..3,
      reference = dx2059_ref
    ),
    output_dir = "D:/outputs/reports/20220823_cardio/",
    output_file = paste0("DX2059_",..1,".html",collapse = ""),
    encoding = "UTF-8",
    clean = TRUE
  )
)

pwalk(
  .l = list(
    ..1 = data_combine$`Sample Name`,
    ..2 = data_combine$data,
    ..3 = data_combine$sampleInfo
  ),
  .f = ~ render(
    "D:/outputs/configFiles/DX2059_PDF.Rmd",
    params = list(
      sampleId = ..1,
      data = ..2,
      sampleInfo = ..3,
      reference = dx2059_ref
    ),
    output_dir = "D:/outputs/reports/20220914_cardio/",
    output_file = paste0(..1,"_DX2059_正式报告_中文_其他",".pdf",collapse = ""),
    encoding = "UTF-8",
    clean = TRUE
  )
)
# #
dx2058 <- read_xlsx("D:/outputs/reports/20220914_cardio/20220914_J220914057_Cer_2_廊坊.xlsx") %>%
  mutate(across(-`Sample Name`,.fns = round,2))
#
sampleInfo3 <- read_xls("D:/outputs/reports/20220829_cardio/sample info.xls") %>%
  select("实验室编号(样本编号)" = "样品编号","姓名","采样日期" = "样品采集日期","性别",
         "出生日期","联系电话" = "电话","送检单位","年龄","入库时间" = "到样日期") %>%
  mutate(`年龄` = map2_dbl(.x = `年龄`,.y = `出生日期`,
                         .f = ~case_when(is.na(.x) ~ round(interval(ymd(.y), ymd("2022-08-29")) / years(1), 0),
                                         TRUE ~ as.numeric(.x)
                                         ))) %>%
  distinct()

data_combine11 <- dx2058 %>%
  nest(data = -`Sample Name`) %>%
  left_join(sampleInfo5, by = c("Sample Name" = "实验室编号(样本编号)")) %>%
  filter(!is.na(`性别`)) %>%
  # select("Sample Name",data,`性别`,`年龄`) %>%
  nest(sampleInfo = c(-`Sample Name`, -data))

dx2058_lack <- data_combine11$`Sample Name`
#
# data_combine12 <- dx2058 %>%
#   nest(data = -`Sample Name`) %>%
#   left_join(sampleInfo1, by = c("Sample Name" = "实验室编号(样本编号)")) %>%
#   filter(!is.na(`姓名`)) %>%
#   nest(sampleInfo = c(-`Sample Name`, -data))
#
sampleInfo4 <- read_xls("D:/outputs/reports/20220804_心脑血管报告/副本样本信息表.xls") %>%
  select(`实验室编号(样本编号)` = `样本编号`,
         `姓名` = `受检者姓名`,
         `出生日期`,
         `联系电话` = `手机号`,
         `采样日期`,
         `性别`,
         `出生日期`,
         `送检单位`
         ) %>%
  left_join(sampleInfo2,by = c("实验室编号(样本编号)" = "Sample Name")) %>%
  mutate(`入库时间` = map_chr(.x = `接收日期`, .f = ~ as.character(ymd(.x)))) %>%
  # mutate(`采样日期` = c("2022-07-26","2022-07-26","2022-07-26","2022-07-30")) %>%
  mutate(`年龄` = round(interval(ymd(`出生日期`), ymd("2022-07-20")) / years(1), 0)) %>%
  mutate(across(.cols = `联系电话`,.fns = as.character))

data_combine13 <- dx2058 %>%
  nest(data = -`Sample Name`) %>%
  left_join(sampleInfo7, by = "Sample Name") %>%
  filter(!is.na(`性别`)) %>%
  nest(sampleInfo = c(-`Sample Name`, -data))
#
dx2058_ref <- read_xlsx("D:/outputs/configFiles/DX2058_reference.xlsx")

pwalk(
  .l = list(
    ..1 = data_combine11$`Sample Name`,
    ..2 = data_combine11$data,
    ..3 = data_combine11$sampleInfo
  ),
  .f = ~ render(
    "D:/outputs/configFiles/DX2058_PDF.Rmd",
    params = list(
      sampleId = ..1,
      data = ..2,
      sampleInfo = ..3,
      reference = dx2058_ref
    ),
    output_dir = "D:/outputs/reports/20220914_cardio/",
    output_file = paste0(..1,"_DX2058_正式报告_中文_其他",".pdf",collapse = ""),
    encoding = "UTF-8",
    clean = TRUE
  )
)

pwalk(
  .l = list(
    ..1 = data_combine11$`Sample Name`,
    ..2 = data_combine11$data,
    ..3 = data_combine11$sampleInfo
  ),
  .f = ~ render(
    "D:/outputs/configFiles/DX2058_HTML.Rmd",
    params = list(
      sampleId = ..1,
      data = ..2,
      sampleInfo = ..3,
      reference = dx2058_ref
    ),
    output_dir = "D:/outputs/reports/20220823_cardio/",
    output_file = paste0("DX2058_",..1,".html",collapse = ""),
    encoding = "UTF-8",
    clean = TRUE
  )
)
# #
# =====================20220819===============
dx2056 <- read_xlsx("D:/outputs/reports/20220914_cardio/20220913_0315_00331_HCY_2_DX2056_廊坊.xlsx") %>%
  mutate(across(-`Sample Name`,.fns = round,2))

dx2056_ref <- read_xlsx("D:/outputs/configFiles/DX2056_reference.xlsx")

sampleInfo <-
  read_xlsx("D:/outputs/reports/20220818_FSV_109/2022-08-18营养及内分泌代谢检测数据.xlsx") %>%
  select(`Sample Name` = `绑定的样本编码`, `性别`, `证件号`) %>%
  # select(`Sample Name` = `绑定的样本编码`, `性别`) %>%
  # filter(`Sample Name` == "MS13036") %>%
  mutate(`Sample Name` = toupper(`Sample Name`)) %>%
  mutate(`年龄` = map_dbl(.x = `证件号`,
                        .f = ~ interval(ymd(
                          substring(as.character(.x), 7, 14)
                        ), ymd("20220809")) / years(1))) %>%
  mutate(across(.cols = `年龄`, .fns = round, 0))

sampleInfo1 <- read_xls("D:/outputs/reports/20220831_cardio/sample info.xls",sheet = 2) %>%
  select("实验室编号(样本编号)" = "样品编号","姓名","采样日期" = "样品采集日期","性别",
         "出生日期","联系电话" = "电话","送检单位","年龄","入库时间" = "到样日期") %>%
  mutate(`年龄` = round(interval(ymd(`出生日期`), ymd("2022-08-31")) / years(1), 0)) %>%
  distinct() %>%
  mutate(`年龄` = map_chr(.x = `年龄`,.f = ~case_when(is.na(.x) ~ " ",
                                                  TRUE ~ as.character(.x))))

sampleInfo5 <-
  read_xlsx("D:\\outputs\\reports\\20220914_cardio/廊坊心脑血管产品送检表.xlsx") %>%
  select(`实验室编号(样本编号)`,
         `姓名`,
         `采样日期`,
         `性别`,
         `出生日期`,
         `联系电话`,
         `送检单位`,
         `到样日期`,
         `检测日期`,
         `年龄`) %>%
  # filter(`实验室编号(样本编号)` %in% c("22S26330726","22S26330727")) %>%
  mutate(across(.cols = `联系电话`,.fns = as.character)) %>%
  # left_join(sampleInfo2,by = c("实验室编号(样本编号)" = "Sample Name")) %>%
  mutate(`到样日期` = map_chr(.x = `到样日期`, .f = ~ as.character(ymd(.x)))) %>%
  mutate(`采样日期` = map_chr(.x = `采样日期`, .f = ~ as.character(ymd(.x)))) %>%
  mutate(`年龄` = round(interval(ymd(`出生日期`), ymd(`采样日期`)) / years(1), 0)) %>%
  mutate(`联系电话` = "")

data_combine21 <- dx2056 %>%
  nest(data = -`Sample Name`) %>%
  left_join(sampleInfo5, by = c("Sample Name" = "实验室编号(样本编号)")) %>%
  # left_join(sampleInfo, by = "Sample Name") %>%
  filter(!is.na(`性别`)) %>%
  # mutate(`联系电话` = map_chr(.x = `联系电话`,.f = ~case_when(is.na(.x) ~ " ",
  #                                                     TRUE ~ as.character(.x)))) %>%
  # mutate(`入库时间` = ymd(20220804)) %>%
  nest(sampleInfo = c(-`Sample Name`, -data))

sampleInfo5 <- read_xls("D:/outputs/reports/20220815_心血管报告/sample.xls") %>%
  select(`实验室编号(样本编号)` = `样品编号`,
         `姓名`,
         `出生日期`,
         `联系电话` = `电话`,
         `采样日期` = `样品采集日期`,
         `性别`,
         `出生日期`,
         `送检单位`,
         `检测日期`
  ) %>%
  left_join(sampleInfo2,by = c("实验室编号(样本编号)" = "Sample Name")) %>%
  # mutate(`采样日期` = c("2022-07-26","2022-07-26","2022-07-26","2022-07-30")) %>%
  mutate(`年龄` = round(interval(ymd(`出生日期`), ymd("2022-08-15")) / years(1), 0)) %>%
  mutate(across(.cols = `联系电话`,.fns = as.character)) %>%
  # slice(1) %>%
  mutate(`入库时间` = c("2022-08-09"))

pwalk(
  .l = list(
    ..1 = data_combine21$`Sample Name`,
    ..2 = data_combine21$data,
    ..3 = data_combine21$sampleInfo
  ),
  .f = ~ render(
    "D:/outputs/configFiles/DX2056_HTML.Rmd",
    params = list(
      sampleId = ..1,
      data = ..2,
      sampleInfo = ..3,
      reference = dx2056_ref
    ),
    output_dir = "D:/outputs/reports/20220823_cardio/",
    output_file = paste0("DX2056_",..1,".html",collapse = ""),
    encoding = "UTF-8",
    clean = TRUE
  )
)

pwalk(
  .l = list(
    ..1 = data_combine21$`Sample Name`,
    ..2 = data_combine21$data,
    ..3 = data_combine21$sampleInfo
  ),
  .f = ~ render(
    "D:/outputs/configFiles/DX2056_PDF.Rmd",
    params = list(
      sampleId = ..1,
      data = ..2,
      sampleInfo = ..3,
      reference = dx2056_ref
    ),
    output_dir = "D:/outputs/reports/20220914_cardio/",
    output_file = paste0(..1,"_DX2056_正式报告_中文_其他",".pdf",collapse = ""),
    encoding = "UTF-8",
    clean = TRUE
  )
)

#
dx2057 <- read_xlsx("D:/outputs/reports/20220906_linlaoshi/DX2056.xlsx") %>%
  mutate(across(-`Sample Name`,.fns = round,2))

dx2057_ref <- read_xlsx("D:/outputs/configFiles/DX2057_reference.xlsx")

sampleInfo5 <- read_xls("D:/outputs/reports/20220906_linlaoshi/sample (8)(1).xls",sheet = 2) %>%
  select(`实验室编号(样本编号)` = `样品编号`,
         `姓名`,
         `出生日期`,
         `联系电话` = `电话`,
         `采样日期` = `样品采集日期`,
         `性别`,
         `出生日期`,
         `送检单位`,
         `到样日期`
  ) %>%
  # left_join(sampleInfo2,by = c("实验室编号(样本编号)" = "Sample Name")) %>%
  mutate(`入库时间` = `到样日期`) %>%
  mutate(`年龄` = round(interval(ymd(`出生日期`), ymd("2022-08-15")) / years(1), 0)) %>%
  mutate(across(.cols = `联系电话`,.fns = as.character))


data_combine31 <- dx2057 %>%
  nest(data = -`Sample Name`) %>%
  left_join(sampleInfo5, by = c("Sample Name" = "实验室编号(样本编号)")) %>%
  filter(!is.na(`性别`)) %>%
  nest(sampleInfo = c(-`Sample Name`, -data))

pwalk(
  .l = list(
    ..1 = data_combine31$`Sample Name`,
    ..2 = data_combine31$data,
    ..3 = data_combine31$sampleInfo
  ),
  .f = ~ render(
    "D:/outputs/configFiles/DX2057_PDF.Rmd",
    params = list(
      sampleId = ..1,
      data = ..2,
      sampleInfo = ..3,
      reference = dx2057_ref
    ),
    output_dir = "D:/outputs/reports/20220906_linlaoshi/",
    output_file = paste0(..1,"_DX2057_正式报告_中文_其他",".pdf",collapse = ""),
    encoding = "UTF-8",
    clean = TRUE
  )
)

pwalk(
  .l = list(
    ..1 = data_combine31$`Sample Name`,
    ..2 = data_combine31$data,
    ..3 = data_combine31$sampleInfo
  ),
  .f = ~ render(
    "D:/outputs/configFiles/DX2057_HTML.Rmd",
    params = list(
      sampleId = ..1,
      data = ..2,
      sampleInfo = ..3,
      reference = dx2057_ref
    ),
    output_dir = "D:/outputs/reports/20220906_linlaoshi/",
    output_file = paste0("DX2057_",..1,".html",collapse = ""),
    encoding = "UTF-8",
    clean = TRUE
  )
)
#
#
# sampleInfo7 <- read_xlsx("D:/outputs/员工体检数据汇总/1659347717424营养及内分泌代谢检测数据.xlsx") %>%
#     select(`Sample Name` = `绑定的样本编码`,`性别`,`年龄`) %>%
#     mutate(`Sample Name` = toupper(`Sample Name`)) %>%
#     mutate(across(.cols = `年龄`,.fns = as.numeric))

