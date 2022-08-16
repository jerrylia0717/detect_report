#' interprete_DX2056
#' @importFrom stringr str_c
#' @importFrom dplyr case_when filter select mutate pull
#' @importFrom tibble tibble
#' @param data tibble
#' @param reference tibble
#' @return list
interprete_DX2056 <- function(data, reference) {
  combined <- data %>%
    pivot_longer(cols = everything(),
                 names_to = "compound",
                 values_to = "value") %>%
    left_join(reference, by = "compound")
  # mark low or high by arrow
  # Hcy is a special case;
  # for others: more than upper limit is uparrow, less than lower limit is downarrow
  marked <- combined %>%
    mutate(mark = pmap_chr(
      .l = list(
        ..1 = compound,
        ..2 = value,
        ..3 = lower_limit,
        ..4 = upper_limit
      ),
      .f = ~ case_when(
        ..1 == "Hcy" ~ case_when(
          ..2 > ..3 ~ str_c(..2, "$\\uparrow$", sep = "  "),
          TRUE ~ as.character(..2)
        ),
        TRUE ~ case_when(
          is.na(..3) ~ case_when(
            ..2 > ..4 ~ str_c(..2, "$\\uparrow$", sep = "  "),
            TRUE ~ as.character(..2)
          ),
          TRUE ~ case_when(
            ..2 < ..3 ~ str_c(..2, "$\\downarrow$", sep = "  "),
            TRUE ~ as.character(..2)
          )
        )
      )
    ))
  # mark by the limit of quantification
  marked <- marked %>%
    mutate(mark = pmap_chr(
      .l = list(
        ..1 = value,
        ..2 = quanLimit_lower,
        ..3 = quanLimit_upper,
        ..4 = mark
      ),
      .f = ~ case_when(
        all(is.na(..2), is.na(..3)) ~ ..4,
        all(!is.na(..2),!is.na(..3)) ~ case_when(..1 < ..2 ~ "小于检测下限值",
                                                 ..1 > ..3 ~ "大于检测上限值",
                                                 TRUE ~ ..4),
        all(is.na(..2),!is.na(..3)) ~ case_when(..1 > ..3 ~ "大于检测上限值",
                                                TRUE ~ ..4),
        all(!is.na(..2), is.na(..3)) ~ case_when(..1 < ..2 ~ "小于检测下限值",
                                                 TRUE ~ ..4),
      )
    )) %>%
    select(ch_compound, mark)
  # get risk status for each compound
  stated <- combined %>%
    mutate(state = pmap_chr(
      .l = list(
        ..1 = compound,
        ..2 = value,
        ..3 = lower_limit,
        ..4 = upper_limit,
        ..5 = middle_limit
      ),
      .f = ~ case_when(
        ..1 == "Hcy" ~ case_when(..2 < ..3 ~ "低风险",
                                 ..2 < ..5 ~ "轻度升高",
                                 ..2 < ..4 ~ "中度升高",
                                 TRUE ~ "重度升高"),
        TRUE ~ case_when(
          is.na(..3) ~ case_when(..2 > ..4 ~ "高风险",
                                 TRUE ~ "低风险"),
          TRUE ~ case_when(..2 < ..3 ~ "高风险",
                           TRUE ~ "低风险")
        )
      )
    )) %>%
    select(ch_compound, state) %>%
    pivot_wider(names_from = "ch_compound", values_from = "state")
  # compound's alias
  alias <-
    tibble(
      ch_compound = c("维生素B2", "同型半胱氨酸", "4-吡哆酸", "5-甲基四氢叶酸", "甲基丙二酸"),
      alias_name = c("维生素$B_2$", "同型半胱氨酸", "维生素$B_6$", "5-甲基四氢叶酸", "维生素$B_{12}$")
    )
  # interpretation statement based on stated
  # case A: hcy normal
  if (stated$`同型半胱氨酸`== "低风险") {
    # case A1
    # "甜菜碱" "4-吡哆酸" "5-甲基四氢叶酸" "甲基丙二酸" all normal
    if (all(
      stated$`4-吡哆酸` == "低风险",
      stated$`5-甲基四氢叶酸` == "低风险",
      stated$`维生素B2`    == "低风险",
      stated$`甲基丙二酸`    == "低风险"
    )) {
      interpretation <-
        "您的同型半胱氨酸（Hcy）水平及其代谢通路相关营养素指标目前均处于良好的平衡状态，建议您继续保持良好的生活作息和饮食习惯。祝您健康愉快！"
    } else if # case A2
    # "甜菜碱" "4-吡哆酸" "5-甲基四氢叶酸" "甲基丙二酸" not all normal
    (
      !all(
        stated$`4-吡哆酸` == "低风险",
        stated$`5-甲基四氢叶酸` == "低风险",
        stated$`维生素B2`    == "低风险",
        stated$`甲基丙二酸`    == "低风险"
      )
    ) {
      temp <- stated %>%
        pivot_longer(cols = everything(),
                     names_to = "ch_compound",
                     values_to = "state") %>%
        filter(ch_compound != "同型半胱氨酸", state != "低风险") %>%
        left_join(alias, by = "ch_compound")
      interpretation <- str_c(
        "您的同型半胱氨酸（Hcy）水平处于正常状态。但是，您目前存在",
        str_c(temp$alias_name, collapse = "、"),
        "缺乏/不足的情况，请您在医生指导下根据检测结果增加相关营养素的摄入，调整饮食结构。建议您定期体检，形成良好的生活作息和饮食习惯，适当增加体育锻炼，保持良好睡眠、放松减压。祝您健康愉快！"
      )
    }
    # case B: Hcy abnormal
  } else if (stated$`同型半胱氨酸`    != "低风险") {
    # case B1
    # "甜菜碱" "4-吡哆酸" "5-甲基四氢叶酸" "甲基丙二酸" all normal
    if (all(
      stated$`4-吡哆酸` == "低风险",
      stated$`5-甲基四氢叶酸` == "低风险",
      stated$`维生素B2`    == "低风险",
      stated$`甲基丙二酸`    == "低风险"
    )) {
      interpretation <- str_c(
        "您的同型半胱氨酸（Hcy）水平",
        stated$`同型半胱氨酸`,
        "，该指标异常与心脑血管疾病、神经系统疾病、肿瘤、高血压等慢性疾病密切相关，需引起重视。Hcy代谢通路相关4种主要营养素指标在此次检测中均处于正常状态，因此推测可能是由于其它营养素指标异常或非营养因素导致的疾病相关指标异常，如Hcy代谢的某些关键酶失活、激素影响、肾功能障碍和损伤、甲状腺功能减退、严重贫血、严重硬皮病及恶性肿瘤等疾病、应用氨甲蝶呤、一氧化氮、抗癫痫药、利尿药、烟酸等药物等。建议您及时就医查明 Hcy 异常升高的原因。祝您健康愉快！"
      )
    } else if # case B2
    # "甜菜碱" "4-吡哆酸" "5-甲基四氢叶酸" "甲基丙二酸" not all normal
    (
      !all(
        stated$`4-吡哆酸` == "低风险",
        stated$`5-甲基四氢叶酸` == "低风险",
        stated$`维生素B2`    == "低风险",
        stated$`甲基丙二酸`    == "低风险"
      )
    ) {
      temp <- stated %>%
        pivot_longer(cols = everything(),
                     names_to = "ch_compound",
                     values_to = "state") %>%
        filter(ch_compound != "同型半胱氨酸", state != "低风险") %>%
        left_join(alias, by = "ch_compound")
      interpretation <- str_c(
        "您的同型半胱氨酸（Hcy）水平",
        stated$`同型半胱氨酸`,
        "，该指标异常与心脑血管疾病、神经系统疾病、肿瘤、高血压等慢性疾病密切相关，需引起重视。您同时还存在",
        str_c(temp$alias_name, collapse = "、"),
        "缺乏/不足的情况，可能是引起Hcy水平升高的因素之一。请您在医生指导下根据检测结果增加相关营养素的摄入，调整饮食结构。建议您定期体检，形成良好的生活作息和饮食习惯，适当增加体育锻炼，保持良好睡眠、放松减压。祝您健康愉快！"
      )
    }
  }

  return(list(
    marked = marked,
    interpretation = interpretation,
    stated = stated
  ))
}

#' interprete_DX2057
#' @importFrom stringr str_c
#' @importFrom dplyr case_when filter select mutate pull
#' @importFrom tibble tibble
#' @param data tibble
#' @param reference tibble
#' @return list
interprete_DX2057 <- function(data, reference) {
  combined <- data %>%
    mutate(`SAM/SAH` = round(SAM / SAH, 2)) %>%
    pivot_longer(cols = everything(),
                 names_to = "compound",
                 values_to = "value") %>%
    left_join(reference, by = "compound")
  # mark low or high by arrow
  # Hcy is a special case;
  # for others: more than upper limit is uparrow, less than lower limit is downarrow
  marked <- combined %>%
    mutate(mark = pmap_chr(
      .l = list(
        ..1 = compound,
        ..2 = value,
        ..3 = lower_limit,
        ..4 = upper_limit
      ),
      .f = ~ case_when(
        ..1 == "Hcy" ~ case_when(
          ..2 > ..3 ~ str_c(..2, "$\\uparrow$", sep = "  "),
          TRUE ~ as.character(..2)
        ),
        TRUE ~ case_when(
          is.na(..3) ~ case_when(
            ..2 > ..4 ~ str_c(..2, "$\\uparrow$", sep = "  "),
            TRUE ~ as.character(..2)
          ),
          TRUE ~ case_when(
            ..2 < ..3 ~ str_c(..2, "$\\downarrow$", sep = "  "),
            TRUE ~ as.character(..2)
          )
        )
      )
    ))
  # mark by the limit of quantification
  marked <- marked %>%
    mutate(mark = pmap_chr(
      .l = list(
        ..1 = value,
        ..2 = quanLimit_lower,
        ..3 = quanLimit_upper,
        ..4 = mark
      ),
      .f = ~ case_when(
        all(is.na(..2), is.na(..3)) ~ ..4,
        all(!is.na(..2),!is.na(..3)) ~ case_when(..1 < ..2 ~ "小于检测下限值",
                                                 ..1 > ..3 ~ "大于检测上限值",
                                                 TRUE ~ ..4),
        all(is.na(..2),!is.na(..3)) ~ case_when(..1 > ..3 ~ "大于检测上限值",
                                                TRUE ~ ..4),
        all(!is.na(..2), is.na(..3)) ~ case_when(..1 < ..2 ~ "小于检测下限值",
                                                 TRUE ~ ..4),
      )
    )) %>%
    select(ch_compound, mark)
  # get risk status for each compound
  stated <- combined %>%
    mutate(state = pmap_chr(
      .l = list(
        ..1 = compound,
        ..2 = value,
        ..3 = lower_limit,
        ..4 = upper_limit,
        ..5 = middle_limit
      ),
      .f = ~ case_when(
        ..1 == "Hcy" ~ case_when(..2 < ..3 ~ "低风险",
                                 ..2 < ..5 ~ "轻度升高",
                                 ..2 < ..4 ~ "中度升高",
                                 TRUE ~ "重度升高"),
        TRUE ~ case_when(
          is.na(..3) ~ case_when(..2 > ..4 ~ "高风险",
                                 TRUE ~ "低风险"),
          TRUE ~ case_when(..2 < ..3 ~ "高风险",
                           TRUE ~ "低风险")
        )
      )
    )) %>%
    select(ch_compound, state) %>%
    pivot_wider(names_from = "ch_compound", values_from = "state")
  # compound's alias
  alias <-
    tibble(
      ch_compound = c(
        "甜菜碱",
        "同型半胱氨酸",
        "4-吡哆酸",
        "5-甲基四氢叶酸",
        "甲基丙二酸",
        "胆碱",
        "维生素B2",
        "S-腺苷甲硫氨酸",
        "S-腺苷同型半胱氨酸",
        "SAM/SAH"
      ),
      alias_name = c(
        "甜菜碱",
        "同型半胱氨酸",
        "维生素$B_6$",
        "叶酸",
        "维生素$B_{12}$",
        "胆碱",
        "维生素$B_2$",
        "S-腺苷甲硫氨酸",
        "S-腺苷同型半胱氨酸",
        "SAM/SAH"
      )
    )
  # interpretation statement based on stated
  # case A: hcy normal
  if (stated$`同型半胱氨酸`   == "低风险") {
    # case A1:
    # 维生素B2 4-吡哆酸 5-甲基四氢叶酸 甲基丙二酸 甜菜碱 胆碱 all normal
    if (all(
      stated$`维生素B2` == "低风险",
      stated$`4-吡哆酸` == "低风险",
      stated$`5-甲基四氢叶酸` == "低风险",
      stated$`甲基丙二酸`   == "低风险",
      stated$`甜菜碱`   == "低风险",
      stated$`胆碱`   == "低风险"
    )) {
      # case A1-1:
      # S-腺苷甲硫氨酸 S-腺苷同型半胱氨酸 SAM/SAH all normal
      if (all(
        stated$`S-腺苷甲硫氨酸` == "低风险",
        stated$`S-腺苷同型半胱氨酸` == "低风险",
        stated$`SAM/SAH` == "低风险"
      )) {
        interpretation <-
          "您的同型半胱氨酸（Hcy）水平及其代谢通路相关指标在此次检测中均处于良好的平衡状态，建议您继续保持良好的生活作息和饮食习惯。祝您健康愉快！"
      } else if # case A1-2:
      # S-腺苷甲硫氨酸 S-腺苷同型半胱氨酸 SAM/SAH not all normal
      (
        !all(
          stated$`S-腺苷甲硫氨酸` == "低风险",
          stated$`S-腺苷同型半胱氨酸` == "低风险",
          stated$`SAM/SAH` == "低风险"
        )
      ) {
        temp <- stated %>%
          pivot_longer(cols = everything(),
                       names_to = "ch_compound",
                       values_to = "state") %>%
          filter(ch_compound %in% c("S-腺苷甲硫氨酸", "S-腺苷同型半胱氨酸", "SAM/SAH"),
                 state != "低风险") %>%
          left_join(alias, by = "ch_compound")
        interpretation <- str_c(
          "您的同型半胱氨酸（Hcy）水平正常，但其它疾病相关指标",
          str_c(temp$alias_name, collapse = "、"),
          "处于异常状态，可能在一定程度上增加动脉粥样硬化、脑卒中等心脑血管疾病的发病风险。您血液中Hcy代谢通路相关营养素指标在此次检测中均处于正常状态，因此推测可能是由于非营养因素导致的疾病相关指标异常。建议您定期体检，形成良好的生活作息和饮食习惯，适当增加体育锻炼，保持良好睡眠、放松减压。祝您健康愉快！"
        )
      }
      # case A2:
      # 维生素B2 4-吡哆酸 5-甲基四氢叶酸 甲基丙二酸 甜菜碱 胆碱 not all normal
    } else if (!all(
      stated$`维生素B2` == "低风险",
      stated$`4-吡哆酸` == "低风险",
      stated$`5-甲基四氢叶酸` == "低风险",
      stated$`甲基丙二酸`   == "低风险",
      stated$`甜菜碱`   == "低风险",
      stated$`胆碱`   == "低风险"
    )) {
      temp <- stated %>%
        pivot_longer(cols = everything(),
                     names_to = "ch_compound",
                     values_to = "state") %>%
        filter(
          ch_compound %in% c("维生素B2", "4-吡哆酸", "5-甲基四氢叶酸", "甲基丙二酸", "甜菜碱", "胆碱"),
          state != "低风险"
        ) %>%
        left_join(alias, by = "ch_compound")
      # case A2-1
      # S-腺苷甲硫氨酸 S-腺苷同型半胱氨酸 SAM/SAH all normal
      if (all(
        stated$`S-腺苷甲硫氨酸` == "低风险",
        stated$`S-腺苷同型半胱氨酸` == "低风险",
        stated$`SAM/SAH` == "低风险"
      )) {
        interpretation <-
          str_c(
            "您的同型半胱氨酸（Hcy）水平与其它疾病相关指标SAM、SAH、SAM/SAH在此次检测中均处于正常状态。但是，您目前存在",
            str_c(temp$alias_name, collapse = "、"),
            "缺乏的情况，请您在医生指导下根据检测结果增加相关营养素的摄入，调整饮食结构。建议您定期体检，形成良好的生活作息和饮食习惯，适当增加体育锻炼，保持良好睡眠、放松减压。祝您健康愉快！"
          )
        # case A2-2
        # S-腺苷甲硫氨酸 S-腺苷同型半胱氨酸 SAM/SAH not all normal
      } else if (!all(
        stated$`S-腺苷甲硫氨酸` == "低风险",
        stated$`S-腺苷同型半胱氨酸` == "低风险",
        stated$`SAM/SAH` == "低风险"
      )) {
        temp2 <- stated %>%
          pivot_longer(cols = everything(),
                       names_to = "ch_compound",
                       values_to = "state") %>%
          filter(ch_compound %in% c("S-腺苷甲硫氨酸", "S-腺苷同型半胱氨酸", "SAM/SAH"),
                 state != "低风险") %>%
          left_join(alias, by = "ch_compound")
        interpretation <- str_c(
          "您的同型半胱氨酸（Hcy）水平正常，但其它疾病相关指标",
          str_c(temp2$alias_name, collapse = "、"),
          "在此次检测中处于异常状态，可能在一定程度上增加动脉粥样硬化、脑卒中等心脑血管疾病的发病风险。同时，您目前存在",
          str_c(temp$alias_name, collapse = "、"),
          "缺乏的情况，请您在医生指导下根据检测结果增加相关营养素的摄入，调整饮食结构。建议您定期体检，形成良好的生活作息和饮食习惯，适当增加体育锻炼，保持良好睡眠、放松减压。祝您健康愉快！"
        )
      }
    }
    # case B:
    # Hcy abnormal
  } else if (stated$`同型半胱氨酸`   != "低风险") {
    # case B1:
    # 维生素B2 4-吡哆酸 5-甲基四氢叶酸 甲基丙二酸 甜菜碱 胆碱 all normal
    if (all(
      stated$`维生素B2` == "低风险",
      stated$`4-吡哆酸` == "低风险",
      stated$`5-甲基四氢叶酸` == "低风险",
      stated$`甲基丙二酸`   == "低风险",
      stated$`甜菜碱`   == "低风险",
      stated$`胆碱`   == "低风险"
    )) {
      # case B1-1:
      # S-腺苷甲硫氨酸 S-腺苷同型半胱氨酸 SAM/SAH all normal
      if (all(
        stated$`S-腺苷甲硫氨酸` == "低风险",
        stated$`S-腺苷同型半胱氨酸` == "低风险",
        stated$`SAM/SAH` == "低风险"
      )) {
        interpretation <- str_c(
          "您的同型半胱氨酸（Hcy）水平",
          stated$`同型半胱氨酸`,
          "，该指标异常与心脑血管疾病、神经系统疾病、肿瘤、高血压等慢性疾病密切相关，需引起重视。您血液中Hcy代谢通路相关营养素指标在此次检测中均处于正常状态，因此推测可能是由于非营养因素导致的疾病相关指标异常，如Hcy代谢的某些关键酶失活、激素影响、肾功能障碍和损伤、甲状腺功能减退、严重贫血、严重硬皮病及恶性肿瘤等疾病、应用氨甲蝶呤、一氧化氮、抗癫痫药、利尿药、烟酸等药物等。建议您及时就医查明Hcy异常升高的原因。祝您健康愉快！"
        )
        # case B1-2:
        # S-腺苷甲硫氨酸 S-腺苷同型半胱氨酸 SAM/SAH not all normal
      } else if (!all(
        stated$`S-腺苷甲硫氨酸` == "低风险",
        stated$`S-腺苷同型半胱氨酸` == "低风险",
        stated$`SAM/SAH` == "低风险"
      )) {
        temp <- stated %>%
          pivot_longer(cols = everything(),
                       names_to = "ch_compound",
                       values_to = "state") %>%
          filter(ch_compound %in% c("S-腺苷甲硫氨酸", "S-腺苷同型半胱氨酸", "SAM/SAH"),
                 state != "低风险") %>%
          left_join(alias, by = "ch_compound")
        interpretation <- str_c(
          "您的同型半胱氨酸（Hcy）水平",
          stated$`同型半胱氨酸`,
          "，其它疾病相关指标",
          str_c(temp$alias_name, collapse = "、"),
          "也处于异常状态，以上指标异常与心脑血管疾病、神经系统疾病、肿瘤、高血压等慢性疾病密切相关，需引起重视。您血液中Hcy代谢通路相关营养素指标在此次检测中均处于正常状态，因此推测可能是由于非营养因素导致的疾病相关指标异常，如Hcy代谢的某些关键酶失活、激素影响、肾功能障碍和损伤、甲状腺功能减退、严重贫血、严重硬皮病及恶性肿瘤等疾病、应用氨甲蝶呤、一氧化氮、抗癫痫药、利尿药、烟酸等药物等。建议您及时就医查明Hcy异常升高的原因。祝您健康愉快！"
        )
      }
      # case B2:
      # 维生素B2 4-吡哆酸 5-甲基四氢叶酸 甲基丙二酸 甜菜碱 胆碱 not all normal
    } else if (!all(
      stated$`维生素B2` == "低风险",
      stated$`4-吡哆酸` == "低风险",
      stated$`5-甲基四氢叶酸` == "低风险",
      stated$`甲基丙二酸`   == "低风险",
      stated$`甜菜碱`   == "低风险",
      stated$`胆碱`   == "低风险"
    )) {
      temp <- stated %>%
        pivot_longer(cols = everything(),
                     names_to = "ch_compound",
                     values_to = "state") %>%
        filter(
          ch_compound %in% c("维生素B2", "4-吡哆酸", "5-甲基四氢叶酸", "甲基丙二酸", "甜菜碱", "胆碱"),
          state != "低风险"
        ) %>%
        left_join(alias, by = "ch_compound")
      # case B2-1
      # S-腺苷甲硫氨酸 S-腺苷同型半胱氨酸 SAM/SAH all normal
      if (all(
        stated$`S-腺苷甲硫氨酸` == "低风险",
        stated$`S-腺苷同型半胱氨酸` == "低风险",
        stated$`SAM/SAH` == "低风险"
      )) {
        interpretation <- str_c(
          "您的同型半胱氨酸（Hcy）水平",
          stated$`同型半胱氨酸`,
          "，该指标异常与心脑血管疾病、神经系统疾病、肿瘤、高血压等慢性疾病密切相关，需引起重视。同时，您目前存在",
          str_c(temp$alias_name, collapse = "、"),
          "缺乏的情况，请您在医生指导下根据检测结果增加相关营养素的摄入，调整饮食结构。建议您定期体检，形成良好的生活作息和饮食习惯，适当增加体育锻炼，保持良好睡眠、放松减压。祝您健康愉快！"
        )
      } else if (!all(
        stated$`S-腺苷甲硫氨酸` == "低风险",
        stated$`S-腺苷同型半胱氨酸` == "低风险",
        stated$`SAM/SAH` == "低风险"
      )) {
        # case A2-2
        # S-腺苷甲硫氨酸 S-腺苷同型半胱氨酸 SAM/SAH not all normal
        temp2 <- stated %>%
          pivot_longer(cols = everything(),
                       names_to = "ch_compound",
                       values_to = "state") %>%
          filter(ch_compound %in% c("S-腺苷甲硫氨酸", "S-腺苷同型半胱氨酸", "SAM/SAH"),
                 state != "低风险") %>%
          left_join(alias, by = "ch_compound")
        interpretation <- str_c(
          "您的同型半胱氨酸（Hcy）水平",
          stated$`同型半胱氨酸`,
          "，其它疾病相关指标",
          str_c(temp2$alias_name, collapse = "、"),
          "也处于异常状态，以上指标异常与心脑血管疾病、神经系统疾病、肿瘤、高血压等慢性疾病密切相关，需引起重视。同时，您目前存在",
          str_c(temp$alias_name, collapse = "、"),
          "缺乏的情况，请您在医生指导下根据检测结果增加相关营养素的摄入，调整饮食结构。建议您定期体检，形成良好的生活作息和饮食习惯，适当增加体育锻炼，保持良好睡眠、放松减压。祝您健康愉快！"
        )
      }
    }
  }
  return(list(
    marked = marked,
    interpretation = interpretation,
    stated = stated
  ))
}

#' interprete_DX2059
#' @importFrom dplyr case_when filter select pull mutate left_join everything
#' @importFrom tibble tibble
#' @importFrom purrr map2_lgl pmap_chr
#' @importFrom stringr str_c
#' @importFrom tidyr pivot_longer pivot_wider
#' @param data tibble
#' @param reference tibble
#' @return list
interprete_DX2059 <- function(data,
                              reference) {
  combined <- data %>%
    pivot_longer(cols = everything(),
                 names_to = "compound",
                 values_to = "value") %>%
    left_join(reference, by = "compound")

  TMAO_lowerLimit <-
    reference %>% filter(compound == 'TMAO') %>% select(lower_limit) %>% pull()

  # mark low or high by arrow
  # only when TMAO is high will mark other compounds
  if (data$TMAO >= TMAO_lowerLimit) {
    # when lower limit isn't NA, compare value with lower limit, otherwise compare value with upper limit
    pre_marked <- combined %>%
      mutate(mark = pmap_chr(
        .l = list(..1 = value, ..2 = lower_limit, ..3 = upper_limit),
        .f = ~ case_when(
          is.na(..2) ~ case_when(
            ..1 > ..3 ~ str_c(..1, "$\\uparrow$", sep = "  "),
            TRUE ~ as.character(..1)
          ),
          TRUE ~ case_when(
            ..1 > ..2 ~ str_c(..1, "$\\uparrow$", sep = "  "),
            TRUE ~ as.character(..1)
          )
        )
      ))
  } else {
    pre_marked <- combined %>%
      mutate(mark = as.character(value))
  }

  # mark by the limit of quantification
  marked <- pre_marked %>%
    mutate(mark = pmap_chr(
      .l = list(
        ..1 = value,
        ..2 = quanLimit_lower,
        ..3 = quanLimit_upper,
        ..4 = mark
      ),
      .f = ~ case_when(
        all(is.na(..2), is.na(..3)) ~ ..4,
        all(!is.na(..2),!is.na(..3)) ~ case_when(..1 < ..2 ~ "小于检测下限值",
                                                 ..1 > ..3 ~ "大于检测上限值",
                                                 TRUE ~ ..4),
        all(is.na(..2),!is.na(..3)) ~ case_when(..1 > ..3 ~ "大于检测上限值",
                                                TRUE ~ ..4),
        all(!is.na(..2), is.na(..3)) ~ case_when(..1 < ..2 ~ "小于检测下限值",
                                                 TRUE ~ ..4),
      )
    )) %>%
    select(ch_compound, mark)

  # get risk status for each compound
  stated <- combined %>%
    mutate(state = pmap_chr(
      .l = list(..1 = value, ..2 = lower_limit, ..3 = upper_limit),
      .f = ~ case_when(
        !is.na(..2) ~ case_when(..1 < ..2 ~ "低风险",
                                ..1 < ..3 ~ "中风险",
                                TRUE ~ "高风险"),
        TRUE ~ case_when(..1 < ..3 ~ "低风险",
                         TRUE ~ "高风险")
      )
    )) %>%
    select(ch_compound, state) %>%
    pivot_wider(names_from = "ch_compound", values_from = "state")

  # interpretation statement based on stated
  if (all(
    stated$`氧化三甲胺` == "低风险",
    stated$`胆碱` == "低风险",
    stated$`甜菜碱` == "低风险",
    stated$`左旋肉碱` == "低风险"
  )) {
    interpretation <-
      "您的氧化三甲胺（TMAO）水平处于正常状态，建议您继续保持良好的生活作息和饮食习惯，保持适当的体育锻炼。祝您健康愉快！"
  } else {
    if (stated$`氧化三甲胺` == "低风险") {
      temp <- stated %>%
        pivot_longer(cols = everything(),
                     names_to = "ch_compound",
                     values_to = "state") %>%
        filter(ch_compound != "氧化三甲胺", state != "低风险")
      interpretation <- str_c(
        "您的氧化三甲胺（TMAO）水平处于正常状态，虽然您的",
        str_c(temp$ch_compound, collapse = "、"),
        "水平高于参考范围，但仅当 TMAO是通过特定肠道微生物的代谢同时升高时，血清",
        str_c(temp$ch_compound, collapse = "、"),
        "（TMAO的饮食前体）的增加才与心血管风险相关，在此情况下，您的",
        str_c(temp$ch_compound, collapse = "、"),
        "水平升高无需担忧。建议您继续保持良好的生活作息和饮食习惯，保持适当的体育锻炼。祝您健康愉快！",
        sep = ""
      )
    } else if (stated$`氧化三甲胺` != "低风险") {
      temp <- stated %>%
        pivot_longer(cols = everything(),
                     names_to = "ch_compound",
                     values_to = "state") %>%
        filter(ch_compound != "氧化三甲胺", state != "低风险")
      if (nrow(temp) == 0) {
        interpretation <-
          "您的氧化三甲胺（TMAO）水平升高，可能在一定程度上增加冠心病、动脉粥样硬化、高血压等心脑血管疾病的发生风险。由于TMAO饮食前体物质（胆碱、甜菜碱、左旋肉碱）水平都正常，因此推测可能是由于非营养因素导致的异常。建议您通过服用益生菌等调节体内肠道菌群结构，帮助降低TMAO水平。祝您健康愉快！"
      } else {
        interpretation <- str_c(
          "您的",
          str_c(temp$ch_compound, collapse = "、"),
          "与氧化三甲胺（TMAO）水平同时升高，可能在一定程度上增加心血管疾病的发生风险。建议您通过服用益生菌等调节体内肠道菌群结构，同时适当减少红肉类（如猪肉、牛肉）、蛋类、海鱼和奶制品摄入，降低TMAO饮食前体物质-",
          str_c(temp$ch_compound, collapse = "、"),
          "的含量，以降低TMAO的水平。祝您健康愉快！",
          sep = ""
        )
      }

    }
  }

  return(
    list(
      marked = marked,
      interpretation = interpretation,
      stated = stated,
      riskLevel = stated$`氧化三甲胺`
    )
  )
}

#' interprete_DX2058
#' @param data tibble
#' @param reference tibble
#' @return list
interprete_DX2058 <- function(data,
                              reference) {
  combined <- data %>%
    pivot_longer(cols = everything(),
                 names_to = "compound",
                 values_to = "value") %>%
    left_join(reference, by = "compound")
  # caculate score
  score <- combined %>%
    # pivot_longer(cols = everything(),
    #              names_to = "compound",
    #              values_to = "value") %>%
    # left_join(reference, by = "compound") %>%
    filter(!is.na(lower_limit)) %>%
    mutate(score = pmap_dbl(
      .l = list(..1 = value,
                ..2 = lower_limit,
                ..3 = upper_limit),
      .f = ~ case_when(..1 < ..2 ~ 0,
                       between(..1, ..2, ..3) ~ 1,
                       ..1 > ..3 ~ 2)
    ))
  finalScore <- sum(score$score)

  # mark by the limit of quantification
  marked <- combined %>%
    mutate(mark = pmap_chr(
      .l = list(
        ..1 = value,
        ..2 = quanLimit_lower,
        ..3 = quanLimit_upper
      ),
      .f = ~ case_when(
        all(is.na(..2), is.na(..3)) ~ as.character(..1),
        all(!is.na(..2), !is.na(..3)) ~ case_when(..1 < ..2 ~ "小于检测下限值",
                                                  ..1 > ..3 ~ "大于检测上限值",
                                                  TRUE ~ as.character(..1)),
        all(is.na(..2), !is.na(..3)) ~ case_when(..1 > ..3 ~ "大于检测上限值",
                                                 TRUE ~ as.character(..1)),
        all(!is.na(..2), is.na(..3)) ~ case_when(..1 < ..2 ~ "小于检测下限值",
                                                 TRUE ~ as.character(..1)),
      )
    )) %>%
    select(compound, mark)
  # interpretation
  interpretation <- case_when(
    finalScore <= 2 ~ "您的神经酰胺风险评估等级为低风险，建议您继续保持健康的生活作息和饮食习惯，保持适当的体育锻炼。祝您健康愉快！",
    finalScore <= 6 ~ "您的神经酰胺风险评估等级为中风险，建议您戒烟、限酒、低糖/低脂饮食，同时适当增加体育锻炼，3~6个月后复查。若您的神经酰胺风险值持续升高，建议您在医生的指导下服用药物治疗一个月后复查。祝您健康愉快！",
    finalScore <= 9 ~ "您的神经酰胺风险评估等级为中高风险，建议您前往心血管专科就诊，排查并控制其他心血管危险因素；同时建议您在医生的指导下服用药物治疗一个月后复查。若您的神经酰胺风险值持续升高，建议您前往心血管专科进行冠状动脉影像学评估。祝您健康愉快！",
    finalScore <= 12 ~ "您的神经酰胺风险评估等级为高风险，建议您前往心血管专科就诊，排查并控制其他心血管危险因素、进行冠状动脉影像学评估，同时在医生的指导下进行药物等干预治疗，并密切随访。祝您健康愉快！"
  )
  risk <- case_when(finalScore <= 2 ~ "低风险",
                    finalScore <= 6 ~ "中风险",
                    finalScore <= 9 ~ "中高风险",
                    finalScore <= 12 ~ "高风险")
  return(
    list(
      marked = marked,
      finalScore = finalScore,
      interpretation = interpretation,
      risk = risk
    )
  )
}
