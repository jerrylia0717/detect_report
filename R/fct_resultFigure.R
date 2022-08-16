#' .trans2showvalueLU
#' @description convert detect value; both lower_limit and upper_limit
#' @importFrom dplyr case_when
#' @param value numeric, detect value
#' @param lower_limit numeric
#' @param upper_limit numeric
#' @return trans_value numeric
.trans2showvalueLU <- function(value,
                               lower_limit,
                               upper_limit,
                               minimal,
                               maximal) {
  if (is.na(as.numeric(value))) {
    trans_value <- 0
  } else {
    value <- as.numeric(value)
    trans_value <- case_when(
      is.na(value) ~ 0,
      value < lower_limit ~ minimal / lower_limit * value,
      value >= lower_limit &&
        value < upper_limit ~ maximal - (maximal - minimal) * (value - upper_limit) /
        (lower_limit - upper_limit),
      value >= upper_limit ~ (2 / pi) * atan(tan(maximal *
                                                   pi / 2) / upper_limit * value)
    )
  }
  # mark by the limit of quantification
  # trans_value <- case_when(
  #   all(is.na(quanLimit_lower), is.na(quanLimit_upper)) ~ trans_value,
  #   all(!is.na(quanLimit_lower), !is.na(quanLimit_upper)) ~ case_when(
  #     value < quanLimit_lower ~ "小于检测下限值",
  #     value > quanLimit_upper ~ "大于检测上限值",
  #     TRUE ~ trans_value
  #   ),
  #   all(is.na(quanLimit_lower), !is.na(quanLimit_upper)) ~ case_when(value > quanLimit_upper ~ "大于检测上限值",
  #                                                                    TRUE ~ trans_value),
  #   all(!is.na(quanLimit_lower), is.na(quanLimit_upper)) ~ case_when(value < quanLimit_lower ~ "小于检测下限值",
  #                                                                    TRUE ~ trans_value)
  # )

  return(trans_value)
}

#' .trans2showvalueL
#' @description convert detect value; only lower_limit
#' @importFrom dplyr case_when
#' @param value numeric, detect value
#' @param lower_limit numeric
#' @return trans_value numeric
.trans2showvalueL <- function(value,
                              lower_limit) {
  if (is.na(as.numeric(value))) {
    trans_value <- 0
  } else {
    value <- as.numeric(value)
    trans_value <- case_when(
      value < lower_limit ~ 0.382 / lower_limit * value,
      value >= lower_limit ~ (2 / pi) * atan(tan(0.382 *
                                                   pi / 2) / lower_limit * value)
    )
  }
  return(trans_value)
}

#' .trans2showvalueU
#' @description convert detect value; only upper_limit
#' @importFrom dplyr case_when
#' @param value numeric, detect value
#' @param upper_limit numeric
#' @return trans_value numeric
.trans2showvalueU <- function(value,
                              upper_limit) {
  if (is.na(as.numeric(value))) {
    trans_value <- 0
  } else {
    value <- as.numeric(value)
    trans_value <- case_when(
      value < upper_limit ~ 0.618 / upper_limit * value,
      value >= upper_limit ~ (2 / pi) * atan(tan(0.618 *
                                                   pi / 2) / upper_limit * value)
    )
  }
  return(trans_value)
}

#' .trans2showvalueLMU
#' @description convert detect value; both lower_limit, middle_limit, upper_limit
#' @importFrom dplyr case_when
#' @param value numeric, detect value
#' @param lower_limit numeric
#' @param middle_limit numeric
#' @param upper_limit numeric
.trans2showvalueLMU <- function(value,
                                lower_limit,
                                middle_limit,
                                upper_limit) {
  if (is.na(as.numeric(value))) {
    trans_value <- 0
  } else {
    value <- as.numeric(value)
    trans_value <- case_when(
      value < lower_limit ~ 0.25 / lower_limit * value,
      value >= lower_limit &&
        value < middle_limit ~ 0.5 - 0.25 * (value - middle_limit) /
        (lower_limit - middle_limit),
      value >= middle_limit &&
        value < upper_limit ~ 0.75 - 0.25 * (value - upper_limit) /
        (middle_limit - upper_limit),
      value >= upper_limit ~ (2 / pi) * atan(tan(0.75 *
                                                   pi / 2) / lower_limit * value)
    )
  }
  return(trans_value)
}

#' trans2show_nutrition_ggplot
#' @description generate showing figure for HCY
#' @importFrom dplyr filter select left_join case_when mutate across
#' @importFrom purrr pmap_dbl map_dbl map2_dbl
#' @importFrom tidyr pivot_longer
#' @importFrom tibble has_name
#' @importFrom forcats as_factor fct_reorder
#' @importFrom ggplot2 ggplot geom_col aes position_stack geom_point geom_text
#'  scale_fill_manual theme element_blank scale_y_discrete element_text element_rect
#' @param data dataframe, detect value
#' @param reference dataframe, limits info
#' @param barWidth barWidth
#' @param textSize textSize
#' @param ch_compound_levels list chr
#' @return ggplot object
trans2show_nutrition_ggplot <- function(data,
                                        reference,
                                        barWidth,
                                        textSize,
                                        ch_compound_levels) {
  transformer <- data %>%
    left_join(reference, by = 'compound')
  # generate transformed value according to value and limits
  transformer <- transformer %>%
    mutate(across(.cols = ch_compound, .fns = as_factor)) %>%
    mutate(trans_value =
             pmap_dbl(
               .l = list(
                 ..1 = value,
                 ..2 = lower_limit,
                 ..3 = upper_limit
               ),
               .f = ~ case_when(
                 all(!is.na(..2),!is.na(..3)) ~ .trans2showvalueLU(..1, ..2,..3,
                                                                   minimal = 0.191, maximal = 0.809),
                 all(is.na(..2), !is.na(..3)) ~ .trans2showvalueU(..1, ..3),
                 all(!is.na(..2), is.na(..3)) ~ .trans2showvalueL(..1, ..2)
               )
             )) %>%
    mutate(value = pmap_chr(
      .l = list(..1 = value, ..2 = quanLimit_lower, ..3 = quanLimit_upper),
      .f = ~ case_when(
        all(is.na(..2), is.na(..3)) ~ as.character(..1),
        all(!is.na(..2),!is.na(..3)) ~ case_when(
          ..1 < ..2 ~ "小于检测下限值",
          ..1 > ..3 ~ "大于检测上限值",
          TRUE ~ as.character(..1)
        ),
        all(is.na(..2),!is.na(..3)) ~ case_when(..1 > ..3 ~ "大于检测上限值",
                                                TRUE ~ as.character(..1)),
        all(!is.na(..2), is.na(..3)) ~ case_when(..1 < ..2 ~ "小于检测下限值",
                                                 TRUE ~ as.character(..1))
      )
    )) %>%
    mutate(value = map_chr(.x = value,.f = ~ case_when(
      str_detect(.x,"检测") ~ .x,
      str_count(str_split(.x, "\\.")[[1]][2]) == 1 ~ str_c(.x, "0", sep = ""),
      is.na(str_split(.x, "\\.")[[1]][2]) ~ str_c(.x,".00", collapse = ""),
      TRUE ~ .x
    )))
  # preparing data for ggplot2
  ggplot_col <- transformer %>%
    select(ch_compound, contains("limit")) %>%
    mutate(across(
      .cols = ch_compound,
      .fns = ordered,
      levels = ch_compound_levels
    )) %>%
    mutate(fig_col1 =
             map2_dbl(
               .x = lower_limit,.y = upper_limit,
               .f = ~ case_when(
                 all(!is.na(.x),!is.na(.y)) ~ 0.191,
                 all(is.na(.x),!is.na(.y)) ~ 0.618,
                 all(!is.na(.x),is.na(.y)) ~ 0.382
               )
             )) %>%
    mutate(fig_col2 =
             map2_dbl(
               .x = lower_limit, .y = upper_limit,
               .f = ~ case_when(
                 all(!is.na(.x),!is.na(.y)) ~ 0.618,
                 all(is.na(.x),!is.na(.y)) ~ 0,
                 all(!is.na(.x),is.na(.y)) ~ 0
               )
             )) %>%
    mutate(fig_col3 =
             map2_dbl(
               .x = lower_limit, .y = upper_limit,
               .f = ~ case_when(
                 all(!is.na(.x),!is.na(.y)) ~ 0.191,
                 all(is.na(.x),!is.na(.y)) ~ 0.382,
                 all(!is.na(.x),is.na(.y)) ~ 0.618
               )
             )) %>%
    pivot_longer(
      cols = c('fig_col1', 'fig_col2', 'fig_col3'),
      names_to = 'fig_position',
      values_to = 'value'
    ) %>%
    mutate(across(.cols = fig_position, .fns = as_factor)) %>%
    mutate(across(
      .cols = fig_position,
      .fns = ordered,
      levels = c('fig_col1', 'fig_col2', 'fig_col3')
    )) %>%
    mutate(compound_figPosition = str_c(ch_compound, fig_position, sep = "") %>% as_factor()) %>%
    mutate(across(.cols = compound_figPosition, .fns = fct_reorder, ch_compound, as.integer)) %>%
    mutate(color = pmap_chr(
      .l = list(
        ..1 = lower_limit,
        ..2 = upper_limit,
        ..3 = fig_position
      ),
      .f = ~ case_when(
        all(!is.na(..1), !is.na(..2),..3 == "fig_col1") ~ "L1",
        all(!is.na(..1), !is.na(..2),..3 == "fig_col2") ~ "L2",
        all(!is.na(..1), !is.na(..2),..3 == "fig_col3") ~ "L3",
        all(!is.na(..1), is.na(..2),..3 == "fig_col1") ~ "L1",
        all(!is.na(..1), is.na(..2),..3 == "fig_col2") ~ "BL",
        all(!is.na(..1), is.na(..2),..3 == "fig_col3") ~ "L2",
        all(is.na(..1), !is.na(..2),..3 == "fig_col1") ~ "L2",
        all(is.na(..1), !is.na(..2),..3 == "fig_col2") ~ "BL",
        all(is.na(..1), !is.na(..2),..3 == "fig_col3") ~ "L3"
      )
    ))

  # ggplot2
  fig <- ggplot() +
    geom_col(
      data = ggplot_col,
      aes(y = ch_compound, x = value, fill = color),
      width = barWidth,
      position = position_stack(reverse = TRUE)
    ) +
    geom_point(
      data = transformer,
      aes(y = ch_compound, x = trans_value),
      size = 1,
      color = 'blue'
    ) +
    geom_text(
      data = transformer,
      aes(
        y = ch_compound,
        x = trans_value,
        label = value
      ),
      size = 3,
      color = "black",
      nudge_y = 0.25
    ) +
    scale_fill_manual(
      values = c(
        "L0" = "#59CE33",
        "L1" = '#ED4D50',
        "L2" = '#59CE33',
        "L3" = '#ED4D50',
        "BL" = '#000000'
      )
    ) +
    theme(
      legend.position = 'none',
      axis.title = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = textSize),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      panel.background = element_rect(fill = "white", colour = "white")
    ) +
    scale_y_discrete(limits = rev)

  return(fig)
}

#' trans2show_cardio_ggplot
#' @description generate showing figure for HCY
#' @importFrom dplyr filter select left_join case_when mutate across
#' @importFrom purrr pmap_dbl map_dbl
#' @importFrom tidyr pivot_longer
#' @importFrom forcats as_factor fct_reorder
#' @importFrom ggplot2 ggplot geom_col aes position_stack geom_point geom_label scale_fill_manual theme element_blank scale_y_discrete element_text
#' @param data dataframe, detect value
#' @param limitsInfo dataframe, limits info
#' @param ch_compound_levels ch_compound_levels
#' @param barWidth barWidth
#' @param textSize textSize
#' @return ggplot object
trans2show_cardio_ggplot <- function(data,
                                     reference,
                                     ch_compound_levels,
                                     barWidth,
                                     textSize) {
  transformer <- data %>%
    left_join(reference, by = 'compound')

  # generate transformed value according to value and limits
  transformer <- transformer %>%
    mutate(across(.cols = ch_compound, .fns = as_factor)) %>%
    # mutate(across(.cols = ch_compound,.fns = ordered,
    #               levels = c("同型半胱氨酸","5-甲基四氢叶酸","甲基丙二酸","4-吡哆酸","维生素B2","甜菜碱","胆碱","S-腺苷甲硫氨酸","S-腺苷同型半胱氨酸","SAM/SAH"))) %>%
    mutate(trans_value =
             pmap_dbl(
               .l = list(
                 ..1 = value,
                 ..2 = lower_limit,
                 ..3 = middle_limit,
                 ..4 = upper_limit
               ),
               .f = ~ case_when(
                 all(!is.na(..2), !is.na(..3), !is.na(..4)) ~ .trans2showvalueLMU(..1, ..2, ..3, ..4),
                 all(!is.na(..2), is.na(..3), !is.na(..4)) ~ .trans2showvalueLU(..1, ..2, ..4, minimal = 0.33, maximal = 0.67),
                 all(is.na(..2), is.na(..3), !is.na(..4)) ~ .trans2showvalueU(..1, ..4),
                 all(!is.na(..2), is.na(..3), is.na(..4)) ~ .trans2showvalueL(..1, ..2)
               )
             )) %>%
    mutate(value = pmap_chr(
      .l = list(..1 = value, ..2 = quanLimit_lower, ..3 = quanLimit_upper),
      .f = ~ case_when(
        all(is.na(..2), is.na(..3)) ~ as.character(..1),
        all(!is.na(..2),!is.na(..3)) ~ case_when(
          ..1 < ..2 ~ "小于检测下限值",
          ..1 > ..3 ~ "大于检测上限值",
          TRUE ~ as.character(..1)
        ),
        all(is.na(..2),!is.na(..3)) ~ case_when(..1 > ..3 ~ "大于检测上限值",
                                                TRUE ~ as.character(..1)),
        all(!is.na(..2), is.na(..3)) ~ case_when(..1 < ..2 ~ "小于检测下限值",
                                                 TRUE ~ as.character(..1))
      )
    )) %>%
    mutate(value = map_chr(.x = value,.f = ~ case_when(
      str_detect(.x,"检测") ~ .x,
      str_count(str_split(.x, "\\.")[[1]][2]) == 1 ~ str_c(.x, "0", sep = ""),
      is.na(str_split(.x, "\\.")[[1]][2]) ~ str_c(.x,".00", collapse = ""),
      TRUE ~ .x
    )))

  # preparing data for ggplot2
  ggplot_col <- transformer %>%
    select(ch_compound, contains("limit")) %>%
    mutate(across(
      .cols = ch_compound,
      .fns = ordered,
      levels = ch_compound_levels
    )) %>%
    mutate(fig_col1 =
             pmap_dbl(
               .l = list(..1 = lower_limit, ..2 = middle_limit, ..3 = upper_limit),
               .f = ~ case_when(
                 all(!is.na(..1), !is.na(..2), !is.na(..3)) ~ 0.25,
                 all(!is.na(..1), is.na(..2), !is.na(..3)) ~ 0.33,
                 all(is.na(..1), is.na(..2), !is.na(..3)) ~ 0.618,
                 all(!is.na(..1), is.na(..2), is.na(..3)) ~ 0.382
               )
             )) %>%
    mutate(fig_col2 =
             pmap_dbl(
               .l = list(..1 = lower_limit, ..2 = middle_limit, ..3 = upper_limit),
               .f = ~ case_when(
                 all(!is.na(..1), !is.na(..2), !is.na(..3)) ~ 0.25,
                 all(!is.na(..1), is.na(..2), !is.na(..3)) ~ 0.34,
                 all(is.na(..1), is.na(..2), !is.na(..3)) ~ 0,
                 all(!is.na(..1), is.na(..2), is.na(..3)) ~ 0
               )
             )) %>%
    mutate(fig_col3 =
             pmap_dbl(
               .l = list(..1 = lower_limit, ..2 = middle_limit, ..3 = upper_limit),
               .f = ~ case_when(
                 all(!is.na(..1), !is.na(..2), !is.na(..3)) ~ 0.25,
                 all(!is.na(..1), is.na(..2), !is.na(..3)) ~ 0,
                 all(is.na(..1), is.na(..2), !is.na(..3)) ~ 0,
                 all(!is.na(..1), is.na(..2), is.na(..3)) ~ 0
               )
             )) %>%
    mutate(fig_col4 =
             pmap_dbl(
               .l = list(..1 = lower_limit, ..2 = middle_limit, ..3 = upper_limit),
               .f = ~ case_when(
                 all(!is.na(..1), !is.na(..2), !is.na(..3)) ~ 0.25,
                 all(!is.na(..1), is.na(..2), !is.na(..3)) ~ 0.33,
                 all(is.na(..1), is.na(..2), !is.na(..3)) ~ 0.382,
                 all(!is.na(..1), is.na(..2), is.na(..3)) ~ 0.618
               )
             )) %>%
    pivot_longer(
      cols = c('fig_col1', 'fig_col2', 'fig_col3', 'fig_col4'),
      names_to = 'fig_position',
      values_to = 'value'
    ) %>%
    mutate(across(.cols = fig_position, .fns = as_factor)) %>%
    mutate(across(
      .cols = fig_position,
      .fns = ordered,
      levels = c('fig_col1', 'fig_col2', 'fig_col3', 'fig_col4')
    )) %>%
    mutate(compound_figPosition = str_c(ch_compound, fig_position, sep = "") %>% as_factor()) %>%
    mutate(across(.cols = compound_figPosition, .fns = fct_reorder, ch_compound, as.integer)) %>%
    mutate(color = pmap_chr(
      .l = list(
        ..1 = lower_limit,
        ..2 = middle_limit,
        ..3 = upper_limit,
        ..4 = fig_position
      ),
      .f = ~ case_when(
        all(!is.na(..1), !is.na(..2), !is.na(..3), ..4 == "fig_col1") ~ "L1",
        all(!is.na(..1), !is.na(..2), !is.na(..3), ..4 == "fig_col2") ~ "L2",
        all(!is.na(..1), !is.na(..2), !is.na(..3), ..4 == "fig_col3") ~ "L3",
        all(!is.na(..1), !is.na(..2), !is.na(..3), ..4 == "fig_col4") ~ "L4",
        all(!is.na(..1), is.na(..2), !is.na(..3), ..4 == "fig_col1") ~ "L1",
        all(!is.na(..1), is.na(..2), !is.na(..3), ..4 == "fig_col2") ~ "L2",
        all(!is.na(..1), is.na(..2), !is.na(..3), ..4 == "fig_col3") ~ "BL",
        all(!is.na(..1), is.na(..2), !is.na(..3), ..4 == "fig_col4") ~ "L6",
        all(is.na(..1), is.na(..2), !is.na(..3), ..4 == "fig_col1") ~ "L1",
        all(is.na(..1), is.na(..2), !is.na(..3), ..4 == "fig_col2") ~ "BL",
        all(is.na(..1), is.na(..2), !is.na(..3), ..4 == "fig_col3") ~ "BL",
        all(is.na(..1), is.na(..2), !is.na(..3), ..4 == "fig_col4") ~ "L5",
        all(!is.na(..1), is.na(..2), is.na(..3), ..4 == "fig_col1") ~ "L0",
        all(!is.na(..1), is.na(..2), is.na(..3), ..4 == "fig_col2") ~ "BL",
        all(!is.na(..1), is.na(..2), is.na(..3), ..4 == "fig_col3") ~ "BL",
        all(!is.na(..1), is.na(..2), is.na(..3), ..4 == "fig_col4") ~ "L1"
      )
    ))


  # ggplot2
  fig <- ggplot() +
    geom_col(
      data = ggplot_col,
      aes(y = ch_compound, x = value, fill = color),
      width = barWidth,
      position = position_stack(reverse = TRUE)
    ) +
    geom_point(
      data = transformer,
      aes(y = ch_compound, x = trans_value),
      size = 1,
      color = 'blue'
    ) +
    geom_text(
      data = transformer,
      aes(y = ch_compound, x = trans_value, label = value),
      size = 3,
      color = "black",
      nudge_y = 0.3
    ) +
    scale_fill_manual(
      values = c(
        "L0" = "#C80509",
        "L1" = '#59CE33',
        "L2" = '#E98F91',
        "L3" = '#ED4D50',
        "L4" = '#C80509',
        "BL" = '#000000',
        "L5" = '#C80509',
        "L6" = '#C80509'
      )
    ) +
    theme(
      legend.position = 'none',
      axis.title = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = textSize),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      panel.background = element_rect(fill = "white", colour = "white")
    ) +
    scale_y_discrete(limits = rev)

  return(fig)
}

#' trans2show_ceram_ggplot
#' @description generate showing figure for ceram
#' @importFrom dplyr filter select left_join case_when mutate across
#' @importFrom tidyr pivot_longer
#' @importFrom forcats as_factor
#' @importFrom ggplot2 ggplot geom_col aes position_stack geom_point geom_label scale_fill_manual theme element_blank scale_y_discrete element_rect
#' @param data dataframe, detect value
#' @param HCY_limitsInfo dataframe, limits info
#' @return ggplot object
trans2show_ceram_ggplot <- function(score) {
  # preparing data for ggplot2
  ggplot_col <- tibble(ch_compound = "风险评估等级",score = score) %>%
    mutate(`低风险` = 2,
           `中风险` = 4,
           `中高风险` = 3,
           `高风险` = 3) %>%
    pivot_longer(cols = contains("风险"),names_to = 'fig_position',values_to = 'value') %>%
    mutate(across(.cols = fig_position,.fns = as_factor)) %>%
    mutate(across(.cols = fig_position,.fns = ordered,levels = c('低风险','中风险','中高风险','高风险')))
  # ggplot2
  fig <- ggplot() +
    geom_col(data = ggplot_col,
             aes(y = ch_compound,x = value,fill = fig_position),width = 0.2,
             position = position_stack(reverse = TRUE)) +
    geom_point(data = ggplot_col,aes(y = ch_compound,x = score),size = 1,color = 'blue') +
    geom_text(data = ggplot_col,
               aes(y = ch_compound,x = score,label = score),size = 3,color = "black",nudge_y = 0.15) +
    scale_fill_manual(values=c('#59CE33', '#E98F91', '#ED4D50','#C80509')) +
    theme(legend.position='none',
          axis.title = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x=element_blank(),
          axis.ticks.y=element_blank(),
          panel.background = element_rect(fill = "white", colour = "white")) +
    scale_y_discrete(limits = rev)
  return(fig)
}
