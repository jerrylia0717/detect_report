#' trans2show_dx1597_ggplot
#' @description generate showing figure for HCY
#' @importFrom dplyr filter select left_join case_when mutate across
#' @importFrom purrr pmap_dbl map_dbl map2_dbl
#' @importFrom tidyr pivot_longer
#' @importFrom tibble has_name
#' @importFrom forcats as_factor fct_reorder2
#' @importFrom ggplot2 ggplot geom_col aes position_stack geom_point geom_text
#'  scale_fill_manual theme element_blank scale_y_discrete element_text element_rect
#' @param data dataframe, detect value
#' @param reference dataframe, limits info
#' @param barWidth barWidth
#' @param textSize textSize
#' @param ch_compound_levels list chr
#' @return ggplot object
trans2show_dx1597_ggplot <- function(data,
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
               .l = list(..1 = value,
                         ..2 = lower_limit,
                         ..3 = upper_limit),
               .f = ~ case_when(
                 all(!is.na(..2), !is.na(..3)) ~ .trans2showvalueLU(..1, ..2, ..3,
                                                                    minimal = 0.191, maximal = 0.809),
                 all(is.na(..2),!is.na(..3)) ~ .trans2showvalueU(..1, ..3),
                 all(!is.na(..2), is.na(..3)) ~ .trans2showvalueL(..1, ..2)
               )
             )) %>%
    mutate(value = pmap_chr(
      .l = list(..1 = value, ..2 = quanLimit_lower, ..3 = quanLimit_upper),
      .f = ~ case_when(
        all(is.na(..2), is.na(..3)) ~ as.character(..1),
        all(!is.na(..2), !is.na(..3)) ~ case_when(
          ..1 < ..2 ~ "小于检测下限值",
          ..1 > ..3 ~ "大于检测上限值",
          TRUE ~ as.character(..1)
        ),
        all(is.na(..2), !is.na(..3)) ~ case_when(..1 > ..3 ~ "大于检测上限值",
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
    mutate(fig_col1 =
             map2_dbl(
               .x = lower_limit,
               .y = upper_limit,
               .f = ~ case_when(
                 all(!is.na(.x), !is.na(.y)) ~ 0.191,
                 all(is.na(.x), !is.na(.y)) ~ 0.618,
                 all(!is.na(.x), is.na(.y)) ~ 0.382
               )
             )) %>%
    mutate(fig_col2 =
             map2_dbl(
               .x = lower_limit,
               .y = upper_limit,
               .f = ~ case_when(
                 all(!is.na(.x), !is.na(.y)) ~ 0.618,
                 all(is.na(.x), !is.na(.y)) ~ 0,
                 all(!is.na(.x), is.na(.y)) ~ 0
               )
             )) %>%
    mutate(fig_col3 =
             map2_dbl(
               .x = lower_limit,
               .y = upper_limit,
               .f = ~ case_when(
                 all(!is.na(.x), !is.na(.y)) ~ 0.191,
                 all(is.na(.x), !is.na(.y)) ~ 0.382,
                 all(!is.na(.x), is.na(.y)) ~ 0.618
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
    # mutate(across(.cols = compound_figPosition, .fns = ordered, levels = ch_compound_levels)) %>%
    mutate(color = pmap_chr(
      .l = list(..1 = lower_limit,
                ..2 = upper_limit,
                ..3 = fig_position),
      .f = ~ case_when(
        all(!is.na(..1),!is.na(..2), ..3 == "fig_col1") ~ "L1",
        all(!is.na(..1),!is.na(..2), ..3 == "fig_col2") ~ "L2",
        all(!is.na(..1),!is.na(..2), ..3 == "fig_col3") ~ "L3",
        all(!is.na(..1), is.na(..2), ..3 == "fig_col1") ~ "L1",
        all(!is.na(..1), is.na(..2), ..3 == "fig_col2") ~ "BL",
        all(!is.na(..1), is.na(..2), ..3 == "fig_col3") ~ "L2",
        all(is.na(..1),!is.na(..2), ..3 == "fig_col1") ~ "L2",
        all(is.na(..1),!is.na(..2), ..3 == "fig_col2") ~ "BL",
        all(is.na(..1),!is.na(..2), ..3 == "fig_col3") ~ "L3"
      )
    )) %>%
    add_row(ch_compound = c("免疫调节激素:",'雄性激素:',"雌性激素:","孕激素:"),value = 0) %>%
    mutate(across(
      .cols = ch_compound,
      .fns = ordered,
      levels = ch_compound_levels
    )) %>%
    arrange(ch_compound)
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
      nudge_y = 0.4
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
