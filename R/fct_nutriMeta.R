#' interpret_nutriMeta
#' @param data detect data
#' @param reference limits info; tibble
#' @param interpret_text interpret text; tibble
#' @param product_label product type; character
#' @param ch_compound_levels list
#' @param collapse collapse; character
interpret_nutriMeta <-
  function(data,
           reference,
           interpret_text,
           product_label,
           ch_compound_levels,
           collapse = '<br />&emsp;&emsp;') {
    outers <- data %>%
      left_join(reference, by = "compound") %>%
      mutate(across(.cols = ch_compound, .fns = as_factor)) %>%
      mutate(across(
        .cols = ch_compound,
        .fns = ordered,
        levels = ch_compound_levels
      )) %>%
      arrange(ch_compound) %>%
      mutate(state = pmap_chr(
        .l = list(..1 = value, ..2 = lower_limit, ..3 = upper_limit),
        .f = ~ case_when(between(..1, ..2, ..3) ~ "normal",
                         ..1 < ..2 ~ "偏低",
                         ..1 > ..3 ~ "偏高")
      )) %>%
      filter(state != "normal")

    if (nrow(outers) == 0) {
      text <- "本次检测，您的各项指标均正常。"
    } else if (all("偏高" %in% unique(outers$state),
                   "偏低" %in% unique(outers$state))) {
      text <- paste0(
        "偏低的",
        product_label,
        "为：",
        paste0(
          outers %>% filter(state == "偏低") %>% select(ch_compound) %>% pull(),
          collapse = '、'
        ),
        "；",
        "偏高的",
        product_label,
        "为：",
        paste0(
          outers %>% filter(state == "偏高") %>% select(ch_compound) %>% pull(),
          collapse = '、'
        ),
        "。"
      )
    } else if (unique(outers$state) == "偏高") {
      text <- paste0(
        "偏高的",
        product_label,
        "为：",
        paste0(
          outers %>% filter(state == "偏高") %>% select(ch_compound) %>% pull(),
          collapse = '、'
        ),
        "。"
      )
    } else if (unique(outers$state) == "偏低") {
      text <- paste0(
        "偏低的",
        product_label,
        "为：",
        paste0(
          outers %>% filter(state == "偏低") %>% select(ch_compound) %>% pull(),
          collapse = '、'
        ),
        "。"
      )
    }

    interpretation_tibble <- interpret_text %>%
      left_join(outers, ., by = c("ch_compound", "state")) %>%
      filter(!is.na(text))

    if (nrow(interpretation_tibble) == 0) {
      interpretation <-
        paste0("您体内的",
              product_label,
              "水平目前均处于正常状态，建议您继续保持良好的生活习惯和饮食习惯，祝您健康愉快！",
              collapse = "")
    } else {
      interpretation_tibble <- interpretation_tibble %>%
        mutate(across(.cols = state, .fns = as_factor)) %>%
        mutate(across(.cols = ch_compound, .fns = as_factor)) %>%
        mutate(across(
          .cols = ch_compound,
          .fns = ordered,
          levels = ch_compound_levels
        )) %>%
        mutate(across(
          .cols = state,
          .fns = ordered,
          levels = c("偏低","偏高")
        )) %>%
        arrange(state,ch_compound)

      interpretation <-
        paste0(interpretation_tibble$text, collapse = collapse)
    }
    return(list(interpretation = interpretation,
                results = text))
  }
