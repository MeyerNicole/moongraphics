
source("R/validations/moon_bar.R")

moon_bar <- function(data, groups, fill_by = NULL, wrap_by = NULL,
                     palette = NULL, text_mode = NULL, stack_limit = 5) {

  # Validating input

  input <- list(groups = groups, fill_by = fill_by, wrap_by = wrap_by)

  message <- validate_moon_bar_parameters(input)

  if(message != "") {stop(message)}

  # Grouping and summarising data

  df <- data %>%
    group_by(!!!syms(groups)) %>%
    summarise(n = n()) %>%
    mutate(aux__ = 1)

  # Handling parameter logics

  if(length(groups) > 1) {

    grouping_factor <- ifelse(is.null(fill_by), wrap_by, fill_by)
    x <- groups[groups != grouping_factor]
    fill <- ifelse(!is.null(fill_by), fill_by, "aux__")
    color <- ifelse(is.null(palette) & !is.null(fill_by), "YlGnBu",
                    ifelse(is.null(palette) & !is.null(wrap_by), "darkblue", palette))

    if(length(unique(df[[grouping_factor]])) > stack_limit) {

      position <- position_dodge()

    } else {

      position <- position_stack()

    }


  } else {

    grouping_factor <- NULL
    x <- groups
    fill <- "aux__"
    color <- ifelse(is.null(palette), "darkblue", palette)
    position <- position_dodge()

  }

  # Defining base plot

  plot <- ggplot(data = df, aes(x = !!sym(x), y = n, fill = !!sym(fill))) +
    theme_bw() + labs(y = "Frequência")

  # Adding the bar layer

  if(length(groups) > 1 & !is.null(fill_by)) {
    plot <- plot + geom_bar(stat = "identity", position = position)
  } else {
    plot <- plot + geom_bar(stat = "identity", fill = color, position = position)
  }

  # Fill: adding fill palette

  if(length(groups) > 1 & !is.null(fill_by)) {

    if(is.null(palette)) {

      plot <- plot + scale_fill_brewer(palette = color)

    } else {

      plot <- plot + scale_fill_manual(values = color)

    }

  }

  # Adding text

  if (!is.null(text_mode)) {

    if (text_mode == "n") {
      plot <- plot + geom_text(data = df, aes(label = n), position = position_dodge(width = 0.9), vjust = -0.25)
    }

    if (text_mode == "%") {

      if (is.null(grouping_factor)) {
        df <- df %>% mutate(per = round(100*(n/sum(n)),1))
      } else {
        df <- df %>% group_by(!!sym(x)) %>% mutate(per = round(100*(n/sum(n)),1))

      }

      plot <- plot + geom_text(data = df, aes(label = paste0(per, "%")), position = position_dodge(width = 0.9), vjust = -0.25)

    }

    if (text_mode == "n%" | text_mode == "%n") {

      if (is.null(grouping_factor)) {
        df <- df %>% mutate(per = round(100*(n/sum(n)),0))
      } else {
        df <- df %>% group_by(!!sym(x)) %>% mutate(per = round(100*(n/sum(n)),0))

      }

      plot <- plot + geom_text(data = df, aes(label = paste0(n, paste0("(", per, "%", ")"))), position = position_dodge(width = 0.9), vjust = -0.25)

    }
  }

  # Wrap: wrapping by selected variable

  if(!is.null(wrap_by)) {

    plot <- plot + facet_wrap(eval(parse(text = paste0("~", wrap_by))))

  }

  return(plot)

}
