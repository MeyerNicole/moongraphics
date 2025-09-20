#' @title Pre-designed Bar Charts
#'
#' @description
#' Creates bar charts with the appropriate groups, frequencies, methods, themes and other features
#' according to the Moon theme definitions.
#'
#' @param data dataframe or tibble. Dataset to be used for the plot.
#' @param groups character vector. Defines the variable(s) that should be used for frequency calculations.
#' This function does not accept vectors with more than 2 variables, as bar charts would become unreadable
#' with more than 3 attributes (2 groups + frequency).
#' @param fill_by character. When using 2 groups, a method should always be chosen to insert the second group into the
#' plot. This parameter defines the which variable should be used as the fill color for the chart.
#' @param wrap_by character. When using 2 groups, a method should always be chosen to insert the second group into the
#' plot. This parameter defines the which variable should be used to separate (wrap) the charts.
#' @param palette character/vector. Pick a color to be used on the chart. Keep in mind that the number of colors
#' specified should be equal to the number of categories in the grouping variable.
#' @param text_mode character. Pick a method to display the frequency labels. Accepts the values: \code{"n"}, for
#' frequency, \code{"p"}, for the frequency in percentage and \code{"np"} or \code{"pn"} for frequency in absolute value and relative percentage.
#'
#' @import dplyr
#' @import ggplot2
#' @import cli
#'
#' @return the requested plot.
#'
#' @export

moon_bar <- function(data, groups, fill_by = NULL, wrap_by = NULL,
                     palette = NULL, text_mode = NULL) {

  # Validating input

  input <- list(groups = groups, fill_by = fill_by, wrap_by = wrap_by, text_mode = text_mode)

  message <- validate_moon_bar_parameters(input)

  if(message != "") {cli::cli_abort(message)}

  # Grouping and summarising data

  df <- data %>%
    group_by(!!!syms(groups)) %>%
    summarise(n = n())

  # Handling parameter logics

  if(length(groups) > 1) {

    grouping_factor <- ifelse(is.null(fill_by), wrap_by, fill_by)
    x <- groups[groups != grouping_factor]
    fill <- ifelse(!is.null(fill_by), fill_by, x)

  } else {

    grouping_factor <- NULL
    x <- groups
    fill <- x

  }

  # Defining base plot

  plot <- ggplot(data = df, aes(x = !!sym(x), y = n, fill = !!sym(fill))) +
    theme_bw() + labs(y = "Frequência")

  # Adding the bar layer

    plot <- plot + geom_bar(stat = "identity", position = position_dodge())

  # Fill: adding fill palette

    if(is.null(palette)) {

      if(length(groups) > 1) {
        plot <- plot + scale_fill_brewer(palette = "YlGnBu")
      } else {
        plot <- plot + scale_fill_manual(values = rep("darkblue", times = length(unique(df[[x]])))) +
          theme(legend.position = "none")
      }

    } else {

      if(length(palette) > 1) {

        plot <- plot + scale_fill_manual(values = palette)

      } else {

        if(!is.null(fill_by)) {
          plot <- plot + scale_fill_manual(values = rep(palette, times = length(unique(df[[grouping_factor]]))))
        } else {
          plot <- plot + scale_fill_manual(values = rep(palette, times = length(unique(df[[x]])))) +
            theme(legend.position = "none")
        }
      }
    }

  # Adding text

  if (!is.null(text_mode)) {

    if (text_mode == "n") {
      plot <- plot + geom_text(data = df, aes(label = n), position = position_dodge(width = 0.9), vjust = -0.25)
    }

    if (text_mode == "p") {

      if (is.null(grouping_factor)) {
        df <- df %>% mutate(per = round(100*(n/sum(n)),1))
      } else {
        df <- df %>% group_by(!!sym(x)) %>% mutate(per = round(100*(n/sum(n)),1))

      }

      plot <- plot + geom_text(data = df, aes(label = paste0(per, "%")),
                               position = position_dodge(width = 0.9), vjust = -0.25)

    }

    if (text_mode == "np" | text_mode == "pn") {

      if (is.null(grouping_factor)) {
        df <- df %>% mutate(per = round(100*(n/sum(n)),0))
      } else {
        df <- df %>% group_by(!!sym(x)) %>% mutate(per = round(100*(n/sum(n)),0))

      }

      plot <- plot + geom_text(data = df, aes(label = paste0(n, paste0("(", per, "%", ")"))),
                               position = position_dodge(width = 0.9), vjust = -0.25)

    }
  }

  # Wrap: wrapping by selected variable

  if(!is.null(wrap_by)) {

    plot <- plot + facet_wrap(eval(parse(text = paste0("~", wrap_by))))

  }

  return(plot)

}
