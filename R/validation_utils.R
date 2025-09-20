validate_moon_bar <- function(params) {

  message <- ""

  if(length(params$groups) == 1 & (!is.null(params$fill_by) | !is.null(params$wrap_by))) {

    message <- "The grouped view parameters fill_by and wrap_by are reserved for usage with 2 grouping variables."

  }

  if(length(params$groups) == 2 & is.null(params$fill_by) & is.null(params$wrap_by)) {

    message <- "To use 2 grouping variables, please pick which one should be the grouping factor and the method using fill_by or wrap_by."

  }

  if(!is.null(params$fill_by) & !is.null(params$group_by)) {

    message <- "Only one of the grouped view methods should be picked, either filling or wrapping."

  }

  if(length(params$groups) > 2) {

    message <- "More than 2 grouping variables is not supported."

  }

  if(!is.null(params$text_mode)) {

    if(!(params$text_mode %in% c("n", "p", "pn", "np"))) {

      message <- "Unrecognized method. Implemented text modes: 'n', 'p', 'pn'/'np'"

    }
  }

  return(message)

}
