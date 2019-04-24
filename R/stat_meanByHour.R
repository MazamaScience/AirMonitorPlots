#' @title Add hourly averages to a plot
#'
#' @description
#' This function calculates the mean y-value for each x-value. Should be used
#' only when \code{x} is discrete. The resulting mean can be mapped to any
#' aesthetic, specified with the \code{output} parameter.
#'
#' @param mapping Set of aesthetic mappings created by \code{aes()}. If
#'   specified and \code{inherit.aes = TRUE} (the default), it is combined with
#'   the default mapping at the top level of the plot. You must supply
#'   \code{mapping} if there is no plot mapping.
#' @param data The data to be displayed in this layer. There are three options:
#'   if \code{NULL}, the default, the data is inherited from the plot data. A
#'   \code{data.frame} or other object, will override the plot data. A
#'   \code{function} will be called witha  single argument, the plot data. The
#'   return value must be a \code{data.frame}, and will be used as the layer
#'   data.
#' @param output "AQIColors", "mv4Colors", "y"
#' @param input The value to find the mean of. If \code{NULL}, the default
#'   \code{y} value will be used.
#' @param geom The geometic object to display the data
#' @param position Position adjustment, either as a string, or the result of a
#'   call to a position adjustment function.
#' @param na.rm remove NA values from data
#' @param show.legend logical indicating whether this layer should be included
#'   in legends.
#' @param inherit.aes if \code{FALSE}, overrides the default aesthetics, rather
#'   than combining with them. This is most useful for helper functions that
#'   define both data and the aesthetics and shouldn't inherit behaviour from
#'   the default plot specificatino, eg \code{borders()}.
#' @param ... additional arguments passed on to \code{layer()}, such as
#'   aesthetics.
#'
#' @import ggplot2
#' @export
#'
#' @examples
#' \dontrun{
#' ggplot_pm25Timeseries(PWFSLSmoke::Northwest_Megafires,
#'                       startdate = 20150820,
#'                       enddate = 20150831) +
#'   geom_point(shape = "square", alpha = 0.05) +
#'   stat_meanByHour(geom = "line", color = "orange", size = 3)
#'
#' ggplot_pm25Diurnal(PWFSLSmoke::Carmel_Valley,
#'                    startdate = 20160801,
#'                    enddate = 20160810) +
#'   geom_path(aes(group = day), color = "gray50") +
#'   stat_meanByHour(geom = "line", size = 4)
#' }

stat_meanByHour <- function(
  mapping = NULL,
  data = NULL,
  input = NULL,
  output = "y",
  geom = "bar",
  position = "identity",
  na.rm = TRUE,
  show.legend = NA,
  inherit.aes = TRUE,
  ...
) {

  if (!is.null(input)) {
    if (is.null(mapping)) {
      mapping <- aes_string(input = input)
    } else {
      mapping$input <- rlang::parse_expr(input)
    }
  }

  list(
    layer(
      stat = StatMeanByGroup,
      data = data,
      mapping = mapping,
      geom = geom,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(
        output = output,
        input = input,
        na.rm = na.rm,
        ...
      )
    )
  )

}


StatMeanByGroup <- ggproto(
  "StatMeanByGroup",
  Stat,
  # BEGIN compute_group function
  compute_group = function(data,
                           scales,
                           params,
                           input,
                           output,
                           na.rm) {

    df <- data
    if (is.null(input)) df$input <- df$y

    means <- df %>%
      dplyr::group_by(.data$x) %>%
      dplyr::summarise(mean = mean(.data$input, na.rm = na.rm),
                       mean_y = mean(.data$y, na.rm = TRUE))

    # Set x and y
    data <- data.frame(x = means$x,
                       y = means$mean_y)

    # Set output aesthetic
    if (output %in% c("AQIColors", "mv4Colors")) {

      # Add column for AQI level
      data$aqi <- .bincode(means$mean, AQI$breaks_24, include.lowest = TRUE)
      if (!"colour" %in% names(data)) {
        data$colour <- ifelse(output == "mv4Colors",
          AQI$mv4Colors[data$aqi],
          AQI$colors[data$aqi]
        )
      }

      if (!"fill" %in% names(data)) {
        data$fill <- ifelse(output == "mv4Colors",
          AQI$mv4Colors[data$aqi],
          AQI$colors[data$aqi]
        )
      }

    } else {
      # Map the mean to the correct aesthetic
      data[output] <- means$mean
    }

    return(data)
  }
  # END compute_group function

)
