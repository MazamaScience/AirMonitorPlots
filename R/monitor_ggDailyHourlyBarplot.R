#' @export
#' @import ggplot2
#' @importFrom rlang .data
#'
#' @title Create a Daily-Hourly plot for many monitors
#'
#' @description
#' Create a time series barplot showing PM2.5 data for the given monitors. The
#' overall plot is faceted by monitor, and each facet has two sets of columns:
#' one for daily levels, and one for hourly levels.
#'
#' The full range of data in \code{monitor} will be used unless both
#' \code{startdate} and \code{enddate} are specified.
#'
#' The timezone specified or, if \code{timezone = NULL}, that of the first
#' monitor encountered will be used for all time axes.
#'
#' @param monitor A \emph{mts_monitor} object.
#' @param startdate Desired start date (integer or character in ymd format or
#'   POSIXct).
#' @param enddate Desired end date (integer or character in ymd format or
#'   POSIXct).
#' @param id Optional vector of deviceDeploymentIDs used to filter the data.
#' @param columns Number of columns the faceted plot should have (default 1).
#' @param title The title of the plot. Defaults to specifying the types of
#'   data present in the plot.
#' @param timezone Olson timezone name for x-axis scale and date parsing. If
#'   NULL the timezone of the specified monitor will be used.
#' @param xLabel The x-axis label of the plot. Defaults to years present in
#'   data.
#' @param yLabel The y-axis label of the plot. Defaults to PM2.5.
#' @param includeLink Option to include a link to an AQI help page at the bottom
#'   of the plot (default `TRUE`).
#' @param hourlyDataType The type of hourly data to include in the plot. The
#'   options include "nowcast" (hourly nowcast values), "raw" (raw hourly values),
#'   or "none" (no hourly data at all) (default "nowcast").
#' @param palette The ordered color palette used to represent each AQI
#'   category. Currently defaults to (and only accepts) "epa_aqi".
#' @param includeLegendAdvice Option to include a third column in the legend for
#'   AQI level advice. Currently in testing (default `False`).
#'
#' @return A **ggplot** plot of the given monitors and data.
#'
#' @examples
#' \dontrun{
#' library(AirMonitorPlots)
#'
#' SF_IDs <- c(
#'   "ccdef3f0f6591e77_060010009",
#'   "06c3f2a66f8b708e_060010012",
#'   "7157b3dbac7c2043_060010011",
#'   "060750fa7ae26987a72cc4_060750005005_01"
#' )
#' SF_daily <- loadDaily() %>% monitor_select(id = SF_IDs)
#' SF_latest <- loadLatest() %>% monitor_select(id = SF_IDs)
#' SF_full <- monitor_join(SF_daily, SF_latest)
#' today <- lubridate::floor_date(lubridate::now('America/Los_Angeles'), unit='day')
#' now <- lubridate::floor_date(lubridate::now('America/Los_Angeles'), unit='hour')
#' starttime <- today - lubridate::ddays(4)
#' SF_4day <- monitor_filterDatetime(SF_full, starttime, now))
#'
#' # Create plot using pre subset data
#' monitor_ggDailyHourlyBarplot(SF_4day, id = SF_IDs)
#'
#' # Create plot using data subset by function
#' monitor_ggDailyHourlyBarplot(SF_full, starttime, now, SF_IDs)
#' }

monitor_ggDailyHourlyBarplot <- function(
  monitor,
  startdate = NULL,
  enddate = NULL,
  id = NULL,
  columns = 1,
  title = NULL,
  timezone = NULL,
  xLabel = NULL,
  yLabel = NULL,
  includeLink = TRUE,
  hourlyDataType = c("nowcast", "raw", "none"),
  palette = "epa_aqi",
  includeLegendAdvice = FALSE
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(monitor)

  # TODO: make function work with tidy monitor data
  #      Need to implement a `monitor_dailyStatistic()` function for tidy
  #      monitor data
  if ( !AirMonitor::monitor_isValid(monitor) ) {
    stop("This function can currently only take in a 'mts_monitor' object")
  }

  # Convert monitor to tidy structure
  mts_tidy <- monitor_toTidy(monitor)

  # Check deviceDeploymentIDs
  if ( any(!id %in% unique(mts_tidy$deviceDeploymentID)) ) {
    invalidIDs <- id[which(!id %in% unique(mts_tidy$deviceDeploymentID))]
    stop(paste0(
      "Invalid ids specified. 'monitor' does not contain deviceDeploymentIDs: ",
      paste0(invalidIDs, collapse = ", ")
    ))
  }

  # Check hourlyDataType
  hourlyDataType <- match.arg(hourlyDataType)

  # Check timezone
  if ( !is.null(timezone) ) {
    if ( !timezone %in% OlsonNames() ) {
      stop("Invalid timezone")
    }
  } else {
    timezone <- unique(monitor$meta$timezone[1])
  }


  # ----- Set up data ----------------------------------------------------------

  # Get data from monitors

  singleMonitor <-
    monitor %>%
    AirMonitor::monitor_select(id)

  # Get time limits

  # Use full time range if startdate or enddate is missing
  if ( is.null(startdate) || is.null(enddate) ) {
    timeRange <- range(monitor$data$datetime)
    startdate <- timeRange[1]
    enddate <- timeRange[2]
  }

  dateRange <- MazamaCoreUtils::dateRange(
    startdate = startdate,
    enddate = enddate,
    timezone = timezone,
    unit = "day",
    ceilingEnd = TRUE
  )

  # Transform data

  # Calculate daily data (or none)
  # TODO: Add ability to include only hourly values (no daily)
  includeDaily <- TRUE
  if (includeDaily) {

    dailyData <-
      singleMonitor %>%
      AirMonitor::monitor_dailyStatistic() %>%
      AirMonitor::monitor_filterDate(startdate, enddate, ceilingEnd = TRUE) %>%
      monitor_toTidy() %>%
      dplyr::mutate(
        aqiCategory = cut(
          round(.data$pm25, digits = 0),
          AirMonitor::US_AQI$breaks_PM2.5_2024,
          right = TRUE,
          include.lowest = TRUE,
          labels = AirMonitor::US_AQI$names_eng
        )
      )

  } else {

    dailyData <- NULL

  }

  # Calculate the appropriate hourly values (or none)
  if ( hourlyDataType != "none" ) {

    if ( hourlyDataType == "nowcast" ) {

      hourlyData <-
        singleMonitor %>%
        AirMonitor::monitor_nowcast(includeShortTerm = TRUE) %>%
        AirMonitor::monitor_filterDate(startdate, enddate, ceilingEnd = TRUE) %>%
        monitor_toTidy() %>%
        dplyr::mutate(
          aqiCategory = cut(
            round(.data$pm25, digits = 0),
            AirMonitor::US_AQI$breaks_PM2.5_2024,
            right = TRUE,
            include.lowest = TRUE,
            labels = AirMonitor::US_AQI$names_eng
          )
        )

    } else {

      # hourlyDataType == "raw"
      hourlyData <-
        singleMonitor %>%
        AirMonitor::monitor_filterDate(startdate, enddate, ceilingEnd = TRUE) %>%
        monitor_toTidy() %>%
        dplyr::mutate(
          aqiCategory = cut(
            round(.data$pm25, digits = 0),
            AirMonitor::US_AQI$breaks_PM2.5_2024,
            right = TRUE,
            include.lowest = TRUE,
            labels = AirMonitor::US_AQI$names_eng
          )
        )
    }

  } else {

    hourlyData <- NULL

  }

  # ----- Set up labels --------------------------------------------------------

  if ( is.null(title) ) {

    if ( includeDaily ) {
      dailyPart <- "Daily (AQI)"
    } else {
      dailyPart <- NULL
    }

    if ( hourlyDataType == "nowcast" ) {
      hourlyPart <- "Hourly (NowCast)"
    } else if ( hourlyDataType == "raw" ) {
      hourlyPart <- "Hourly (raw)"
    } else {
      hourlyPart <- NULL
    }

    if ( includeDaily && hourlyDataType != "none" ) {
      titlePart <- paste(dailyPart, hourlyPart, sep = " and ")
    } else {
      titlePart <- paste0(dailyPart, hourlyPart)
    }

    title <- bquote(.(titlePart) ~ PM[2.5] ~ "Levels")

  }

  if ( is.null(xLabel) ) {
    yearPart <- paste(
      unique(lubridate::year(dailyData$datetime)),
      collapse = ", ")

    xLabel <- paste0("Date, midnight to midnight (", yearPart, ")")
  }

  if ( is.null(yLabel) ) {
    yLabel <- expression(paste("PM"[2.5] * " (", mu, "g/m"^3 * ")"))
  }

  if ( includeLink ) {
    caption <-
      "Learn more about AQI at: airnow.gov/index.cfm?action=aqibasics.aqi"
  } else {
    caption <- NULL
  }

  # ----- Define scales --------------------------------------------------------

  # Color scale

  aqiNames <- AirMonitor::US_AQI$names_eng
  aqiActions <- AirMonitor::US_AQI$actions_eng

  if ( palette == "epa_aqi" ) {
    aqiColors <- AirMonitor::US_AQI$colors_EPA
  } else {
    message(
      "Color scale not recognized: ",
      palette,
      ". Defaulting to EPA AQI colors."
    )
    aqiColors <- AirMonitor::US_AQI$colors_EPA
  }

  # Time scale

  minDate <- lubridate::as_datetime(min(dailyData$datetime))
  maxDate <- lubridate::as_datetime(max(dailyData$datetime))

  timeSpan <- lubridate::as.interval(maxDate - minDate, minDate)
  numDays <- timeSpan %/% lubridate::days(1)

  if (numDays <= 14) {
    timeScale <- "day"
  } else if (numDays <= 30) {
    timeScale <- "2 days"
  } else if (numDays <= 150) {
    timeScale <- "week"
  } else {
    timeScale <- "month"
  }

  ## datetime vector for date labels

  if ( is.null(hourlyData) ) {
    datetimeValues <- dailyData$datetime
  } else {
    datetimeValues <- hourlyData$datetime
  }

  datetimeValues <- datetimeValues %>% lubridate::with_tz(tzone = timezone)

  datetimeLabelsMajor <- unique(
    lubridate::floor_date(
      datetimeValues,
      unit = timeScale
    )
  ) + lubridate::dhours(12)

  datetimeLabelsMinor <- unique(
    lubridate::floor_date(
      datetimeValues,
      unit = "day"
    )
  )

  # ----- Plot data ------------------------------------------------------------

  # TODO: create new ggplot stat object to handle daily data computation
  # TODO: add ability to create plot with either raw hourly data or nowcast
  gg <-
    ggplot2::ggplot(
      dailyData,
      ggplot2::aes_(
        x = ~ datetime,
        y = ~ pm25,
        fill = ~ aqiCategory)
      ) +
    ggplot2::geom_col(
      data = hourlyData,
      ggplot2::aes_(color = ~ aqiCategory), # needed for legend
      width = 3600 * .45,
      size = 0
    ) +
    ggplot2::geom_col(
      data = dailyData,
      ggplot2::aes_(x = ~ datetime + lubridate::dhours(12)),
      width = 86400,
      alpha = 0.3,
      color = "grey20",
      size = .1
    ) +
    ggplot2::facet_wrap(~ locationName, ncol = columns) +

    # TODO: combine AQI text into single scale
    # TODO: make legend scale with plot size
    ggplot2::scale_fill_manual(
      name = "Daily Air Quality Index (24hr AQI)",
      values = aqiColors,
      labels = aqiNames,
      drop = FALSE,
      guide = ggplot2::guide_legend(
        order = 1,
        reverse = TRUE,
        override.aes = list(alpha = 1, color = NA)
      )
    ) +
    ggplot2::scale_color_manual(
      name = "Hourly NowCast (actions to protect yourself)",
      values = aqiColors,
      labels = aqiActions,
      drop = FALSE,
      guide = ggplot2::guide_legend(
        order = 2,
        reverse = TRUE,
        override.aes = list(color = NA, fill = NA)
      )
    ) +

    ggplot2::scale_x_datetime(
      breaks = datetimeLabelsMajor,
      minor_breaks = datetimeLabelsMinor,
      date_labels = "%b %d",
      expand = c(0, 0)
    ) +

    # TODO: make labels a parameter
    # TODO: make labels scale with plot size
    ggplot2::labs(
      title = title,
      x = xLabel,
      y = yLabel,
      caption = caption
    ) +
    theme_dailyHourlyBarplot_airfire(base_size = 12)

  if (includeLegendAdvice) {
    gg <-
      gg +
      ggplot2::geom_point(
        data = dailyData,
        aes_(
          x = ~ datetime + lubridate::dhours(12),
          shape = ~ aqiCategory
        ),
        alpha = 0
      ) +
      ggplot2::scale_shape_manual(
        values = c(6:11),
        name = "Standard Advice",
        labels = c(
          "Tempus vitae molestie Convallis curabitur vestibulum",
          "Justo rutrum A ut arcu felis eget litora libero",
          "ligula morbi vestibulum eu tellus vel consectetur",
          "congue. Ultricies et commodo amet accumsan nunc eget",
          "mattis tincidunt nonummy donec viverra sed nec iaculis",
          "consectetuer lectus dictum. Velit iaculis a"
        ),
        drop = FALSE,
        na.translate = FALSE
      ) +
      ggplot2::theme(legend.text = element_text(size = rel(.5)))
  }

  return(gg)

}
