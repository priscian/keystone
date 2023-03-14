## Get cardinal-point coordinates from plot device.
#' @export
get_cardinal_point <- function(
  cardinal_position = "center",
  margin_x_percent = 0, margin_y_percent = margin_x_percent,
  as_list = TRUE
)
{
  cp <- cardinal_position

  marginXWidth <- (par("usr")[2] - par("usr")[1]) * (margin_x_percent / 100)
  marginYHeight <- (par("usr")[4] - par("usr")[3]) * (margin_y_percent / 100)
  top <- par("usr")[4] - marginYHeight
  right <- par("usr")[2] - marginXWidth
  bottom <-  par("usr")[3] + marginYHeight
  left <- par("usr")[1] + marginXWidth
  centerX <- left + (right - left) / 2
  centerY <- bottom + (top - bottom) / 2

  coordinates <- switch(cp,
    n =,
    north = c(x = centerX, y = top),
    ne =,
    northeast = c(x = right, y = top),
    e =,
    east = c(x = right, y = centerY),
    se =,
    southeast = c(x = right, y = bottom),
    s =,
    south = c(x = centerX, y = bottom),
    sw =,
    southwest = c(x = left, y = bottom),
    w =,
    west = c(x = left, y = centerY),
    nw =,
    northwest = c(x = left, y = top),
    c =,
    center = c(x = centerX, y = centerY),
    c(x = centerX, y = centerY) # default option
  )

  if (as_list)
    coordinates <- as.list(coordinates)

  return (coordinates)
}


#' @export
get_cp_coords <- function(...)
{
  coordinates <- get_cardinal_point(...)

  return (xy.coords(coordinates$x, coordinates$y))
}

#' @export
cp_coords <- get_cp_coords


#' @export
scale_color_brewer_recycle <- function (..., type = "seq", palette = 1L, direction = 1L)
{
  ggplot2::discrete_scale(aesthetics = "colour",
    scale_name = palette,
    palette = brewer_pal_recycle(type, palette, direction),
    ...)
}

#' @export
scale_colour_brewer_recycle <- scale_color_brewer_recycle


#' @export
brewer_pal_recycle <- function (type = "seq", palette = 1L, direction = 1L)
{
  pal <- scales:::pal_name(palette, type)

  function(n) {
    suppressWarnings(cols <- RColorBrewer:::brewer.pal(n, pal))
    pal <- rep(cols, length.out=n)
    if (direction == -1L)
      pal <- rev(pal)

    return (pal)
  }
}


#' @export
vary_brightness <- function(color, len, end = NULL)
{
  # 'color' is a single value of any of the three kinds of R color specifications, i.e. either a color name
  #   (as listed by 'colors()'), a hexadecimal string of the form "#rrggbb", or a positive integer i meaning 'palette()[i]'.
  # 'len' is the number of brightness values to be in the palette.
  # 'end' is the ending brightness level in the palette; if NULL, 'end' will be set to half the brightness level of 'color'
  #   (i.e. the palette will move from brighter to darker).

  if (length(color) > 1L) color <- color[1L]
  startColorHsv <- grDevices::rgb2hsv(col2rgb(color))

  BuildHsvMatrix <- function(mat, n) { if (n == 0) return (mat); BuildHsvMatrix(cbind(mat, mat[, 1L]), n - 1L) }
  hsvMat <- BuildHsvMatrix(startColorHsv, len - 1L)

  start <- hsvMat["v", 1L]
  if (is.null(end))
    end <- hsvMat["v", 1L] * 0.5

  hsvMat["v", ] <- seq(start, end, length.out = NCOL(hsvMat))

  colorsOut <- apply(hsvMat, 2, function(x) do.call(grDevices::hsv, as.list(x)))

  return (colorsOut)
}


#' @export
change_luminance <- function(col, lum = 1.0)
{
  as.vector(apply(sapply(col, grDevices::col2rgb) / 255, 2,
    function(x) { x <- x + lum; x[x > 1.0] <- 1.0; grDevices::rgb(x[1], x[2], x[3]) }))
} # Also see 'scales::col2hcl()'.


#' @export
color_nm_map <- c(
  red = 700,
  orange = 620,
  yellow = 580,
  yg = 560,
  green = 530,
  cyan = 500,
  blue = 470,
  indigo = 450,
  violet = 420,
  uv = 300
)

## V. JavaScript source for https://academo.org/demos/wavelength-to-colour-relationship/
nm_to_rgb <- function(wavelength, Gamma = 0.8, IntensityMax = 255)
{
  if ((wavelength >= 380) && (wavelength < 440)) {
    red <- -(wavelength - 440) / (440 - 380)
    green <- 0.0
    blue <- 1.0
  } else if ((wavelength >= 440) && (wavelength < 490)) {
    red <- 0.0
    green <- (wavelength - 440) / (490 - 440)
    blue <- 1.0
  } else if ((wavelength >= 490) && (wavelength < 510)) {
    red <- 0.0
    green <- 1.0
    blue <- -(wavelength - 510) / (510 - 490)
  } else if ((wavelength >= 510) && (wavelength < 580)) {
    red <- (wavelength - 510) / (580 - 510)
    green <- 1.0
    blue <- 0.0
  } else if ((wavelength >= 580) && (wavelength < 645)) {
    red <- 1.0
    green <- -(wavelength - 645) / (645 - 580)
    blue <- 0.0
  } else if ((wavelength >= 645) && (wavelength < 781)) {
    red <- 1.0
    green <- 0.0
    blue <- 0.0
  } else {
    red <- 0.0
    green <- 0.0
    blue <- 0.0
  }

  ## Let intensity fall off near vision limits.
  if((wavelength >= 380) && (wavelength < 420)) {
    factor <- 0.3 + 0.7 * (wavelength - 380) / (420 - 380)
  } else if((wavelength >= 420) && (wavelength < 701)) {
    factor <- 1.0
  } else if((wavelength >= 701) && (wavelength < 781)) {
    factor <- 0.3 + 0.7 * (780 - wavelength) / (780 - 700)
  } else {
    factor <- 0.0
  }

  if (red != 0) {
    red <- round(IntensityMax * (red * factor)^Gamma)
  }
  if (green != 0) {
    green <- round(IntensityMax * (green * factor)^Gamma)
  }
  if (blue != 0) {
    blue <- round(IntensityMax * (blue * factor)^Gamma)
  }

  list(R = red, G = green, B = blue)
}


#' @export
wavelength2col <- Vectorize(function(wavelength, Gamma = 0.8, IntensityMax = 255, ...)
{
  if (is_invalid(wavelength))
    return (NA)

  #RGB <- colorscience::heuristic.wlnm2RGB(wavelength, Gamma, IntensityMax)
  RGB <- nm_to_rgb(wavelength, Gamma, IntensityMax)

  grDevices::rgb(RGB$R, RGB$G, RGB$B, maxColorValue = IntensityMax, ...) # Can add e.g. 'alpha' here.
})


#' @export
vline <- function(
  mark_x,
  abline... = list(),
  text... = list(),
  ## For 'y_prop' v. https://english.stackexchange.com/a/286524/26862
  y_prop = 0.9
)
{
  ablineArgs <- list(
    v = mark_x,
    col = scales::alpha("black", 0.4),
    lty = "dashed"
  )
  ablineArgs <- utils::modifyList(ablineArgs, abline..., keep.null = TRUE)
  do.call(graphics::abline, ablineArgs)

  textArgs <- list(
    x = mark_x,
    y =  par("usr")[3] + ((par("usr")[4] - par("usr")[3]) * y_prop),
    labels = mark_x,
    cex = 0.8,
    srt = 270,
    adj = c(NA, -0.25)
  )
  textArgs <- utils::modifyList(textArgs, text..., keep.null = TRUE)
  do.call(graphics::text, textArgs)

  nop()
}


## grDevices::dev.print() may not include a background color; this update explicitly adds one
## N.B. This doesn't work. Set e.g. 'par(bg = "white")' before plotting to screen device.
#' @export
dev_print <- function(
  device = png,
  ...,
  bg = "white" # R default is often = "transparent"
)
{
  op <- par(no.readonly = TRUE) # Store all current graphical parameters
  par(bg = bg)

  tryCatch({
    r <- grDevices::dev.print(device = device, ..., bg = bg)
  }, error = function(e) e, finally = par(op))  # Restore graphical parameters
  #par(op)

  return (r)
}

## usage:
# dev_print(file = "./Downloads/001 - Maecker & Trotter 2006.png", device = png, width = 9.375, height = 7.3, units = "in", res = 600)
