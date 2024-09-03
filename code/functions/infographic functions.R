f_draw_square <- function(data, params, size) {
  if (is.null(data$size)) {
    data$size <- 0.4
  }
  lwd <- 2
  grid::rectGrob(
    width = unit(1, "snpc") - unit(lwd, "mm"),
    height = unit(1, "snpc") - unit(lwd, "mm"),
    gp = gpar(
      col = data$colour %||% NA,
      fill = alpha(data$fill %||% "grey20", data$alpha),
      lty = data$linetype %||% 1,
      lwd = lwd * .pt,
      linejoin = params$linejoin %||% "mitre",
      lineend = if (identical(params$linejoin, "round")) "round" else "square"
    )
  )
}

f_addline_format <- function(x, ...) {
  gsub("\\s", "\n", x)
}