plotQQ <- function(pvals_list, simplify = FALSE){
  colorPalette <- c("red", "blue", "green", "purple")
  if (simplify){
    pvals_list <- lapply(pvals_list, function(x) {
      x[x < 1e-10] <- 1e-10
      x
    })
  }
  pvals_data <- vector("list", length(pvals_list))
  p1 <- ggplot()
  
  for (i in 1:length(pvals_list)){
    pvals_data[[i]] <-
      data.frame(
        X = -log10(sort(pvals_list[[i]])),
        expected = -log10(ppoints(length(pvals_list[[i]]))),
        clower   = -log10(qbeta(
          p = (1 - 0.95) / 2,
          shape1 = 1:length(pvals_list[[i]]),
          shape2 = length(pvals_list[[i]]):1
        )),
        cupper   = -log10(qbeta(
          p = (1 + 0.95) / 2,
          shape1 = 1:length(pvals_list[[i]]),
          shape2 = length(pvals_list[[i]]):1
        )),
        Ancestry = rep(names(pvals_list)[[i]], length(pvals_list[[i]]))
      )
    p1 <- p1 + geom_point(data = pvals_data[[i]],
                          aes( x = expected, y = X, color = Ancestry),
                          size = 1,
                          shape = 20,
                          stroke = .2
    )
  }
  p1 <- p1 +
    geom_abline(intercept = 0,
                slope = 1,
                alpha = 0.5) +
    geom_line(data = pvals_data[[i]], aes(expected, cupper), linetype = 2) +
    geom_line(data = pvals_data[[i]], aes(expected, clower), linetype = 2) +
    xlab(expression(paste("Expected -log"[10], plain(P)))) +
    ylab(expression(paste("Observed -log"[10], plain(P)))) +
    theme_bw() +
    theme(
      legend.text = element_text(size = 8),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 10),
      plot.margin = unit(c(.1, .1, .1, .1), "cm"),
      aspect.ratio = 1,
      legend.key.size = unit(0.3, "cm"),
      legend.position = c(0.3, 0.7),
      legend.background = element_rect(
        color = "black",
        fill = "white",
        size = 0.2,
        linetype = "solid"
      ),
      legend.margin = ggplot2::margin(
        t = .1,
        r = .1,
        b = .1,
        l = .1,
        unit = 'cm'
      )
    ) +
    scale_color_manual(values = colorPalette[1:length(pvals_list)]) #+
    #labs(color = expression(lambda[1000]))
  p1
}
