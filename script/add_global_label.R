add_global_label <- function(pwobj, Xlab = NULL, Ylab = NULL, Xgap = 0.03, Ygap = 0.03, ...) {
        ylabgrob <- patchwork::plot_spacer()
        if (!is.null(Ylab)) {
                ylabgrob <- ggplot() +
                        geom_text(aes(x = .5, y = .5), label = Ylab, angle = 90, ...) +
                        theme_void()
        }
        if (!is.null(Xlab)) {
                xlabgrob <- ggplot() +
                        geom_text(aes(x = .5, y = .5), label = Xlab, ...) +
                        theme_void()
        }
        if (!is.null(Ylab) & is.null(Xlab)) {
                return((ylabgrob + patchworkGrob(pwobj)) + 
                               patchwork::plot_layout(widths = 100 * c(Ygap, 1 - Ygap)))
        }
        if (is.null(Ylab) & !is.null(Xlab)) {
                return((ylabgrob + pwobj) + 
                               (xlabgrob) +
                               patchwork::plot_layout(heights = 100 * c(1 - Xgap, Xgap),
                                                      widths = c(0, 100),
                                                      design = "
                                   AB
                                   CC
                                   "
                               ))
        }
        if (!is.null(Ylab) & !is.null(Xlab)) {
                return((ylabgrob + pwobj) + 
                               (xlabgrob) +
                               patchwork::plot_layout(heights = 100 * c(1 - Xgap, Xgap),
                                                      widths = 100 * c(Ygap, 1 - Ygap),
                                                      design = "
                                   AB
                                   CC
                                   "
                               ))
        }
        return(pwobj)
}
