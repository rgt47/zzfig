#' Generate Observed and Change Plots with Grouped Statistics
#'
#' This function creates plots of observed values and changes in values over time, grouped by specified variables.
#' It supports faceting, customization of plot aesthetics, and dynamic error representation.
#'
#' @param df A data frame containing the data to be plotted.
#' @param form A formula specifying the variables for the x-axis, grouping, and y-axis.
#'   Format: `y ~ x | group`.
#' @param facet_form A formula specifying the variables for faceting.
#'   Format: `facet_y ~ facet_x`. Default is `NULL`.
#' @param xlab A character string for the x-axis label. Default is `"visit"`.
#' @param ylab A character string for the y-axis label of the observed values plot. Default is `"measure"`.
#' @param ylab2 A character string for the y-axis label of the change plot. Default is `"measure change"`.
#' @param title A character string for the title of the observed values plot. Default is `"measure"`.
#' @param title2 A character string for the title of the change plot. Default is `"measure change"`.
#' @param subtitle A character string for the subtitle of the observed values plot. Default is an empty string.
#' @param subtitle2 A character string for the subtitle of the change plot. Default is an empty string.
#' @param caption A character string for the caption of the observed values plot. Default is an empty string.
#' @param caption2 A character string for the caption of the change plot. Default is an empty string.
#' @param ytype A character string specifying the type of plot to return. Options are `"obs"` (observed values),
#'   `"cng"` (change values), or `"both"` to return both plots combined. Default is `"obs"`.
#' @param etype A character string specifying the type of error representation. Options are `"bar"` (error bars)
#'   or `"band"` (error ribbons). Default is `"bar"`.
#' @return A `ggplot2` object or a combination of objects representing the requested plots.
#' @details The function calculates grouped statistics (mean, standard deviation, standard error) for
#' the observed values and their changes over time. It generates plots using these statistics and allows
#' for optional faceting.
#' @examples
#' \dontrun{
#' data <- data.frame(
#'         rid = rep(1:10, each = 3),
#'         visit = rep(c("bl", "m1", "m2"), times = 10),
#'         measure = rnorm(30, mean = 50, sd = 10),
#'         group = rep(c("A", "B"), length.out = 30)
#' )
#' zzfig(data, form = measure ~ visit | group, ytype = "obs")
#' zzfig(data, form = measure ~ visit | group, facet_form = group ~ visit, ytype = "cng")
#' zzfig(data, form = measure ~ visit | group, ytype = "both")
#' }
#' @importFrom dplyr group_by mutate summarize filter across
#' @importFrom ggplot2 ggplot aes geom_line geom_errorbar geom_ribbon labs theme_bw theme
#' @export

zzfig <- function(
    df, form, facet_form = NULL, xlab = "visit", ylab = "measure",
    ylab2 = "measure change", title = "measure", title2 = "measure change",
    subtitle = "", subtitle2 = "", caption = "", caption2 = "",
    ytype = "obs", etype = "bar") {
        # Parse formula inputs
        browser()
        parsed_form <- parse_formula(form)
        parsed_facet <- if (!is.null(facet_form)) parse_formula(facet_form) else NULL
        # Compute grouped statistics
        stats <- compute_stats(df, parsed_form$x, parsed_form$y, parsed_form$group)
        # Generate plots
        fig_obs <- generate_plot(
                stats, parsed_form$x, "mn", parsed_form$group, etype,
                xlab, ylab, title, subtitle, caption, parsed_facet
        )
        browser()
        fig_cng <- generate_plot(
                stats, parsed_form$x, "cng_mn", parsed_form$group, etype,
                xlab, ylab2, title2, subtitle2, caption2, parsed_facet
        )

        # Return plots based on ytype
        return_plots(ytype, fig_obs, fig_cng)
}

# Helper Functions
parse_formula <- function(formula) {
        components <- as.character(formula)
        rhs <- strsplit(components[3], " \\| ")[[1]]
        list(
                y = components[2],
                x = rhs[1],
                group = rhs[2]
        )
}

compute_stats <- function(df, x, y, group) {
        df %>%
                group_by(rid) %>%
                mutate(cng = .data[[y]] - .data[[y]][.data[[x]] == "bl"]) %>%
                group_by(.data[[group]], x) %>%
                summarize(
                        mn = mean(.data[[y]], na.rm = TRUE),
                        cng_mn = mean(cng, na.rm = TRUE),
                        N = n(),
                        sd = sd(.data[[y]], na.rm = TRUE),
                        cng_sd = sd(cng, na.rm = TRUE),
                        .groups = "drop"
                ) %>%
                mutate(
                        se = sd / sqrt(N),
                        cng_se = cng_sd / sqrt(N),
                        bl = mn - se,
                        bu = mn + se,
                        bl_cng = cng_mn - cng_se,
                        bu_cng = cng_mn + cng_se
                )
}

generate_plot <- function(stats, x, y, group, etype, xlab, ylab, title, subtitle, caption, facet) {
        plot <- ggplot(stats, aes(
                x = .data[[x]], y = .data[[y]], group = .data[[group]],
                color = .data[[group]], fill = .data[[group]]
        )) +
                geom_line(aes(linetype = .data[[group]])) +
        {
          if (etype == "bar") {
            geom_errorbar(aes(ymin = bl, ymax = bu), width = 0.2, color = "black", alpha = 0.3)
          } else {
            geom_ribbon(aes(ymin = bl, ymax = bu), alpha = 0.2)
          }
        } +
        labs(title = title, subtitle = subtitle, caption = caption) +
                ylab(ylab) +
                xlab(xlab) +
                theme_bw() +
                theme(legend.position = "bottom")

        if (!is.null(facet)) {
                plot <- plot + facet_grid(vars(.data[[facet$facet_y]]), vars(.data[[facet$facet_x]]))
        }
        plot
}

return_plots <- function(ytype, fig_obs, fig_cng) {
        switch(ytype,
                "obs" = fig_obs,
                "cng" = fig_cng,
                "both" = cowplot::plot_grid(fig_obs, fig_cng, ncol = 1),
                stop("Invalid ytype. Use 'obs', 'cng', or 'both'.")
        )
}
