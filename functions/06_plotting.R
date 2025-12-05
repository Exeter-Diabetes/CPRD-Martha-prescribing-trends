# ---------------------------------------------------------
# 06_plotting.R
# Plotting functions for prescribing trends,
# response outcomes, complications, and ITS analyses
# ---------------------------------------------------------

# Colour palette and labels for drug classes 
drugclass_colours <- c(
  SGLT2 = "#E69F00",
  GLP1  = "#56B4E9",
  SU    = "#CC79A7",
  DPP4  = "#0072B2",
  Other = "darkgrey"
)

drugclass_labels <- c(
  "SGLT2" = "SGLT2i",
  "DPP4"  = "DPP4i",
  "GLP1"  = "GLP-1RA",
  "SU"    = "SU",
  "Other" = "Other"
)


#' Plot drug-class prescribing trends by subgroup
#'
#' @param trends Output from `drug_proportions()`.
#' @param cohort Original cohort used to determine n per group.
#' @param group_var Grouping variable name (character).
#' @param y_lab Y-axis label.
#'
#' @return A faceted ggplot object.
plot_drug_trends_by_group <- function(trends,
                                      cohort,
                                      group_var,
                                      y_lab = "Second-line initiations (%)") {
  
  group_levels <- levels(cohort[[group_var]])
  
  # Compute n by group for facet labels
  group_ns <- cohort %>%
    dplyr::filter(!is.na(.data[[group_var]])) %>%
    dplyr::group_by(.data[[group_var]]) %>%
    dplyr::summarise(
      n_group = dplyr::n_distinct(patid),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      !!group_var := factor(.data[[group_var]], levels = group_levels),
      facet_label = paste0(.data[[group_var]], " (n = ", n_group, ")")
    ) %>%
    dplyr::arrange(.data[[group_var]])
  
  trends_lab <- trends %>%
    dplyr::left_join(group_ns, by = group_var) %>%
    dplyr::mutate(
      facet_label = factor(facet_label, levels = group_ns$facet_label)
    )
  
  ggplot2::ggplot(
    trends_lab,
    ggplot2::aes(
      x = year,
      y = proportion,
      colour = drug_class,
      group = drug_class
    )
  ) +
    ggplot2::geom_line(linewidth = 1.1) +
    ggplot2::geom_point(size = 1.8) +
    ggplot2::facet_wrap(ggplot2::vars(facet_label)) +
    ggplot2::scale_color_manual(
      values = drugclass_colours,
      labels = drugclass_labels,
      name   = "Drug class"
    ) +
    ggplot2::labs(
      x = "Year",
      y = y_lab
    ) +
    ggplot2::coord_cartesian(ylim = c(0, 75)) +
    ggplot2::theme_minimal(base_size = 22) +
    ggplot2::theme(
      legend.position = "right",
      strip.text      = ggplot2::element_text(size = 20, face = "bold"),
      axis.title      = ggplot2::element_text(size = 22),
      axis.text       = ggplot2::element_text(size = 18)
    )
}


#' Plot marginal counterfactual response over calendar time
#'
#' @param preds Output from `average_predictions_by_year_frailty()`.
#' @param y_label Y-axis label.
#' @param y_limits Optional y-axis limits.
#' @param y_breaks Optional y-axis breaks.
#' @param x_label X-axis label.
#' @param palette Optional named colour vector for frailty groups.
#' @param legend_title Title for the legend.
#'
#' @return A ggplot object.
plot_response_outcome <- function(preds,
                                  y_label,
                                  y_limits = NULL,
                                  y_breaks = NULL,
                                  x_label = "Year",
                                  palette = NULL,
                                  legend_title = "Frailty group") {
  
  p <- ggplot2::ggplot(
    preds,
    ggplot2::aes(
      x = year,
      y = estimate,
      color = predrug_frail_elderly_cat,
      fill  = predrug_frail_elderly_cat,
      group = predrug_frail_elderly_cat
    )
  ) +
    ggplot2::geom_line(linewidth = 1.2) +
    ggplot2::geom_point(size = 2.2) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = conf.low, ymax = conf.high),
      alpha = 0.16,
      color = NA
    ) +
    ggplot2::labs(
      x     = x_label,
      y     = y_label,
      color = legend_title,
      fill  = legend_title
    ) +
    ggplot2::theme_minimal(base_size = 26) +
    ggplot2::theme(
      axis.title   = ggplot2::element_text(size = 24),
      axis.text    = ggplot2::element_text(size = 24),
      legend.title = ggplot2::element_text(size = 24),
      legend.text  = ggplot2::element_text(size = 24)
    )
  
  if (!is.null(palette)) {
    p <- p +
      ggplot2::scale_color_manual(values = palette) +
      ggplot2::scale_fill_manual(values = palette)
  }
  
  if (!is.null(y_limits)) {
    p <- p + ggplot2::scale_y_continuous(limits = y_limits, breaks = y_breaks)
  }
  
  p
}



#' Plot predicted complication incidence rates
#'
#' @param preds Output from `poisson_predictions_by_year_frailty()`.
#' @param y_label Y-axis label.
#' @param y_max Upper y-axis limit.
#' @param legend_pos Legend position.
#'
#' @return A ggplot object.
plot_complication_outcome <- function(preds,
                                      y_label,
                                      y_max = 75,
                                      legend_pos = "right") {
  
  ggplot2::ggplot(
    preds,
    ggplot2::aes(
      x = year,
      y = estimate,
      color = predrug_frail_elderly_cat,
      fill  = predrug_frail_elderly_cat,
      group = predrug_frail_elderly_cat
    )
  ) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = conf.low, ymax = conf.high),
      alpha = 0.16,
      color = NA
    ) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    ggplot2::labs(x = "Year", y = y_label) +
    ggplot2::coord_cartesian(ylim = c(0, y_max)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = legend_pos,
      axis.title      = ggplot2::element_text(size = 24),
      axis.text       = ggplot2::element_text(size = 22)
    )
}




#' Plot interrupted time series: observed vs counterfactual trends
#'
#' @param its_df Data frame containing observed proportions and ITS predictions.
#' @param intervention_date Intervention date.
#' @param y_label Y-axis label.
#' @param y_limits Numeric vector of y-axis limits.
#' @param x_limits Date range for the plot.
#'
#' @return A ggplot object.
plot_interrupted_time_series <- function(its_df,
                                         intervention_date,
                                         y_label  = "Second-line SGLT2i initiations (%)",
                                         y_limits = c(0, 72),
                                         x_limits = c("2019-01-01", "2024-03-31")) {
  
  intervention_date <- as.Date(intervention_date)
  x_limits          <- as.Date(x_limits)
  
  ggplot2::ggplot(its_df, ggplot2::aes(x = date, y = proportion)) +
    
    # Observed data
    ggplot2::geom_point(color = "#999999", alpha = 0.6) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = factual_lower, ymax = factual_upper),
      fill  = "#B2DF8A",
      alpha = 0.4
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = factual_predictions),
      color     = "black",
      linewidth = 1.6
    ) +
    
    # Counterfactual post-intervention
    ggplot2::geom_ribbon(
      data = dplyr::filter(its_df, date >= intervention_date),
      ggplot2::aes(ymin = counterfactual_lower, ymax = counterfactual_upper),
      fill  = "#FB9A99",
      alpha = 0.35
    ) +
    ggplot2::geom_line(
      data = dplyr::filter(its_df, date >= intervention_date),
      ggplot2::aes(y = counterfactual_predictions),
      color     = "#E31A1C",
      linetype  = "33",
      linewidth = 1.8
    ) +
    
    # Intervention marker
    ggplot2::geom_segment(
      ggplot2::aes(
        x = intervention_date, xend = intervention_date,
        y = 0, yend = max(y_limits)
      ),
      linetype  = "33",
      color     = "#253494",
      linewidth = 2.2
    ) +
    ggplot2::annotate(
      "text",
      x = intervention_date,
      y = max(y_limits),
      label = "Intervention (NICE guidelines)",
      color = "#253494",
      fontface = "bold",
      size = 7,
      hjust = 0.5,
      vjust = -0.5
    ) +
    
    ggplot2::labs(x = "Year", y = y_label) +
    ggplot2::theme_minimal(base_size = 26) +
    ggplot2::theme(
      legend.position  = "none",
      axis.title       = ggplot2::element_text(size = 26),
      axis.text        = ggplot2::element_text(size = 24),
      panel.grid.minor = ggplot2::element_line(color = "grey90"),
      plot.margin      = margin(40, 5.5, 5.5, 5.5)
    ) +
    ggplot2::scale_x_date(
      date_breaks = "1 year",
      date_labels = "%Y",
      limits      = x_limits
    ) +
    ggplot2::ylim(y_limits)
}