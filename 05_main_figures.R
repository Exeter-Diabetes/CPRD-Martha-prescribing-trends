# ---------------------------------------------------------
# 05_main_figures.R
#
# Creates the final four main figures used in the paper.
#
# Assumes all previous analysis scripts have been run and that the
# individual plot objects already exist in the environment.
#
# This script:
#   - Combines individual plots into multi-panel figures
#   - Adds line-end labels (instead of legends) 
#   - Annotates plots with year-on-year differences (2023 or 2022)
#
# Figures produced:
#   Figure 1: prescribing_trends_frailty_plot
#   Figure 2: hba1c_response_plot, weight_response_plot, discontinuation_plot
#   Figure 3: ukpds_complication_plot, hf_complication_plot, kidney_complication_plot
#   Figure 4: interrupted_time_series_plot
# ---------------------------------------------------------


# ---------------------------------------------------------
# Figure 1: Prescribing trends by frailty group
# ---------------------------------------------------------

frailty_trends <- prescribing_trends_frailty_plot$data %>%
  dplyr::mutate(year_num = as.numeric(as.character(year)))

end_pts <- frailty_trends %>%
  dplyr::group_by(predrug_frail_elderly_cat, drug_class) %>%
  dplyr::slice_max(order_by = year_num, n = 1, with_ties = FALSE) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    vjust_end = dplyr::case_when(
      predrug_frail_elderly_cat == "Age \u2264 70 years" & drug_class == "DPP4" ~ -0.1,
      predrug_frail_elder_elderly_cat == "Age \u2264 70 years" & drug_class == "SU"   ~  0.6,
      TRUE ~ 0.5
    )
  )

start_other <- frailty_trends %>%
  dplyr::filter(drug_class == "Other") %>%
  dplyr::group_by(predrug_frail_elderly_cat, drug_class) %>%
  dplyr::slice_min(order_by = year_num, n = 1, with_ties = FALSE) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(vjust_other = 1.1)

main_figure1 <- prescribing_trends_frailty_plot +
  ggplot2::geom_text(
    data = end_pts %>% dplyr::filter(drug_class != "Other"),
    ggplot2::aes(label = drugclass_labels[as.character(drug_class)], vjust = vjust_end),
    hjust = -0.1, size = 6, fontface = "bold"
  ) +
  ggplot2::geom_text(
    data = start_other,
    ggplot2::aes(label = drugclass_labels[as.character(drug_class)], vjust = vjust_other),
    hjust = 0, nudge_x = 0.1, size = 6, fontface = "bold"
  ) +
  ggplot2::coord_cartesian(clip = "off") +
  ggplot2::theme(
    legend.position = "none",
    plot.margin = ggplot2::margin(5.5, 150, 5.5, 5.5),
    panel.spacing = grid::unit(4, "lines")
  )

ggplot2::ggsave(
  "output/figures/main/main_figure1.png",
  main_figure1, width = 19, height = 7.5, dpi = 600
)



# ---------------------------------------------------------
# Figure 2: HbA1c, Weight, Discontinuation
# ---------------------------------------------------------

# --- HbA1c ----
hba1c_text <- paste(
  "Mean 12-month HbA1c response 2023 vs 2019:",
  "Frail >70: -1.3 (-2.4, -0.2) mmol/mol",
  "Non-frail >70: -0.9 (-1.9, -0.0003) mmol/mol",
  "Age ≤ 70: -1.6 (-2.0, -1.2) mmol/mol",
  sep = "\n"
)

hba1c_response_plot <- hba1c_response_plot +
  annotate("text", x = 0.5, y = -0.5, label = hba1c_text,
           size = 5.5, hjust = 0, fontface = "bold") +
  geom_text(
    data = pred_hba1c %>% group_by(predrug_frail_elderly_cat) %>%
      slice_max(as.numeric(year), n = 1),
    aes(label = frailty_labels[as.character(predrug_frail_elderly_cat)]),
    hjust = -0.04, vjust = 0.5, size = 7, fontface = "bold"
  ) +
  coord_cartesian(clip = "off") +
  theme(legend.position = "none", plot.margin = margin(5.5, 100, 5.5, 5.5))


# --- Weight ----
weight_text <- paste(
  "Mean 12-month weight change 2023 vs 2019:",
  "Frail >70: -1.4 (-1.9, -0.9) kg",
  "Non-frail >70: -1.0 (-1.5, -0.6) kg",
  "Age ≤ 70: -1.1 (-1.3, -0.9) kg",
  sep = "\n"
)

weight_response_plot <- weight_response_plot +
  annotate("text", x = 0.5, y = -3.0, label = weight_text,
           size = 5.5, hjust = 0, fontface = "bold") +
  geom_text(
    data = pred_weight %>% group_by(predrug_frail_elderly_cat) %>%
      slice_max(as.numeric(year), n = 1),
    aes(label = frailty_labels[as.character(predrug_frail_elder_elderly_cat)],
        vjust = case_when(
          predrug_frail_elderly_cat == "Age ≤ 70 years" ~ 1.01,
          TRUE ~ 0.5
        )),
    hjust = -0.04, size = 7, fontface = "bold"
  ) +
  coord_cartesian(clip = "off") +
  theme(legend.position = "none",
        plot.margin = margin(5.5, 120, 5.5, 5.5)) +
  ylim(-4, 0)


# --- Discontinuation ----
discontinuation_text <- paste(
  "Mean 12-month treatment discontinuation 2022 vs 2019:",
  "Frail >70: 0.2 (-2.5, 2.0) %",
  "Non-frail >70: -0.2 (-2.4, 2.0) %",
  "Age ≤ 70: 0.5 (-0.6, 1.5) %",
  sep = "\n"
)

discontinuation_plot <- discontinuation_plot +
  annotate("text", x = 0.5, y = 0.13, label = discontinuation_text,
           size = 5.5, hjust = 0, fontface = "bold") +
  geom_text(
    data = pred_disc %>% group_by(predrug_frail_elderly_cat) %>%
      slice_max(as.numeric(year), n = 1),
    aes(label = frailty_labels[as.character(predrug_frail_elderly_cat)],
        vjust = case_when(
          predrug_frail_elderly_cat == "Non-frail > 70 years" ~ -0.05,
          predrug_frail_elderly_cat == "Frail > 70 years"     ~  1.05,
          TRUE                                                ~  0.5
        )),
    hjust = -0.04, size = 7, fontface = "bold"
  ) +
  scale_y_continuous(
    limits = c(0, 0.5),
    labels = scales::percent_format(scale = 100)
  ) +
  coord_cartesian(clip = "off") +
  theme(legend.position = "none",
        plot.margin = margin(5.5, 120, 5.5, 5.5))


main_figure2 <- (hba1c_response_plot /
                   weight_response_plot /
                   discontinuation_plot) +
  patchwork::plot_annotation(
    tag_levels = "A",
    theme = theme(plot.tag = element_text(size = 20, face = "bold"))
  )

ggsave(
  "output/figures/main/main_figure2.png",
  main_figure2, width = 13, height = 19, dpi = 600
)



# ---------------------------------------------------------
# Figure 3: Complications (UKPDS, HF, Kidney)
# ---------------------------------------------------------

# --- UKPDS ---
ukpds_text <- paste(
  "Adjusted rate difference 2022 vs 2019:",
  "Frail >70: 3.9 (-10.2, 18.0)",
  "Non-frail >70: 0.9 (-5.8, 7.5)",
  "Age ≤ 70: -1.3 (-3.8, 1.2)",
  sep = "\n"
)

ukpds_complication_plot <- ukpds_complication_plot +
  annotate("text", x = 0.5, y = 75, label = ukpds_text,
           size = 5.5, hjust = 0, fontface = "bold") +
  geom_text(
    data = pred_ukpds %>% group_by(predrug_frail_elder_elderly_cat) %>%
      slice_max(as.numeric(year), n = 1),
    aes(label = frailty_labels[as.character(predrug_frail_elderly_cat)]),
    hjust = -0.04, size = 7, fontface = "bold"
  ) +
  coord_cartesian(clip = "off") +
  theme(legend.position = "none")


# --- Heart failure ---
hf_text <- paste(
  "Adjusted rate difference 2022 vs 2019:",
  "Frail >70: 3.6 (-6.9, 14.2)",
  "Non-frail >70: 0.8 (-3.0, 4.7)",
  "Age ≤ 70: 0.7 (-0.5, 2.0)",
  sep = "\n"
)

hf_complication_plot <- hf_complication_plot +
  annotate("text", x = 0.5, y = 75, label = hf_text,
           size = 5.5, hjust = 0, fontface = "bold") +
  geom_text(
    data = pred_hf %>% group_by(predrug_frail_elderly_cat) %>%
      slice_max(as.numeric(year), n = 1),
    aes(label = frailty_labels[as.character(predrug_frail_elderly_cat)],
        vjust = case_when(
          predrug_frail_elderly_cat == "Non-frail > 70 years" ~ 0.2,
          TRUE                                                ~ 0.5
        )),
    hjust = -0.04, size = 7, fontface = "bold"
  ) +
  coord_cartesian(clip = "off") +
  theme(legend.position = "none")


# --- Kidney failure ---
kf_text <- paste(
  "Adjusted rate difference 2022 vs 2019:",
  "Frail >70: -1.8 (-5.4, 1.7)",
  "Non-frail >70: -2.0 (-4.7, 0.7)",
  "Age ≤ 70: -0.9 (-5.3, 1.8)",
  sep = "\n"
)

kidney_complication_plot <- kidney_complication_plot +
  annotate("text", x = 0.5, y = 75, label = kf_text,
           size = 5.5, hjust = 0, fontface = "bold") +
  geom_text(
    data = pred_kidney %>% group_by(predrug_frail_elderly_cat) %>%
      slice_max(as.numeric(year), n = 1),
    aes(label = frailty_labels[as.character(predrug_frail_elderly_cat)],
        vjust = case_when(
          predrug_frail_elderly_cat == "Age ≤ 70 years"       ~  1.4,
          predrug_frail_elderly_cat == "Non-frail > 70 years" ~  0.25,
          TRUE                                                ~ -0.5
        )),
    hjust = -0.04, size = 7, fontface = "bold"
  ) +
  coord_cartesian(clip = "off") +
  theme(legend.position = "none")


main_figure3 <- (
  ukpds_complication_plot /
    hf_complication_plot /
    kidney_complication_plot
) +
  patchwork::plot_annotation(
    tag_levels = "A",
    theme = theme(plot.tag = element_text(size = 20, face = "bold"))
  )

ggsave(
  "output/figures/main/main_figure3.png",
  main_figure3,
  width = 13, height = 21, dpi = 600
)



# ---------------------------------------------------------
# Figure 4: Interrupted Time Series
# ---------------------------------------------------------
main_figure4 <- interrupted_time_series_plot

ggsave(
  "output/figures/main/main_figure4.png",
  interrupted_time_series_plot,
  width = 14, height = 8, dpi = 600
)