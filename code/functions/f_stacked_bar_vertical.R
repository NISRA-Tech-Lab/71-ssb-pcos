f_stacked_bar_vertical <- function(df) {
  legend_labels <- if (names(df)[2] == "trust") {
    c("Tend to trust/\ntrust a great deal", "Tend to distrust/\ndistrust a great deal")
  } else if (names(df)[2] == "agree") {
    c("Strongly agree/\nTend to agree", "Tend to disagree/\nStrongly disagree")
  }

  bar_gap <- if (names(df)[1] == "year") {
    0.4
  } else if (names(df)[1] == "org") {
    0.7
  }

  df <- df %>%
    mutate(
      cat_1_inside = case_when(
        .[[2]] >= 3.5 ~ paste0("<b>", round_half_up(.[[2]]), "</b>"),
        TRUE ~ ""
      ),
      cat_1_outside = case_when(
        .[[2]] < 3.5 ~ paste0("<b>", round_half_up(.[[2]]), "</b>"),
        TRUE ~ ""
      ),
      cat_2_inside = case_when(
        .[[3]] >= 3.5 ~ paste0("<b>", round_half_up(.[[3]]), "</b>"),
        TRUE ~ ""
      ),
      cat_2_outside = case_when(
        .[[3]] < 3.5 ~ paste0("<b>", round_half_up(.[[3]]), "</b>"),
        TRUE ~ ""
      ),
      cat_3_inside = case_when(
        dont_know >= 3.5 ~ paste0("<b>", round_half_up(dont_know), "</b>"),
        TRUE ~ ""
      ),
      cat_3_outside = case_when(
        dont_know < 3.5 ~ paste0("<b>", round_half_up(dont_know), "</b>"),
        TRUE ~ ""
      )
    )

  offset <- if ("org" %in% names(df)) {
    0.81
  } else if ("year" %in% names(df)) {
    0.6
  }

  plot_ly(df,
    x = df[[1]],
    y = df[[2]],
    type = "bar",
    marker = list(color = nisra_navy),
    name = legend_labels[1],
    hovertext = ~ paste0(legend_labels[1], "\n", ": ", round_half_up(df[[2]]), "%"),
    hoverinfo = "x+text"
  ) %>%
    add_trace(
      y = df[[3]],
      marker = list(color = nisra_blue),
      name = legend_labels[2],
      hovertext = ~ paste0(legend_labels[2], ": ", round_half_up(df[[3]]), "%")
    ) %>%
    add_trace(
      y = ~dont_know,
      marker = list(color = "#757575"),
      name = "Don't Know",
      hovertext = ~ paste0("Don't Know: ", round_half_up(dont_know), "%")
    ) %>%
    add_annotations(
      x = as.numeric(row.names(df)) - 1,
      y = df[[2]] / 2,
      yanchor = "middle",
      text = ~cat_1_inside,
      showarrow = FALSE,
      font = list(color = "#ffffff")
    ) %>%
    add_annotations(
      x = as.numeric(row.names(df)) - offset,
      y = df[[2]] / 2,
      yanchor = "middle",
      text = ~cat_1_outside,
      showarrow = FALSE,
      font = list(color = "#000000")
    ) %>%
    add_annotations(
      x = as.numeric(row.names(df)) - 1,
      y = df[[2]] + df[[3]] / 2,
      yanchor = "middle",
      text = ~cat_2_inside,
      showarrow = FALSE,
      font = list(color = "#ffffff")
    ) %>%
    add_annotations(
      x = as.numeric(row.names(df)) - offset,
      y = df[[2]] + df[[3]] / 2,
      yanchor = "middle",
      text = ~cat_2_outside,
      showarrow = FALSE,
      font = list(color = "#000000")
    ) %>%
    add_annotations(
      x = as.numeric(row.names(df)) - 1,
      y = ~ df[[2]] + df[[3]] + dont_know / 2,
      yanchor = "middle",
      text = ~cat_3_inside,
      showarrow = FALSE,
      font = list(color = "#ffffff")
    ) %>%
    add_annotations(
      x = as.numeric(row.names(df)) - offset,
      y = ~ df[[2]] + df[[3]] + dont_know / 2,
      yanchor = "middle",
      text = ~cat_3_outside,
      showarrow = FALSE,
      font = list(color = "#000000")
    ) %>%
    layout(
      font = list(family = "Arial", size = 12),
      barmode = "stack",
      yaxis = list(
        title = "",
        fixedrange = TRUE
      ),
      xaxis = list(
        title = "",
        fixedrange = TRUE
      ),
      legend = list(y = 0.5),
      annotations = list(
        text = "Percentage",
        x = 0,
        xref = "paper",
        xanchor = "center",
        y = 1,
        yref = "paper",
        yanchor = "bottom",
        showarrow = FALSE
      ),
      bargap = bar_gap
    ) %>%
    config(displayModeBar = FALSE)
}
