f_stacked_bar_vertical <- function (df) {
  
  legend_labels <- if (names(df)[2] == "trust") {
    c("Tend to trust/\ntrust a great deal", "Tend to distrust/\ndistrust a great deal")
  } else if (names(df)[2] == "agree") {
    c("Strongly agree/\nTend to agree", "Tend to disagree/\nStrongly disagree")
  }
  
  bar_gap <- if (names(df)[1] == "year") {
    0.4
  } else if (names(df)[1] == "org") {
    0.5
  }
  
  plot_ly(df,
          x = df[[1]],
          y = df[[2]],
          type = "bar",
          marker = list(color = nisra_navy),
          name = legend_labels[1],
          hovertext = ~paste0(legend_labels[1], ": ", round_half_up(df[[2]]), "%"),
          hoverinfo = "x+text") %>%
    add_trace(y = df[[3]],
              marker = list(color = nisra_blue),
              name = legend_labels[2],
              hovertext = ~paste0(legend_labels[2], ": ", round_half_up(df[[3]]), "%")) %>%
    add_trace(y = ~dont_know,
              marker = list(color = "#757575"),
              name = "Don't Know",
              hovertext = ~paste0("Don't Know: ", round_half_up(dont_know), "%")) %>%
    add_annotations(x = as.numeric(row.names(df)) - 1,
                    y = df[[2]] / 2,
                    yanchor = "middle",
                    text = paste0("<b>", round_half_up(df[[2]]), "</b>"),
                    showarrow = FALSE,
                    font = list(color = "#ffffff")) %>%
    add_annotations(x = as.numeric(row.names(df)) - 1,
                    y = df[[2]] + df[[3]] / 2,
                    yanchor = "middle",
                    text = paste0("<b>", round_half_up(df[[3]]), "</b>"),
                    showarrow = FALSE,
                    font = list(color = "#ffffff")) %>%
    add_annotations(x = as.numeric(row.names(df)) - 1,
                    y = ~df[[2]] + df[[3]] + dont_know / 2,
                    yanchor = "middle",
                    text = ~paste0("<b>", round_half_up(dont_know), "</b>"),
                    showarrow = FALSE,
                    font = list(color = "#ffffff")) %>%
    layout(font = list(family = "Arial", size = 12),
           barmode = "stack",
           yaxis = list(title = "",
                        fixedrange = TRUE),
           xaxis = list(title = "",
                        fixedrange = TRUE),
           legend = list(y = 0.5),
           annotations = list(text = "Percentage",
                              x = 0,
                              xref = "paper",
                              xanchor = "center",
                              y = 1,
                              yref = "paper",
                              yanchor = "bottom",
                              showarrow = FALSE),
           bargap = bar_gap) %>%
    config(displayModeBar = FALSE)
  
  
}
