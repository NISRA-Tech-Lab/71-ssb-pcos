library(here)
source(paste0(here(), "/code/infographic/infographic prep.R"))

# Overview Infographic ####
## Respondents' awareness of NISRA ####

label_cols <- c("#00205b", "#889956")
names(label_cols) <- donut_chart_df$label

aware_nisra_chart <- ggplot(donut_chart_df, aes(x = 2.5, y = Percentage, fill = Answer)) +
  geom_col(
    colour = NA,
    size = 0
  ) +
  coord_polar(theta = "y", start = 0) +
  xlim(c(
    0.8,
    3.3
  )) +
  theme_void() +
  theme(
    legend.position = "none",
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(
      size = 18,
      color = "#747474",
      hjust = 0.5
    ),
    plot.subtitle = element_text(
      size = 18,
      color = "#747474",
      hjust = 0.5
    )
  ) +
  scale_fill_manual(values = c("#CBE346", "#00205b")) +
  scale_color_manual(values = label_cols)

overview_chart_1 <- aware_nisra_chart +
  labs(
    title = "Respondents' awareness",
    subtitle = bquote("of" ~ bold("NISRA") ~ .(current_year))
  ) +
  geom_text(
    x = c(3.7, -2.15),
    aes(
      y = c(12, 12),
      label = label,
      color = label
    ),
    size = 6,
    fontface = "bold"
  )

overview_chart_1

save_plot(paste0(here(), "/outputs/infographics/Overview1.png"), fig = overview_chart_1, width = 11, height = 9)

## Trust in NISRA statistics ####

aware_trust_chart_2 <- ggplot(trust_df, aes(fill = Category, x = Percentage, y = Year)) +
  geom_bar(
    colour = NA, size = 0, position = "stack", width = 0.6, stat = "identity",
    key_glyph = f_draw_square
  ) +
  scale_fill_manual(
    values = alpha(c("#888A87", "#CBE346", "#00205b")),
    labels = c(
      " \nDon't know\n ", " \nTend to distrust/\ndistrust greatly\n ",
      " \nTend to trust/\ntrust a great deal\n "
    )
  ) +
  labs(title = bquote("Trust in" ~ bold("NISRA") ~ "statistics")) +
  theme(
    axis.line = element_line(colour = "black"),
    axis.line.x = element_blank(),
    axis.line.y = element_line(color = "#D9D9D9"),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.title.y = element_blank(),
    text = element_text(size = 15),
    legend.key = element_blank(),
    legend.title = element_blank(),
    plot.title = element_text(
      hjust = 1,
      vjust = -2,
      size = 20,
      margin = margin(0, 0, 30, 0),
      color = "#747474"
    )
  ) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  geom_text(aes(label = Percentage),
    fontface = "bold",
    size = 3.5,
    position = position_stack(vjust = 0.5),
    color = ifelse(trust_df$Category == "Tend to distrust/distrust a great deal",
      "#404040",
      "#ffffff"
    )
  ) +
  guides(fill = guide_legend(reverse = TRUE)) +
  coord_cartesian(expand = FALSE)

aware_trust_chart_2

save_plot(paste0(here(), "/outputs/infographics/Overview2.png"), fig = aware_trust_chart_2, width = 13, height = 10)

## Personal information provided to NISRA will be kept confidential ####

aware_trust_chart_3 <- ggplot(
  confidentiality,
  aes(fill = Category, y = Percentage, x = Year)
) +
  geom_bar(
    position = "stack",
    width = 0.6,
    stat = "identity",
    colour = NA,
    size = 0,
    key_glyph = f_draw_square
  ) +
  scale_fill_manual(
    values = alpha(c("#888A87", "#CBE346", "#00205b")),
    labels = c(
      " \nDon't know\n ", " \nTend to disagree/\nstrongly disagree\n ",
      " \nStrongly agree/\ntend to agree\n "
    )
  ) +
  labs(
    title = "Personal Information provided to",
    subtitle = bquote(~ bold("NISRA") ~ "will be kept confidential")
  ) +
  theme( # axis.line = element_line(colour = "black"),
    axis.line.x = element_line(color = "#D9D9D9"),
    axis.line.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.title.x = element_blank(),
    text = element_text(size = 16),
    legend.key = element_blank(),
    legend.text = element_text(size = 8),
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(-7, -7, -7, -7),
    legend.title = element_blank(),
    plot.title = element_text(
      size = 14,
      color = "#747474",
      hjust = 0.5,
      vjust = 4
    ),
    plot.subtitle = element_text(
      size = 14,
      color = "#747474",
      hjust = 0.5,
      vjust = 4
    ),
    plot.margin = margin(r = 10, t = 20)
  ) +
  geom_text(aes(label = Percentage),
    fontface = "bold",
    size = 3.5,
    position = position_stack(vjust = 0.5),
    color = ifelse(confidentiality$Category == "Strongly Agree/Tend to agree" |
      confidentiality$Category == "Don't know",
    "white", "black"
    ),
    hjust = ifelse(confidentiality$Percentage < 4, -2, 0.5)
  ) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  coord_cartesian(
    xlim = c(0.5, length(unique(confidentiality$Year)) + 0.7),
    expand = FALSE
  )

aware_trust_chart_3

save_plot(paste0(here(), "/outputs/infographics/Overview3.png"), fig = aware_trust_chart_3, width = 11, height = 8)

## NISRA statistics are important to understand Northern Ireland ####
important_df$Category <- factor(important_df$Category,
  levels = c(
    "Don't know",
    "Tend to disagree/strongly disagree",
    "Strongly agree/tend to agree"
  )
)

# Create plot
aware_trust_chart_4 <- ggplot(important_df, aes(fill = Category, y = Percentage, x = Year)) +
  geom_col(
    position = "stack", key_glyph = f_draw_square, width = 0.6,
    stat = "identity", colour = NA, size = 0
  ) +
  scale_fill_manual(
    values = alpha(c("#888A87", "#CBE346", "#00205b", "#00205b", "#00205b")),
    labels = c(
      " \nDon't know\n ", " \nTend to disagree/\nstrongly disagree\n ",
      " \nStrongly agree/\ntend to agree\n "
    )
  ) +
  labs(
    title = bquote(~ bold("NISRA") ~ "statistics are important to understand"),
    subtitle = "Northern Ireland"
  ) +
  theme(
    text = element_text(size = 12),
    axis.line.x = element_line(color = "#D9D9D9"),
    axis.line.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.title.x = element_blank(),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    plot.title = element_text(
      size = 12,
      color = "#747474",
      hjust = 0.5,
      vjust = 0.6
    ),
    plot.subtitle = element_text(
      size = 12,
      color = "#747474",
      hjust = 0.5,
      vjust = 1.5
    ),
    plot.margin = margin(l = 10)
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  geom_text(aes(label = Percentage),
    fontface = "bold",
    size = 3,
    position = position_stack(vjust = 0.5),
    color = ifelse(important_df$Category == "Tend to disagree/strongly disagree",
      "#000000", "#ffffff"
    ),
    hjust = ifelse(important_df$Percentage < 4, -3.2, 0.5)
  ) +
  guides(fill = guide_legend(reverse = TRUE)) +
  coord_cartesian(
    xlim = c(0.5, length(unique(important_df$Year)) + 0.7),
    expand = FALSE
  )

aware_trust_chart_4

save_plot(paste0(here(), "/outputs/infographics/Overview4.png"), fig = aware_trust_chart_4, width = 11, height = 7)

## Trust in NISRA compared to other institutions ####

bar_order <- c("The NI Assembly ", "The Civil Service ", "The media ", "NISRA ")

aware_trust_chart_5 <- ggplot(
  trust_compared_df,
  aes(
    fill = Category,
    x = Percentage,
    y = factor(Institution, level = bar_order),
    label = Percentage
  )
) +
  geom_bar(position = "stack", width = 0.5, stat = "identity", colour = NA, size = 0, key_glyph = f_draw_square) +
  scale_fill_manual(values = alpha(c("#888A87", "#CBE346", "#00205b"))) +
  labs(title = bquote("Trust in" ~ bold("NISRA") ~ "compared to other institutions")) +
  theme(
    text = element_text(size = 18),
    legend.position = "top",
    legend.justification = "center",
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 8),
    axis.text = element_text(size = 10),
    legend.title = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks = element_blank(),
    legend.text = element_text(size = 8),
    legend.key = element_blank(),
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    plot.title = element_text(
      size = 14,
      color = "#747474",
      hjust = 0.5,
      vjust = 5
    ),
    plot.margin = margin(l = 20, r = 20, t = 50)
  ) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  geom_text(aes(label = Percentage),
    fontface = "bold",
    size = 3,
    position = position_stack(vjust = 0.5),
    color = ifelse(trust_compared_df$Category == "Tend to distrust/distrust a great deal",
      "#000000",
      "#ffffff"
    )
  ) +
  guides(fill = guide_legend(reverse = TRUE)) +
  coord_cartesian(expand = FALSE)

aware_trust_chart_5

save_plot(paste0(here(), "/outputs/infographics/Overview5.png"), fig = aware_trust_chart_5, width = 18, height = 10)

## Convert to PDF ####
infographic_template <- readLines(paste0(here(), "/code/infographic/Overview - Infographic template.svg")) %>%
  gsub('id="tspan12">YEAR', paste0('id="tspan12">', current_year), ., fixed = TRUE)

for (plot in c("Overview1", "Overview2", "Overview3", "Overview4", "Overview5")) {
  infographic_template <- gsub(paste0("../../outputs/infographics/", plot, ".png"),
    paste0("data:image/png;base64,", base64_encode(paste0(here(), "/outputs/infographics/", plot, ".png"))),
    infographic_template,
    fixed = TRUE
  )
}

writeLines(infographic_template, paste0(here(), "/outputs/Overview Infographic - ", current_year, ".svg"))

rsvg_pdf(
  svg = paste0(here(), "/outputs/Overview Infographic - ", current_year, ".svg"),
  file = paste0(here(), "/outputs/Overview Infographic - ", current_year, ".pdf")
)

unlink(paste0(here(), "/outputs/Overview Infographic - ", current_year, ".svg"))


# Trust Infographic ####
## Respondents' Trust NISRA Statistics ####

donut_cols <- c("#a8a4a4", "#CBE346", "#00205b")

label_cols <- c("#00205b", "#889956", "#888A87")
names(label_cols) <- trust_info_data1$label

trust_chart_1 <- ggplot(trust_info_data1, aes(x = 2, y = prop, fill = class)) +
  geom_bar(stat = "identity", colour = NA, size = 0) +
  coord_polar(theta = "y", start = 0) +
  scale_fill_manual(values = donut_cols) +
  theme_void() +
  coord_polar(theta = "y") +
  xlim(-.2, 3) +
  theme(legend.position = "none") +
  annotate("text",
    x = -0.2, y = 48, size = 4.6,
    label = paste0("Respondents’ trust\nin NISRA statistics\n", current_year),
    color = "#747474"
  ) +
  geom_text(
    aes(
      label = label,
      y = c(10, 86, 94),
      x = c(3, 3, 3),
      color = label
    ),
    size = 5,
    fontface = "bold"
  ) +
  scale_color_manual(values = label_cols)

trust_chart_1

save_plot(paste0(here(), "/outputs/infographics/trust1.png"), fig = trust_chart_1, width = 12, height = 10)

## Trust in NISRA ####

title_perc <- paste0(trust_info_data2$`Percentage\n`[trust_info_data2$Year == current_year], "%")

trust_chart_2 <- ggplot(trust_info_data2, aes(Year, `Percentage\n`)) +
  geom_point(color = "#1F497D", size = 6) +
  geom_line(
    data = trust_info_data2[!is.na(trust_info_data2$`Percentage\n`), ],
    aes(color = Category),
    linewidth = 2
  ) +
  scale_colour_manual(values = "#1F497D") +
  scale_x_continuous(
    limits = c(2014, current_year),
    breaks = seq(2014, current_year, by = 1)
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 10)
  ) +
  ggtitle(label = bquote("Trust in NISRA" ~ bold("remains high at") ~ bold(.(title_perc)))) +
  geom_text(aes(label = paste0(`Percentage\n`, "%")),
    vjust = 2,
    size = 8,
    color = "#002060",
    fontface = "bold"
  ) +
  theme_void() +
  theme(
    text = element_text(size = 25),
    axis.text.x = element_text(size = 25),
    axis.text.y = element_text(size = 25),
    plot.title = element_text(
      hjust = 0.5,
      vjust = 1,
      size = 34, ,
      color = "#747474"
    ),
    legend.position = c(0.5, -0.15),
    legend.text = element_text(size = 25),
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 25, angle = 90),
    panel.grid.major.y = element_line(color = "#D9D9D9"),
    panel.grid.minor.y = element_line(color = "#D9D9D9"),
    plot.margin = margin(b = 60)
  )

trust_chart_2

save_plot(paste0(here(), "/outputs/infographics/trust2.png"), fig = trust_chart_2, width = 40, height = 18)

## NISRA stats are free from political interference ####

chart_3_perc <- paste0(trust_info_data3$Percentage[trust_info_data3$Year == current_year], "%")

trust_chart_3 <- ggplot(
  trust_info_data3,
  aes(x = Percentage, y = Year)
) +
  geom_bar(
    position = "dodge",
    width = 0.5,
    fill = "#00205b",
    stat = "identity",
    colour = NA,
    size = 0
  ) +
  labs(
    title = bquote("The belief that" ~ bold("NISRA") ~ "statistics are free from political"),
    subtitle = bquote("interference" ~ bold("remains high at" ~ .(chart_3_perc)))
  ) +
  theme(
    text = element_text(size = 20),
    legend.position = "bottom",
    axis.title.y = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 20),
    axis.text.y = element_text(size = 17),
    axis.text.x = element_text(size = 17),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    plot.title = element_text(
      size = 20,
      color = "#747474",
      hjust = 0.5
    ),
    plot.subtitle = element_text(
      size = 20,
      color = "#747474",
      hjust = 0.5
    ),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    axis.line.y = element_line(color = "#D9D9D9"),
    plot.margin = margin(l = 1, r = 15)
  ) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  geom_text(
    size = 5.3,
    aes(label = paste0(Percentage, "%")),
    position = position_dodge(width = 1),
    vjust = 0.4,
    hjust = -0.1,
    fontface = "bold"
  ) +
  coord_cartesian(expand = FALSE)

trust_chart_3

## Trust in statistics compared to ONS ####

chart_4_perc <- paste0(trust_info_data4$Percentage[trust_info_data4$Year == current_year], "%")

trust_chart_4 <- ggplot(trust_info_data4, aes(x = Year, y = Percentage, group = factor(Organisation))) +
  geom_bar(
    stat = "identity",
    colour = NA,
    size = 0,
    width = 0.6,
    aes(fill = factor(Organisation)),
    position = position_dodge(width = 0.7)
  ) +
  ggtitle(label = bquote("Trust in" ~ bold("NISRA") ~ "statistics is" ~ bold("similar") ~ "to trust in" ~ bold("ONS") ~ "statistics at" ~ bold(.(chart_4_perc)))) +
  scale_fill_manual(values = alpha(c("#00205b", "#5094dc"))) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    legend.text = element_text(size = 24),
    axis.text.x = element_text(size = 24),
    plot.title = element_text(
      hjust = 0.5,
      size = 29,
      margin = margin(0, 0, 30, 0),
      color = "#747474"
    ),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.line.x = element_line(
      color = "#000000",
      linewidth = 2
    )
  ) +
  geom_text(
    size = 7.5,
    aes(label = paste0(Percentage, "%")),
    position = position_dodge(width = 0.9),
    vjust = -0.25,
    fontface = "bold"
  ) +
  scale_y_continuous(limits = c(0, 100)) +
  coord_cartesian(expand = FALSE)

trust_chart_4

save_plot(paste0(here(), "/outputs/infographics/trust4.png"), fig = trust_chart_4, width = 32, height = 18)



# Awareness Infographic ####

## Respondents' Awareness of NISRA ####

caption_1 <- ggplot() +
  annotate("text",
    x = 0,
    y = 0,
    label = "Respondents'\nawareness of",
    color = "#747474",
    size = 7
  ) +
  coord_cartesian(clip = "off") +
  theme_void()

caption_2 <- ggplot() +
  annotate("text",
    x = 0,
    y = 0,
    label = bquote(bold("NISRA") ~ .(current_year)),
    color = "#747474",
    size = 7
  ) +
  coord_cartesian(clip = "off") +
  theme_void()

aware_nisra_chart <- aware_nisra_chart +
  geom_text(
    x = c(3.7, -2.15),
    aes(
      y = c(12, 12),
      label = label,
      color = label
    ),
    size = 8,
    fontface = "bold"
  ) +
  inset_element(caption_1, left = 0.5, right = 0.5, top = 0.55, bottom = 0.55) +
  inset_element(caption_2, left = 0.5, right = 0.5, top = 0.45, bottom = 0.45)

aware_nisra_chart

save_plot(paste0(here(), "/outputs/infographics/info1.png"), fig = aware_nisra_chart, width = 14, height = 14)

## Bubble Chart ####

current_trend <- aware_nisra_z$significance[aware_nisra_z$Year == current_year - 1]

current_trend_length <- 1

for (i in 2:nrow(aware_nisra_z)) {
  if (aware_nisra_z$significance[aware_nisra_z$Year == current_year - i] == current_trend) {
    current_trend_length <- current_trend_length + 1
  } else {
    break
  }
}

current_trend_years <- ""

for (i in 1:current_trend_length) {
  if (i == current_trend_length) {
    current_trend_years <- paste0(current_trend_years, rev(aware_nisra_z$Year)[nrow(aware_nisra_z) - current_trend_length + i], ",")
  } else if (i == current_trend_length - 1) {
    current_trend_years <- paste0(current_trend_years, rev(aware_nisra_z$Year)[nrow(aware_nisra_z) - current_trend_length + i], " and ")
  } else {
    current_trend_years <- paste0(current_trend_years, rev(aware_nisra_z$Year)[nrow(aware_nisra_z) - current_trend_length + i], ", ")
  }
}

previous_trend <- aware_nisra_z$significance[aware_nisra_z$Year == current_year - current_trend_length - 1]


caption_1 <- ggplot() +
  annotate("text",
    x = 0,
    y = 0,
    label = bquote(bold("Awareness of NISRA") ~ is ~ bold(.(current_trend)) ~ "than in" ~ .(current_trend_years) ~ "but"),
    color = "#747474",
    size = 3
  ) +
  coord_cartesian(clip = "off") +
  theme_void()

caption_2 <- ggplot() +
  annotate("text",
    x = 0,
    y = 0,
    label = bquote(bold(.(previous_trend)) ~ "than in previous years."),
    color = "#747474",
    size = 3
  ) +
  coord_cartesian(clip = "off") +
  theme_void()

bubble_chart <- ggplot() +
  theme_void() +
  theme(
    rect = element_rect(fill = "transparent", color = NA_character_)
  ) +
  inset_element(
    p = caption_1,
    left = 0.5,
    bottom = 0.74,
    right = 0.5,
    top = 0.74
  ) +
  inset_element(
    p = caption_2,
    left = 0.5,
    bottom = 0.7,
    right = 0.5,
    top = 0.7
  )

connecting_line <- ggplot() +
  theme_void() +
  geom_hline(
    yintercept = 0,
    linewidth = 2
  )

for (i in 1:nrow(awareness_info_data1)) {
  diameter <- awareness_info_data1$diameter[i]

  if (i == 1) {
    left <- 0
  } else {
    left <- right
  }

  right <- left + diameter

  year_label <- ggplot() +
    annotate("text",
      x = 0,
      y = 0,
      label = awareness_info_data1$Year[i]
    ) +
    coord_cartesian(clip = "off") +
    theme_void()

  pct_label <- ggplot() +
    annotate("text",
      x = 0,
      y = 0,
      label = paste0(awareness_info_data1$Percentage[i], "%"),
      size = awareness_info_data1$text_size[i],
      fontface = "bold",
      color = awareness_info_data1$text_colour[i]
    ) +
    coord_cartesian(clip = "off") +
    theme_void()

  bubble_chart <- bubble_chart +
    inset_element(
      p = readPNG(paste0(here(), "/data/images/", awareness_info_data1$shape[i], ".png"), native = TRUE, info = TRUE),
      left = left,
      bottom = 0.5 - diameter / 2,
      right = right,
      top = 0.5 + diameter / 2
    ) +
    inset_element(
      p = year_label,
      left = left,
      bottom = 0.63,
      right = right,
      top = 0.63
    ) +
    inset_element(
      p = pct_label,
      left = left,
      bottom = 0.5,
      right = right,
      top = 0.5
    )

  if (i != 1) {
    bubble_chart <- bubble_chart +
      inset_element(
        p = connecting_line,
        left = left - 0.015,
        bottom = 0.5,
        right = left + 0.015,
        top = 0.5
      )
  }
}

bubble_chart

png(
  filename = paste0(here(), "/outputs/infographics/info2.png"),
  width = 11, height = 10, units = "cm", res = 300
)
print(bubble_chart)
dev.off()

## Compared to ONS ####

nisra_ons_z <- f_return_z(
  p1 = aware_nisra_trend[[as.character(current_year)]][1] / 100,
  n1 = aware_nisra_trend[[as.character(current_year)]][2],
  p2 = unweighted_ons$Yes[unweighted_ons$`Related Variable` == "PCOS1"] / unweighted_ons_base$`Unweighted base`[unweighted_ons_base$`Related Variable` == "PCOS1"],
  n2 = unweighted_ons_base$`Unweighted base`[unweighted_ons_base$`Related Variable` == "PCOS1"]
)

nisra_ons_sig <- if (nisra_ons_z < qnorm(0.975) * -1) {
  "significantly lower"
} else if (nisra_ons_z > qnorm(0.975)) {
  "significantly higher"
} else {
  "similar"
}

pub_awareness_chart_2 <- ggplot(awareness_info_data2, aes(
  fill = Group,
  y = Percentage,
  x = year,
  label = Percentage
)) +
  geom_bar(
    position = "dodge",
    stat = "identity",
    height = 0.8,
    colour = NA,
    size = 0,
    width = 0.7,
    key_glyph = f_draw_square
  ) +
  scale_fill_manual(values = alpha(c("#3878c5", "#CBE346"))) +
  labs(
    title = bquote(underline("COMPARED TO ONS")),
    subtitle = bquote(bold("NISRA") ~ "awareness" ~ bold(.(nisra_ons_sig)) ~ .(if (nisra_ons_sig == "similar") "to" else "than") ~ bold("ONS."))
  ) +
  theme(
    legend.position = "bottom",
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    legend.key = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 10),
    plot.title = element_text(
      hjust = 0.5,
      color = "#747474",
      size = 14
    ),
    plot.subtitle = element_text(
      hjust = 0.5,
      color = "#747474",
      size = 14
    ),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.line.x = element_line(
      color = "#000000",
      linewidth = 1.5
    )
  ) +
  geom_text(
    size = 3.5,
    aes(label = paste0(Percentage, "%")),
    position = position_dodge(width = 0.9),
    vjust = -0.25
  ) +
  coord_cartesian(
    ylim = c(0, max(awareness_info_data2$Percentage, na.rm = TRUE) + 5),
    expand = FALSE
  )

pub_awareness_chart_2

save_plot(paste0(here(), "/outputs/infographics/info3.png"), fig = pub_awareness_chart_2, width = 12, height = 10)

## Awareness of specific NISRA statistics ####
aes(reorder(variable, value), value)

chart_order <- awareness_info_data3 %>%
  filter(Answer == "Yes") %>%
  arrange(desc(Percentage)) %>%
  pull("Group")

pub_awareness_chart_3 <- ggplot(
  awareness_info_data3,
  aes(
    fill = Answer,
    x = Percentage,
    y = factor(Group,
      levels = rev(chart_order)
    ),
    label = Percentage
  )
) +
  geom_bar(
    width = 0.7,
    position = "stack",
    stat = "identity",
    color = NA,
    size = 0
  ) +
  labs(title = bquote(bold("Awareness") ~ "of specific" ~ bold("NISRA statistics") ~ "for respondents who were not aware of" ~ bold("NISRA"))) +
  scale_fill_manual(values = alpha(c("#757575", "#98b4d4", "#00205b"))) +
  geom_text(
    data = awareness_info_data3[awareness_info_data3$Answer == "No", ],
    label = round_half_up(awareness_info_data3$Percentage[awareness_info_data3$Answer == "No"]),
    aes(x = 95),
    fontface = "bold",
    size = 6
  ) +
  geom_text(
    data = awareness_info_data3[awareness_info_data3$Answer == "Yes", ],
    label = round_half_up(awareness_info_data3$Percentage[awareness_info_data3$Answer == "Yes"]),
    aes(x = 5),
    fontface = "bold",
    size = 6,
    color = "white"
  ) +
  geom_text(
    data = awareness_info_data3[awareness_info_data3$Answer == "Don't Know", ],
    label = awareness_info_data3$Percentage[awareness_info_data3$Answer == "Don't Know"],
    aes(x = 103),
    fontface = "bold",
    size = 6
  ) +
  theme(
    text = element_text(size = 12),
    axis.text.x = element_text(size = 15),
    axis.title.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    axis.ticks = element_blank(),
    plot.title = element_text(
      hjust = 1,
      color = "#747474",
      size = 24
    ),
    legend.position = "top",
    legend.text = element_text(size = 12),
    legend.title = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank()
  ) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  coord_cartesian(
    xlim = c(0, 105),
    expand = FALSE
  )

pub_awareness_chart_3

save_plot(paste0(here(), "/outputs/infographics/info4.png"), fig = pub_awareness_chart_3, width = 34, height = 17)

## Awareness Infographic Output ####






# Convert to PDF ####



## Awareness ####

awareness_template <- readLines(paste0(here(), "/code/infographic/Awareness - Infographic template.svg")) %>%
  gsub("Statistics and Research Agency YEAR", paste0("Statistics and Research Agency ", current_year), ., fixed = TRUE)

for (plot in c("info1", "info2", "info3", "info4")) {
  awareness_template <- gsub(paste0("../../outputs/infographics/", plot, ".png"),
    paste0("data:image/png;base64,", base64_encode(paste0(here(), "/outputs/infographics/", plot, ".png"))),
    awareness_template,
    fixed = TRUE
  )
}

writeLines(awareness_template, paste0(here(), "/outputs/Awareness Infographic - ", current_year, ".svg"))

rsvg_pdf(
  svg = paste0(here(), "/outputs/Awareness Infographic - ", current_year, ".svg"),
  file = paste0(here(), "/outputs/Awareness Infographic - ", current_year, ".pdf")
)

unlink(paste0(here(), "/outputs/Awareness Infographic - ", current_year, ".svg"))


## Trust ####

trust_template <- readLines(paste0(here(), "/code/infographic/Trust - Infographic template.svg")) %>%
  gsub("Statistics and Research Agency YEAR", paste0("Statistics and Research Agency ", current_year), ., fixed = TRUE)

for (plot in c("trust1", "trust2", "trust3", "trust4")) {
  trust_template <- gsub(paste0("../../outputs/infographics/", plot, ".png"),
    paste0("data:image/png;base64,", base64_encode(paste0(here(), "/outputs/infographics/", plot, ".png"))),
    trust_template,
    fixed = TRUE
  )
}

writeLines(trust_template, paste0(here(), "/outputs/Trust Infographic - ", current_year, ".svg"))

rsvg_pdf(
  svg = paste0(here(), "/outputs/Trust Infographic - ", current_year, ".svg"),
  file = paste0(here(), "/outputs/Trust Infographic - ", current_year, ".pdf")
)

unlink(paste0(here(), "/outputs/Trust Infographic - ", current_year, ".svg"))
