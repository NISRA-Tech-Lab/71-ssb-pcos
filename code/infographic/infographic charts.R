library(here)
source(paste0(here(), "/code/infographic/infographic prep.R"))

# Trust Infographic ####
## Chart 1 ####
trust_info_data1$label <- paste0(trust_info_data1$prop, "% ", trust_info_data1$class)
mycols <- c("#a8a4a4", "#CBE346", "#00205b")
trust_chart_1 <- ggplot(trust_info_data1, aes(x = 2, y = prop, fill = class)) +
  geom_bar(stat = "identity", colour=NA,size=0) +
  coord_polar(theta = "y", start = 0)+
  scale_fill_manual(values = mycols) +
  theme_void() +
  coord_polar(theta="y") +
  xlim(-.2, 3) +
  theme(legend.position="none") +
  annotate("text", x = -.2, y = 48, size = 4.6, 
           label = paste0(label = "Respondents’ trust\n in NISRA statistics\n", current_year)) +
  geom_text(aes(label = label, y = c(86, 50.5, 94), x = c(3, 2.7, 3), color = label), size = 5,
            fontface = "bold") +
  scale_color_manual(values = c("4% No" = "#889956",
                              "85% Yes" = "#00205b",
                              "11% Don't Know" = "#888A87"))

## Chart 2 ####
line_chart_perc <- "85%"
line_chart_title <- paste0("Trust in NISRA statistics remains high at ",  line_chart_perc)
trust_chart_2 <- ggplot(trust_info_data2, aes(Year, Percentage)) + 
  geom_point(color= "black", size = 5) +
  geom_line(data=trust_info_data2[!is.na(trust_info_data2$Percentage),],
            aes(color=Category), 
            linewidth = 4) +
  scale_colour_manual(values= "#00205b")+
  scale_x_continuous(limits = c(2014, current_year), breaks = seq(2014, current_year, by = 1)) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  ggtitle(line_chart_title)  +
  geom_text(aes(label = paste0(Percentage, "%")), vjust = -0.5,
            size = 10, color = "black")+
  theme_void() +
  theme(text = element_text(size = 30),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 30),
        plot.title = element_text(hjust = 0.5, vjust = 3, size = 32),
        legend.position = "bottom",
        legend.text=element_text(size=30),
        legend.title=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y =element_text(size=30, angle = 90),
        panel.grid.major.y = element_line(),
        panel.grid.minor.y = element_line())
trust_chart_2

## Chart 3 ####
trust_chart_3 <- ggplot(trust_info_data3, aes(y=Percentage, x=Year)) + 
  geom_bar(position="dodge", width = 0.6, fill = "#00205b", stat="identity",
           colour=NA,size=0) +
  theme(text = element_text(size = 20)) +
  theme(legend.position = "bottom",
        axis.title.y = element_blank(),
        legend.title=element_blank(),
        legend.text=element_text(size=20),
        axis.text.y =element_text(size=17),
        axis.text.x =element_text(size=17),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5, size =18),
        panel.background = element_blank()) +
  ggtitle("The belief that NISRA statistics are free from political\n interference remains high at 82%") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  geom_text(size = 5.3,
            aes(label=paste0(Percentage, "%")), 
            position = position_dodge(width= 1), vjust= 0.4, hjust = -0.1) + 
  coord_flip()
trust_chart_3

## Chart 4 ####
# Trust in NISRA statistics remains high at 85%


trust_chart_4 <- ggplot(trust_info_data4, aes(x = Year, y = Percentage, group = factor(Organisation))) +
  geom_bar(
    stat = "identity",
    colour=NA,size=0,
    width = .75,
    aes(fill = factor(Organisation)),
    position = position_dodge(width = 0.9)
  ) +
  ggtitle("Trust in NISRA statistics is similar to trust in ONS statistics at 85%") +
  scale_fill_manual(values = alpha(c("#00205b", "#5094dc"))) +
  theme(axis.text.y=element_blank(),axis.ticks=element_blank(),
        legend.position = "bottom",
        legend.title=element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.text=element_text(size=24),
        axis.text.x =element_text(size=24),
        plot.title = element_text(hjust = 0.5, size =27,margin=margin(0,0,30,0)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
    geom_text(size =7.5,
            aes(label=paste0(Percentage, "%")), 
            position=position_dodge(width=0.9), 
            vjust=-0.25) 
trust_chart_4

## Trust Infographic Output ####
save_plot("code/infographic/trust1.png", fig = trust_chart_1, width=12, height=10)
save_plot("code/infographic/trust2.png", fig = trust_chart_2, width=40, height=20)
save_plot("code/infographic/trust3.png", fig = trust_chart_3, width=18, height=14)
save_plot("code/infographic/trust4.png", fig = trust_chart_4, width=30, height=20)

# Public Awareness/Trust Infographic ####
## Chart 1 ####
# transpose
hsize <- 2.5
donut_chart_df$label <- paste0(donut_chart_df$Percentage, "% ", donut_chart_df$Answer)
donut_chart_df$ymax <- cumsum(donut_chart_df$Percentage)
donut_chart_df$ymin <- c(0, head(donut_chart_df$ymax, n=-1))
donut_chart_df$label <- toupper(donut_chart_df$label)
addline_format <- function(x,...){
  gsub('\\s','\n',x)
}
aware_trust_chart_1 <- ggplot(donut_chart_df, aes(x = hsize, y = Percentage, fill = Answer)) +
  geom_col(colour=NA,size=0) +
  coord_polar(theta = "y") +
  xlim(c(0.8, hsize + .8)) +
  theme_void() +
  theme(legend.position = "none",
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks = element_blank()) +
  geom_text(x=c(3.7, -2.15), aes(y=c(13, 35), label=label, color = label), size=6,
            fontface = "bold") +
  scale_fill_manual(values = c("#CBE346", "#00205b")) +
  # ggtitle(paste0(label = expression(
  #                "Respondents’ awareness\n of",  bold("NISRA"),
  #                current_year))) +
  scale_color_manual(values = c("51% NO" = "#889956",
                                "49% YES" = "#00205b")) +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0, size =20))

## Chart 2 ####
# Stacked
draw_square <- function(data, params, size) {
  if (is.null(data$size)) {
    data$size <- 0.5
  }
  lwd <- min(data$size, min(size) /4)
  grid::rectGrob(
    width  = unit(1, "snpc") - unit(lwd, "mm"),
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

chart_labels <- c("Don't know\n ", "Tend to disagree/\nstrongly disagree\n ",
                  "Strongly agree/\ntend to agree")
aware_trust_chart_2 <- ggplot(trust_df, aes(fill=Category, y=Percentage, x=Year)) + 
  geom_bar(colour=NA,size=0, position="stack",  width = 0.6, stat="identity",
           key_glyph = draw_square) +
  scale_fill_manual(values = alpha(c("#888A87", "#CBE346", "#00205b")),
                    labels = chart_labels) +
  coord_flip() +
  labs(Category="Long title shortened\nwith wrapping") +
  theme(axis.line = element_line(colour = "black"),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.y =element_blank(),
        text = element_text(size = 15),
        legend.key=element_blank(),
        legend.title=element_blank(),
        ) + 
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  geom_text(aes(label = Percentage),
            fontface = "bold",
            size = 3.5,
            position = position_stack(vjust = 0.5),
            color = ifelse(trust_df$Category == "Tend to trust/trust a great deal"|
                           trust_df$Category == "Don't know",
                           "white", "black")
  ) +
  guides(fill = guide_legend(reverse = TRUE))

## Chart 3 ####
confidentiality$Category <- factor(confidentiality$Category, 
                                   levels = c("Don't know", "Tend to disagree/Strongly disagree",
                                              "Strongly Agree/Tend to agree"))
aware_trust_chart_3 <- ggplot(confidentiality, aes(fill=Category, y=Percentage, x=Year)) + 
  geom_bar(position="stack", width = 0.6, stat="identity", colour=NA,size=0,
           key_glyph = draw_square) +
  scale_fill_manual(values = alpha(c("#888A87", "#CBE346", "#00205b")),
                    labels = chart_labels) +
  theme(axis.line = element_line(colour = "black"),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.x =element_blank(),
        text = element_text(size = 16),
        legend.key=element_blank(),
        legend.text=element_text(size=9),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-7,-7,-7,-7),
        legend.title=element_blank(),
  ) +
  geom_text(aes(label = Percentage),
            fontface = "bold",
            size = 3.5,
            position = position_stack(vjust = 0.5),
            color = ifelse(confidentiality$Category == "Strongly Agree/Tend to agree"|
                           confidentiality$Category == "Don't know",
                           "white", "black"),
            hjust = ifelse(confidentiality$Percentage < 4, -2.7, 0.5)) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  guides(fill = guide_legend(reverse = TRUE))

## Chart 4 ####
important_df$Category <- factor(important_df$Category, 
                                   levels = c("Don't know", 
                                              "Tend to disagree/strongly disagree",
                                              "Strongly agree/tend to agree"))

chart_labels <- c("Don't know\n ", "Tend to disagree/\nstrongly disagree\n ",
                  "Strongly agree/\ntend to agree")
#Create plot
aware_trust_chart_4 <- ggplot(important_df, aes(fill = Category, y = Percentage, x = Year)) + 
  geom_col(position = 'stack', key_glyph = draw_square, width = 0.6, 
           stat="identity", colour=NA,size=0) + 
  scale_fill_manual(values = alpha(c("#888A87", "#CBE346", "#00205b", "#00205b", "#00205b")),
                    labels = chart_labels) +
  theme(text = element_text(size = 20),
        axis.line = element_line(colour = "black"),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.x =element_blank(),
        legend.key=element_blank(),
        legend.title=element_blank(),
        legend.text=element_text(size=12)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  geom_text(aes(label = Percentage),
            fontface = "bold",
            size = 4.5,
            position = position_stack(vjust = 0.5),
            color = ifelse(important_df$Category == "Strongly agree/tend to agree"|
                           important_df$Category == "Don't know",
                           "white", "black"),
            hjust = ifelse(important_df$Percentage < 4, -2.7, 0.5)) +
  guides(fill = guide_legend(reverse = TRUE))

bar_order <- c('The NI Assembly ', 'The Civil Service ', 'The media ', 'NISRA ')

## Chart 5 ####
aware_trust_chart_5 <- ggplot(trust_compared_df, 
                              aes(fill=Category, 
                                  y=Percentage, 
                                  x=factor(Institution, level = bar_order),
                                  label = Percentage)) + 
  geom_bar(position="stack", width = 0.6, stat="identity", colour=NA,size=0) +
  coord_flip() +
  scale_fill_manual(values = alpha(c("#888A87", "#CBE346","#00205b" ))) +
  theme(text = element_text(size = 19.5)) +
  theme(legend.position = "top",
        legend.justification='right',
        axis.title.y = element_blank(),
        legend.title=element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks = element_blank(),
        legend.text=element_text(size=12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  geom_text(aes(label = Percentage),
          fontface = "bold",
          size = 4.5,
          position = position_stack(vjust = 0.5),
          color = ifelse(trust_compared_df$Category == "Tend to trust/trust a great deal"|
                           trust_compared_df$Category == "Don't know",
                         "white", "black")) +
  guides(fill = guide_legend(reverse = TRUE)) 
aware_trust_chart_5

## Awareness/Trust Output ####
save_plot("code/infographic/Awareness1.png", fig = aware_trust_chart_1, width=11, height=9)
save_plot("code/infographic/Awareness2.png", fig = aware_trust_chart_2, width=13, height=8)
save_plot("code/infographic/Awareness3.png", fig = aware_trust_chart_3, width=10, height=8)
save_plot("code/infographic/Awareness4.png", fig = aware_trust_chart_4, width=13, height=8)
save_plot("code/infographic/Awareness5.png", fig = aware_trust_chart_5, width=16, height=8)



## Public Awareness Infographic ####
# 2. Circle Chart
# https://stackoverflow.com/questions/24738172/bubble-chart-with-bubbles-aligned-along-their-bottom-edges
circle <- function(center,radius) {
  th <- seq(0,2*pi,len=200)
  data.frame(x=center[1]+radius*cos(th),y=center[2]+radius*sin(th))
}

awareness_info_data1 <- awareness_info_data1 %>%
  filter(between(Year, 2017, current_year))

awareness_info_data1$end <- awareness_info_data1$Percentage/2
max <- max(awareness_info_data1$Percentage)
n.bubbles <- nrow(awareness_info_data1)
scale <- 0.4/sum(sqrt(awareness_info_data1$Percentage))

# calculate scaled centers and radii of bubbles
radii <- scale*sqrt(awareness_info_data1$Percentage)
ctr.x <- cumsum(c(radii[1],head(radii,-1)+tail(radii,-1)+.01))
# starting (larger) bubbles
gg.1  <- do.call(rbind,lapply(1:n.bubbles,function(i)cbind(group=i,circle(c(ctr.x[i],radii[i]),radii[i]))))
text.1 <- data.frame(x=ctr.x,y=-0.05,label=paste(awareness_info_data1$Year))
text.1 <- data.frame(x=ctr.x,y=-0.05,label=paste(awareness_info_data1$Year))

radii <- scale*sqrt(awareness_info_data1$Percentage)
gg.2  <- do.call(rbind,lapply(1:n.bubbles,function(i)cbind(group=i,circle(c(ctr.x[i],radii[i]),radii[i]))))
text.2 <- data.frame(x=ctr.x,y=2*radii+0.02,label=awareness_info_data1$Percentage)
text.2$y <- text.2$y/2

# make the plot
color_list <- c("#3878c5", "#00205b",  "#757575", "#68a41e", "#732777", "#ce70d2")
gg.1

pub_awareness_chart_1 <- ggplot()+
  geom_polygon(data=gg.1,aes(x,y,group=group),fill="dodgerblue")+
  geom_path(data=gg.1,aes(x,y,group=group),color="grey50")+
  geom_text(data=text.1,aes(x,y,label=label), size =3)+
  geom_text(data=text.2,aes(x,y,label=label), size =2.5, color = "white", fontface = "bold")+
  labs(x="",y="")+scale_y_continuous(limits=c(-0.1,2.5*scale*sqrt(max(awareness_info_data1$Percentage))))+
  coord_fixed() +
  theme(axis.text=element_blank(),axis.ticks=element_blank(),panel.grid=element_blank()) +
  theme(panel.background = element_blank()) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank()) +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank()) +
  ggtitle(paste0("Awareness of NISRA is significantly lower than in 2020 and 2021, but\n significantly higher than in previous years.")) +
  theme(plot.title = element_text(hjust = 0.5, size = 8, face = "bold")) +
  theme_void() 

save_plot("code/infographic/info2.png", fig = pub_awareness_chart_1, width=10, height=8)

## Chart 3 ####
title1 <- "NISRA awareness "  
title2 <- "significantly lower "
title3 <- "than ONS."
pub_awareness_chart_2 <- ggplot(awareness_info_data2, aes(fill=Group, 
                                                  y=Percentage, 
                                                  x=year,
                                                  label = Percentage)) + 
  geom_bar(position="dodge", stat="identity", height = 0.8,
           colour=NA,size=0) +
  scale_fill_manual(values = alpha(c("#3878c5", "#CBE346"))) +
  ggtitle(paste0(title1, title2, title3)) +
  theme(legend.position = "bottom",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.title=element_blank(),
        legend.text=element_text(size=11),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.text.x =element_text(size=12),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  geom_text(size =3.5,
            aes(label=paste0(Percentage, "%")), 
            position=position_dodge(width=0.9), 
            vjust=-0.25) 

## Chart 4 ####
aes(reorder(variable, value), value)
awareness_info_data3$Percentage <- round_half_up(awareness_info_data3$Percentage)

chart_order <- c("The NI Census every ten years", "The unemployment rate in NI", 
                 "Statistics on hospital waiting times \nin NI", 
                 "The number of people who live in NI", "The number of deaths in NI",
                 "Recorded levels of crime in NI", "People living in poverty in NI", 
                 "Qualifications of school leavers in \nNI", 
                 "Percentage of journeys made by \nwalking, cycling or public transport")
pub_awareness_chart_3 <- ggplot(awareness_info_data3, aes(fill=Answer, 
                                                  y=Percentage, 
                                                  x=factor(Group, levels = rev(chart_order)),
                                                  label = Percentage)) + 
  geom_bar(width=0.5, position="stack", stat="identity", colour=NA,size=0) +
  ggtitle("Awareness of specific NISRA statistics for respondents who were not aware of NISRA") +
  scale_fill_manual(values = alpha(c("#757575", "#98b4d4", "#00205b"))) +
  geom_text(data = awareness_info_data3[awareness_info_data3$Answer == "no",],
            aes(y = 95),
            fontface = "bold",
            size = 6) + 
  geom_text(data = awareness_info_data3[awareness_info_data3$Answer == "yes",],
            aes(y = 5),
            fontface = "bold",
            size = 6,
            color = "white") + 
  geom_text(label = 0.6,
            aes(y = 105),
            fontface = "bold",
            size = 6) + 
  coord_flip() +
  theme(text = element_text(size = 12),
        axis.text.x = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.text.y = element_text(size = 14, vjust =
                                     0),
        plot.title = element_text(hjust = 0.2),
        legend.position = "top",
        legend.text=element_text(size=12),
        legend.title=element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  guides(fill = guide_legend(reverse = TRUE))

## Awareness Infographic Output ####
save_plot("code/infographic/info1.png", fig = aware_trust_chart_1, width = 20, height = 14)
save_plot("code/infographic/info2.png", fig = pub_awareness_chart_1, width = 10, height = 8)
save_plot("code/infographic/info3.png", fig = pub_awareness_chart_2, width = 11, height = 10)
save_plot("code/infographic/info4.png", fig = pub_awareness_chart_3, width = 28, height = 17)




# Convert to PDF ####
## Awareness and Trust
infographic_template <- readLines(paste0(here(), "/code/infographic/Public Awareness and Trust Infographic.svg"))

for (plot in c("Awareness1", "Awareness2", "Awareness3", "Awareness4", "Awareness5")) {
  
  infographic_template <- gsub(paste0(plot, ".svg"),
                             paste0("data:image/png;base64,", base64_encode(paste0("code/infographic/", plot, ".png"))),
                             infographic_template,
                             fixed = TRUE)
  
}

writeLines(infographic_template, paste0(here(), "/outputs/Public Awareness and Trust Infographic - ", current_year, ".svg"))

rsvg_pdf(svg = paste0(here(), "/outputs/Public Awareness and Trust Infographic - ", current_year, ".svg"),
         file = paste0(here(), "/outputs/Public Awareness and Trust Infographic - ", current_year, ".pdf"))

unlink(paste0(here(), "/outputs/Public Awareness and Trust Infographic - ", current_year, ".svg"))


## Awareness ####

awareness_template <- readLines(paste0(here(), "/code/infographic/Public Awareness Infographic.svg"))

for (plot in c("info1", "info2", "info3", "info4")) {
  
  awareness_template <- gsub(paste0(plot, ".svg"),
                             paste0("data:image/png;base64,", base64_encode(paste0("code/infographic/", plot, ".png"))),
                             awareness_template,
                             fixed = TRUE)
  
}

writeLines(awareness_template, paste0(here(), "/outputs/Public Awareness Infographic - ", current_year, ".svg"))

rsvg_pdf(svg = paste0(here(), "/outputs/Public Awareness Infographic - ", current_year, ".svg"),
         file = paste0(here(), "/outputs/Public Awareness Infographic - ", current_year, ".pdf"))

unlink(paste0(here(), "/outputs/Public Awareness Infographic - ", current_year, ".svg"))


## Trust ####

trust_template <- readLines(paste0(here(), "/code/infographic/Public Trust Infographic.svg"))

for (plot in c("trust1", "trust2", "trust3", "trust4")) {
  
  trust_template <- gsub(paste0(plot, ".svg"),
                         paste0("data:image/png;base64,", base64_encode(paste0("code/infographic/", plot, ".png"))),
                         trust_template,
                         fixed = TRUE)
  
}

writeLines(trust_template, paste0(here(), "/outputs/Public Trust Infographic - ", current_year, ".svg")) 

rsvg_pdf(svg = paste0(here(), "/outputs/Public Trust Infographic - ", current_year, ".svg"),
         file = paste0(here(), "/outputs/Public Trust Infographic - ", current_year, ".pdf"))

unlink(paste0(here(), "/outputs/Public Trust Infographic - ", current_year, ".svg"))