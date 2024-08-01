# Trust Inforgraphic
# Chart 1
donut_chart_df <- data.frame(
  class = c("No", "Yes", "Don't Know"),
  prop = c(4, 85, 11))

donut_chart_df$label <- paste0(donut_chart_df$prop, "% ", donut_chart_df$class)

mycols <- c("#a8a4a4", "#CBE346", "#00205b")
trust_chart_1 <- ggplot(donut_chart_df, aes(x = 2, y = prop, fill = class)) +
  geom_bar(stat = "identity", colour=NA,size=0) +
  coord_polar(theta = "y", start = 0)+
  scale_fill_manual(values = mycols) +
  theme_void() +
  coord_polar(theta="y") +
  xlim(-.2, 3) +
  theme(legend.position="none") +
  annotate("text", x = -.2, y = 48, size = 4.6, 
           label = paste0(label = "Respondents’ trust\n in NISRA statistics\n", current_year)) +
  geom_text(aes(label = label, y = c(86, 50.5, 94), x = c(3, 2.7, 3), color = label), size = 5) +
  scale_color_manual(values = c("4% No" = "#889956",
                              "85% Yes" = "#00205b",
                              "11% Don't Know" = "#888A87"))
trust_chart_1

# Chart 2
line_chart_df <- data.frame(Year = c(2014:2022),
                            Percentage = c(83, NA, 84, NA, NA, 84, 90, 86, 85),
                            Category = c(rep("Trust in NISRA Statistics", 9)))
line_chart_perc <- "85%"
line_chart_title <- paste0("Trust in NISRA statistics remains high at ",  line_chart_perc)
trust_chart_2 <- ggplot(line_chart_df, aes(Year, Percentage)) + 
  geom_point(color= "black", size = 5) +
  geom_line(data=line_chart_df[!is.na(line_chart_df$Percentage),],
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

# Chart 3
bar_chart2 <- data.frame(Year = c("2014", "2016", "2019", "2020", "2021", "2022"),
                         Percentage = c(77, 77, 82, 88, 83, 82))
trust_chart_3 <- ggplot(bar_chart2, aes(y=Percentage, x=Year)) + 
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

# Chart 4
# Trust in NISRA statistics remains high at 85%
trust_df <- data.frame(Organisation = c("NISRA", "NISRA", "NISRA", "NISRA", 
                                        "ONS", "ONS", "ONS", "ONS"),
                       Year = c("2014", "2016", "2021", "2022", "2014", "2016", "2021", "2022"),
                       Percentage = c(83, 84, 86, 85, 67, 69, 84, NA))

trust_chart_4 <- ggplot(trust_df, aes(x = Year, y = Percentage, group = factor(Organisation))) +
  geom_bar(
    stat = "identity",
    colour=NA,size=0,
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

# Trust Infographic Output
save_plot("code/infographic/trust1.svg", fig = trust_chart_1, width=12, height=10)
save_plot("code/infographic/trust2.svg", fig = trust_chart_2, width=40, height=20)
save_plot("code/infographic/trust3.svg", fig = trust_chart_3, width=18, height=14)
save_plot("code/infographic/trust4.svg", fig = trust_chart_4, width=30, height=20)

# Chart 1
donut_chart_df <- data.frame(Organisation = c("No", "Yes"),
                             Percentage = c(51, 49))
hsize <- 2.5
donut_chart_df$label <- paste0(donut_chart_df$Percentage, "% ", donut_chart_df$Organisation)
donut_chart_df$ymax <- cumsum(donut_chart_df$Percentage)
donut_chart_df$ymin <- c(0, head(donut_chart_df$ymax, n=-1))
donut_chart_df$label <- toupper(donut_chart_df$label)
aware_trust_chart_1 <- ggplot(donut_chart_df, aes(x = hsize, y = Percentage, fill = Organisation)) +
  geom_col(colour=NA,size=0) +
  coord_polar(theta = "y") +
  xlim(c(0.2, hsize + 0.5)) +
  theme_void() +
  theme(legend.position = "none",
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks = element_blank()) +
  scale_fill_manual(values = c("#CBE346", "#00205b")) +
  geom_text(x=c(-3.25, 3.6), aes(y=c(35, 13), label=label), size=6) +
  annotate("text", x = 0.3, y = 48, 
           size = 6, 
           label = paste0(label = "Respondents’ trust \nin NISRA statistics \n", current_year)) +
  scale_color_manual(values = c("51% NO" = "#00205b",
                                "49% YES" = "#889956"))
aware_trust_chart_1

save_plot("code/infographic/Awareness1.svg", fig = aware_trust_chart_1, width=13, height=11)

# Public Awwareness/Trust Infogrpahic
# Chart 2
trust_df <- data.frame(Year = c("2016", "2019", "2020", "2021", "2022",
                                "2016", "2019", "2020", "2021", "2022",
                                "2016", "2019", "2020", "2021", "2022"),
                       Percentage = c(84, 84, 90, 86, 85, 8, 7, 4, 3, 4, 8, 9, 6, 11, 11),
                       Category = c("Tend to trust/trust a great deal", "Tend to trust/trust a great deal", 
                                    "Tend to trust/trust a great deal", "Tend to trust/trust a great deal", 
                                    "Tend to trust/trust a great deal", "Tend to distrust/distrust a great deal", 
                                    "Tend to distrust/distrust a great deal", "Tend to distrust/distrust a great deal", 
                                    "Tend to distrust/distrust a great deal", "Tend to distrust/distrust a great deal", 
                                    "Don't know", "Don't know", "Don't know", "Don't know", "Don't know"))
# Stacked
aware_trust_chart_2 <- ggplot(trust_df, aes(fill=Category, y=Percentage, x=Year)) + 
  geom_bar(colour=NA,size=0, position="stack",  width = 0.6, stat="identity") +
  scale_fill_manual(values = alpha(c("#888A87", "#CBE346", "#00205b")),
                    labels = label_wrap_gen(width = 16)) +
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
        text = element_text(size = 20)) + 
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20))
aware_trust_chart_2

# Chart 3
personal_info_df <- data.frame(Year = c("2016", "2019", "2020", "2021", "2022",
                                        "2016", "2019", "2020", "2021", "2022",
                                        "2016", "2019", "2020", "2021", "2022"),
                               Percentage = c(89, 92, 95, 92, 92, 6, 2, 1, 1, 1, 5, 6, 4, 7, 6),
                               Category = c("Tend to trust/trust a great deal", "Tend to trust/trust a great deal", 
                                            "Tend to trust/trust a great deal", "Tend to trust/trust a great deal", 
                                            "Tend to trust/trust a great deal", "Tend to distrust/distrust a great deal", 
                                            "Tend to distrust/distrust a great deal", "Tend to distrust/distrust a great deal", 
                                            "Tend to distrust/distrust a great deal", "Tend to distrust/distrust a great deal", 
                                            "Don't know", "Don't know", "Don't know", "Don't know", "Don't know"))
# Stacked
aware_trust_chart_3 <- ggplot(personal_info_df, aes(fill=Category, y=Percentage, x=Year)) + 
  geom_bar(position="stack", width = 0.6, stat="identity", colour=NA,size=0) +
  scale_fill_manual(values = alpha(c("#888A87", "#CBE346", "#00205b")),
                    labels = label_wrap_gen(width = 16)) +
  theme(axis.line = element_line(colour = "black"),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.x =element_blank(),
        text = element_text(size = 20)) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20))

# Chart 4
important_df <- data.frame(Year = c("2016", "2019", "2020", "2021", "2022",
                                    "2016", "2019", "2020", "2021", "2022",
                                    "2016", "2019", "2020", "2021", "2022"),
                           Percentage = c(88, 90, 94, 91, 90, 6, 4, 2, 2, 3, 6, 6, 4, 8, 7),
                           Category = c("Tend to trust/trust a great deal", "Tend to trust/trust a great deal", 
                                        "Tend to trust/trust a great deal", "Tend to trust/trust a great deal", 
                                        "Tend to trust/trust a great deal", "Tend to distrust/distrust a great deal", 
                                        "Tend to distrust/distrust a great deal", "Tend to distrust/distrust a great deal", 
                                        "Tend to distrust/distrust a great deal", "Tend to distrust/distrust a great deal", 
                                        "Don't know", "Don't know", "Don't know", "Don't know", "Don't know"))
# Stacked
aware_trust_chart_4 <- ggplot(important_df, aes(fill=Category, y=Percentage, x=Year)) + 
  geom_bar(position="stack", width = 0.6, stat="identity", colour=NA,size=0) +
  scale_fill_manual(values = alpha(c("#888A87", "#CBE346", "#00205b")),
                    labels = label_wrap_gen(width = 16)) +
  theme(text = element_text(size = 20),
        axis.line = element_line(colour = "black"),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.x =element_blank()) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))
aware_trust_chart_4

# Chart 5
trust_compared_df <- data.frame(Institution = c("NISRA", "The Media", "The Civil Service", "NI Assembly",
                                                "NISRA", "The Media", "The Civil Service", "NI Assembly",
                                                "NISRA", "The Media", "The Civil Service", "NI Assembly"),
                                Category = c("Tend to trust/trust a great deal", "Tend to trust/trust a great deal", 
                                             "Tend to trust/trust a great deal", "Tend to trust/trust a great deal", 
                                             "Tend to distrust/distrust a great deal", "Tend to distrust/distrust a great deal", 
                                             "Tend to distrust/distrust a great deal", "Tend to distrust/distrust a great deal", 
                                             "Don't know", "Don't know", "Don't know", "Don't know"),
                                Percentage = c(84, 36, 77, 21, 3, 61, 18, 74, 13, 3, 6, 6))
# Stacked
aware_trust_chart_5 <- ggplot(trust_compared_df, aes(fill=Category, 
                                                     y=Percentage, 
                                                     x=Institution,
                                                     label = Percentage)) + 
  geom_bar(position="stack", width = 0.6, stat="identity", colour=NA,size=0) +
  coord_flip() +
  scale_fill_manual(values = alpha(c("#888A87", "#CBE346", "#00205b"))) +
  theme(text = element_text(size = 16)) +
  theme(legend.position = "top",
        axis.title.y = element_blank(),
        legend.title=element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks = element_blank(),
        legend.text=element_text(size=9),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  geom_text(size = 3, color = "white", nudge_y = 10)
aware_trust_chart_5

# Awareness/Trust Output
save_plot("code/infographic/Awareness1.svg", fig = aware_trust_chart_1, width=11, height=9)
save_plot("code/infographic/Awareness2.svg", fig = aware_trust_chart_2, width=13, height=8)
save_plot("code/infographic/Awareness3.svg", fig = aware_trust_chart_3, width=10, height=8)
save_plot("code/infographic/Awareness4.svg", fig = aware_trust_chart_4, width=13, height=8)
save_plot("code/infographic/Awareness5.svg", fig = aware_trust_chart_5, width=16, height=8)

# Public Awareness Infographic
# 2. Circle Chart
# https://stackoverflow.com/questions/24738172/bubble-chart-with-bubbles-aligned-along-their-bottom-edges
circle <- function(center,radius) {
  th <- seq(0,2*pi,len=200)
  data.frame(x=center[1]+radius*cos(th),y=center[2]+radius*sin(th))
}

df <- chart_1_data
df <- df %>%
  filter(between(year, 2017, current_year))
df <- data.frame(bank=c("2017", "2018", "2019", "2020", "2021", "2022"),
                 start=c(33, 35, 35, 58, 55, 49))    
df$end <- df$start/2
max <- max(df$start)
n.bubbles <- nrow(df)
scale <- 0.4/sum(sqrt(df$start))
# calculate scaled centers and radii of bubbles
radii <- scale*sqrt(df$start)
ctr.x <- cumsum(c(radii[1],head(radii,-1)+tail(radii,-1)+.01))
# starting (larger) bubbles
gg.1  <- do.call(rbind,lapply(1:n.bubbles,function(i)cbind(group=i,circle(c(ctr.x[i],radii[i]),radii[i]))))
text.1 <- data.frame(x=ctr.x,y=-0.05,label=paste(df$bank))
text.1 <- data.frame(x=ctr.x,y=-0.05,label=paste(df$bank))

radii <- scale*sqrt(df$start)
gg.2  <- do.call(rbind,lapply(1:n.bubbles,function(i)cbind(group=i,circle(c(ctr.x[i],radii[i]),radii[i]))))
text.2 <- data.frame(x=ctr.x,y=2*radii+0.02,label=df$start)
text.2$y <- text.2$y/2

# make the plot
color_list <- c("#3878c5", "#00205b",  "#757575", "#68a41e", "#732777", "#ce70d2")
gg.1

pub_awareness_chart_1 <- ggplot()+
  geom_polygon(data=gg.1,aes(x,y,group=group),fill="dodgerblue")+
  geom_path(data=gg.1,aes(x,y,group=group),color="grey50")+
  geom_text(data=text.1,aes(x,y,label=label), size =3)+
  geom_text(data=text.2,aes(x,y,label=label), size =2.5, color = "white", fontface = "bold")+
  labs(x="",y="")+scale_y_continuous(limits=c(-0.1,2.5*scale*sqrt(max(df$start))))+
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
pub_awareness_chart_1
save_plot("code/infographic/info2.svg", fig = pub_awareness_chart_1, width=10, height=8)

# Chart 3
awareness_df <- data.frame(Year = c("2016", "2018", "2021", "2022",
                                    "2016", "2018", "2021", "2022"),
                           Percentage = c(33, 35, 55, 49, 71, 69, 75, NA),
                           Group = c("NISRA", "NISRA", "NISRA", "NISRA", 
                                     "ONS", "ONS", "ONS", "ONS"))
title1 <- "NISRA awareness "  
title2 <- "significantly lower "
title3 <- "than ONS."
pub_awareness_chart_2 <- ggplot(awareness_df, aes(fill=Group, 
                                                  y=Percentage, 
                                                  x=Year,
                                                  label = Percentage)) + 
  geom_bar(position="dodge", stat="identity", height = 0.8) +
  scale_fill_manual(values = alpha(c("#3878c5", "#00205b"))) +
  ggtitle(paste0(title1, title2, title3)) +
  theme(legend.position = "bottom",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.title=element_blank(),
        legend.text=element_text(size=15),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.text.x =element_text(size=15),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  geom_text(size =3.5,
            aes(label=paste0(Percentage, "%")), 
            position=position_dodge(width=0.9), 
            vjust=-0.25) 

pub_awareness_chart_2
save_plot("code/infographic/info3.svg", fig = pub_awareness_chart_2, width=11, height=10)

# Chart 4
not_aware_df <- data.frame(Group = c("The NI Census every ten years", 
                                     "The unemployment rate in NI",
                                     "Statistics on hospital waiting times in NI",
                                     "The number of people who live in NI",
                                     "The number of deaths in NI",
                                     "Recorded levels of crime in NI",
                                     "People living in poverty in NI",
                                     "Qualifications of scholl leavers in NI",
                                     "Journeys by waling, cycling, public transport",
                                     "The NI Census every ten years", 
                                     "The unemployment rate in NI",
                                     "Statistics on hospital waiting times in NI",
                                     "The number of people who live in NI",
                                     "The number of deaths in NI",
                                     "Recorded levels of crime in NI",
                                     "People living in poverty in NI",
                                     "Qualifications of scholl leavers in NI",
                                     "Journeys by waling, cycling, public transport",
                                     "The NI Census every ten years", 
                                     "The unemployment rate in NI",
                                     "Statistics on hospital waiting times in NI",
                                     "The number of people who live in NI",
                                     "The number of deaths in NI",
                                     "Recorded levels of crime in NI",
                                     "People living in poverty in NI",
                                     "Qualifications of scholl leavers in NI",
                                     "Journeys by waling, cycling, public transport"), 
                           Percentage = c(69, 38, 35, 34, 27, 23, 20, 18, 7, 31, 62, 65, 66, 72, 76, 80, 82, 93, 
                                          0.6,0.6,0.6,0.6,0.6,0.6,0.6,0.6,0.6),
                           Answer = c("Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes","No", "No", "No", "No", 
                                      "No", "No", "No", "No", "No", "Don't Know", "Don't Know", "Don't Know", "Don't Know", "Don't Know", 
                                      "Don't Know", "Don't Know", "Don't Know", "Don't Know"))
pub_awareness_chart_3 <- ggplot(not_aware_df, aes(fill=Answer, 
                                                  y=Percentage, 
                                                  x=Group,
                                                  label = Percentage)) + 
  geom_bar(width=0.5, position="stack", stat="identity") +
  ggtitle("Awareness of specific NISRA statistics for respondents who were not aware of NISRA") +
  scale_fill_manual(values = alpha(c("#757575", "#3878c5", "#00205b"))) +
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
  geom_text(size = 5, colour = "black", position = position_stack(vjust = 0.5), hjust = -0.3) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))

pub_awareness_chart_3

# Awareness Infographic Output
save_plot("code/infographic/info1.svg", fig = aware_trust_chart_1, width=20, height=14)
save_plot("code/infographic/info2.svg", fig = pub_awareness_chart_1, width=10, height=8)
save_plot("code/infographic/info3.svg", fig = pub_awareness_chart_2, width=11, height=10)
save_plot("code/infographic/info4.svg", fig = pub_awareness_chart_3, width=28, height=17)

