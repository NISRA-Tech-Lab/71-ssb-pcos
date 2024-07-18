# Trust Inforgraphic
# Chart 1
donut_chart_df <- data.frame(Organisation = c("No", "Yes", "Don't Know"),
                             Percentage = c(4, 85, 11))
hsize <- 2.5
donut_chart_df$label <- paste0(donut_chart_df$Percentage, "% ", donut_chart_df$Organisation)
donut_chart_df$ymax <- cumsum(donut_chart_df$Percentage)
donut_chart_df$ymin <- c(0, head(donut_chart_df$ymax, n=-1))
trust_chart_1 <- ggplot(donut_chart_df, aes(x = hsize, y = Percentage, fill = Organisation)) +
  geom_col() +
  coord_polar(theta = "y") +
  xlim(c(0.2, hsize + 0.5)) +
  theme_void() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("#3878c5", "#68a41e", "#00205b")) +
  geom_text(x=c(1, 3.5, 3.2), aes(y=c(50, 15, 0), label=label), size=4)
trust_chart_1

# Chart 2
line_chart_df <- data.frame(Year = c(2014:2022),
                            Percentage = c(83, NA, 84, NA, NA, 84, 90, 86, 85),
                            Category = c(rep("Trust in NISRA Statistics", 9)))
line_chart_perc <- "85%"
line_chart_title <- paste0("Trust in NISRA statistics remains high at ",  line_chart_perc)
trust_chart_2 <- ggplot(line_chart_df, aes(Year, Percentage)) + 
  geom_point(color= "00205b", size = 5) +
  geom_line(data=line_chart_df[!is.na(line_chart_df$Percentage),],
            aes(color=Category), 
            linewidth = 4) +
  scale_colour_manual(values= "#00205b")+
  ylim(0, 100) +
  ggtitle(line_chart_title)  +
  theme(text = element_text(size = 25),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.text=element_text(size=25),
        legend.title=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y =element_text(size=25))

# Chart 3
bar_chart2 <- data.frame(Year = c("2014", "2016", "2019", "2020", "2021", "2022"),
                         Percentage = c(77, 77, 82, 88, 83, 82))
trust_chart_3 <- ggplot(bar_chart2, aes(y=Percentage, x=Year)) + 
  geom_bar(position="dodge", width = 0.6, fill = "#00205b", stat="identity") +
  ggtitle(bar_chart1_title) +
  theme(text = element_text(size = 22)) +
  theme(legend.position = "bottom",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.title=element_blank(),
        legend.text=element_text(size=25),
        axis.text.y =element_text(size=25),
        axis.text.x =element_text(size=25)) +
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
    aes(fill = factor(Organisation)),
    position = position_dodge(width = 0.9)
  ) +
  ggtitle("Trust in NISRA statistics is similar to trust in ONS statistics at 85%") +
  scale_fill_manual(values = alpha(c("#3878c5", "#00205b"))) +
  theme(axis.text.y=element_blank(),axis.ticks=element_blank(),
        legend.position = "bottom",
        legend.title=element_blank(),
        axis.title.y = element_blank())
trust_chart_4

# Trust Infographic Output
save_plot("code/infographic/trust1.svg", fig = trust_chart_1, width=10, height=8)
save_plot("code/infographic/trust2.svg", fig = trust_chart_2, width=40, height=22)
save_plot("code/infographic/trust3.svg", fig = trust_chart_3, width=18, height=14)
save_plot("code/infographic/trust4.svg", fig = trust_chart_4, width=30, height=20)

# Chart 1
donut_chart_df <- data.frame(Organisation = c("No", "Yes"),
                             Percentage = c(51, 49))
hsize <- 2.5
donut_chart_df$label <- paste0(donut_chart_df$Percentage, "% ", donut_chart_df$Organisation)
donut_chart_df$ymax <- cumsum(donut_chart_df$Percentage)
donut_chart_df$ymin <- c(0, head(donut_chart_df$ymax, n=-1))
aware_trust_chart_1 <- ggplot(donut_chart_df, aes(x = hsize, y = Percentage, fill = Organisation)) +
  geom_col() +
  coord_polar(theta = "y") +
  xlim(c(0.2, hsize + 0.5)) +
  theme_void() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("#3878c5", "#00205b")) +
  geom_text(x=c(3.5, -3.25), aes(y=c(13, 35), label=label), size=4)

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
library(stringr)
aware_trust_chart_2 <- ggplot(trust_df, aes(fill=Category, y=Percentage, x=Year)) + 
  geom_bar(position="stack",  width = 0.6, stat="identity") +
  scale_fill_manual(values = alpha(c("#68a41e", "#00205b", "#3878c5")),
                    labels = label_wrap_gen(width = 16)) +
  coord_flip() +
  labs(Category="Long title shortened\nwith wrapping") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 
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
  geom_bar(position="stack", width = 0.6, stat="identity") +
  scale_fill_manual(values = alpha(c("#68a41e", "#00205b", "#3878c5")),
                    labels = label_wrap_gen(width = 16)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 
aware_trust_chart_3

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
  geom_bar(position="stack", width = 0.6, stat="identity") +
  scale_fill_manual(values = alpha(c("#68a41e", "#00205b", "#3878c5")),
                    labels = label_wrap_gen(width = 16)) +
  theme(text = element_text(size = 17),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 
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
aware_trust_chart_5 <- ggplot(trust_compared_df, aes(fill=Category, y=Percentage, x=Institution)) + 
  geom_bar(position="stack", width = 0.6, stat="identity") +
  coord_flip() +
  scale_fill_manual(values = alpha(c("#68a41e", "#00205b", "#3878c5"))) +
  theme(text = element_text(size = 16)) +
  theme(legend.position = "top",
        axis.title.y = element_blank(),
        legend.title=element_blank(),
        legend.text=element_text(size=9),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()
  ) 
aware_trust_chart_5

# Awareness/Trust Output
save_plot("code/infographic/Awareness1.svg", fig = aware_trust_chart_1, width=8, height=6)
save_plot("code/infographic/Awareness2.svg", fig = aware_trust_chart_2, width=13, height=8)
save_plot("code/infographic/Awareness3.svg", fig = aware_trust_chart_3, width=10, height=8)
save_plot("code/infographic/Awareness4.svg", fig = aware_trust_chart_4, width=13, height=8)
save_plot("code/infographic/Awareness5.svg", fig = aware_trust_chart_5, width=16, height=8)

# Public Awareness Infographic
# 2. Circle Chart
# https://stackoverflow.com/questions/24738172/bubble-chart-with-bubbles-aligned-along-their-bottom-edges
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
color_list <- c("#3878c5", "#00205b", "#68a41e", "#732777", "#ce70d2","grey50")
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
        axis.text.y=element_blank())
# Chart 3
awareness_df <- data.frame(Year = c("2016", "2018", "2021", "2022",
                                    "2016", "2018", "2021", "2022"),
                           Percentage = c(33, 35, 55, 49, 71, 69, 75, NA),
                           Group = c("NISRA", "NISRA", "NISRA", "NISRA", 
                                     "ONS", "ONS", "ONS", "ONS"))
title1 <- "NISRA awareness "  
title2 <- "significantly lower "
title3 <- "than ONS."
pub_awareness_chart_2 <- ggplot(awareness_df, aes(fill=Group, y=Percentage, x=Year)) + 
  geom_bar(position="dodge", stat="identity") +
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
        plot.title = element_text(hjust = 0.5))
pub_awareness_chart_2

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
pub_awareness_chart_3 <- ggplot(not_aware_df, aes(fill=Answer, y=Percentage, x=Group)) + 
  geom_bar(width=0.5, position="stack", stat="identity") +
  ggtitle("Awareness of specific NISRA statistics for respondents who were not aware of NISRA") +
  scale_fill_manual(values = alpha(c("#757575", "#3878c5", "#00205b"))) +
  coord_flip() +
  theme(text = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(hjust = 0.2),
        legend.position = "top",
        legend.text=element_text(size=12),
        legend.title=element_blank(),
        axis.title.y = element_blank())
pub_awareness_chart_3

# Awareness Infographic Output
save_plot("code/infographic/info1.svg", fig = aware_trust_chart_1, width=13, height=7)
save_plot("code/infographic/info2.svg", fig = pub_awareness_chart_1, width=10, height=8)
save_plot("code/infographic/info3.svg", fig = pub_awareness_chart_2, width=11, height=8)
save_plot("code/infographic/info4.svg", fig = pub_awareness_chart_3, width=28, height=17)
