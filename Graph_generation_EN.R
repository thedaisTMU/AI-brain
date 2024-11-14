library(tidyverse)
library(data.table)
library(stringr)
library(scales)
library(ggplot2)
library(DaisTheme)
library(plyr)
library(readr)

graph.data <-  fread("Graphs_spreadsheet.csv")
graph.data[, Graph_label_3 := gsub("\\\\n", "\n", Graph_label_3)]
graph.data[, Graph_label_2 := gsub("\\\\n", "\n", Graph_label_2)]
graph.data[, Graph_label_1 := gsub("\\\\n", "\n", Graph_label_1)]

figure_1_data  <- fread("Figure_1.csv")

median_comp<- median(figure_1_data$groupmean)
median_AIOE<- median(figure_1_data$aioe, na.rm=TRUE)

fig_1<-
  ggplot(figure_1_data, aes(x = aioe, y = groupmean)) +
  dais.base.theme() +
  coord_cartesian(xlim = c(min(figure_1_data$aioe, na.rm=TRUE), max(figure_1_data$aioe, na.rm=TRUE)+0.1), ylim = c(min(figure_1_data$groupmean, na.rm=TRUE),max(figure_1_data$groupmean, na.rm=TRUE)), clip="off")+
  geom_point(alpha = 0.75,data = figure_1_data[is.na(figure_1_data$label),],color = "#eb0072", shape = 16, size = 3) + 
  geom_point(alpha = 0.75,data = figure_1_data[!is.na(figure_1_data$label),], color = "#004c9b",shape = 16, size = 3) +
  scale_shape_manual(values = c(4)) +
  geom_text(aes(label = label), color = "black", hjust = 0, nudge_x = -0.07, nudge_y = -0.01, na.rm = TRUE, size = 6) +
  geom_hline(yintercept = median_comp, linetype = "solid", color = "#d7d7d7", linewidth = 1,alpha = 0.7) +
  geom_vline(xintercept = median_AIOE, linetype = "solid", color = "#d7d7d7", linewidth = 1,alpha = 0.7) +
  geom_label(aes(label = graph.data[graph.data$Figure_number=="Figure 1",Graph_label_1]), 
             nudge_x = 0.2, 
             x = max(figure_1_data$aioe)-0.15,
             y = max(figure_1_data$groupmean),
             nudge_y = 0.1,
             fill = "lightblue",
             color = "black",
             label.padding = unit(0.2, "lines"),
             size = 6)+
  geom_label(aes(label = graph.data[graph.data$Figure_number=="Figure 1",Graph_label_2]), 
             nudge_x = 0.2, 
             x = max(figure_1_data$aioe)-0.15,
             y = min(figure_1_data$groupmean),
             nudge_y = 0.1,
             fill = "lightblue",
             color = "black",
             label.padding = unit(0.2, "lines"),
             size = 6)+
  geom_label(aes(label = graph.data[graph.data$Figure_number=="Figure 1",Graph_label_3]), 
             nudge_x = 0.2, 
             x = min(figure_1_data$aioe)+0.25,
             y = min(figure_1_data$groupmean),
             nudge_y = 0.1,
             fill = "lightblue",
             color = "black",
             label.padding = unit(0.2, "lines"),
             size = 6)+
  geom_label(aes(label = graph.data[graph.data$Figure_number=="Figure 1",Graph_label_4]), 
             nudge_x = 0.2, 
             x = min(figure_1_data$aioe)+0.25,
             y = max(figure_1_data$groupmean),
             nudge_y = 0.1,
             fill = "lightblue",
             color = "black",
             label.padding = unit(0.2, "lines"),
             size = 6)+
  labs(x = graph.data[graph.data$Figure_number=="Figure 1",X_Axis],
       y = graph.data[graph.data$Figure_number=="Figure 1",Y_Axis],
       title = graph.data[graph.data$Figure_number=="Figure 1", Figure_number],
       subtitle = graph.data[graph.data$Figure_number=="Figure 1",Figure_title]) +
  theme(legend.position = "none",
        plot.title = element_text(size = 22), 
        plot.subtitle = element_text(size = 20), 
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.margin = margin(10, 10, 15, 10, "pt"), # top, right, bottom, left
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())

figure_2_data  <- fread("Figure_2.csv")
fig_2<-ggplot(figure_2_data, aes(x = month, y = `N`, color = Quadrant, group = Quadrant)) +
  geom_line() +
  labs(title = graph.data[graph.data$Figure_number=="Figure 2", Figure_number],
       subtitle = graph.data[graph.data$Figure_number=="Figure 2",Figure_title],
       caption = graph.data[graph.data$Figure_number=="Figure 2", Caption],
       x = graph.data[graph.data$Figure_number=="Figure 2",X_Axis],
       y = graph.data[graph.data$Figure_number=="Figure 2",Y_Axis],
       color = graph.data[graph.data$Figure_number=="Figure 2",Legend_name]) +
  scale_color_manual(values = c("black", "#eb0072", "#0077c8"),
                     labels = c(graph.data[graph.data$Figure_number=="Figure 2",Legend_label_1],
                                graph.data[graph.data$Figure_number=="Figure 2",Legend_label_2],
                                graph.data[graph.data$Figure_number=="Figure 2",Legend_label_3])) +
  scale_y_continuous(labels = label_comma()) + 
  scale_x_date(limits = c(min(figure_2_data$month), max(figure_2_data$month)),
               expand = c(0, 0)) +
  coord_cartesian(xlim = c(min(figure_2_data$month), max(figure_2_data$month)+62)) +  # Extend right side
  # Add vertical lines for April 2022 and November 2022
  geom_vline(xintercept = as.Date("2020-03-01"), linetype = "solid", color = "black") +
  geom_vline(xintercept = as.Date("2022-04-01"), linetype = "solid", color = "black") +
  geom_vline(xintercept = as.Date("2022-11-01"), linetype = "solid", color = "red") +
  # Add annotations for the vertical lines
  annotate("text", x = as.Date("2020-03-01"), y = Inf, label = graph.data[graph.data$Figure_number=="Figure 2",Graph_label_1], vjust = 1, hjust= 1.1, color = "black", family = "sans") +
  annotate("text", x = as.Date("2022-04-01"), y = Inf, label = graph.data[graph.data$Figure_number=="Figure 2",Graph_label_2], vjust = 1, hjust= 1.1, color = "black", family = "sans") +
  annotate("text", x = as.Date("2022-11-01"), y = Inf, label = graph.data[graph.data$Figure_number=="Figure 2",Graph_label_3], vjust = 1, hjust= -0.1, color = "red", family = "sans") +
  dais.base.theme()+
  theme(axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
        legend.text = element_text(size = 14),  
        legend.title = element_text(size = 16), 
        legend.key.width = unit(2, "cm"), 
        legend.key.height = unit(1, "cm"),
        plot.title = element_text(size = 22), 
        plot.subtitle = element_text(size = 20), 
        axis.title.x = element_text(size = 18,margin = margin(r = 0, t = 10, b = 0, l = 0)),
        axis.title.y = element_text(size = 18, margin = margin(r = 10, t = 0, b = 0, l = 0)),
        plot.margin = margin(10, 10, 15, 10, "pt"),
        axis.text.y = element_text(size = 14))

figure_3_data  <- fread("Figure_3.csv")
fig_3<-ggplot(figure_3_data, aes(x = month, y = index, color = Quadrant, group = Quadrant)) +
  geom_line() +
  scale_y_continuous(breaks = seq(0, max(figure_3_data$index, na.rm = TRUE), by = 20)) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "grey", size = 0.5, alpha = 0.9) +
  labs(title = graph.data[graph.data$Figure_number=="Figure 3", Figure_number],
       subtitle = graph.data[graph.data$Figure_number=="Figure 3",Figure_title],
       caption = graph.data[graph.data$Figure_number=="Figure 3", Caption],
       x = graph.data[graph.data$Figure_number=="Figure 3",X_Axis],
       y = graph.data[graph.data$Figure_number=="Figure 3",Y_Axis],
       color = graph.data[graph.data$Figure_number=="Figure 3",Legend_name]) +
  scale_color_manual(values = c("black", "#eb0072", "#0077c8"),
                     labels = c(graph.data[graph.data$Figure_number=="Figure 3",Legend_label_1],
                                graph.data[graph.data$Figure_number=="Figure 3",Legend_label_2],
                                graph.data[graph.data$Figure_number=="Figure 3",Legend_label_3])) +
  scale_x_date(limits = c(min(figure_3_data$month), max(figure_3_data$month)),
               expand = c(0, 0)) +
  coord_cartesian(xlim = c(min(figure_3_data$month), max(figure_3_data$month)+62)) +  # Extend right side
  # Add vertical lines for April 2022 and November 2022
  geom_vline(xintercept = as.Date("2020-03-01"), linetype = "solid", color = "black") +
  geom_vline(xintercept = as.Date("2022-04-01"), linetype = "solid", color = "black") +
  geom_vline(xintercept = as.Date("2022-11-01"), linetype = "solid", color = "red") +
  # Add annotations for the vertical lines
  annotate("text", x = as.Date("2020-03-01"), y = Inf, label = graph.data[graph.data$Figure_number=="Figure 3",Graph_label_1], vjust = 1, hjust= 1.1, color = "black", family = "sans") +
  annotate("text", x = as.Date("2022-04-01"), y = Inf, label = graph.data[graph.data$Figure_number=="Figure 3",Graph_label_2], vjust = 1, hjust= 1.1, color = "black", family = "sans") +
  annotate("text", x = as.Date("2022-11-01"), y = Inf, label = graph.data[graph.data$Figure_number=="Figure 3",Graph_label_3], vjust = 1, hjust= -0.1, color = "red", family = "sans") +
  dais.base.theme()+
  theme(axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
        legend.text = element_text(size = 14),  
        legend.title = element_text(size = 16), 
        legend.key.width = unit(2, "cm"), 
        legend.key.height = unit(1, "cm"),
        plot.title = element_text(size = 22), 
        plot.subtitle = element_text(size = 20), 
        axis.title.x = element_text(size = 18,margin = margin(r = 0, t = 10, b = 0, l = 0)),
        axis.title.y = element_text(size = 18, margin = margin(r = 10, t = 0, b = 0, l = 0)),
        plot.margin = margin(10, 10, 15, 10, "pt"),
        axis.text.y = element_text(size = 14))

figure_4_data  <- fread("Figure_4.csv")
fig_4<-ggplot(figure_4_data, aes(x = month, y = avg_remuneration, color = Quadrant, group = Quadrant)) +
  geom_line(size = 1) +
  labs(title = graph.data[graph.data$Figure_number=="Figure 4", Figure_number],
       subtitle = graph.data[graph.data$Figure_number=="Figure 4",Figure_title],
       caption = graph.data[graph.data$Figure_number=="Figure 4", Caption],
       x = graph.data[graph.data$Figure_number=="Figure 4",X_Axis],
       y = graph.data[graph.data$Figure_number=="Figure 4",Y_Axis],
       color = graph.data[graph.data$Figure_number=="Figure 4",Legend_name]) +
  scale_color_manual(values = c("black", "#eb0072", "#0077c8"),
                     labels = c(graph.data[graph.data$Figure_number=="Figure 4",Legend_label_1],
                                graph.data[graph.data$Figure_number=="Figure 4",Legend_label_2],
                                graph.data[graph.data$Figure_number=="Figure 4",Legend_label_3])) +
  scale_x_date(limits = c(min(figure_4_data$month), max(figure_4_data$month)),
               expand = c(0, 0)) +
  coord_cartesian(xlim = c(min(figure_4_data$month), max(figure_4_data$month)+62)) +  # Extend right side
  scale_y_continuous(labels = dollar) + 
  # Add vertical lines for April 2022 and November 2022
  geom_vline(xintercept = as.Date("2020-03-01"), linetype = "solid", color = "black") +
  geom_vline(xintercept = as.Date("2022-04-01"), linetype = "solid", color = "black") +
  geom_vline(xintercept = as.Date("2022-11-01"), linetype = "solid", color = "red") +
  # Add annotations for the vertical lines
  annotate("text", x = as.Date("2020-03-01"), y = Inf, label = graph.data[graph.data$Figure_number=="Figure 4",Graph_label_1], vjust = 1, hjust= 1.1, color = "black", family = "sans") +
  annotate("text", x = as.Date("2022-04-01"), y = Inf, label = graph.data[graph.data$Figure_number=="Figure 4",Graph_label_2], vjust = 1, hjust= 1.1, color = "black", family = "sans") +
  annotate("text", x = as.Date("2022-11-01"), y = Inf, label = graph.data[graph.data$Figure_number=="Figure 4",Graph_label_3], vjust = 1, hjust= -0.1, color = "red", family = "sans") +
  dais.base.theme()+
  theme(axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
        legend.text = element_text(size = 14),  
        legend.title = element_text(size = 16), 
        legend.key.width = unit(2, "cm"), 
        legend.key.height = unit(1, "cm"),
        plot.title = element_text(size = 22), 
        plot.subtitle = element_text(size = 20), 
        axis.title.x = element_text(size = 18,margin = margin(r = 0, t = 10, b = 0, l = 0)),
        axis.title.y = element_text(size = 18, margin = margin(r = 10, t = 0, b = 0, l = 0)),
        plot.margin = margin(10, 10, 15, 10, "pt"),
        axis.text.y = element_text(size = 14)) 

figure_5_data  <- fread("Figure_5.csv")
fig_5 <- ggplot(figure_5_data, aes(x = month, y = index, color = interaction(Quadrant, education), linetype = interaction(Quadrant, education), group = interaction(Quadrant, education))) +
  geom_line(size = 1) +
  labs(title = graph.data[graph.data$Figure_number=="Figure 5", Figure_number],
       subtitle = graph.data[graph.data$Figure_number=="Figure 5",Figure_title],
       caption = graph.data[graph.data$Figure_number=="Figure 5", Caption],
       x = graph.data[graph.data$Figure_number=="Figure 5",X_Axis],
       y = graph.data[graph.data$Figure_number=="Figure 5",Y_Axis],
       color = graph.data[graph.data$Figure_number=="Figure 5",Legend_name],
       linetype = graph.data[graph.data$Figure_number=="Figure 5",Legend_name]) +
  scale_x_date(limits = c(min(figure_5_data$month), max(figure_5_data$month)),
               expand = c(0, 0)) +
  coord_cartesian(xlim = c(min(figure_5_data$month), max(figure_5_data$month)+62)) +  # Extend right side
  # Set y-axis ticks at intervals of 20
  scale_y_continuous(breaks = seq(0, max(figure_5_data$index, na.rm = TRUE), by = 20)) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "grey", size = 0.5, alpha = 0.9) +
  # Custom colors and linetypes for each combination of quadrant and education
  scale_color_manual(values = c("#eb0072", "black", "#0077c8"),
                     labels = c(graph.data[graph.data$Figure_number=="Figure 5",Legend_label_1],
                                graph.data[graph.data$Figure_number=="Figure 5",Legend_label_2],
                                graph.data[graph.data$Figure_number=="Figure 5",Legend_label_3])) +
  scale_linetype_manual(values = c("solid", "solid", "solid"),
                        labels = c(graph.data[graph.data$Figure_number=="Figure 5",Legend_label_1],
                                   graph.data[graph.data$Figure_number=="Figure 5",Legend_label_2],
                                   graph.data[graph.data$Figure_number=="Figure 5",Legend_label_3])) +
  # Add vertical lines for April 2022 and November 2022
  geom_vline(xintercept = as.Date("2020-03-01"), linetype = "solid", color = "black") +
  geom_vline(xintercept = as.Date("2022-04-01"), linetype = "solid", color = "black") +
  geom_vline(xintercept = as.Date("2022-11-01"), linetype = "solid", color = "red") +
  # Add annotations for the vertical lines
  annotate("text", x = as.Date("2020-03-01"), y = Inf, label = graph.data[graph.data$Figure_number=="Figure 5",Graph_label_1], vjust = 1, hjust= 1.1, color = "black", family = "sans") +
  annotate("text", x = as.Date("2022-04-01"), y = Inf, label = graph.data[graph.data$Figure_number=="Figure 5",Graph_label_2], vjust = 1, hjust= 1.1, color = "black", family = "sans") +
  annotate("text", x = as.Date("2022-11-01"), y = Inf, label = graph.data[graph.data$Figure_number=="Figure 5",Graph_label_3], vjust = 1, hjust= -0.1, color = "red", family = "sans") +
  dais.base.theme()+
  theme(axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
        legend.text = element_text(size = 14),  
        legend.title = element_text(size = 16), 
        legend.key.width = unit(2, "cm"), 
        legend.key.height = unit(1, "cm"),
        plot.title = element_text(size = 22), 
        plot.subtitle = element_text(size = 20), 
        axis.title.x = element_text(size = 18,margin = margin(r = 0, t = 10, b = 0, l = 0)),
        axis.title.y = element_text(size = 18, margin = margin(r = 10, t = 0, b = 0, l = 0)),
        plot.margin = margin(10, 10, 15, 10, "pt"),
        axis.text.y = element_text(size = 14))

figure_6_data  <- fread("Figure_6.csv")
fig_6<-ggplot(figure_6_data, aes(x = month, y = `N`, color = interaction(Quadrant, ind), linetype = interaction(Quadrant, ind), group = interaction(Quadrant, ind))) +
  geom_line(size = 1) +
  labs(title = graph.data[graph.data$Figure_number=="Figure 6",Figure_number],
       subtitle = graph.data[graph.data$Figure_number=="Figure 6",Figure_title],
       caption = graph.data[graph.data$Figure_number=="Figure 6",Caption],
       x = graph.data[graph.data$Figure_number=="Figure 6",X_Axis],
       y = graph.data[graph.data$Figure_number=="Figure 6",Y_Axis],
       color = graph.data[graph.data$Figure_number=="Figure 6",Legend_name],
       linetype = graph.data[graph.data$Figure_number=="Figure 6",Legend_name]) +
  scale_y_continuous(labels = label_comma()) + 
  scale_x_date(limits = c(min(figure_6_data$month), max(figure_6_data$month)),
               expand = c(0, 0)) +
  coord_cartesian(xlim = c(min(figure_6_data$month), max(figure_6_data$month)+62)) +  # Extend right side
  # Custom colors and linetypes for each combination of quadrant and ind
  scale_color_manual(values = c("black", "#eb0072", "#0077c8"),
                     labels = c(graph.data[graph.data$Figure_number=="Figure 6",Legend_label_1],
                                graph.data[graph.data$Figure_number=="Figure 6",Legend_label_2],
                                graph.data[graph.data$Figure_number=="Figure 6",Legend_label_3])) +
  scale_linetype_manual(values = c("solid", "solid", "solid"),
                        labels = c(graph.data[graph.data$Figure_number=="Figure 6",Legend_label_1],
                                   graph.data[graph.data$Figure_number=="Figure 6",Legend_label_2],
                                   graph.data[graph.data$Figure_number=="Figure 6",Legend_label_3])) +
  # Add vertical lines for April 2022 and November 2022
  geom_vline(xintercept = as.Date("2020-03-01"), linetype = "solid", color = "black") +
  geom_vline(xintercept = as.Date("2022-04-01"), linetype = "solid", color = "black") +
  geom_vline(xintercept = as.Date("2022-11-01"), linetype = "solid", color = "red") +
  # Add annotations for the vertical lines
  annotate("text", x = as.Date("2020-03-01"), y = Inf, label = graph.data[graph.data$Figure_number=="Figure 6",Graph_label_1], vjust = 1, hjust= 1.1, color = "black", family = "sans") +
  annotate("text", x = as.Date("2022-04-01"), y = Inf, label = graph.data[graph.data$Figure_number=="Figure 6",Graph_label_2], vjust = 1, hjust= 1.1, color = "black", family = "sans") +
  annotate("text", x = as.Date("2022-11-01"), y = Inf, label = graph.data[graph.data$Figure_number=="Figure 6",Graph_label_3], vjust = 1, hjust= -0.1, color = "red", family = "sans") +
  dais.base.theme() +
  theme(axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
        legend.text = element_text(size = 14),  
        legend.title = element_text(size = 16), 
        legend.key.width = unit(2, "cm"), 
        legend.key.height = unit(1, "cm"),
        plot.title = element_text(size = 22), 
        plot.subtitle = element_text(size = 20), 
        axis.title.x = element_text(size = 18,margin = margin(r = 0, t = 10, b = 0, l = 0)),
        axis.title.y = element_text(size = 18, margin = margin(r = 10, t = 0, b = 0, l = 0)),
        plot.margin = margin(10, 10, 15, 10, "pt"),
        axis.text.y = element_text(size = 14))

figure_7_data  <- fread("Figure_7.csv")
fig_7 <- ggplot(figure_7_data, aes(x = month, y = index, color = interaction(Quadrant, ind), linetype = interaction(Quadrant, ind), group = interaction(Quadrant, ind))) +
  geom_line(size = 1) +
  labs(title = graph.data[graph.data$Figure_number=="Figure 7",Figure_number],
       subtitle = graph.data[graph.data$Figure_number=="Figure 7",Figure_title],
       caption = graph.data[graph.data$Figure_number=="Figure 7", Caption],
       x = graph.data[graph.data$Figure_number=="Figure 7",X_Axis],
       y = graph.data[graph.data$Figure_number=="Figure 7",Y_Axis],
       color = graph.data[graph.data$Figure_number=="Figure 7",Legend_name],
       linetype = graph.data[graph.data$Figure_number=="Figure 7",Legend_name]) +
  # Set y-axis ticks at intervals of 20
  scale_x_date(limits = c(min(figure_7_data$month), max(figure_7_data$month)),
               expand = c(0, 0)) +
  coord_cartesian(xlim = c(min(figure_7_data$month), max(figure_7_data$month)+62)) +  # Extend right side
  scale_y_continuous(breaks = seq(0, max(figure_7_data$index, na.rm = TRUE), by = 20)) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "grey", size = 0.5, alpha = 0.9) +
  scale_color_manual(values = c("black", "#eb0072"),
                     labels = c(graph.data[graph.data$Figure_number=="Figure 7",Legend_label_1],
                                graph.data[graph.data$Figure_number=="Figure 7",Legend_label_2])) +
  scale_linetype_manual(values = c("solid", "solid","solid"),
                        labels = c(graph.data[graph.data$Figure_number=="Figure 7",Legend_label_1], graph.data[graph.data$Figure_number=="Figure 7",Legend_label_2])) +
  # Add vertical lines for April 2022 and November 2022
  geom_vline(xintercept = as.Date("2020-03-01"), linetype = "solid", color = "black") +
  geom_vline(xintercept = as.Date("2022-04-01"), linetype = "solid", color = "black") +
  geom_vline(xintercept = as.Date("2022-11-01"), linetype = "solid", color = "red") +
  # Add annotations for the vertical lines
  annotate("text", x = as.Date("2020-03-01"), y = Inf, label = graph.data[graph.data$Figure_number=="Figure 7",Graph_label_1], vjust = 1, hjust= 1.1, color = "black", family = "sans") +
  annotate("text", x = as.Date("2022-04-01"), y = Inf, label = graph.data[graph.data$Figure_number=="Figure 7",Graph_label_2], vjust = 1, hjust= 1.1, color = "black", family = "sans") +
  annotate("text", x = as.Date("2022-11-01"), y = Inf, label = graph.data[graph.data$Figure_number=="Figure 7",Graph_label_3], vjust = 1, hjust= -0.1, color = "red", family = "sans") +
  dais.base.theme() +
  theme(axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
        legend.text = element_text(size = 14),  
        legend.title = element_text(size = 16), 
        legend.key.width = unit(2, "cm"), 
        legend.key.height = unit(1, "cm"),
        plot.title = element_text(size = 22), 
        plot.subtitle = element_text(size = 20), 
        axis.title.x = element_text(size = 18,margin = margin(r = 0, t = 10, b = 0, l = 0)),
        axis.title.y = element_text(size = 18, margin = margin(r = 10, t = 0, b = 0, l = 0)),
        plot.margin = margin(10, 10, 15, 10, "pt"),
        axis.text.y = element_text(size = 14))

figure_8_data  <- fread("Figure_8.csv")
figure_8_data$label <- str_wrap(figure_8_data$label, width = 22)
fig_8<-
  ggplot(figure_8_data, aes(x = aioe, y = groupmean)) +
  dais.base.theme() +
  coord_cartesian(xlim = c(min(figure_8_data$aioe, na.rm=TRUE), max(figure_8_data$aioe, na.rm=TRUE)+0.1), ylim = c(min(figure_8_data$groupmean, na.rm=TRUE),max(figure_8_data$groupmean, na.rm=TRUE)), clip="off")+
  geom_point(alpha = 0.75, data = figure_8_data[is.na(figure_8_data$label),],color = "#eb0072", shape = 16, size = 4) + 
  geom_point(alpha = 0.75, data = figure_8_data[!is.na(figure_8_data$label),], color = "#004c9b",shape = 16, size = 4) +
  geom_text(aes(label = label), color = "black", hjust = 0, nudge_x = 0.005, nudge_y = 0.007, na.rm = TRUE, size = 6, lineheight = 0.9) +
  geom_hline(yintercept = median_comp, linetype = "solid", color = "#818081", linewidth = 1,alpha = 0.7) +
  labs(x = graph.data[graph.data$Figure_number=="Figure 8",X_Axis],
       y = graph.data[graph.data$Figure_number=="Figure 8",Y_Axis],
       title = graph.data[graph.data$Figure_number=="Figure 8", Figure_number],
       subtitle = graph.data[graph.data$Figure_number=="Figure 8",Figure_title])+
  theme(legend.position = "none",
        plot.title = element_text(size = 22), 
        plot.subtitle = element_text(size = 20), 
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.margin = margin(10, 10, 15, 10, "pt"), # top, right, bottom, left
        axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())

figure_9_data  <- fread("Figure_9.csv")
fig_9<-
  ggplot(figure_9_data, aes(x = aioe, y = groupmean)) +
  dais.base.theme() +
  coord_cartesian(xlim = c(min(figure_9_data$aioe, na.rm=TRUE), max(figure_9_data$aioe, na.rm=TRUE)+0.1), ylim = c(min(figure_9_data$groupmean, na.rm=TRUE),max(figure_9_data$groupmean, na.rm=TRUE)), clip="off")+
  geom_point(alpha = 0.75, data = figure_9_data[is.na(figure_9_data$label),],color = "#eb0072", shape = 16, size = 4) + 
  geom_point(alpha = 0.75, data = figure_9_data[!is.na(figure_9_data$label),], color = "#004c9b",shape = 16, size = 4) +
  geom_text(aes(label = label), color = "black", hjust = 0, nudge_x = 0.005,nudge_y = -0.003, na.rm = TRUE, size = 6) +
  geom_hline(yintercept = median_comp, linetype = "solid", color = "#818081", linewidth = 1,alpha = 0.7) +
  labs(x = graph.data[graph.data$Figure_number=="Figure 9",X_Axis],
       y = graph.data[graph.data$Figure_number=="Figure 9",Y_Axis],
       title = graph.data[graph.data$Figure_number=="Figure 9", Figure_number],
       subtitle = graph.data[graph.data$Figure_number=="Figure 9",Figure_title])+
  theme(legend.position = "none",
        plot.title = element_text(size = 22), 
        plot.subtitle = element_text(size = 20), 
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.margin = margin(10, 10, 15, 10, "pt"), # top, right, bottom, left
        axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())

figure_10_data  <- fread("Figure_10.csv")
fig_10<-
  ggplot(figure_10_data, aes(x = aioe, y = groupmean)) +
  dais.base.theme() +
  coord_cartesian(xlim = c(min(figure_10_data$aioe, na.rm=TRUE), max(figure_10_data$aioe, na.rm=TRUE)+0.1), ylim = c(min(figure_10_data$groupmean, na.rm=TRUE),max(figure_10_data$groupmean, na.rm=TRUE)), clip="off")+
  geom_point(alpha = 0.75, data = figure_10_data[is.na(figure_10_data$label),],color = "#eb0072", shape = 16, size = 4) + 
  geom_point(alpha = 0.75, data = figure_10_data[!is.na(figure_10_data$label),], color = "#004c9b",shape = 16, size = 4) +
  scale_y_continuous(position = "right")+
  geom_text(aes(label = label), color = "black", hjust = 0, nudge_x = 0.008, na.rm = TRUE, size = 6) +
  geom_hline(yintercept = median_comp, linetype = "solid", color = "#818081", linewidth = 1,alpha = 0.7) +
  labs(x = graph.data[graph.data$Figure_number=="Figure 10",X_Axis],
       y = graph.data[graph.data$Figure_number=="Figure 10",Y_Axis],
       title = graph.data[graph.data$Figure_number=="Figure 10", Figure_number],
       subtitle = graph.data[graph.data$Figure_number=="Figure 10",Figure_title])+
  theme(legend.position = "none",
        plot.title = element_text(size = 22), 
        plot.subtitle = element_text(size = 20), 
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.margin = margin(10, 10, 15, 10, "pt"), # top, right, bottom, left
        axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())

figure_B1_data <- fread("Figure_B1.csv")
fig_B1<-ggplot(figure_B1_data, aes(x = month, y = `N`, color = STC_Quadrant, group = STC_Quadrant)) +
  geom_line() +
  labs(title = graph.data[graph.data$Figure_number=="Figure B1",Figure_number],
       subtitle = graph.data[graph.data$Figure_number=="Figure B1", Figure_title],
       caption = graph.data[graph.data$Figure_number=="Figure B1", Caption],
       x = graph.data[graph.data$Figure_number=="Figure B1", X_Axis],
       y = graph.data[graph.data$Figure_number=="Figure B1", Y_Axis],
       color = graph.data[graph.data$Figure_number=="Figure B1", Legend_name]) +
  scale_color_manual(values = c("black", "#eb0072", "#0077c8"),
                     labels = c(graph.data[graph.data$Figure_number=="Figure B1",Legend_label_1],
                                graph.data[graph.data$Figure_number=="Figure B1",Legend_label_2],
                                graph.data[graph.data$Figure_number=="Figure B1",Legend_label_3])) +
  # Add vertical lines for April 2022 and November 2022
  geom_vline(xintercept = as.Date("2020-03-01"), linetype = "solid", color = "black") +
  geom_vline(xintercept = as.Date("2022-04-01"), linetype = "solid", color = "black") +
  geom_vline(xintercept = as.Date("2022-11-01"), linetype = "solid", color = "red") +
  scale_y_continuous(labels = label_comma()) + 
  scale_x_date(limits = c(min(figure_B1_data$month), max(figure_2_data$month)),
               expand = c(0, 0)) +
  coord_cartesian(xlim = c(min(figure_B1_data$month), max(figure_2_data$month)+62)) +  # Extend right side
  # Add annotations for the vertical lines
  annotate("text", x = as.Date("2020-03-01"), y = Inf, label = graph.data[graph.data$Figure_number=="Figure B1",Graph_label_1], vjust = 1, hjust= 1.1, color = "black", family = "sans") +
  annotate("text", x = as.Date("2022-04-01"), y = Inf, label = graph.data[graph.data$Figure_number=="Figure B1",Graph_label_2], vjust = 1, hjust= 1.1, color = "black", family = "sans") +
  annotate("text", x = as.Date("2022-11-01"), y = Inf, label = graph.data[graph.data$Figure_number=="Figure B1",Graph_label_3], vjust = 1, hjust= -0.1, color = "red", family = "sans") +
  dais.base.theme() +
  theme(axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
        legend.text = element_text(size = 14),  
        legend.title = element_text(size = 16), 
        legend.key.width = unit(2, "cm"), 
        legend.key.height = unit(1, "cm"),
        plot.title = element_text(size = 22), 
        plot.subtitle = element_text(size = 20), 
        axis.title.x = element_text(size = 18,margin = margin(r = 0, t = 10, b = 0, l = 0)),
        axis.title.y = element_text(size = 18, margin = margin(r = 10, t = 0, b = 0, l = 0)),
        plot.margin = margin(10, 10, 15, 10, "pt"),
        axis.text.y = element_text(size = 14))

figure_B2_data  <- fread("Figure_B2.csv")
fig_B2<-ggplot(figure_B2_data, aes(x = month, y = index, color = STC_Quadrant, group = STC_Quadrant)) +
  geom_line() +
  scale_y_continuous(breaks = seq(0, max(figure_B2_data$index, na.rm = TRUE), by = 20)) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "grey", size = 0.5, alpha = 0.9) +
  labs(title = graph.data[graph.data$Figure_number=="Figure B2", Figure_number],
       subtitle = graph.data[graph.data$Figure_number=="Figure B2",Figure_title],
       caption = graph.data[graph.data$Figure_number=="Figure B2", Caption],
       x = graph.data[graph.data$Figure_number=="Figure B2",X_Axis],
       y = graph.data[graph.data$Figure_number=="Figure B2",Y_Axis],
       color = graph.data[graph.data$Figure_number=="Figure B2",Legend_name]) +
  scale_color_manual(values = c("black", "#eb0072", "#0077c8"),
                     labels = c(graph.data[graph.data$Figure_number=="Figure B2",Legend_label_1],
                                graph.data[graph.data$Figure_number=="Figure B2",Legend_label_2],
                                graph.data[graph.data$Figure_number=="Figure B2",Legend_label_3])) +
  scale_x_date(limits = c(min(figure_B2_data$month), max(figure_B2_data$month)),
               expand = c(0, 0)) +
  coord_cartesian(xlim = c(min(figure_B2_data$month), max(figure_B2_data$month)+62)) +  # Extend right side
  # Add vertical lines for April 2022 and November 2022
  geom_vline(xintercept = as.Date("2020-03-01"), linetype = "solid", color = "black") +
  geom_vline(xintercept = as.Date("2022-04-01"), linetype = "solid", color = "black") +
  geom_vline(xintercept = as.Date("2022-11-01"), linetype = "solid", color = "red") +
  # Add annotations for the vertical lines
  annotate("text", x = as.Date("2020-03-01"), y = Inf, label = graph.data[graph.data$Figure_number=="Figure B2",Graph_label_1], vjust = 1, hjust= 1.1, color = "black", family = "sans") +
  annotate("text", x = as.Date("2022-04-01"), y = Inf, label = graph.data[graph.data$Figure_number=="Figure B2",Graph_label_2], vjust = 1, hjust= 1.1, color = "black", family = "sans") +
  annotate("text", x = as.Date("2022-11-01"), y = Inf, label = graph.data[graph.data$Figure_number=="Figure B2",Graph_label_3], vjust = 1, hjust= -0.1, color = "red", family = "sans") +
  dais.base.theme()+
  theme(axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
        legend.text = element_text(size = 14),  
        legend.title = element_text(size = 16), 
        legend.key.width = unit(2, "cm"), 
        legend.key.height = unit(1, "cm"),
        plot.title = element_text(size = 22), 
        plot.subtitle = element_text(size = 20), 
        axis.title.x = element_text(size = 18,margin = margin(r = 0, t = 10, b = 0, l = 0)),
        axis.title.y = element_text(size = 18, margin = margin(r = 10, t = 0, b = 0, l = 0)),
        plot.margin = margin(10, 10, 15, 10, "pt"),
        axis.text.y = element_text(size = 14))

figure_B3_data  <- fread("Figure_B3.csv")
fig_B3<-ggplot(figure_B3_data, aes(x = month, y = avg_remuneration, color = STC_Quadrant, group = STC_Quadrant)) +
  geom_line(size = 1) +
  labs(title = graph.data[graph.data$Figure_number=="Figure B3", Figure_number],
       subtitle = graph.data[graph.data$Figure_number=="Figure B3",Figure_title],
       caption = graph.data[graph.data$Figure_number=="Figure B3", Caption],
       x = graph.data[graph.data$Figure_number=="Figure B3",X_Axis],
       y = graph.data[graph.data$Figure_number=="Figure B3",Y_Axis],
       color = graph.data[graph.data$Figure_number=="Figure B3",Legend_name]) +
  scale_color_manual(values = c("black", "#eb0072", "#0077c8"),
                     labels = c(graph.data[graph.data$Figure_number=="Figure B3",Legend_label_1],
                                graph.data[graph.data$Figure_number=="Figure B3",Legend_label_2],
                                graph.data[graph.data$Figure_number=="Figure B3",Legend_label_3])) +
  scale_x_date(limits = c(min(figure_B3_data$month), max(figure_B3_data$month)),
               expand = c(0, 0)) +
  coord_cartesian(xlim = c(min(figure_B3_data$month), max(figure_B3_data$month)+62)) +  # Extend right side
  scale_y_continuous(labels = dollar) + 
  # Add vertical lines for April 2022 and November 2022
  geom_vline(xintercept = as.Date("2020-03-01"), linetype = "solid", color = "black") +
  geom_vline(xintercept = as.Date("2022-04-01"), linetype = "solid", color = "black") +
  geom_vline(xintercept = as.Date("2022-11-01"), linetype = "solid", color = "red") +
  # Add annotations for the vertical lines
  annotate("text", x = as.Date("2020-03-01"), y = Inf, label = graph.data[graph.data$Figure_number=="Figure B3",Graph_label_1], vjust = 1, hjust= 1.1, color = "black", family = "sans") +
  annotate("text", x = as.Date("2022-04-01"), y = Inf, label = graph.data[graph.data$Figure_number=="Figure B3",Graph_label_2], vjust = 1, hjust= 1.1, color = "black", family = "sans") +
  annotate("text", x = as.Date("2022-11-01"), y = Inf, label = graph.data[graph.data$Figure_number=="Figure B3",Graph_label_3], vjust = 1, hjust= -0.1, color = "red", family = "sans") +
  dais.base.theme()+
  theme(axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
        legend.text = element_text(size = 14),  
        legend.title = element_text(size = 16), 
        legend.key.width = unit(2, "cm"), 
        legend.key.height = unit(1, "cm"),
        plot.title = element_text(size = 22), 
        plot.subtitle = element_text(size = 20), 
        axis.title.x = element_text(size = 18,margin = margin(r = 0, t = 10, b = 0, l = 0)),
        axis.title.y = element_text(size = 18, margin = margin(r = 10, t = 0, b = 0, l = 0)),
        plot.margin = margin(10, 10, 15, 10, "pt"),
        axis.text.y = element_text(size = 14)) 

figure_C1_data <- fread("Figure_C1.csv")
fig_C1 <- plot.line.dais(figure_C1_data,tf,tf_adjusted,group.by=Dais_quadrant,
                         caption = graph.data[graph.data$Figure_number=="Figure C1",Caption])+
  labs(x = graph.data[graph.data$Figure_number=="Figure C1", X_Axis],
       y = graph.data[graph.data$Figure_number=="Figure C1", Y_Axis],
       title = graph.data[graph.data$Figure_number=="Figure C1", Figure_number],
       subtitle = graph.data[graph.data$Figure_number=="Figure C1", Figure_title]) +
  scale_x_continuous(breaks = scales::breaks_pretty(), labels = function(x) round(x,2),
                     limits = c( min(figure_C1_data$tf), max(figure_C1_data$tf)),
                     expand = c(0, 0))+
  scale_y_continuous(limits = c(0, max(figure_C1_data$tf_adjusted)),
                          expand = c(0, 0))+
  scale_color_manual(values = c( "#eb0072","black", "#0077c8","#ffc800"),
                     labels = c(graph.data[graph.data$Figure_number=="Figure C1",Legend_label_1],
                                graph.data[graph.data$Figure_number=="Figure C1",Legend_label_2],
                                graph.data[graph.data$Figure_number=="Figure C1",Legend_label_3],
                                graph.data[graph.data$Figure_number=="Figure C1",Legend_label_4])) +
  coord_cartesian(xlim = c( min(figure_C1_data$tf)-0.0001, max(figure_C1_data$tf)+0.005),
                  ylim = c(0, max(figure_C1_data$tf_adjusted+0.005))) + 
  labs(color = graph.data[graph.data$Figure_number=="Figure C1", Legend_name])+
  theme(legend.position = 'right',
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        plot.title = element_text(size = 22), 
        plot.subtitle = element_text(size = 20), 
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        legend.text = element_text(size = 14),  
        legend.title = element_text(size = 16), 
        legend.key.width = unit(2, "cm"), 
        legend.key.height = unit(1, "cm"))+
  guides(color = guide_legend(ncol=1))

figure_2_data  <- fread("Figure_2.csv")
figure_D1_data <- figure_2_data
setorder(figure_D1_data, Quadrant, month)

figure_D1_data[, value_lag := shift(N, type = "lag"), by = Quadrant]

figure_D1_data[, rate_of_change := (N - value_lag) / value_lag ]

fig_D1<-ggplot(figure_D1_data, aes(x = month, y = rate_of_change, color = Quadrant, group = Quadrant)) +
  geom_line() +
  labs(title = graph.data[graph.data$Figure_number=="Figure D1", Figure_number],
       subtitle = graph.data[graph.data$Figure_number=="Figure D1",Figure_title],
       caption = graph.data[graph.data$Figure_number=="Figure D1", Caption],
       x = graph.data[graph.data$Figure_number=="Figure D1",X_Axis],
       y = graph.data[graph.data$Figure_number=="Figure D1",Y_Axis],
       color = graph.data[graph.data$Figure_number=="Figure D1",Legend_name]) +
  scale_color_manual(values = c("black", "#eb0072", "#0077c8"),
                     labels = c(graph.data[graph.data$Figure_number=="Figure D1",Legend_label_1],
                                graph.data[graph.data$Figure_number=="Figure D1",Legend_label_2],
                                graph.data[graph.data$Figure_number=="Figure D1",Legend_label_3])) +
  scale_y_continuous(labels = percent_format()) + 
  scale_x_date(limits = c(min(figure_D1_data$month), max(figure_D1_data$month)),
               expand = c(0, 0)) +
  coord_cartesian(xlim = c(min(figure_D1_data$month), max(figure_D1_data$month)+62)) +  # Extend right side
  # Add vertical lines for April 2022 and November 2022
  geom_vline(xintercept = as.Date("2020-03-01"), linetype = "solid", color = "black") +
  geom_vline(xintercept = as.Date("2022-04-01"), linetype = "solid", color = "black") +
  geom_vline(xintercept = as.Date("2022-11-01"), linetype = "solid", color = "red") +
  # Add annotations for the vertical lines
  annotate("text", x = as.Date("2020-03-01"), y = Inf, label = graph.data[graph.data$Figure_number=="Figure D1",Graph_label_1], vjust = 1, hjust= 1.1, color = "black", family = "sans") +
  annotate("text", x = as.Date("2022-04-01"), y = Inf, label = graph.data[graph.data$Figure_number=="Figure D1",Graph_label_2], vjust = 1, hjust= 1.1, color = "black", family = "sans") +
  annotate("text", x = as.Date("2022-11-01"), y = Inf, label = graph.data[graph.data$Figure_number=="Figure D1",Graph_label_3], vjust = 1, hjust= -0.1, color = "red", family = "sans") +
  dais.base.theme()+
  theme(axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
        legend.text = element_text(size = 14),  
        legend.title = element_text(size = 16), 
        legend.key.width = unit(2, "cm"), 
        legend.key.height = unit(1, "cm"),
        plot.title = element_text(size = 22), 
        plot.subtitle = element_text(size = 20), 
        axis.title.x = element_text(size = 18,margin = margin(r = 0, t = 10, b = 0, l = 0)),
        axis.title.y = element_text(size = 18, margin = margin(r = 10, t = 0, b = 0, l = 0)),
        plot.margin = margin(10, 10, 15, 10, "pt"),
        axis.text.y = element_text(size = 14))

figure_4_data  <- fread("Figure_4.csv")
figure_D2_data <- figure_4_data
setorder(figure_D2_data, Quadrant, month)

figure_D2_data[, value_lag := shift(avg_remuneration, type = "lag"), by = Quadrant]

figure_D2_data[, rate_of_change := (avg_remuneration - value_lag) / value_lag]

fig_D2<-ggplot(figure_D2_data, aes(x = month, y = rate_of_change, color = Quadrant, group = Quadrant)) +
  geom_line(size = 1) +
  labs(title = graph.data[graph.data$Figure_number=="Figure D2", Figure_number],
       subtitle = graph.data[graph.data$Figure_number=="Figure D2",Figure_title],
       caption = graph.data[graph.data$Figure_number=="Figure D2", Caption],
       x = graph.data[graph.data$Figure_number=="Figure D2",X_Axis],
       y = graph.data[graph.data$Figure_number=="Figure D2",Y_Axis],
       color = graph.data[graph.data$Figure_number=="Figure D2",Legend_name]) +
  scale_color_manual(values = c("black", "#eb0072", "#0077c8"),
                     labels = c(graph.data[graph.data$Figure_number=="Figure D2",Legend_label_1],
                                graph.data[graph.data$Figure_number=="Figure D2",Legend_label_2],
                                graph.data[graph.data$Figure_number=="Figure D2",Legend_label_3])) +
  scale_x_date(limits = c(min(figure_D2_data$month), max(figure_D2_data$month)),
               expand = c(0, 0)) +
  coord_cartesian(xlim = c(min(figure_D2_data$month), max(figure_D2_data$month)+62)) +  # Extend right side
  scale_y_continuous(labels = percent_format()) + 
  # Add vertical lines for April 2022 and November 2022
  geom_vline(xintercept = as.Date("2020-03-01"), linetype = "solid", color = "black") +
  geom_vline(xintercept = as.Date("2022-04-01"), linetype = "solid", color = "black") +
  geom_vline(xintercept = as.Date("2022-11-01"), linetype = "solid", color = "red") +
  # Add annotations for the vertical lines
  annotate("text", x = as.Date("2020-03-01"), y = Inf, label = graph.data[graph.data$Figure_number=="Figure D2",Graph_label_1], vjust = 1, hjust= 1.1, color = "black", family = "sans") +
  annotate("text", x = as.Date("2022-04-01"), y = Inf, label = graph.data[graph.data$Figure_number=="Figure D2",Graph_label_2], vjust = 1, hjust= 1.1, color = "black", family = "sans") +
  annotate("text", x = as.Date("2022-11-01"), y = Inf, label = graph.data[graph.data$Figure_number=="Figure D2",Graph_label_3], vjust = 1, hjust= -0.1, color = "red", family = "sans") +
  dais.base.theme()+
  theme(axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
        legend.text = element_text(size = 14),  
        legend.title = element_text(size = 16), 
        legend.key.width = unit(2, "cm"), 
        legend.key.height = unit(1, "cm"),
        plot.title = element_text(size = 22), 
        plot.subtitle = element_text(size = 20), 
        axis.title.x = element_text(size = 18,margin = margin(r = 0, t = 10, b = 0, l = 0)),
        axis.title.y = element_text(size = 18, margin = margin(r = 10, t = 0, b = 0, l = 0)),
        plot.margin = margin(10, 10, 15, 10, "pt"),
        axis.text.y = element_text(size = 14)) 

export.dais.plot("Graph_exports_EN/Figure_1.pdf",fig_1,p.height = 12, p.width = 18)
export.dais.plot("Graph_exports_EN/Figure_2.pdf",fig_2,p.height = 12, p.width = 18)
export.dais.plot("Graph_exports_EN/Figure_3.pdf",fig_3,p.height = 12, p.width = 18)
export.dais.plot("Graph_exports_EN/Figure_4.pdf",fig_4,p.height = 12, p.width = 18)
export.dais.plot("Graph_exports_EN/Figure_5.pdf",fig_5,p.height = 12, p.width = 18)
export.dais.plot("Graph_exports_EN/Figure_6.pdf",fig_6,p.height = 12, p.width = 18)
export.dais.plot("Graph_exports_EN/Figure_7.pdf",fig_7,p.height = 12, p.width = 18)
export.dais.plot("Graph_exports_EN/Figure_8.pdf",fig_8,p.height = 12, p.width = 18)
export.dais.plot("Graph_exports_EN/Figure_9.pdf",fig_9,p.height = 12, p.width = 18)
export.dais.plot("Graph_exports_EN/Figure_10.pdf",fig_10,p.height = 12, p.width = 18)
export.dais.plot("Graph_exports_EN/Figure_B1.pdf",fig_B1,p.height = 12, p.width = 18)
export.dais.plot("Graph_exports_EN/Figure_B2.pdf",fig_B2,p.height = 12, p.width = 18)
export.dais.plot("Graph_exports_EN/Figure_B3.pdf",fig_B3,p.height = 12, p.width = 18)
export.dais.plot("Graph_exports_EN/Figure_C1.pdf",fig_C1,p.height = 12, p.width = 18)
export.dais.plot("Graph_exports_EN/Figure_C1.pdf",fig_C1,p.height = 12, p.width = 18)
export.dais.plot("Graph_exports_EN/Figure_D1.pdf",fig_D1,p.height = 12, p.width = 18)