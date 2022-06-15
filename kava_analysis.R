library(readxl)
Kava_Data <- read_excel("Kava Data.xlsx")

Kava_Data = Kava_Data[!is.na(Kava_Data$Focus), ]

Kava_Data_Active = Kava_Data[Kava_Data$`C/A`=="Active", ]
Kava_Data_Control = Kava_Data[Kava_Data$`C/A`=="Control", ]

Kava_Data_Active_Test_1 = Kava_Data_Active[Kava_Data_Active$Test=="1", ]
Kava_Data_Active_Test_2 = Kava_Data_Active[Kava_Data_Active$Test=="2", ]
Kava_Data_Active_Test_3 = Kava_Data_Active[Kava_Data_Active$Test=="3", ]

Kava_Data_Control_Test_1 = Kava_Data_Control[Kava_Data_Control$Test=="1", ]
Kava_Data_Control_Test_2 = Kava_Data_Control[Kava_Data_Control$Test=="2", ]
Kava_Data_Control_Test_3 = Kava_Data_Control[Kava_Data_Control$Test=="3", ]

################################################################################

Kava_Data_Active_1_v_3_Focus = t.test(Kava_Data_Active_Test_1$Focus, Kava_Data_Active_Test_3$Focus, paired = TRUE)
Kava_Data_Active_1_v_3_Accuracy = t.test(Kava_Data_Active_Test_1$Accuracy, Kava_Data_Active_Test_3$Accuracy, paired = TRUE)
Kava_Data_Active_1_v_3_Toj = t.test(Kava_Data_Active_Test_1$TOJ, Kava_Data_Active_Test_3$TOJ, paired = TRUE)
Kava_Data_Active_1_v_3_Time_Percept = t.test(Kava_Data_Active_Test_1$`Time Percept`, Kava_Data_Active_Test_3$`Time Percept`, paired = TRUE)
Kava_Data_Active_1_v_3_Plasticity = t.test(Kava_Data_Active_Test_1$Plasticity, Kava_Data_Active_Test_3$Plasticity, paired = TRUE)
Kava_Data_Active_1_v_3_Fatigue = t.test(Kava_Data_Active_Test_1$Fatigue, Kava_Data_Active_Test_3$Fatigue, paired = TRUE)
Kava_Data_Active_1_v_3_Overall = t.test(Kava_Data_Active_Test_1$OVERALL, Kava_Data_Active_Test_3$OVERALL, paired = TRUE)

Kava_Data_Control_1_v_3_Focus = t.test(Kava_Data_Control_Test_1$Focus, Kava_Data_Control_Test_3$Focus, paired = TRUE)
Kava_Data_Control_1_v_3_Accuracy = t.test(Kava_Data_Control_Test_1$Accuracy, Kava_Data_Control_Test_3$Accuracy, paired = TRUE)
Kava_Data_Control_1_v_3_Toj = t.test(Kava_Data_Control_Test_1$TOJ, Kava_Data_Control_Test_3$TOJ, paired = TRUE)
Kava_Data_Control_1_v_3_Time_Percept = t.test(Kava_Data_Control_Test_1$`Time Percept`, Kava_Data_Control_Test_3$`Time Percept`, paired = TRUE)
Kava_Data_Control_1_v_3_Plasticity = t.test(Kava_Data_Control_Test_1$Plasticity, Kava_Data_Control_Test_3$Plasticity, paired = TRUE)
Kava_Data_Control_1_v_3_Fatigue = t.test(Kava_Data_Control_Test_1$Fatigue, Kava_Data_Control_Test_3$Fatigue, paired = TRUE)
Kava_Data_Control_1_v_3_Overall = t.test(Kava_Data_Control_Test_1$OVERALL, Kava_Data_Control_Test_3$OVERALL, paired = TRUE)

Kava_Data_Control_v_Active_1_Focus = t.test(Kava_Data_Control_Test_1$Focus, Kava_Data_Active_Test_1$Focus)
Kava_Data_Control_v_Active_1_Accuracy = t.test(Kava_Data_Control_Test_1$Accuracy, Kava_Data_Active_Test_1$Accuracy)
Kava_Data_Control_v_Active_1_Toj = t.test(Kava_Data_Control_Test_1$TOJ, Kava_Data_Active_Test_1$TOJ)
Kava_Data_Control_v_Active_1_Time_Percept = t.test(Kava_Data_Control_Test_1$`Time Percept`, Kava_Data_Active_Test_1$`Time Percept`)
Kava_Data_Control_v_Active_1_Plasticity = t.test(Kava_Data_Control_Test_1$Plasticity, Kava_Data_Active_Test_1$Plasticity)
Kava_Data_Control_v_Active_1_Fatigue = t.test(Kava_Data_Control_Test_1$Fatigue, Kava_Data_Active_Test_1$Fatigue)
Kava_Data_Control_v_Active_1_Overall = t.test(Kava_Data_Control_Test_1$OVERALL, Kava_Data_Active_Test_1$OVERALL)

################################################################################

library(ggplot2)

Control_1_Focus_Boxplot = ggplot(Kava_Data_Control_Test_1, aes(x=Test, y=Focus)) + 
  geom_boxplot() + labs(title = "Focus for control group at test 1") + theme(plot.title = element_text(hjust = 0.5))
Control_1_Accuracy_Boxplot = ggplot(Kava_Data_Control_Test_1, aes(x=Test, y=Accuracy)) + 
  geom_boxplot() + labs(title = "Accuracy for control group at test 1") + theme(plot.title = element_text(hjust = 0.5))
Control_1_Toj_Boxplot = ggplot(Kava_Data_Control_Test_1, aes(x=Test, y=TOJ)) + 
  geom_boxplot() + labs(title = "Temporal order judgement for control group at test 1") + theme(plot.title = element_text(hjust = 0.5))
Control_1_Time_Percept_Boxplot = ggplot(Kava_Data_Control_Test_1, aes(x=Test, y=`Time Percept`)) + 
  geom_boxplot() + labs(title = "Time perception for control group at test 1") + theme(plot.title = element_text(hjust = 0.5))
Control_1_Plasticity_Boxplot = ggplot(Kava_Data_Control_Test_1, aes(x=Test, y=Plasticity)) + 
  geom_boxplot() + labs(title = "Plasticity for control group at test 1") + theme(plot.title = element_text(hjust = 0.5))
Control_1_Fatigue_Boxplot = ggplot(Kava_Data_Control_Test_1, aes(x=Test, y=Fatigue)) + 
  geom_boxplot() + labs(title = "Fatigue for control group at test 1") + theme(plot.title = element_text(hjust = 0.5))
Control_1_Overall_Boxplot = ggplot(Kava_Data_Control_Test_1, aes(x=Test, y=OVERALL)) + 
  geom_boxplot() + labs(title = "Overall for control group at test 1") + theme(plot.title = element_text(hjust = 0.5))


Control_2_Focus_Boxplot = ggplot(Kava_Data_Control_Test_2, aes(x=Test, y=Focus)) + 
  geom_boxplot() + labs(title = "Focus for control group at test 2") + theme(plot.title = element_text(hjust = 0.5))
Control_2_Accuracy_Boxplot = ggplot(Kava_Data_Control_Test_2, aes(x=Test, y=Accuracy)) + 
  geom_boxplot() + labs(title = "Accuracy for control group at test 2") + theme(plot.title = element_text(hjust = 0.5))
Control_2_Toj_Boxplot = ggplot(Kava_Data_Control_Test_2, aes(x=Test, y=TOJ)) + 
  geom_boxplot() + labs(title = "Temporal order judgement for control group at test 2") + theme(plot.title = element_text(hjust = 0.5))
Control_2_Time_Percept_Boxplot = ggplot(Kava_Data_Control_Test_2, aes(x=Test, y=`Time Percept`)) + 
  geom_boxplot() + labs(title = "Time perception for control group at test 2") + theme(plot.title = element_text(hjust = 0.5))
Control_2_Plasticity_Boxplot = ggplot(Kava_Data_Control_Test_2, aes(x=Test, y=Plasticity)) + 
  geom_boxplot() + labs(title = "Plasticity for control group at test 2") + theme(plot.title = element_text(hjust = 0.5))
Control_2_Fatigue_Boxplot = ggplot(Kava_Data_Control_Test_2, aes(x=Test, y=Fatigue)) + 
  geom_boxplot() + labs(title = "Fatigue for control group at test 2") + theme(plot.title = element_text(hjust = 0.5))
Control_2_Overall_Boxplot = ggplot(Kava_Data_Control_Test_2, aes(x=Test, y=OVERALL)) + 
  geom_boxplot() + labs(title = "Overall for control group at test 2") + theme(plot.title = element_text(hjust = 0.5))



Control_3_Focus_Boxplot = ggplot(Kava_Data_Control_Test_3, aes(x=Test, y=Focus)) + 
  geom_boxplot() + labs(title = "Focus for control group at test 3") + theme(plot.title = element_text(hjust = 0.5))
Control_3_Accuracy_Boxplot = ggplot(Kava_Data_Control_Test_3, aes(x=Test, y=Accuracy)) + 
  geom_boxplot() + labs(title = "Accuracy for control group at test 3") + theme(plot.title = element_text(hjust = 0.5))
Control_3_Toj_Boxplot = ggplot(Kava_Data_Control_Test_3, aes(x=Test, y=TOJ)) + 
  geom_boxplot() + labs(title = "Temporal order judgement for control group at test 3") + theme(plot.title = element_text(hjust = 0.5))
Control_3_Time_Percept_Boxplot = ggplot(Kava_Data_Control_Test_3, aes(x=Test, y=`Time Percept`)) + 
  geom_boxplot() + labs(title = "Time perception for control group at test 3") + theme(plot.title = element_text(hjust = 0.5))
Control_3_Plasticity_Boxplot = ggplot(Kava_Data_Control_Test_3, aes(x=Test, y=Plasticity)) + 
  geom_boxplot() + labs(title = "Plasticity for control group at test 3") + theme(plot.title = element_text(hjust = 0.5))
Control_3_Fatigue_Boxplot = ggplot(Kava_Data_Control_Test_3, aes(x=Test, y=Fatigue)) + 
  geom_boxplot() + labs(title = "Fatigue for control group at test 3") + theme(plot.title = element_text(hjust = 0.5))
Control_3_Overall_Boxplot = ggplot(Kava_Data_Control_Test_3, aes(x=Test, y=OVERALL)) + 
  geom_boxplot() + labs(title = "Overall for control group at test 3") + theme(plot.title = element_text(hjust = 0.5))



Active_1_Focus_Boxplot = ggplot(Kava_Data_Active_Test_1, aes(x=Test, y=Focus)) + 
  geom_boxplot() + labs(title = "Focus for active group at test 1") + theme(plot.title = element_text(hjust = 0.5))
Active_1_Accuracy_Boxplot = ggplot(Kava_Data_Active_Test_1, aes(x=Test, y=Accuracy)) + 
  geom_boxplot() + labs(title = "Accuracy for active group at test 1") + theme(plot.title = element_text(hjust = 0.5))
Active_1_Toj_Boxplot = ggplot(Kava_Data_Active_Test_1, aes(x=Test, y=TOJ)) + 
  geom_boxplot() + labs(title = "Temporal order judgement for active group at test 1") + theme(plot.title = element_text(hjust = 0.5))
Active_1_Time_Percept_Boxplot = ggplot(Kava_Data_Active_Test_1, aes(x=Test, y=`Time Percept`)) + 
  geom_boxplot() + labs(title = "Time perception for active group at test 1") + theme(plot.title = element_text(hjust = 0.5))
Active_1_Plasticity_Boxplot = ggplot(Kava_Data_Active_Test_1, aes(x=Test, y=Plasticity)) + 
  geom_boxplot() + labs(title = "Plasticity for active group at test 1") + theme(plot.title = element_text(hjust = 0.5))
Active_1_Fatigue_Boxplot = ggplot(Kava_Data_Active_Test_1, aes(x=Test, y=Fatigue)) + 
  geom_boxplot() + labs(title = "Fatigue for active group at test 1") + theme(plot.title = element_text(hjust = 0.5))
Active_1_Overall_Boxplot = ggplot(Kava_Data_Active_Test_1, aes(x=Test, y=OVERALL)) + 
  geom_boxplot() + labs(title = "Overall for active group at test 1") + theme(plot.title = element_text(hjust = 0.5))


Active_2_Focus_Boxplot = ggplot(Kava_Data_Active_Test_2, aes(x=Test, y=Focus)) + 
  geom_boxplot() + labs(title = "Focus for active group at test 2") + theme(plot.title = element_text(hjust = 0.5))
Active_2_Accuracy_Boxplot = ggplot(Kava_Data_Active_Test_2, aes(x=Test, y=Accuracy)) + 
  geom_boxplot() + labs(title = "Accuracy for active group at test 2") + theme(plot.title = element_text(hjust = 0.5))
Active_2_Toj_Boxplot = ggplot(Kava_Data_Active_Test_2, aes(x=Test, y=TOJ)) + 
  geom_boxplot() + labs(title = "Temporal order judgement for active group at test 2") + theme(plot.title = element_text(hjust = 0.5))
Active_2_Time_Percept_Boxplot = ggplot(Kava_Data_Active_Test_2, aes(x=Test, y=`Time Percept`)) + 
  geom_boxplot() + labs(title = "Time perception for active group at test 2") + theme(plot.title = element_text(hjust = 0.5))
Active_2_Plasticity_Boxplot = ggplot(Kava_Data_Active_Test_2, aes(x=Test, y=Plasticity)) + 
  geom_boxplot() + labs(title = "Plasticity for active group at test 2") + theme(plot.title = element_text(hjust = 0.5))
Active_2_Fatigue_Boxplot = ggplot(Kava_Data_Active_Test_2, aes(x=Test, y=Fatigue)) + 
  geom_boxplot() + labs(title = "Fatigue for active group at test 2") + theme(plot.title = element_text(hjust = 0.5))
Active_2_Overall_Boxplot = ggplot(Kava_Data_Active_Test_2, aes(x=Test, y=OVERALL)) + 
  geom_boxplot() + labs(title = "Overall for active group at test 2") + theme(plot.title = element_text(hjust = 0.5))


Active_3_Focus_Boxplot = ggplot(Kava_Data_Active_Test_3, aes(x=Test, y=Focus)) + 
  geom_boxplot() + labs(title = "Focus for active group at test 3") + theme(plot.title = element_text(hjust = 0.5))
Active_3_Accuracy_Boxplot = ggplot(Kava_Data_Active_Test_3, aes(x=Test, y=Accuracy)) + 
  geom_boxplot() + labs(title = "Accuracy for active group at test 3") + theme(plot.title = element_text(hjust = 0.5))
Active_3_Toj_Boxplot = ggplot(Kava_Data_Active_Test_3, aes(x=Test, y=TOJ)) + 
  geom_boxplot() + labs(title = "Temporal order judgement for active group at test 3") + theme(plot.title = element_text(hjust = 0.5))
Active_3_Time_Percept_Boxplot = ggplot(Kava_Data_Active_Test_3, aes(x=Test, y=`Time Percept`)) + 
  geom_boxplot() + labs(title = "Time perception for active group at test 3") + theme(plot.title = element_text(hjust = 0.5))
Active_3_Plasticity_Boxplot = ggplot(Kava_Data_Active_Test_3, aes(x=Test, y=Plasticity)) + 
  geom_boxplot() + labs(title = "Plasticity for active group at test 3") + theme(plot.title = element_text(hjust = 0.5))
Active_3_Fatigue_Boxplot = ggplot(Kava_Data_Active_Test_3, aes(x=Test, y=Fatigue)) + 
  geom_boxplot() + labs(title = "Fatigue for active group at test 3") + theme(plot.title = element_text(hjust = 0.5))
Active_3_Overall_Boxplot = ggplot(Kava_Data_Active_Test_3, aes(x=Test, y=OVERALL)) + 
  geom_boxplot() + labs(title = "Overall for active group at test 3") + theme(plot.title = element_text(hjust = 0.5))


################################################################################

Control_1_Focus_Boxplot
Control_1_Accuracy_Boxplot
Control_1_Toj_Boxplot
Control_1_Time_Percept_Boxplot
Control_1_Plasticity_Boxplot
Control_1_Fatigue_Boxplot
Control_1_Overall_Boxplot

Control_2_Focus_Boxplot
Control_2_Accuracy_Boxplot
Control_2_Toj_Boxplot
Control_2_Time_Percept_Boxplot
Control_2_Plasticity_Boxplot
Control_2_Fatigue_Boxplot
Control_2_Overall_Boxplot

Control_3_Focus_Boxplot
Control_3_Accuracy_Boxplot
Control_3_Toj_Boxplot
Control_3_Time_Percept_Boxplot
Control_3_Plasticity_Boxplot
Control_3_Fatigue_Boxplot
Control_3_Overall_Boxplot


Active_1_Focus_Boxplot
Active_1_Accuracy_Boxplot
Active_1_Toj_Boxplot
Active_1_Time_Percept_Boxplot
Active_1_Plasticity_Boxplot
Active_1_Fatigue_Boxplot
Active_1_Overall_Boxplot

Active_2_Focus_Boxplot
Active_2_Accuracy_Boxplot
Active_2_Toj_Boxplot
Active_2_Time_Percept_Boxplot
Active_2_Plasticity_Boxplot
Active_2_Fatigue_Boxplot
Active_2_Overall_Boxplot

Active_3_Focus_Boxplot
Active_3_Accuracy_Boxplot
Active_3_Toj_Boxplot
Active_3_Time_Percept_Boxplot
Active_3_Plasticity_Boxplot
Active_3_Fatigue_Boxplot
Active_3_Overall_Boxplot

################################################################################

library(gridExtra)
library(ggpubr)

Focus_Boxplot_Grid = grid.arrange(Control_1_Focus_Boxplot, Active_1_Focus_Boxplot, Control_2_Focus_Boxplot,
                                  Active_2_Focus_Boxplot, Control_3_Focus_Boxplot, Active_3_Focus_Boxplot, nrow = 3,
                                  top = text_grob("Focus", size = 20, face = "bold"))

Accuracy_Boxplot_Grid = grid.arrange(Control_1_Accuracy_Boxplot, Active_1_Accuracy_Boxplot, Control_2_Accuracy_Boxplot,
                                  Active_2_Accuracy_Boxplot, Control_3_Accuracy_Boxplot, Active_3_Accuracy_Boxplot, nrow = 3,
                                  top = text_grob("Accuracy", size = 20, face = "bold"))

Toj_Boxplot_Grid = grid.arrange(Control_1_Toj_Boxplot, Active_1_Toj_Boxplot, Control_2_Toj_Boxplot,
                                  Active_2_Toj_Boxplot, Control_3_Toj_Boxplot, Active_3_Toj_Boxplot, nrow = 3,
                                top = text_grob("Temporal Order Judgment", size = 20, face = "bold"))

Time_Percept_Boxplot_Grid = grid.arrange(Control_1_Time_Percept_Boxplot, Active_1_Time_Percept_Boxplot, Control_2_Time_Percept_Boxplot,
                                  Active_2_Time_Percept_Boxplot, Control_3_Time_Percept_Boxplot, Active_3_Time_Percept_Boxplot, nrow = 3,
                                  top = text_grob("Time Perception", size = 20, face = "bold"))

Plasticity_Boxplot_Grid = grid.arrange(Control_1_Plasticity_Boxplot, Active_1_Plasticity_Boxplot, Control_2_Plasticity_Boxplot,
                                  Active_2_Plasticity_Boxplot, Control_3_Plasticity_Boxplot, Active_3_Plasticity_Boxplot, nrow = 3,
                                  top = text_grob("Plasticity", size = 20, face = "bold"))

Fatigue_Boxplot_Grid = grid.arrange(Control_1_Fatigue_Boxplot, Active_1_Fatigue_Boxplot, Control_2_Fatigue_Boxplot,
                                       Active_2_Fatigue_Boxplot, Control_3_Fatigue_Boxplot, Active_3_Fatigue_Boxplot, nrow = 3,
                                    top = text_grob("Fatigue", size = 20, face = "bold"))

Overall_Boxplot_Grid = grid.arrange(Control_1_Overall_Boxplot, Active_1_Overall_Boxplot, Control_2_Overall_Boxplot,
                                       Active_2_Overall_Boxplot, Control_3_Overall_Boxplot, Active_3_Overall_Boxplot, nrow = 3,
                                    top = text_grob("Overall", size = 20, face = "bold"))

################################################################################

Control_1_TOJ_Violin = ggplot(Kava_Data_Control_Test_1, aes(x=Test, y=TOJ)) + 
  geom_violin() + labs(title = "Temporal Order Judgment for control group at test 1") + theme(plot.title = element_text(hjust = 0.5))

Control_2_Focus_Boxplot = ggplot(Kava_Data_Control_Test_2, aes(x=Test, y=TOJ)) + 
  geom_violin() + labs(title = "Temporal Order Judgment for control group at test 2") + theme(plot.title = element_text(hjust = 0.5))

Control_3_Focus_Boxplot = ggplot(Kava_Data_Control_Test_3, aes(x=Test, y=TOJ)) + 
  geom_violin() + labs(title = "Temporal Order Judgment for control group at test 3") + theme(plot.title = element_text(hjust = 0.5))


Active_1_Focus_Boxplot = ggplot(Kava_Data_Active_Test_1, aes(x=Test, y=TOJ)) + 
  geom_violin() + labs(title = "Temporal Order Judgment for active group at test 1") + theme(plot.title = element_text(hjust = 0.5))

Active_2_Focus_Boxplot = ggplot(Kava_Data_Active_Test_2, aes(x=Test, y=TOJ)) + 
  geom_violin() + labs(title = "Temporal Order Judgment for active group at test 2") + theme(plot.title = element_text(hjust = 0.5))

Active_3_Focus_Boxplot = ggplot(Kava_Data_Active_Test_3, aes(x=Test, y=TOJ)) + 
  geom_violin() + labs(title = "Temporal Order Judgment for active group at test 3") + theme(plot.title = element_text(hjust = 0.5))


TOJ_Violin_Grid = grid.arrange(Control_1_TOJ_Violin, Active_1_Focus_Boxplot, Control_2_Focus_Boxplot,
                               Active_2_Focus_Boxplot, Control_3_Focus_Boxplot, Active_3_Focus_Boxplot, nrow = 3,
                               top = text_grob("Overall", size = 20, face = "bold"))

################################################################################

stacked_plot_active = ggplot(Kava_Data_Active, aes(x=Test, y=TOJ)) + geom_violin(trim = FALSE, draw_quantiles = c(0.25, 0.5, 0.75)) + 
  coord_flip() +
  scale_y_continuous(limits = c(0, 100)) + 
  labs(
    title = "Active",
    y = "TOJ Score"
  )

stacked_plot_control = ggplot(Kava_Data_Control, aes(x=Test, y=TOJ)) + geom_violin(trim = FALSE, draw_quantiles = c(0.25, 0.5, 0.75)) + 
  coord_flip() +
  scale_y_continuous(limits = c(0, 100)) +
  labs(
    title = "Control",
    y = "TOJ Score"
  )

grid.arrange(stacked_plot_active, stacked_plot_control)

stacked_plot_active2 = ggplot(Kava_Data_Active, aes(x=Test, y=TOJ)) + geom_violin(trim = FALSE, draw_quantiles = c(0.25, 0.5, 0.75)) + 
  coord_flip() +
  scale_y_continuous(limits = c(0, 100)) + 
  scale_x_discrete(limits = rev) +
  labs(
    title = "Active",
    y = "TOJ Score"
  )

stacked_plot_control2 = ggplot(Kava_Data_Control, aes(x=Test, y=TOJ)) + geom_violin(trim = FALSE, draw_quantiles = c(0.25, 0.5, 0.75)) + 
  coord_flip() +
  scale_y_continuous(limits = c(0, 100)) +
  scale_x_discrete(limits = rev) +
  labs(
    title = "Control",
    y = "TOJ Score"
  )

grid.arrange(stacked_plot_active2, stacked_plot_control2)

GeomSplitViolin <- ggproto("GeomSplitViolin", GeomViolin,
                           draw_group = function(self, data, ..., draw_quantiles = NULL) {
                             # Original function by Jan Gleixner (@jan-glx)
                             # Adjustments by Wouter van der Bijl (@Axeman)
                             data <- transform(data, xminv = x - violinwidth * (x - xmin), xmaxv = x + violinwidth * (xmax - x))
                             grp <- data[1, "group"]
                             newdata <- plyr::arrange(transform(data, x = if (grp %% 2 == 1) xminv else xmaxv), if (grp %% 2 == 1) y else -y)
                             newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
                             newdata[c(1, nrow(newdata) - 1, nrow(newdata)), "x"] <- round(newdata[1, "x"])
                             if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
                               stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <= 1))
                               quantiles <- create_quantile_segment_frame(data, draw_quantiles, split = TRUE, grp = grp)
                               aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
                               aesthetics$alpha <- rep(1, nrow(quantiles))
                               both <- cbind(quantiles, aesthetics)
                               quantile_grob <- GeomPath$draw_panel(both, ...)
                               ggplot2:::ggname("geom_split_violin", grid::grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
                             }
                             else {
                               ggplot2:::ggname("geom_split_violin", GeomPolygon$draw_panel(newdata, ...))
                             }
                           }
)

create_quantile_segment_frame <- function(data, draw_quantiles, split = FALSE, grp = NULL) {
  dens <- cumsum(data$density) / sum(data$density)
  ecdf <- stats::approxfun(dens, data$y)
  ys <- ecdf(draw_quantiles)
  violin.xminvs <- (stats::approxfun(data$y, data$xminv))(ys)
  violin.xmaxvs <- (stats::approxfun(data$y, data$xmaxv))(ys)
  violin.xs <- (stats::approxfun(data$y, data$x))(ys)
  if (grp %% 2 == 0) {
    data.frame(
      x = ggplot2:::interleave(violin.xs, violin.xmaxvs),
      y = rep(ys, each = 2), group = rep(ys, each = 2)
    )
  } else {
    data.frame(
      x = ggplot2:::interleave(violin.xminvs, violin.xs),
      y = rep(ys, each = 2), group = rep(ys, each = 2)
    )
  }
}

geom_split_violin <- function(mapping = NULL, data = NULL, stat = "ydensity", position = "identity", ..., 
                              draw_quantiles = NULL, trim = TRUE, scale = "area", na.rm = FALSE, 
                              show.legend = NA, inherit.aes = TRUE) {
  layer(data = data, mapping = mapping, stat = stat, geom = GeomSplitViolin, position = position, 
        show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(trim = trim, scale = scale, draw_quantiles = draw_quantiles, na.rm = na.rm, ...))
}

split_toj_plot = ggplot(Kava_Data, aes(x=Test, y=TOJ, fill=`C/A`)) + 
  geom_split_violin(draw_quantiles = c(0.25, 0.5, 0.75)) + labs(y="Temporal order judgement score")

split_focus_plot = ggplot(Kava_Data, aes(x=Test, y=Focus, fill=`C/A`)) +
  geom_split_violin(draw_quantiles = c(0.25, 0.5, 0.75)) + labs(y="Focus score")
split_accuracy_plot = ggplot(Kava_Data, aes(x=Test, y=Accuracy, fill=`C/A`)) +
  geom_split_violin(draw_quantiles = c(0.25, 0.5, 0.75)) + labs(y="Accuracy score")
split_timeper_plot = ggplot(Kava_Data, aes(x=Test, y=`Time Percept`, fill=`C/A`)) +
  geom_split_violin( draw_quantiles = c(0.25, 0.5, 0.75)) + labs(y="Time perception score")
split_plasticity_plot = ggplot(Kava_Data, aes(x=Test, y=Plasticity, fill=`C/A`)) +
  geom_split_violin(draw_quantiles = c(0.25, 0.5, 0.75)) + labs(y="Plasticity score")
split_fatigue_plot = ggplot(Kava_Data, aes(x=Test, y=Fatigue, fill=`C/A`)) + 
  geom_split_violin(draw_quantiles = c(0.25, 0.5, 0.75)) + labs(y="Fatigue score")

ggarrange(split_accuracy_plot, split_fatigue_plot, split_focus_plot, split_plasticity_plot, split_toj_plot, split_timeper_plot,
          common.legend = TRUE, legend = "bottom")

################################################################################

grouped_focus_plot = ggplot(data = Kava_Data, aes(x = Test, y = Focus, fill = `C/A`)) + 
                      geom_boxplot(show.legend = FALSE) +
                      #scale_y_log10() +
                      scale_x_discrete(drop = FALSE, name = 'Test') +
                      labs(y="Focus score")

grouped_accuracy_plot = ggplot(data = Kava_Data, aes(x = Test, y = Accuracy, fill = `C/A`)) + 
  geom_boxplot() +
  #scale_y_log10() +
  scale_x_discrete(drop = FALSE, name = 'Test') +
  labs(y="Accuracy score")

grouped_timeper_plot = ggplot(data = Kava_Data, aes(x = Test, y = `Time Percept`, fill = `C/A`)) + 
  geom_boxplot() +
  #scale_y_log10() +
  scale_x_discrete(drop = FALSE, name = 'Test') +
  labs(y="Time perception score")

grouped_plasticity_plot = ggplot(data = Kava_Data, aes(x = Test, y = Plasticity, fill = `C/A`)) + 
  geom_boxplot() +
  #scale_y_log10() +
  scale_x_discrete(drop = FALSE, name = 'Test') +
  labs(y="Plasticity score")

grouped_fatigue_plot = ggplot(data = Kava_Data, aes(x = Test, y = Fatigue, fill = `C/A`)) + 
  geom_boxplot() +
  #scale_y_log10() +
  scale_x_discrete(drop = FALSE, name = 'Test') +
  labs(y="Fatigue score")

grouped_toj_plot = ggplot(data = Kava_Data, aes(x = Test, y = TOJ, fill = `C/A`)) + 
  geom_boxplot() +
  #scale_y_log10() +
  scale_x_discrete(drop = FALSE, name = 'Test') +
  labs(y="Temporal order judgement score")

ggarrange(grouped_accuracy_plot, grouped_fatigue_plot, grouped_focus_plot, grouped_plasticity_plot, grouped_toj_plot, grouped_timeper_plot,
             common.legend=TRUE, legend="bottom")
