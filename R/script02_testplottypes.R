#-------------------------------------------------------------------------------------#
# Project: Programming Exercise
# Purpose: Test plots that can show up in the shiny application
# Author: Artemio Sison III
# R Version: 4.0.1 "See Things Now"
#-------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------#
# Load Dependencies
#-------------------------------------------------------------------------------------#
library(gganimate)
library(hrbrthemes)
library(ggplot2)
library(viridis)

#-------------------------------------------------------------------------------------#
# Testing various plots before putting them in the Shiny application
#-------------------------------------------------------------------------------------#

# Plotting Mean 
# test_plot1 <- melt_dat %>%
#   filter(VISIT %in% c("PEAK", "SCRN_DELTA", "WK1_DELTA", "WK2_DELTA", "WK3_DELTA",
#                       "WK4_DELTA", "WK5_DELTA", "PEAK_DELTA") == FALSE) %>%
#   # this will be the code that the reactive input will select
#   filter(LBTEST == "Alanine Aminotransferase Measurement") %>%
#   group_by(VISIT, ACTARM, BMRKR2) %>%
#   summarize(MEAN = mean(RESULT, na.rm = TRUE),
#             SD = sd(RESULT, na.rm = TRUE),
#             MAX1 = max(RESULT),
#             MIN1 = min(RESULT)) 
# 
# ggplot(data = test_plot1,
#        aes(x = VISIT, y = MEAN, fill = BMRKR2)) +
#   facet_wrap(BMRKR2~ACTARM) +
#   geom_bar(stat = "identity", 
#            color = "black",
#            # width = 0.4,
#            position = position_dodge()) +
#   geom_errorbar(aes(ymin = MEAN - SD, 
#                 ymax = MEAN + SD),
#                 width = 0.2,
#                 position = position_dodge(0.9)) +
#   theme(axis.text.x = element_text(angle = 45, 
#                                    hjust = 1.0),
#         legend.position = "bottom") 
# 
# 
# ggplot(data = test_plot1,
#        aes(x = VISIT, y = MEAN, color = ACTARM)) +
#   facet_wrap(~ BMRKR2) +
#   geom_point(stat = "identity", aes(size = MEAN)) + 
#   theme(axis.text.x = element_text(angle = 45, 
#                                    hjust = 1.0),
#         legend.position = "bottom") 

