# Load Packages
library(tidyverse)
library(ggplot2)
library(readr)
library(janitor)
library(gt)
library(cowplot)
library(tidytuesdayR)
library(ggthemes)
library(extrafont)
library(magick)


# Load in the data
tuesdata <- tidytuesdayR::tt_load('2023-04-25')
                             
# Our data consists of two dataframes:
# 1. Winners of London Marathon since 1981 (includes race type)
# 2. Number of Applicants, Starters, Finishers, Money Raised
# These dataframes can be loaded in using $

# Load the winners data frame
winners <- tuesdata$winners
# This turns our list into a dataframe titled `winners`

# Inspect the dataframe types
str(winners)
# Looks like we have some character variables that represent the Category of Race, Athlete Name,
# Nationality Name, along with some numeric data that records the final time of the race winner

# I think it would be interesting to see if there are any repeat winners of the London Marathon.
# Let's start by just looking at the Men's race.
mens_winners <- winners %>%
  filter(Category == "Men")

# Now that we have a segmented dataframe of our larger dataframe `winners` let's take a look and
# see if there are any repeat winners of the race. We will do this by first grouping the data by
# athlete, and then counting the number of times the name appears, since this data already includes
# all winners of the race.
mens_winners_plot <- mens_winners %>%
  # group the dataframe by athlete
  group_by(Athlete) %>%
  # collect the count of races_won
  summarize(races_won = n()) %>%
  # arrange the data from largest to smallest by races_won
  arrange(desc(races_won)) %>%
  # return first 10 values of this ordered summary
  head(5)

# Eliud Kipchoge is a beast! He's a 4x winner of the event!
# Let's display this grouped sumamry as a barplot

final_winners_plot <- mens_winners_plot %>%
  ggplot(aes(x=reorder(Athlete, -races_won), y=races_won))+
  geom_bar(stat='identity', fill = "#b41f1f")+
  geom_text(aes(label = races_won), vjust = -0.5, size = 4)+
  labs(title = "Most Wins in London Marathon History",
       subtitle = "Data is from 1981 - 2022 and includes Men's race only",
       x = "Athletes",
       y = "# of times won")+
  theme(
    axis.title = element_text(family = "Comic Sans MS", hjust = 0.5),
    title = element_text(family="Comic Sans MS", face = "italic", color = "black"),
    axis.text = element_text(family="Comic Sans MS", face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "floralwhite"),
    plot.background = element_rect(fill = "floralwhite")
  )

pictured_plot <- ggdraw(final_winners_plot)+
  draw_image("eliud_kipchoge.png", height = .30, x= -0.35, y=0.3)
pictured_plot

ggsave("eliud_plot.png", pictured_plot, dpi = 300)
