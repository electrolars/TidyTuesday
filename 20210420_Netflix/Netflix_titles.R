
# Libraries ----------------------------------------------------------

library(tidytuesdayR)
library(tidyverse)
library(patchwork)
library(magick)


# Load data 2021, week 17 -------------------------------------------------

date <- "2021-04-20"
data <- tidytuesdayR::tt_load(date)
save(data, file = "Data/tt_data.R")

df <- data$netflix_titles
names(df) <-  str_to_title(names(df))

df <- df %>% 
  mutate(Date_added = as.Date(str_trim(Date_added, side = "both"), "%B %d, %Y"),
         Weekday_added = format(Date_added, "%a"))


# Plot --------------------------------------------------------------------

netflix_logo <- image_read("Data/Netflix_Logo_RGB.png")

p1 <- netflix_logo %>% 
  image_ggplot() +
  theme(plot.background = element_rect(fill = "black", color = NA),
        text = element_text(color = "white"),
        axis.title.x = element_text()) +
  xlab("#TidyTuesday\n#electrolars")

p2 <- df_ %>% 
  filter(Year_added >= "2016") %>% 
  group_by(Year_added) %>% 
  count(Weekday_added) %>% 
  mutate(n = (n / sum(n))*100,
         Weekday = fct_relevel(Weekday_added,"Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")) %>% 
  ggplot(aes(x = Year_added, y = n, fill = Weekday)) +
  geom_area() +
  scale_fill_manual(values = scales::seq_gradient_pal(low = "#fac4c0", high = "#ff1200")(seq(0, 1, length.out = 7))) +
  scale_y_continuous(expand = c(0.01, 0))+
  scale_x_continuous(expand = c(0.01, 0)) +
  ylab("Share of newly added content") +
  xlab("Year added") +
  theme(plot.background = element_rect(fill = "black", colour = NA),
        panel.background = element_rect(fill = "black"),
        panel.border = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.background = element_rect(fill = "black"),
        legend.key = element_rect(fill = "black"),
        axis.text = element_text(color = "white"), 
        text = element_text(color = "white"),
        axis.ticks = element_line(colour = "white")
        )

p <- p1 + p2 + plot_layout(widths = unit(6, "cm"), heights = unit(6, "cm"))

ggsave("Figures/TidyTuesday_electrolars.png", plot = p, width = 20, height = 10, units = "cm")
