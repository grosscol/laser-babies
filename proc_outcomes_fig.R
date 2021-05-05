library("readr")
library("dplyr")
library("ggplot2")
library("tidyr")

df <- read_csv('SCT_percutaneous_interventions.csv')

##############
# Ring Plots # 
##############

ring_plot_data <- df %>% 
  select(intervention, outcome)  %>%
  filter(!is.na(outcome) & intervention == "Laser") %>%
  mutate(outcome = recode(outcome, `1`= 'Fetal Demise',  `2` = 'NND',  
                          `3` = 'Discharge', `4` = 'Abortion', 
                          .default=as.character(outcome))) %>%
  group_by(intervention, outcome) %>%
  summarize(numerator = n()) %>%
  left_join(x=., y=intervention_counts, by="intervention") %>%
  mutate(rate = numerator/denominator,
         rate_label = sprintf("%2.0f%%",rate*100),
         position=9,
         fract_label = sprintf("%% %i/%i",numerator, denominator),
         denom_label = sprintf("%i Cases", denominator))

ring_laser_plot <- ggplot(data=ring_plot_data, aes(x=position, y=rate, group=outcome)) +
  geom_bar(aes(fill=outcome), color='black', width=2, stat="identity") +
  geom_text(aes(label=rate_label), position = position_stack(vjust=.5), size = 8) +
  geom_text(aes(label=intervention), y=1, x=4, size = 32) +
  geom_text(aes(label=denom_label), y=1, x=1.5, size = 16) +
  scale_x_continuous(limits=c(0,10)) +
  coord_polar(theta="y", direction=1) +
  scale_fill_brewer(palette = "PRGn", name="Outcome") +
  theme_classic() +
  theme(axis.line=element_blank(),    axis.text.x=element_blank(),
        axis.text.y=element_blank(),  axis.ticks=element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank(),
        panel.background=element_blank(), panel.border=element_blank(),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        plot.margin = unit(c(-.1,-.1,-.1,-.1),"npc"),
        legend.position = c(.5,.4),
        legend.text = element_text(size=20),
        legend.title = element_text(size=22, hjust=.45))
ring_laser_plot

# Save plot
ggsave("outcome_by_proc_fig.png", plot=ring_laser_plot, device="png")