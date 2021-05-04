library("readr")
library("dplyr")
library("ggplot2")
library("tidyr")

df <- read_csv('SCT_percutaneous_interventions.csv')

#############################
# Primary indication figure #
#############################

# Recode primary indication
# 1 TFR, 2 Hydrops, 3 Cardiac, 4 other
ind_plot_data <- df %>% 
  mutate(primary_indication = recode(primary_indication, 
                                      `1`= 'TFR',  `2` = 'Hydrops',  `3` = 'Cardic', 
                                      `4` = 'HOCF', `5` = 'Growth Rate', 
                                      `6` = 'Polyhydramnios')) %>%
  select(primary_indication) %>%
  group_by(primary_indication) %>%
  summarize(indication_counts = n()) %>%
  mutate(primary_indication= factor(primary_indication,
                                      levels = primary_indication[order(indication_counts, decreasing=T)],
                                      ordered=T))
  
# Create plot
ind_plot <- ggplot(data=ind_plot_data, 
                   mapping = aes(x=primary_indication, y=indication_counts)) + 
  geom_col(mapping=aes(fill=primary_indication), color="black") +
  scale_fill_brewer(palette = "PRGn", guide=F) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  labs(title="Primary Indication for Intervention", fill = "Indication")
  ylab("Case Count")

# Print plot
ind_plot

# Save plot
ggsave("indication_counts_fig.png", plot=ind_plot, device="png")

#################################
# outcomes by procedure figures #
#################################

intervention_counts <- df %>%
  filter(!is.na(outcome)) %>%
  select(intervention) %>%
  group_by(intervention) %>%
  summarize(denominator = n())

# Turn off "helpful" message from summarize about experimental .grouping feature
options('dplyr.summarise.inform'=FALSE)

outc_plot_data <- df %>% 
  select(intervention, outcome)  %>%
  filter(!is.na(outcome)) %>%
  mutate(outcome = recode(outcome, `1`= 'Fetal Demise',  `2` = 'NND',  
                          `3` = 'Discharge', `4` = 'Abortion', 
                          .default=as.character(outcome))) %>%
  group_by(intervention, outcome) %>%
  summarize(numerator = n()) %>%
  left_join(x=., y=intervention_counts, by="intervention") %>%
  mutate(rate = numerator/denominator,
         rate_label = sprintf("%2.0f%%",rate*100)
         )
outc_plot_data 

outc_plot <- ggplot(data=outc_plot_data, aes(x=intervention, y=rate, group=outcome)) +
  geom_bar(aes(fill=outcome), stat="identity") +
  scale_fill_brewer(palette = "PRGn") +
  geom_text(size = 4,
            aes(label = rate_label),
            color='black',
            fontface='bold',
            position = position_stack(vjust = 0.5),
            show.legend = F) +
  ylab("Rate") +
  xlab("Intervention") +
  theme_classic() +
  theme(axis.text.y = element_blank()) 
outc_plot

#########################
# Ring Plots Experiment #
#########################

# Begin with outcomes data from above
ring_plot_data <- outc_plot_data %>% 
  filter(intervention == "Laser") %>%
  mutate(position=9,
         fract_label = sprintf("%% %i/%i",numerator, denominator),
         denom_label = sprintf("%i Cases", denominator))

ring_laser <- ggplot(data=ring_plot_data, aes(x=position, y=rate, group=outcome)) +
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

