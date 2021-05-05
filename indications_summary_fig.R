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