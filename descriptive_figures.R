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

out_plot_data <- df %>% 
  select(intervention, outcome)  %>%
  filter(!is.na(outcome)) %>%
  mutate(outcome = recode(outcome, `1`= 'Fetal Demise',  `2` = 'NND',  
                          `3` = 'Discharge', `4` = 'Abortion', 
                          .default=as.character(outcome))) %>%
  group_by(intervention, outcome) %>%
  summarize(numerator = n()) %>%
  left_join(x=., y=intervention_counts, by="intervention") %>%
  mutate(rate = numerator/denominator,
         numer_label = sprintf("%% %2.0f",rate*100)
         )
out_plot_data 

ggplot(data=out_plot_data, aes(x=intervention, y=rate, group=outcome)) +
  geom_bar(aes(fill=outcome), stat="identity") +
  scale_fill_brewer(palette = "PRGn") +
  geom_text(size = 4,
            aes(label = numer_label),
            color='black',
            fontface='bold',
            position = position_stack(vjust = 0.5),
            show.legend = F)


