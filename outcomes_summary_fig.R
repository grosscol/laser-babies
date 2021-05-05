library("readr")
library("dplyr")
library("ggplot2")
library("tidyr")

df <- read_csv('SCT_percutaneous_interventions.csv')

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

# Save plot
ggsave("outcomes_summary_fig.png", plot=outc_plot, device="png")