library("readxl")
library("dplyr")
library("ggplot2")

df <- read_xlsx('SCT Percutaneous Interventions Refined Data.xlsx', 'Database_Refined_2')

#############################
# Primary indication figure #
#############################

# Recode primary indication
# 1 TFR, 2 Hydrops, 3 Cardiac, 4 other
ind_plot_data <- df %>% 
  mutate(`Primary Indication`= recode(`Primary Indication`, 
                                      `1`= 'TFR',  `2` = 'Hydrops',  `3` = 'Cardic', 
                                      `4` = 'HOCF', `5` = 'Growth Rate', 
                                      `6` = 'Polyhydramnios')) %>%
  select(`Primary Indication`) %>%
  group_by(`Primary Indication`) %>%
  summarize(`Indication Counts` = n()) %>%
  mutate(`Primary Indication`= factor(`Primary Indication`,
                                      levels = `Primary Indication`[order(`Indication Counts`, decreasing=T)],
                                      ordered=T))
  
# Create plot
ind_plot <- ggplot(data=ind_plot_data, 
                   mapping = aes(x=`Primary Indication`, y=`Indication Counts`)) + 
  geom_col(mapping=aes(fill=`Primary Indication`), color="black") +
  scale_fill_brewer(palette = "PRGn") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Print plot
ind_plot

# Save plot
ggsave("indication_counts_fig.png", plot=ind_plot, device="png")

####################
# Outcomes figures #
####################
out_plot_data <- df %>% 
  select(Outcome)  %>%
  filter(!is.na(Outcome)) %>%
  mutate(Outcome = recode(Outcome, `1`= 'Fetal Demise',  `2` = 'NND',  
                          `3` = 'Discharge', .default=as.character(Outcome)))

  
