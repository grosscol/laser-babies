library("readr")
library("dplyr")
library("ggplot2")
library("tidyr")
library("stringr")

df <- read_csv('SCT_percutaneous_interventions.csv')

#######################
# Base Ring Plot Data # 
#######################

# Like to_title, but doesn't lower the rest of the chars
upcase_first <- function(x){
  
}

intervention_counts <- df %>%
  filter(!is.na(outcome)) %>%
  select(intervention) %>%
  group_by(intervention) %>%
  summarize(denominator = n())

# Base data will need to be filtered by intervention.
ring_plot_data <- df %>% 
  select(intervention, outcome)  %>%
  filter(!is.na(outcome)) %>%
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

# Text sizes
sz_percents <- 4
sz_center_sub <- 6
sz_legend <- 20
sz_legend_title <- 22
sz_facet = 12

multi_plot <- ggplot(data=ring_plot_data, aes(x=position, y=rate, group=outcome)) +
  geom_bar(aes(fill=outcome), color='black', width=2, stat="identity") +
  geom_text(aes(label=rate_label, x=position-2.5), 
            position = position_stack(vjust=.5), size = sz_percents) +
  geom_text(aes(label=denom_label), y=1, x=0.5, size = sz_center_sub) +
  scale_x_continuous(limits=c(0,10)) +
  coord_polar(theta="y", direction=1) +
  scale_fill_brewer(palette = "PRGn", name="Outcome") +
  theme_classic() +
  theme(axis.line=element_blank(),    axis.text.x=element_blank(),
        axis.text.y=element_blank(),  axis.ticks=element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank(),
        panel.background=element_blank(), panel.border=element_blank(),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        legend.position = c(.8,.35),
        legend.text = element_text(size=sz_legend),
        legend.title = element_text(size=sz_legend_title, hjust=.45),
        strip.text.x = element_text(size=sz_facet),
        plot.margin = margin(.2,0,.2,0, "in")) +
  facet_wrap(facets=vars(intervention), nrow=2, ncol=2)

# Save plot
ggsave("outcome_by_proc_fig.png", plot=multi_plot, 
       width = 8, height = 7, units='in', device="png")
