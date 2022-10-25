pacman::p_load(
        openxlsx,
        tidyverse,
        stringr,
        scales,
        paletteer,
        patchwork,
        ggpubr,
        lubridate
)

load('data/mean_contact_ls.Rdata')

# Ox index
date_index <- data.frame(days = 0:6, 
                         date = seq.Date(from = as.Date('2022/03/13'),
                                         by = 'day',
                                         length.out = length(0:6)))
ox_index <- read.xlsx('data/ox_index.xlsx', sheet = 1) %>% 
        select('date' = Date, 'ox_index' = StringencyIndex) %>% 
        mutate(date = ymd(date)) %>% 
        filter(date %in% date_index[ ,'date']) %>% 
        left_join(date_index, by = 'date')
# Tidy data: mean contact over time by age group
mean_contact_by_age_ls <- set_names(map(ls(pattern = '.*data'), ~get(.x)), 
                             ls(pattern = '.*data'))
mean_contact_by_age_ls <- map(mean_contact_by_age_ls, ~.x %>% filter(days %in% c(0:6)))

mean_contact_by_age_ls <- map(mean_contact_by_age_ls, ~.x %>% left_join(ox_index, by = 'days'))
cor_ls <- map(mean_contact_by_age_ls, ~.x %>% 
                           group_by(age_group) %>% 
                           summarise(cor = cor(mean_contact, ox_index,
                                               method = 'pearson',
                                               use = 'complete.obs')))
with_cor_ls <- map2(mean_contact_by_age_ls, cor_ls, 
                    function(data, cor_text) {
                            data %>% left_join(cor_text, by = 'age_group')
})

# Tidy data: contact over time by scenario and annotate by interventions--------------------------------------
mean_contact_overall_ls <- set_names(map(ls(pattern = '.*overall'), ~get(.x)), 
                                    ls(pattern = '.*overall'))
mean_contact_overall_ls <- map(mean_contact_overall_ls, ~.x %>% filter(days %in% c(0:6)))

mean_contact_overall_ls <- map(mean_contact_overall_ls, ~.x %>% left_join(ox_index, by = 'days'))
overall_cor_ls <- map(mean_contact_overall_ls, ~.x %>% 
                      group_by(age_group) %>% 
                      summarise(cor = cor(mean_contact, ox_index,
                                          method = 'pearson',
                                          use = 'complete.obs')))
overall_with_cor_ls <- map2(mean_contact_overall_ls, overall_cor_ls, 
                    function(data, cor_text) {
                            data %>% left_join(cor_text, by = 'age_group')
                    })
# PLOT: 4 facets of 'all age groups'
plot_scenarios_overall <- function(i){
        max_contact <- max(with_cor_ls[[i]]$upper) + 5
        fig <- overall_with_cor_ls[[i]] %>% 
                ggplot(aes(x = days, y = mean_contact))+
                scale_x_continuous(limits = c(0,6), 
                                   breaks = seq(0, 6, 1), 
                                   labels = seq(1, 7, 1))+
                scale_y_continuous(expand = expansion(mult = c(0, 0.1)),
                                   breaks = pretty(c(0, max_contact)),
                                   limits = range(pretty(c(0, max_contact)))) +
                annotate('rect', xmin = 3, ymin = -Inf,
                         xmax = Inf, ymax = Inf, fill = "#E1DAEA") +
                annotate('text', x = 4.5, y = 0,
                         label = 'No policy change', size = 4, vjust = -1)+
                geom_line(color = 'black', size = 0.5)+
                geom_ribbon(aes(ymin = lower, ymax = upper, fill = cor), alpha = 0.6)+
                geom_label(aes(label = format(round(cor, digits = 2), nsmall = 2)),
                           color = 'darkgrey',
                           fontface = "bold",
                           size = 4,
                           x = Inf, y = Inf, 
                           hjust   = 1,
                           vjust   = 1) +
                scale_fill_gradient2(low = 'steelblue',
                                     high = 'red',
                                     mid = 'white',
                                     midpoint = 0,
                                     limit = c(-1, 1),
                                     name = 'Pearson\nCorrelation')
        return(fig)
}
scenarios_overall_plot <- map(1:4, plot_scenarios_overall)

mcnt_phased <- scenarios_overall_plot[[3]] + 
        scenarios_overall_plot[[4]] +
        scenarios_overall_plot[[2]] +
        scenarios_overall_plot[[1]] +
        plot_layout(nrow = 4)&
        theme_classic2()+
        theme(strip.background = element_blank(),
              panel.grid = element_blank(),
              axis.text = element_text(color = 'black'),
              plot.title = element_text(size = 10, hjust = 0.5),
              legend.position = 'none') &
        labs(x = NULL,
             y = NULL,
             title = 'All Age Groups')

# PLOT: 4 rows of different scenarios by age groups
facet_label_ls <- list('Overall' = c('0-9', '10-19', '20-29', '30-39', '40-49', '50-59', '60+'),
                       'Household' = c('0-19', '20-29', '30-39', '40-49', '50-59', '60+'), 
                       'School' = c('0-9', '10-14', '15-20', '20+'),
                       'Workplace' = c('0-19', '20-29', '30-39', '40-49', '50-59', '60+'))
scenario_names <- names(facet_label_ls)
plot_all_scenario_by_age <- function(i){
        facet_label <- facet_label_ls[[i]]
        facet_hide <- levels(with_cor_ls[[i]]$age_group)
        names(facet_label) <- facet_hide
        max_contact <- max(with_cor_ls[[i]]$upper) + 5
        fig <- with_cor_ls[[i]] %>% 
                mutate(scenario = scenario_names[[i]]) %>% 
                ggplot(aes(x = days, y = mean_contact))+
                geom_line(color = 'black', size = 0.5)+
                geom_ribbon(aes(ymin = lower, ymax = upper, fill = cor), alpha = 0.6)+
                geom_label(aes(label = format(round(cor, digits = 2), nsmall = 2)),
                           color = 'darkgrey',
                           fontface = "bold",
                           size = 4,
                           x = Inf, y = Inf, 
                           hjust   = 1,
                           vjust   = 1) +
                scale_fill_gradient2(low = 'steelblue',
                                     high = 'red',
                                     mid = 'white',
                                     midpoint = 0,
                                     limit = c(-1, 1),
                                     name = 'Pearson\nCorrelation') +
                facet_grid(scenario ~ age_group, 
                           labeller = labeller(age_group = facet_label))+
                scale_x_continuous(limits = c(0,6), 
                                   breaks = seq(0, 6, 1), 
                                   labels = seq(1, 7, 1))+
                scale_y_continuous(expand = expansion(mult = c(0, 0.1)),
                                   breaks = pretty(c(0, max_contact)),
                                   limits = range(pretty(c(0, max_contact)))) +
                labs(x = NULL, 
                     y = NULL)
        return(fig)
}
all_scenario_by_age_plot <- map(1:4, plot_all_scenario_by_age)

mcnt_by_age <- all_scenario_by_age_plot[[3]] + # school
        all_scenario_by_age_plot[[4]] +  # workplace
        all_scenario_by_age_plot[[2]] +  # household
        all_scenario_by_age_plot[[1]] +  # all
        plot_layout(nrow = 4, guides = 'collect')&
        theme_classic2()+
        theme(strip.background = element_blank(),
              axis.ticks.y = element_blank(),
              axis.text.y = element_blank(),
              axis.title = element_text(size = 12),
              axis.text = element_text(size = 12, color = 'black'))&
        labs(x = NULL,
             y = NULL)




source('add_global_label.R')
add_global_label(((mcnt_phased | mcnt_by_age) + 
                          plot_layout(ncol = 2, widths = c(1,2))+
                          plot_annotation(caption = 'Days since outbreak',
                                          theme = theme(plot.caption = element_text(hjust = 0.5, 
                                                                                    size = 12)))),
                 Ylab = "Mean Contacts",
                 Ygap = 0.02)

ggsave('fig1.pdf', family = 'Times New Roman', 
       height = 10, width = 15,
       device = cairo_pdf)
