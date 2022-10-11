pacman::p_load(
        tidyverse,
        openxlsx,
        lubridate,
        cowplot,
        scales,
        patchwork,
        ggpubr
)

load('mean_contact_ls.Rdata')

# Pick contacts only during cases rapidly increased period 
# (first week since outbreak was announced)

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
# Mean contact over time
mean_contact_ls <- set_names(map(ls(pattern = '.*data|.*overall'), ~get(.x)), 
                             ls(pattern = '.*data|.*overall'))
mean_contact_ls <- map(mean_contact_ls, ~.x %>% filter(days %in% c(0:6)))
mean_contact_ls <- map2(mean_contact_ls, names(mean_contact_ls),
     function(df, name) {df %>% 
                     mutate(scenario = str_extract(name,
                                                   pattern = '^[^_]+(?=_)'))})

mean_contact_nest <- bind_rows(mean_contact_ls) %>% 
        group_by(scenario) %>% 
        nest() 
with_cor <- mean_contact_nest %>% 
        mutate(cor_text = map(data, ~.x %>% 
                                      left_join(ox_index, by = 'days') %>% 
                                      group_by(age_group) %>% 
                                      summarise(cor = cor(mean_contact, ox_index,
                                                          method = 'pearson',
                                                          use = 'complete.obs'))),
               data = map(data, ~.x %>% left_join(ox_index, by = 'days')))
# scenario_vector <- with_cor$scenario
# text_data <- with_cor[[3]][[3]]
# with_cor[[2]][[3]] %>%
#         ggplot(aes(x = days, y = mean_contact))+
#         geom_line()+
#         geom_line(aes(x = days, y = ox_index))+
#         facet_wrap(~ age_group)+
#         geom_text(data = text_data,
#                    aes(label = str_glue('Pearson = {round(cor, digits = 2)}')),
#                    x = Inf, y = Inf,
#                    hjust   = 1,
#                    vjust   = 1)
        
add_cor <- function(data, text_data) {
        p1 <- data %>% drop_na() %>% 
                ggplot(aes(x = as.POSIXct(date), y = mean_contact))+
                geom_line(color = '#771434FF')+
                geom_ribbon(aes(ymin = lower, ymax = upper), fill = '#D2A6B4FF', alpha = 0.6)+
                geom_text(data = text_data, 
                          aes(label = round(cor, digits = 2)),
                          x = Inf, y = Inf, 
                          hjust   = 2,
                          vjust   = 1)+
                scale_x_datetime(labels = label_date_short())+
                facet_wrap(~age_group, nrow = 1)+
                theme_cowplot()+
                labs(x = NULL,
                     # title = scenario,
                     y = 'Mean Contact')
        
        p2 <- data %>% drop_na() %>% 
                ggplot(aes(x = as.POSIXct(date), y = ox_index))+
                geom_line(color = 'darkgray', size = 0.7)+
                scale_x_datetime(labels = label_date_short())+
                scale_y_continuous(
                        position = "right")+
                facet_wrap(~age_group, nrow = 1)+
                theme_cowplot()+
                labs(x = 'Date',
                     y = 'Government\nResponse Index')
        aligned_plots <- cowplot::align_plots(p1, p2, align="hv", axis="tblr")         # align the two plots and save them as list
        aligned_plotted <- ggdraw(aligned_plots[[1]]) + draw_plot(aligned_plots[[2]])  # overlay them and save the visual plot
        
        return(aligned_plotted)
}
with_cor_plot_container <- map2(with_cor$data, with_cor$cor_text,
                                add_cor)
patch_plot <- do.call(patchwork::wrap_plots, with_cor_plot_container)
patch_plot + plot_layout(nrow = 4)



# 
# p1 <- with_cor[[2]][[3]] %>% drop_na() %>%
#         ggplot(aes(x = as.Date(date), y = mean_contact))+
#         geom_line(color = '#771434FF')+
#         geom_ribbon(aes(ymin = lower, ymax = upper), fill = '#D2A6B4FF', alpha = 0.6)+
#         geom_text(data = text_data,
#                   aes(label = round(cor, digits = 2)),
#                   x = Inf, y = Inf,
#                   hjust   = 2,
#                   vjust   = 1)+
#         scale_x_date(labels = label_date_short())+
#         facet_wrap(~age_group, nrow = 1)+
#         theme_cowplot()+
#         # theme(axis.text.x = element_blank())+
#         labs(x = NULL,
#              # title = scenario,
#              y = 'Mean Contact')
# 
# p2 <- with_cor[[2]][[3]] %>% drop_na() %>%
#         ggplot(aes(x = as.Date(date), y = ox_index))+
#         geom_line(color = 'darkgray', size = 0.7)+
#         scale_x_date(labels = label_date_short())+
#         scale_y_continuous(
#                 position = "right")+
#         facet_wrap(~age_group, nrow = 1)+
#         theme_cowplot()+
#         labs(x = 'Date',
#              y = 'Government\nResponse Index')
# aligned_plots <- cowplot::align_plots(p1, p2, align="hv", axis="tblr")         # align the two plots and save them as list
# aligned_plotted <- ggdraw(aligned_plots[[1]]) + draw_plot(aligned_plots[[2]])  # overlay them and save the visual plot
# 
# 
# 
# 
# mean_contact_by_scenario <- map(mean_contact, ~ .x %>% arrange(days) %>%
#                                         select(mean_contact,
#                                                lower, upper, days))
# contact_and_ox_by_scenario <- map(mean_contact_by_scenario,
#                                   ~.x %>% left_join(date_index, by = 'days') %>%
#                                           left_join(ox_index, by = 'date'))
# aligned_plotted <- list()
# plot_contact_ox <- function(data, scenario) {
#         p1 <- data %>% drop_na() %>%
#                 ggplot(aes(x = date, y = mean_contact))+
#                 geom_line(color = '#771434FF')+
#                 geom_ribbon(aes(ymin = lower, ymax = upper), fill = '#D2A6B4FF', alpha = 0.6)+
#                 scale_x_date(breaks = breaks_width("5 days"))+
#                 theme_cowplot()+
#                 labs(x = NULL,
#                      title = scenario,
#                      y = 'Mean Contact')
# 
#         p2 <- data %>% drop_na() %>%
#                 ggplot(aes(x = date, y = ox_index))+
#                 geom_line(color = 'darkgray', size = 0.7)+
#                 scale_x_date(breaks = breaks_width("5 days"))+
#                 scale_y_continuous(
#                         position = "right")+
#                 theme_cowplot()+
#                 labs(x = 'Date',
#                      y = 'Government\nResponse Index')
#         aligned_plots <- cowplot::align_plots(p1, p2, align="hv", axis="tblr")         # align the two plots and save them as list
#         aligned_plotted[[scenario]] <- ggdraw(aligned_plots[[1]]) + draw_plot(aligned_plots[[2]])  # overlay them and save the visual plot
# }
# 
# aligned_plotted <- map2(contact_and_ox_by_scenario,
#                         names(contact_and_ox_by_scenario), plot_contact_ox)
# aligned_plotted$all + aligned_plotted$school +
#         aligned_plotted$work + aligned_plotted$home + plot_layout(nrow = 2)
# 
# 
# 
