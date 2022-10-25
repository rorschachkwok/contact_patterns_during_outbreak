pacman::p_load(
        openxlsx,
        tidyverse,
        stringr,
        scales,
        patchwork,
        paletteer,
        ggpubr
)
# Analysis of individuals with a high number of contacts and high reproductive 
# number

qz_close_contact <- readRDS('data/qz_close_contact.rds')

# Individuals with a high number of contacts
contacts_of_each <- qz_close_contact %>% 
        count(part_id) %>% 
        left_join(qz_close_contact, by = 'part_id') %>% 
        select(part_id, n, severity) %>% 
        distinct()

severity_by_contacts <- data.frame(severity = factor(), n = integer(), index = integer())
contacts_index <- c(50, 100, 150, 200, 250)
for(i in 1:5) {
        df <- contacts_of_each %>% 
                filter(n > contacts_index[i]) %>% 
                count(severity) %>% 
                mutate(index = contacts_index[i])
        severity_by_contacts <- bind_rows(severity_by_contacts, df)
}

severity_by_contacts <- severity_by_contacts %>% 
        group_by(index) %>% 
        summarise(percentage = n / sum(n),
                  severity = severity) %>% 
        mutate(severity = fct_relevel(severity, 'symptomatic', 'asymptomatic'),
               index = as.factor(index)) %>% 
        mutate(position = cumsum(percentage) - (0.5 * percentage))

# Individuals with a high reproductive number
rt_of_each <- qz_close_contact %>% 
        group_by(part_id) %>% 
        summarise(rt = sum(cont_is_case)) %>% 
        left_join(qz_close_contact, by = 'part_id') %>% 
        select(part_id, rt, severity) %>% 
        distinct()

severity_by_rt <- data.frame(severity = factor(), n = integer(), index = integer())
rt_index <- c(10, 15, 20, 25, 30)
for(i in 1:5) {
        df <- rt_of_each %>% 
                filter(rt > rt_index[i]) %>% 
                count(severity) %>% 
                mutate(index = rt_index[i])
        severity_by_rt <- bind_rows(severity_by_rt, df)
}

severity_by_rt <- severity_by_rt %>%
        group_by(index) %>% 
        summarise(percentage = n / sum(n),
                  severity = severity) %>% 
        mutate(severity = fct_relevel(severity, 'symptomatic', 'asymptomatic'),
               index = as.factor(index)) %>% 
        mutate(position = cumsum(percentage) - (0.5 * percentage))

# Stacked + percent plot
severity_by_contacts_p <- severity_by_contacts %>% 
        ggplot(aes(x = index, y = percentage, fill = severity)) + 
        geom_bar(position = "fill", stat = "identity") +
        scale_fill_manual(values = c("#D6D5D1", "#8E4D51"),
                          labels = c('Symptomatic', 'Asymptomatic'))+
        geom_text(aes(y = position, color = severity, 
                      label = paste0(round(percentage * 100, 0),"%")),
                  size = 4, show.legend = F)+
        scale_color_manual(values = c('black', 'white'))+
        scale_y_continuous(expand = expansion(mult = c(0, 0)))+
        theme_bw()+
        labs(x = 'Number of Contacts',
             y = 'Proportion',
             title = 'd')
severity_by_rt_p <- severity_by_rt %>% 
        ggplot(aes(x = index, y = percentage, fill = severity)) + 
        geom_bar(position = "fill", stat = "identity") +
        scale_fill_manual(values = c("#D6D5D1", "#8E4D51"),
                          labels = c('Symptomatic', 'Asymptomatic'))+
        geom_text(aes(y = position, color = severity, 
                      label = paste0(round(percentage * 100, 0),"%")),
                  size = 4, show.legend = F)+
        scale_color_manual(values = c('black', 'white'))+
        scale_y_continuous(expand = expansion(mult = c(0, 0)))+
        theme_bw()+
        theme(legend.position = 'none')+
        labs(x = 'Number of Secondary Cases Caused',
             y = NULL,
             title = 'e')


load('data/contact_by_severity.RData')
mean_contact_by_severity_p <- ggplot(data = scenario_df, mapping = aes(x = scenario, y = mean))+
        geom_jitter(
                width = 0.2, color = 'grey', size = 0.5)+
        stat_summary(fun.data = mean_se, fun.args = list(mult=qnorm(0.975)),
                     geom="pointrange", color="#771434FF", size =0.15)+
        
        scale_x_discrete(labels = rev(c('Asymptomatic', 'Symptomatic')))+
        coord_flip()+
        theme_bw() +
        theme(panel.grid = element_blank(),
              plot.margin = margin(0.5, 0.5, 0.5, 0.5),
              axis.text.y.left = element_text(angle = 90, hjust = 0.5))+
        labs(
                x = NULL,
                y = 'Mean Contacts',
                title = 'a'
        )
asymp_p <- ggplot(asymp_cnt_long)+    
        geom_tile(aes(x = index_case,
                      y = contact,
                      fill = freq))+   
        annotate(geom = 'text', x = 75/2, y = Inf, vjust = -1,
                 label = 'Asymptomatic', size = 14/14*4) +
        coord_fixed(clip = 'off')+
        scale_fill_gradientn(colours = c('white', paletteer_d("unikn::pal_bordeaux")),
                             limits=c(0, 5), breaks=seq(0,5,by=1)) +
        scale_x_continuous(
                expand = c(0,0),
                breaks = seq(0,70,10)
        )+
        scale_y_continuous(
                expand = c(0,0),
                breaks = seq(0,70,10)
        )+
        theme_bw() +
        theme(legend.position = 'none')+
        labs(title = 'b',
             x = 'Age of Index Case',
             y = 'Age of Close Contact')
symp_p <- ggplot(symp_cnt_long)+    
        geom_tile(aes(x = index_case,
                      y = contact,
                      fill = freq))+   
        annotate(geom = 'text', x = 75/2, y = Inf, vjust = -1,
                 label = 'Symptomatic', size = 14/14*4) +
        coord_fixed(clip = 'off')+
        scale_fill_gradientn(colours = c('white', paletteer_d("unikn::pal_bordeaux")),
                             limits=c(0, 5), breaks=seq(0,5,by=1)) +
        scale_x_continuous(
                expand = c(0,0),
                breaks = seq(0,70,10)
        )+
        scale_y_continuous(
                expand = c(0,0),
                breaks = seq(0,70,10)
        )+
        theme_bw() +
        labs(title = 'c',
             x = 'Age of Index Case',
             y = NULL)

patch_contact_by_severity <- (mean_contact_by_severity_p | asymp_p | symp_p) /
        (severity_by_contacts_p | severity_by_rt_p) + 
        plot_layout(heights = c(1,1), widths = c(1,1), guides = 'collect') &
        theme(text = element_text(colour = 'black'),
              legend.position = 'bottom',
              legend.text = element_text(size = 12),
              plot.tag.position = c(.15, .95),
              plot.margin = margin(.5, 5, .5, 5),
              title = element_text(size = 14),
              axis.title = element_text(size = 12),
              axis.text = element_text(size = 12, colour = 'black'),
              axis.ticks = element_line(size=0.5),
              plot.title = element_text(hjust = 0)) &
        labs(fill = NULL)
ggsave(filename = 'fig2.pdf', plot = patch_contact_by_severity, device = cairo_pdf,
       family = 'Times New Roman', width = 10, height = 8)

