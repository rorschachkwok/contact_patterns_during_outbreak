load('data/contact_by_severity.RData')
mean_contact_by_severity_p <- ggplot(data = scenario_df, mapping = aes(x = scenario, y = mean))+
        geom_jitter(width = 0.2, color = 'grey', size = 0.5)+
        stat_summary(fun.data = mean_sdl, fun.args = list(mult=2), 
                     geom="pointrange", color="#771434FF", size =0.25)+
        
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
                                          (severity_by_rt_p | severity_by_contacts_p) + 
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
ggsave(filename = 'fig1.pdf', plot = patch_contact_by_severity, device = cairo_pdf,
       family = 'Times New Roman', width = 10, height = 10)
