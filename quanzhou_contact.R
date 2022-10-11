pacman::p_load(
        openxlsx,
        tidyverse,
        epitrix,
        stringr,
        socialmixr,
        scales,
        paletteer,
        patchwork,
        ggpubr
)

max_eigen <- function(m) {
        eig <- eigen(m)
        max_eig <- eig$values[[1]]
        max_eig
}
# total close contact----------------------------------------------------------
qz_close_contact <- readRDS('data/qz_close_contact.rds')
qz_close_contact <- qz_close_contact %>% 
        mutate(across(.cols = contains('age'), 
                      .fns  = as.integer))
set.seed(10)
participants <- qz_close_contact %>% 
        distinct(part_id, .keep_all = T) %>% 
        select(part_id, part_age) 

contacts <- qz_close_contact %>% 
        select(cont_id, cnt_age_exact, part_id)

# import population vector
pop <- read.xlsx('data/pop.xlsx', sheet = 2)
pop <- pop %>% 
        mutate(across(.cols = everything(),
                      .fns = as.integer))
contact_pairs <- list('participants' = participants, 'contacts' = contacts)

qz_cc <- survey(contact_pairs$participants, contact_pairs$contacts) # qz_cc: quanzhou_close_contact

cnt_m <- contact_matrix(qz_cc, age.limits = seq(0,75,5), survey.pop = pop, 
                        symmetric = T, n = 100)
cnt_mr <- Reduce("+", lapply(cnt_m$matrices, function(x) {x$matrix})) / length(cnt_m$matrices)
total_cntr <- as.data.frame(cnt_mr / 2)

boot_matrix <- lapply(cnt_m$matrices, function(x) {x$matrix/2})
total_vector <- sapply(boot_matrix, max_eigen)

# data.frame for plotting contact matrix
qz_total_cntm <- total_cntr %>% 
        mutate(group = seq(0, 75,5)) %>% 
        select(group, everything())
names(qz_total_cntm) <- c('group', seq(0, 75, 5))
total_cnt_long <- qz_total_cntm %>% 
        pivot_longer(
                cols = -group,
                names_to = 'contact',
                values_to = 'freq'
        ) %>% 
        rename(index_case = 1) %>% 
        mutate(contact = as.numeric(contact))

# close contact in school --------------------------------------
set.seed(10)
contacts <- qz_close_contact %>% 
        filter(contact_scenarios == 'School') %>% 
        select(cont_id, cnt_age_exact, part_id)

contact_pairs <- list('participants' = participants, 'contacts' = contacts)

qz_school <- survey(contact_pairs$participants, contact_pairs$contacts)

cnt_m <- contact_matrix(qz_school, age.limits = seq(0,75,5), survey.pop = pop, 
                        symmetric = T, n = 100)
cnt_mr <- Reduce("+", lapply(cnt_m$matrices, function(x) {x$matrix})) / length(cnt_m$matrices)
school_cntr <- as.data.frame(cnt_mr / 2)

boot_matrix <- lapply(cnt_m$matrices, function(x) {x$matrix/2})
school_vector <- sapply(boot_matrix, max_eigen)

# data.frame for plotting contact matrix
qz_school_cntm <- school_cntr %>% 
        mutate(group = seq(0, 75,5)) %>% 
        select(group, everything())
names(qz_school_cntm) <- c('group', seq(0, 75, 5))
school_cnt_long <- qz_school_cntm %>% 
        pivot_longer(
                cols = 2:17,
                names_to = 'contact',
                values_to = 'freq'
        ) %>% 
        rename(index_case = 1) %>% 
        mutate(across(.cols = -freq,
                      .fns = as.numeric))

# close contact in workplace --------------------------------------
set.seed(10)

contacts <- qz_close_contact %>% 
        filter(contact_scenarios == 'Workplace') %>% 
        select(cont_id, cnt_age_exact, part_id)

contact_pairs <- list('participants' = participants, 'contacts' = contacts)

qz_work <- survey(contact_pairs$participants, contact_pairs$contacts)

cnt_m <- contact_matrix(qz_work, age.limits = seq(0,75,5), survey.pop = pop, 
                        symmetric = T, n = 100)
cnt_mr <- Reduce("+", lapply(cnt_m$matrices, function(x) {x$matrix})) / length(cnt_m$matrices)
work_cntr <- as.data.frame(cnt_mr / 2)

boot_matrix <- lapply(cnt_m$matrices, function(x) {x$matrix/2})
work_vector <- sapply(boot_matrix, max_eigen)

# data.frame for plotting contact matrix
qz_work_cntm <- work_cntr %>% 
        mutate(group = seq(0, 75,5)) %>% 
        select(group, everything())
names(qz_work_cntm) <- c('group', seq(0, 75, 5))

work_cnt_long <- qz_work_cntm %>% 
        pivot_longer(
                cols = 2:17,
                names_to = 'contact',
                values_to = 'freq'
        ) %>% 
        rename(index_case = 1) %>% 
        mutate(across(.cols = -freq,
                      .fns = as.numeric))

# close contact in household --------------------------------------
set.seed(10)

contacts <- qz_close_contact %>% 
        filter(contact_scenarios == 'Household') %>% 
        select(cont_id, cnt_age_exact, part_id)

contact_pairs <- list('participants' = participants, 'contacts' = contacts)

qz_home <- survey(contact_pairs$participants, contact_pairs$contacts)

cnt_m <- contact_matrix(qz_home, age.limits = seq(0,75,5), survey.pop = pop, 
                        symmetric = T, n = 100)
cnt_mr <- Reduce("+", lapply(cnt_m$matrices, function(x) {x$matrix})) / length(cnt_m$matrices)
home_cntr <- as.data.frame(cnt_mr / 2)

boot_matrix <- lapply(cnt_m$matrices, function(x) {x$matrix/2})
home_vector <- sapply(boot_matrix, max_eigen)

# data.frame for plotting contact matrix
qz_home_cntm <- home_cntr %>% 
        mutate(group = seq(0, 75,5)) %>% 
        select(group, everything())
names(qz_home_cntm) <- c('group', seq(0, 75, 5))

home_cnt_long <- qz_home_cntm %>% 
        pivot_longer(
                cols = 2:17,
                names_to = 'contact',
                values_to = 'freq'
        ) %>% 
        rename(index_case = 1) %>% 
        mutate(across(.cols = -freq,
                      .fns = as.numeric))


# close contact in others --------------------------------------
set.seed(10)

contacts <- qz_close_contact %>% 
        filter(contact_scenarios == 'Others') %>% 
        select(cont_id, cnt_age_exact, part_id)

contact_pairs <- list('participants' = participants, 'contacts' = contacts)

qz_others <- survey(contact_pairs$participants, contact_pairs$contacts)

cnt_m <- contact_matrix(qz_others, age.limits = seq(0,75,5), survey.pop = pop, 
                        symmetric = T, n = 100)
cnt_mr <- Reduce("+", lapply(cnt_m$matrices, function(x) {x$matrix})) / length(cnt_m$matrices)
others_cntr <- as.data.frame(cnt_mr / 2)

boot_matrix <- lapply(cnt_m$matrices, function(x) {x$matrix/2})
others_vector <- sapply(boot_matrix, max_eigen)

# data.frame for plotting contact matrix
qz_others_cntm <- others_cntr %>% 
        mutate(group = seq(0, 75,5)) %>% 
        select(group, everything())
names(qz_others_cntm) <- c('group', seq(0, 75, 5))

others_cnt_long <- qz_others_cntm %>% 
        pivot_longer(
                cols = 2:17,
                names_to = 'contact',
                values_to = 'freq'
        ) %>% 
        rename(index_case = 1) %>% 
        mutate(across(.cols = -freq,
                      .fns = as.numeric))

# close contact in urban area ---------------------------------------------
set.seed(10)

contacts <- qz_close_contact %>% 
        filter(urban_rural == 'urban') %>% 
        select(cont_id, cnt_age_exact, part_id)

contact_pairs <- list('participants' = participants, 'contacts' = contacts)

qz_urban <- survey(contact_pairs$participants, contact_pairs$contacts)

cnt_m <- contact_matrix(qz_urban, age.limits = seq(0,75,5), survey.pop = pop,
                        symmetric = T, n = 100)
cnt_mr <- Reduce("+", lapply(cnt_m$matrices, function(x) {x$matrix})) / length(cnt_m$matrices)
urban_cntr <- as.data.frame(cnt_mr / 2)

boot_matrix <- lapply(cnt_m$matrices, function(x) {x$matrix/2})
urban_vector <- sapply(boot_matrix, max_eigen)

# data.frame for plotting contact matrix
qz_urban_cntm <- urban_cntr %>% 
        mutate(group = seq(0, 75,5)) %>% 
        select(group, everything())
names(qz_urban_cntm) <- c('group', seq(0, 75, 5))

urban_cnt_long <- qz_urban_cntm %>% 
        pivot_longer(
                cols = 2:17,
                names_to = 'contact',
                values_to = 'freq'
        ) %>% 
        rename(index_case = 1) %>% 
        mutate(across(.cols = -freq,
                      .fns = as.numeric))


# close contact in rural area ---------------------------------------------
set.seed(10)

contacts <- qz_close_contact %>% 
        filter(urban_rural == 'rural') %>% 
        select(cont_id, cnt_age_exact, part_id)

contact_pairs <- list('participants' = participants, 'contacts' = contacts)

qz_rural <- survey(contact_pairs$participants, contact_pairs$contacts)

cnt_m <- contact_matrix(qz_rural, age.limits = seq(0,75,5), survey.pop = pop,
                        symmetric = T, n = 100)
cnt_mr <- Reduce("+", lapply(cnt_m$matrices, function(x) {x$matrix})) / length(cnt_m$matrices)
rural_cntr <- as.data.frame(cnt_mr / 2)

boot_matrix <- lapply(cnt_m$matrices, function(x) {x$matrix/2})
rural_vector <- sapply(boot_matrix, max_eigen)

# data.frame for plotting contact matrix
qz_rural_cntm <- rural_cntr %>% 
        mutate(group = seq(0, 75,5)) %>% 
        select(group, everything())
names(qz_rural_cntm) <- c('group', seq(0, 75, 5))

rural_cnt_long <- qz_rural_cntm %>% 
        pivot_longer(
                cols = 2:17,
                names_to = 'contact',
                values_to = 'freq'
        ) %>% 
        rename(index_case = 1) %>% 
        mutate(across(.cols = -freq,
                      .fns = as.numeric))


# close contact linked to asymptomatic cases ------------------------------
set.seed(10)
contacts <- qz_close_contact %>% 
        filter(severity == 'asymptomatic') %>% 
        select(cont_id, cnt_age_exact, part_id)

contact_pairs <- list('participants' = participants, 'contacts' = contacts)

qz_asymptomatic <- survey(contact_pairs$participants, contact_pairs$contacts)

cnt_m <- contact_matrix(qz_asymptomatic, age.limits = seq(0,75,5), survey.pop = pop, 
                        symmetric = T, n = 100)
cnt_mr <- Reduce("+", lapply(cnt_m$matrices, function(x) {x$matrix})) / length(cnt_m$matrices)
asymptomatic_cntr <- as.data.frame(cnt_mr / 2)

boot_matrix <- lapply(cnt_m$matrices, function(x) {x$matrix/2})
asymptomatic_vector <- sapply(boot_matrix, max_eigen)

# data.frame for plotting contact matrix
qz_asymptomatic_cntm <- asymptomatic_cntr %>% 
        mutate(group = seq(0, 75,5)) %>% 
        select(group, everything())
names(qz_asymptomatic_cntm) <- c('group', seq(0, 75, 5))

asymp_cnt_long <- qz_asymptomatic_cntm %>% 
        pivot_longer(
                cols = 2:17,
                names_to = 'contact',
                values_to = 'freq'
        ) %>% 
        rename(index_case = 1) %>% 
        mutate(across(.cols = -freq,
                      .fns = as.numeric))

# close contact linked to symptomatic cases --------------------------------------
set.seed(10)
contacts <- qz_close_contact %>% 
        filter(severity == 'symptomatic') %>% 
        select(cont_id, cnt_age_exact, part_id)

contact_pairs <- list('participants' = participants, 'contacts' = contacts)

qz_symptomatic <- survey(contact_pairs$participants, contact_pairs$contacts)

cnt_m <- contact_matrix(qz_symptomatic, age.limits = seq(0,75,5), survey.pop = pop, 
                        symmetric = T, n = 100)
cnt_mr <- Reduce("+", lapply(cnt_m$matrices, function(x) {x$matrix})) / length(cnt_m$matrices)
symptomatic_cntr <- as.data.frame(cnt_mr / 2)

boot_matrix <- lapply(cnt_m$matrices, function(x) {x$matrix/2})
symptomatic_vector <- sapply(boot_matrix, max_eigen)

# data.frame for plotting contact matrix
qz_symptomatic_cntm <- symptomatic_cntr %>% 
        mutate(group = seq(0, 75,5)) %>% 
        select(group, everything())
names(qz_symptomatic_cntm) <- c('group', seq(0, 75, 5))

symp_cnt_long <- qz_symptomatic_cntm %>% 
        pivot_longer(
                cols = 2:17,
                names_to = 'contact',
                values_to = 'freq'
        ) %>% 
        rename(index_case = 1) %>% 
        mutate(across(.cols = -freq,
                      .fns = as.numeric))


# plot contact matrix for multiple scenarios ------------------------------

plot_cntm <- function(data, scenario) {
        plot <- ggplot(data)+    
                geom_tile(aes(x = index_case,
                              y = contact,
                              fill = freq))+   
                annotate(geom = 'text', x = 75/2, y = Inf, vjust = -0.4,
                         label = scenario, size = 14/14*4) +
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
                labs(title = '?')
        return(plot)
}

cnt_long_ls <- list('Total' = total_cnt_long, 'School' = school_cnt_long, 
                 'Workplace' = work_cnt_long, 'Household' = home_cnt_long,
                 'Others' = others_cnt_long, 
                 'Urban' = urban_cnt_long, 'Rural' = rural_cnt_long, 
                 'Asymptomatic' = asymp_cnt_long, 'Symptomatic' = symp_cnt_long)
p_cntm <- map2(cnt_long_ls, names(cnt_long_ls), plot_cntm)

# data.frame for plotting contact matrix mean, 95%CI contact -------------------------------------------------
scenario_vector <- cbind(total_vector, work_vector, school_vector, 
                         home_vector, others_vector, asymptomatic_vector,
                         symptomatic_vector, rural_vector, urban_vector)
scenario_vector <- scenario_vector %>% 
        as.data.frame() %>% 
        pivot_longer(
                cols = 1:9,
                names_to = 'scenario',
                values_to = 'mean'
        ) %>% 
        mutate(scenario = fct_relevel(scenario, rev(c('total_vector', 'work_vector', 'school_vector', 
                                                  'home_vector', 'others_vector',
                                                  'asymptomatic_vector', 'symptomatic_vector',
                                                  'rural_vector', 'urban_vector'))))

p_mean_contact <- ggplot(data = scenario_vector, mapping = aes(x = scenario, y = mean))+
        geom_jitter(width = 0.2, color = 'grey', size = 0.5)+
        stat_summary(fun.data = mean_sdl, fun.args = list(mult=2), 
                     geom="pointrange", color="red", size =0.25)+
        
        scale_x_discrete(labels = rev(c('Total', 'Workplace', 'School', 
                                    'Household', 'Others',
                                    'Asymptomatic', 'Symptomatic',
                                    'Rual', 'Urban')))+
        coord_flip()+
        theme_bw()+
        theme(panel.grid = element_blank(),
              plot.margin = margin(0.5, 5, 0.5, 0.5))+
        labs(
                x = NULL,
                y = 'Mean Contacts',
                title = 'a'
        )


# patch sub-figure 'a' to 'j'  --------------------------------------------
patch_qz_cc <- (p_mean_contact | ((p_qz_cntm | p_qz_work_cntm | p_qz_school_cntm) /
        (p_qz_home_cntm | p_qz_asymptomatic_cntm | p_qz_symptomatic_cntm) / 
        (p_qz_others_cntm | p_qz_rural_cntm | p_qz_urban_cntm))) + 
        plot_layout(widths = c(1,3), guides = 'collect') &
        theme_bw(base_family = 'Times New Roman') +
        theme(legend.position = 'right',
              legend.text = element_text(size = 12, face = 'bold'),
              plot.tag.position = c(.15, .95),
              plot.margin = margin(.5, 5, 5, 5),
              title = element_text(size = 14, face = 'bold', color = 'black'),
              axis.title = element_text(size = 12, color = 'black'),
              axis.text = element_text(size = 12, face = 'bold', color = 'black'),
              axis.ticks = element_line(size=0.5),
              legend.key.height = unit(1.5, "cm"),
              plot.title = element_text(hjust = 0)) 
# ggsave('plot_contact.pdf', width = 12, height = 9, dpi = 300)

# total indirect contact --------------------------------------
# set.seed(10)
# qz_indirect_contact <- read.xlsx('data/qz_indirect_contact.xlsx')
