pacman::p_load(
        openxlsx,
        tidyverse,
        stringr,
        socialmixr,
        scales,
        paletteer,
        patchwork,
        ggthemes,
        showtext
)

max_eigen <- function(m) {
        eig <- eigen(m)
        max_eig <- eig$values[[1]]
        max_eig
}
find_yend <- function(x, d) {
        as.numeric(x[x$days == d, "upper"]) + 0.05
}

# qz contact over time ----------------------------------------------------
qz_close_contact <- readRDS(file = 'data/qz_close_contact.rds')
# population of age group for qz contact over time
pop <- read.xlsx('data/pop.xlsx', sheet = 3)
pop <- pop %>% 
        mutate(across(.cols = everything(),
                      .fns = as.integer))
boot_df <- data.frame(temp = rep(NA, 100))
age_df <- data.frame(matrix(nrow = 100, ncol = 7))
contact_by_age_ls <- list()
na_mat <- list()
for (i in 1:100) {
        na_mat[[i]] <- matrix(NA, 7, 7)
}

set.seed(10)
# rolling window: 7 days
for (i in -3:24) {
        # select contacts in this rolling window
        qz_rolling_cc <- qz_close_contact[qz_close_contact$day_since_outbreak %in% c(seq(i-3, i+3,1)), ]
        # prepare participants and contacts
        participants <- qz_rolling_cc %>% 
                distinct(part_id, .keep_all = T) %>% 
                select(part_id, part_age) %>% 
                mutate(part_age = as.integer(part_age))
        contacts <- qz_rolling_cc %>% 
                select(cont_id, cnt_age_exact, part_id) %>% 
                mutate(cnt_age_exact = as.integer(cnt_age_exact))
        contact_pairs <- list('participants' = participants, 'contacts' = contacts)
        qz_rolling_cc <- survey(contact_pairs$participants, contact_pairs$contacts) 
        
        cnt_m <- tryCatch(
                expr = {contact_matrix(qz_rolling_cc, age.limits = seq(0,60,10), 
                                       survey.pop = pop, symmetric = T, n = 100)},
                error = function(e) {
                        message(paste('Day', i, 
                                      'does not have participants onset in every age group',
                                      sep = ' '))
                        cnt_m <- na_mat
                }
        )
        
        cnt_mr <- Reduce("+", lapply(cnt_m$matrices, function(x) {x$matrix})) / length(cnt_m$matrices)
        
        # get mean contact over time
        boot_matrix <- lapply(cnt_m$matrices, function(x) {x$matrix/2})
        boot_vector <- tryCatch(
                expr = {sapply(boot_matrix, max_eigen)}, 
                error = function(e) {
                        message(paste('Day', i, 
                                      'does not have contacts onset in every age group',
                                      sep = ' '))
                        NA
                }
        )
        boot_df[, paste0(i)] <- boot_vector
        # get mean contact by age group over time
        age_ls <- lapply(boot_matrix, rowSums)
        for(r in seq_along(age_ls)) {
                age_df[r, ] <- age_ls[[r]]
        }
        contact_by_age <- age_df %>% 
                pivot_longer(
                        cols = 1:7,
                        names_to = 'age_group',
                        values_to = 'boot_contact') %>% 
                group_by(age_group) %>% 
                summarise(mean_contact = mean(boot_contact),
                          stddev       = sd(boot_contact)) %>% 
                mutate(error = qnorm(0.975)*stddev/sqrt(100),
                       upper = mean_contact + error,
                       lower = mean_contact - error,
                       days = as.numeric(paste0(i)))
        contact_by_age_ls[[i+4]] <- contact_by_age
}


# data.frame for phased plots
boot_df <- boot_df[, -1]
all_scenario_overall <- boot_df %>% 
        pivot_longer(
                cols = 1:21,
                names_to = 'days',
                values_to = 'boot_contact') %>% 
        group_by(days) %>% 
        summarise(mean_contact = mean(boot_contact),
                  stddev       = sd(boot_contact)) %>% 
        mutate(error = qnorm(0.975)*stddev/sqrt(100),
               upper = mean_contact + error,
               lower = mean_contact - error) %>% 
        mutate(days = as.numeric(days),
               age_group = 'all') %>% 
        select(age_group, 2:6, days)



# qz school contact over time ----------------------------------------------------

# population of age group for qz contact over time
pop <- read.xlsx('data/pop.xlsx', sheet = 'school_over_time')
pop <- pop %>% 
        mutate(across(.cols = everything(),
                      .fns = as.integer))
boot_df <- data.frame(temp = 1:100)
age_df <- data.frame(matrix(nrow = 100, ncol = 4))
contact_by_age_ls <- list()
set.seed(10)
# rolling window: 7 days
for (i in -3:24) {
        # select contacts in this rolling window
        qz_rolling_cc <- qz_close_contact[qz_close_contact$day_since_outbreak %in% c(seq(i-3, i+3,1)), ]
        # prepare participants and contacts
        participants <- qz_rolling_cc %>% 
                distinct(part_id, .keep_all = T) %>% 
                select(part_id, part_age)
        participants <- participants %>% 
                mutate(across(.cols = part_age,
                              .fns  = as.integer))
        contacts <- qz_rolling_cc %>% 
                filter(part_job == 1) %>% 
                select(cont_id, cnt_age_exact, part_id)
        contacts <- contacts %>% 
                mutate(across(.cols = cnt_age_exact,
                              .fns  = as.integer))
        contact_pairs <- list('participants' = participants, 'contacts' = contacts)
        qz_rolling_cc <- survey(contact_pairs$participants, contact_pairs$contacts) 
        
        cnt_m <- tryCatch(
                expr = {contact_matrix(qz_rolling_cc, age.limits = c(0,10,15,20), 
                                       survey.pop = pop, symmetric = T, n = 100)},
                error = function(e) {
                        message(paste('Day', i, 
                                      ' does not have participants onset in every age group',
                                      sep = ' '))
                        cnt_m <- na_mat
                }
                
        ) 
        
        cnt_mr <- Reduce("+", lapply(cnt_m$matrices, function(x) {x$matrix})) / length(cnt_m$matrices)
        
        # 
        boot_matrix <- lapply(cnt_m$matrices, function(x) {x$matrix/2})
        boot_vector <- tryCatch(
                expr = {sapply(boot_matrix, max_eigen)},
                error = function(e) {
                        message(paste('Day', i, 
                                      'does not have contacts in every age group',
                                      sep = ' '))
                        NA
                }
        )
        boot_df[, paste0(i)] <- boot_vector
        # 
        age_ls <- lapply(boot_matrix, rowSums)
        for(r in seq_along(age_ls)) {
                age_df[r, ] <- age_ls[[r]]
        }
        contact_by_age <- age_df %>% 
                pivot_longer(
                        cols = 1:4,
                        names_to = 'age_group',
                        values_to = 'boot_contact') %>% 
                group_by(age_group) %>% 
                summarise(mean_contact = mean(boot_contact),
                          stddev       = sd(boot_contact)) %>% 
                mutate(error = qnorm(0.975)*stddev/sqrt(100),
                       upper = mean_contact + error,
                       lower = mean_contact - error,
                       days = as.numeric(paste0(i)))
        contact_by_age_ls[[i+4]] <- contact_by_age
}


# data.frame for phased plots
boot_df <- boot_df[, -1]
school_overall <- boot_df %>% 
        select(where(is.numeric)) %>% 
        pivot_longer(
                cols = 1:20,
                names_to = 'days',
                values_to = 'boot_contact') %>% 
        group_by(days) %>% 
        summarise(mean_contact = mean(boot_contact),
                  stddev       = sd(boot_contact)) %>% 
        mutate(error = qnorm(0.975)*stddev/sqrt(100),
               upper = mean_contact + error,
               lower = mean_contact - error) %>% 
        mutate(days = as.numeric(days),
               age_group = 'all') %>% 
        select(age_group, 2:6, days)

# qz workplace contact over time ----------------------------------------------------

# population of age group for qz contact over time
pop <- read.xlsx('data/pop.xlsx', sheet = 'work_over_time')
pop <- pop %>% 
        mutate(across(.cols = everything(),
                      .fns = as.integer))
boot_df <- data.frame(temp = 1:100)
age_df <- data.frame(matrix(nrow = 100, ncol = 6))
contact_by_age_ls <- list()
set.seed(10)
# rolling window: 7 days
for (i in -3:24) {
        # select contacts in this rolling window
        qz_rolling_cc <- qz_close_contact[qz_close_contact$day_since_outbreak %in% c(seq(i-3, i+3,1)), ]
        # prepare participants and contacts
        participants <- qz_rolling_cc %>% 
                distinct(part_id, .keep_all = T) %>% 
                select(part_id, part_age)
        participants <- participants %>% 
                mutate(across(.cols = part_age,
                              .fns  = as.integer))
        contacts <- qz_rolling_cc %>% 
                filter(part_job == 2) %>% 
                select(cont_id, cnt_age_exact, part_id)
        contacts <- contacts %>% 
                mutate(across(.cols = cnt_age_exact,
                              .fns  = as.integer))
        contact_pairs <- list('participants' = participants, 'contacts' = contacts)
        qz_rolling_cc <- survey(contact_pairs$participants, contact_pairs$contacts) 
        
        cnt_m <- tryCatch(
                expr = {contact_matrix(qz_rolling_cc, 
                                       age.limits = c(0, 20, 30, 40, 50, 60),
                                       survey.pop = pop, symmetric = T, n = 100,
                                       missing.participant.age = 'remove')},
                error = function(e) {
                        message(paste('Day', i, 
                                      'does not have participants onset in every age group',
                                      sep = ' '))
                        cnt_m <- na_mat
                }
        ) 
        
        cnt_mr <- Reduce("+", lapply(cnt_m$matrices, function(x) {x$matrix})) / length(cnt_m$matrices)
        
        # 
        boot_matrix <- lapply(cnt_m$matrices, function(x) {x$matrix/2})
        boot_vector <- tryCatch(
                expr = {sapply(boot_matrix, max_eigen)},
                error = function(e) {
                        message(paste('Day', i, 
                                      'does not have contacts in every age group',
                                      sep = ' '))
                        NA
                }
        )
        boot_df[, paste0(i)] <- boot_vector
        # 
        age_ls <- lapply(boot_matrix, rowSums)
        for(r in seq_along(age_ls)) {
                age_df[r, ] <- age_ls[[r]]
        }
        contact_by_age <- age_df %>% 
                pivot_longer(
                        cols = 1:6,
                        names_to = 'age_group',
                        values_to = 'boot_contact') %>% 
                group_by(age_group) %>% 
                summarise(mean_contact = mean(boot_contact),
                          stddev       = sd(boot_contact)) %>% 
                mutate(error = qnorm(0.975)*stddev/sqrt(100),
                       upper = mean_contact + error,
                       lower = mean_contact - error,
                       days = as.numeric(paste0(i)))
        contact_by_age_ls[[i+4]] <- contact_by_age
}



# data.frame for phased plots
boot_df <- boot_df[, -1]
work_overall <- boot_df %>% 
        pivot_longer(
                cols = 1:21,
                names_to = 'days',
                values_to = 'boot_contact') %>% 
        group_by(days) %>% 
        summarise(mean_contact = mean(boot_contact),
                  stddev       = sd(boot_contact)) %>% 
        mutate(error = qnorm(0.975)*stddev/sqrt(100),
               upper = mean_contact + error,
               lower = mean_contact - error) %>% 
        mutate(days = as.numeric(days),
               age_group = 'all') %>% 
        select(age_group, 2:6, days)

# qz household contact over time ----------------------------------------------------

# population of age group for qz contact over time
pop <- read.xlsx('data/pop.xlsx', sheet = 'home_over_time')
pop <- pop %>% 
        mutate(across(.cols = everything(),
                      .fns = as.integer))
boot_df <- data.frame(temp = 1:100)
age_df <- data.frame(matrix(nrow = 100, ncol = 6))
contact_by_age_ls <- list()
set.seed(10)
# rolling window: 7 days
for (i in -3:24) {
        # select contacts in this rolling window
        qz_rolling_cc <- qz_close_contact[qz_close_contact$day_since_outbreak %in% c(seq(i-3, i+3,1)), ]
        # prepare participants and contacts
        participants <- qz_rolling_cc %>% 
                distinct(part_id, .keep_all = T) %>% 
                select(part_id, part_age)
        participants <- participants %>% 
                mutate(across(.cols = part_age,
                              .fns  = as.integer))
        contacts <- qz_rolling_cc %>% 
                filter(part_job == 3) %>% 
                select(cont_id, cnt_age_exact, part_id)
        contacts <- contacts %>% 
                mutate(across(.cols = cnt_age_exact,
                              .fns  = as.integer))
        contact_pairs <- list('participants' = participants, 'contacts' = contacts)
        qz_rolling_cc <- survey(contact_pairs$participants, contact_pairs$contacts) 
        
        cnt_m <- tryCatch(
                expr = {contact_matrix(qz_rolling_cc, age.limits = c(0, 20, 30, 40, 50, 60), 
                                       survey.pop = pop, symmetric = T, n = 100)},
                error = function(e) {
                        message(paste('Day', i, 
                                      'does not have participants onset in every age group',
                                      sep = ' '))
                        cnt_m <- na_mat
                }
        ) 
        
        cnt_mr <- Reduce("+", lapply(cnt_m$matrices, function(x) {x$matrix})) / length(cnt_m$matrices)
        
        # 
        boot_matrix <- lapply(cnt_m$matrices, function(x) {x$matrix/2})
        boot_vector <- tryCatch(
                expr = {sapply(boot_matrix, max_eigen)},
                error = function(e) {
                        message(paste('Day', i, 
                                      'does not have contacts in every age group',
                                      sep = ' '))
                        NA
                }
        )
        boot_df[, paste0(i)] <- boot_vector
        # 
        age_ls <- lapply(boot_matrix, rowSums)
        for(r in seq_along(age_ls)) {
                age_df[r, ] <- age_ls[[r]]
        }
        contact_by_age <- age_df %>% 
                pivot_longer(
                        cols = 1:6,
                        names_to = 'age_group',
                        values_to = 'boot_contact') %>% 
                group_by(age_group) %>% 
                summarise(mean_contact = mean(boot_contact),
                          stddev       = sd(boot_contact)) %>% 
                mutate(error = qnorm(0.975)*stddev/sqrt(100),
                       upper = mean_contact + error,
                       lower = mean_contact - error,
                       days = as.numeric(paste0(i)))
        contact_by_age_ls[[i+4]] <- contact_by_age
}


# data.frame for phased plots
boot_df <- boot_df[, -1]
home_overall <- boot_df %>% 
        pivot_longer(
                cols = 1:21,
                names_to = 'days',
                values_to = 'boot_contact') %>% 
        group_by(days) %>% 
        summarise(mean_contact = mean(boot_contact),
                  stddev       = sd(boot_contact)) %>% 
        mutate(error = qnorm(0.975)*stddev/sqrt(100),
               upper = mean_contact + error,
               lower = mean_contact - error) %>% 
        mutate(days = as.numeric(days),
               age_group = 'all') %>% 
        select(age_group, 2:6, days)

# rio::export(ls(pattern = '.*data|.*overall'), file = 'mean_contact_ls.Rdata')

# Plot contact over time by scenario and annotate by interventions--------------------------------------

# ALL
all_phased_p <- all_scenario_overall %>% 
        ggplot(aes(x = days, y = mean_contact))+
        geom_line(color = '#771434FF', size = 0.5)+
        geom_ribbon(aes(ymin = lower, ymax = upper), fill = '#D2A6B4FF', alpha = 0.6)+
        scale_x_continuous(breaks = seq(0,20,5))+
        scale_y_continuous(limits = c(0,42), breaks = seq(0,40,10), labels = seq(0,40,10))+
        geom_segment(aes(x = 0, y = 40, xend = 0, yend = find_yend(all_scenario_overall, 0)),
                     arrow = arrow(length = unit(0.2, "cm"), type = 'closed'),
                     color = 'grey')+
        geom_segment(aes(x = 1, y = 35, xend = 1, yend = find_yend(all_scenario_overall, 1)),
                     arrow = arrow(length = unit(0.2, "cm"), type = 'closed'),
                     color = 'grey')+
        geom_segment(aes(x = 2, y = 30, xend = 2, yend = find_yend(all_scenario_overall, 2)),
                     arrow = arrow(length = unit(0.2, "cm"), type = 'closed'),
                     color = 'grey', alpha = 0.7)+
        geom_segment(aes(x = 9, y = 40, xend = 9, yend = find_yend(all_scenario_overall, 9)),
                     arrow = arrow(length = unit(0.2, "cm"), type = 'closed'),
                     color = 'grey')+
        geom_segment(aes(x = 6, y = 25, xend = 6, yend = find_yend(all_scenario_overall, 6)),
                     arrow = arrow(length = unit(0.2, "cm"), type = 'closed'),
                     color = 'grey')+
        annotate('text', x = 0.2, y = 39, size = 3,
                 label = 'Increasing lockdown zones\nSuspension of public events',hjust = 0)+
        annotate('text', x = 1.2, y = 34, size = 3,
                 label = 'Closing schools',hjust = 0)+
        annotate('text', x = 2.2, y = 29, size = 3,
                 label = 'Traffic restriction',hjust = 0)+
        annotate('text', x = 9.2, y = 39, size = 3,
                 label = 'Phased re-opening of schools',hjust = 0)+
        annotate('text', x = 6.2, y = 23, size = 3,
                 label = 'Restricting citizens from\nleaving downtown',hjust = 0)

# School
s_phased_p <-  school_overall %>% 
        ggplot(aes(x = days, y = mean_contact))+
        geom_line(color = '#771434FF', size = 0.5)+
        geom_ribbon(aes(ymin = lower, ymax = upper), fill = '#D2A6B4FF', alpha = 0.6)+
        scale_x_continuous(limits = c(0,20), breaks = seq(0, 20,5))+
        scale_y_continuous(limits = c(0,14), breaks = seq(0,12,4))+
        geom_segment(aes(x = 1, y = 14, xend = 1, yend = find_yend(school_overall, 1)),
                     arrow = arrow(length = unit(0.2, "cm"), type = 'closed'),
                     color = 'grey')+
        geom_segment(aes(x = 9, y = 14, xend = 9, yend = find_yend(school_overall, 9)),
                     arrow = arrow(length = unit(0.2, "cm"), type = 'closed'),
                     color = 'grey')+
        annotate('text', x = 1.2, y = 13, size = 3,
                 label = 'Closing schools',hjust = 0)+
        annotate('text', x = 9.2, y = 13, size = 3,
                 label = 'Phased re-opening of schools',hjust = 0)

# Workplace
w_phased_p <- work_overall %>% 
        ggplot(aes(x = days, y = mean_contact))+
        geom_line(color = '#771434FF', size = 0.5)+
        geom_ribbon(aes(ymin = lower, ymax = upper), fill = '#D2A6B4FF', alpha = 0.6)+
        scale_x_continuous(limits = c(0,20), breaks = seq(0, 20,5))+
        scale_y_continuous(limits = c(0,32), breaks = seq(0,30,10))+
        geom_segment(aes(x = 0, y = 30, xend = 0, yend = find_yend(work_overall, 0)),
                     arrow = arrow(length = unit(0.2, "cm"), type = 'closed'),
                     color = 'grey')+
        geom_segment(aes(x = 2, y = 25, xend = 2, yend = find_yend(work_overall, 2)),
                     arrow = arrow(length = unit(0.2, "cm"), type = 'closed'),
                     color = 'grey')+
        annotate('text', x = 0.2, y = 29, size = 3,
                 label = 'Increasing lockdown zones\nSuspension of public events',hjust = 0)+
        annotate('text', x = 2.2, y = 24, size = 3,
                 label = 'Traffic restriction',hjust = 0)

# Household
h_phased_p <- home_overall %>% 
        ggplot(aes(x = days, y = mean_contact))+
        geom_line(color = '#771434FF', size = 0.5)+
        geom_ribbon(aes(ymin = lower, ymax = upper), fill = '#D2A6B4FF', alpha = 0.6)+
        scale_x_continuous(limits = c(0,20), breaks = seq(0, 20,5))+
        scale_y_continuous(limits = c(0,16), breaks = seq(0,16,4))+
        geom_segment(aes(x = 0, y = 16, xend = 0, yend = find_yend(home_overall, 0)),
                     arrow = arrow(length = unit(0.2, "cm"), type = 'closed'),
                     color = 'grey')+
        geom_segment(aes(x = 1, y = 12, xend = 1, yend = find_yend(home_overall, 1)),
                     arrow = arrow(length = unit(0.2, "cm"), type = 'closed'),
                     color = 'grey')+
        geom_segment(aes(x = 2, y = 8, xend = 2, yend = find_yend(home_overall, 2)),
                     arrow = arrow(length = unit(0.2, "cm"), type = 'closed'),
                     color = 'grey')+
        geom_segment(aes(x = 6, y = 12, xend = 6, yend = c(find_yend(home_overall, 6))),
                     arrow = arrow(length = unit(0.2, "cm"), type = 'closed'),
                     color = 'grey')+
        annotate('text', x = 0.2, y = 15, size = 3,
                 label = 'Increasing lockdown zones\nSuspension of public events',hjust = 0)+
        annotate('text', x = 1.2, y = 11, size = 3,
                 label = 'Closing schools',hjust = 0)+
        annotate('text', x = 2.2, y = 7, size = 3,
                 label = 'Traffic restriction',hjust = 0)+
        annotate('text', x = 6.5, y = 12, size = 3,
                 label = 'Restricting citizens from\nleaving downtown',hjust = 0)

# patchwork ---------------------------------------------------------------

font_add(family = 'Times New Roman', regular = 'Times New Roman/Times New Roman.ttf')
showtext_auto()

# plot mean contacts in all settings by age-group over time
all_by_age_data <- bind_rows(contact_by_age_ls)
all_by_age_data$age_group <- as.factor(all_by_age_data$age_group)
facet_label <- c('0-9', '10-19', '20-29', '30-39', '40-49', '50-59', '60+') 
facet_hide <- levels(all_by_age_data$age_group)
names(facet_label) <- facet_hide
all_by_age_plot <- all_by_age_data %>% 
        mutate(days = as.numeric(days),
               scenario = 'All') %>%
        ggplot(aes(x = days, y = mean_contact))+
        geom_line(color = '#771434FF', size = 0.5)+
        geom_ribbon(aes(ymin = lower, ymax = upper), fill = '#D2A6B4FF', alpha = 0.6)+
        scale_x_continuous(breaks = seq(0,20,5))+
        scale_y_continuous(limits = c(0,42), breaks = seq(0,40,10), 
                           labels = seq(0,40,10))+
        facet_grid(scenario~age_group, 
                   labeller = labeller(age_group = facet_label))

# plot mean contacts in school by age-group over time
school_by_age_data <- bind_rows(contact_by_age_ls)
school_by_age_data$age_group <- as.factor(school_by_age_data$age_group)
facet_label <- c('0-9', '10-14', '15-20', '20+') 
facet_hide <- levels(school_by_age_data$age_group)
names(facet_label) <- facet_hide
school_by_age_plot <- school_by_age_data %>% 
        mutate(days = as.numeric(days),
               scenario = 'School') %>%
        ggplot(aes(x = days, y = mean_contact))+
        geom_line(color = '#771434FF', size = 0.5)+
        geom_ribbon(aes(ymin = lower, ymax = upper), fill = '#D2A6B4FF', alpha = 0.6)+
        facet_grid(scenario~age_group, 
                   labeller = labeller(age_group = facet_label))+
        scale_x_continuous(limits = c(0,20), breaks = seq(0, 20,5))+
        scale_y_continuous(limits = c(0,14), breaks = seq(0,12,4))

# plot mean contacts in workplace by age-group over time
work_by_age_data <- bind_rows(contact_by_age_ls)
work_by_age_data$age_group <- as.factor(work_by_age_data$age_group)
facet_label <- c('0-19', '20-29', '30-39', '40-49', '50-59', '60+') 
facet_hide <- levels(work_by_age_data$age_group)
names(facet_label) <- facet_hide
work_by_age_plot <- work_by_age_data %>% 
        mutate(days = as.numeric(days),
               scenario = 'Workplace') %>%
        ggplot(aes(x = days, y = mean_contact))+
        geom_line(color = '#771434FF', size = 0.5)+
        geom_ribbon(aes(ymin = lower, ymax = upper), fill = '#D2A6B4FF', alpha = 0.6)+
        facet_grid(scenario~age_group, 
                   labeller = labeller(age_group = facet_label))+
        scale_x_continuous(limits = c(0,20), breaks = seq(0, 20,5))+
        scale_y_continuous(limits = c(0,32), breaks = seq(0,30,10))
mcnt_by_age <- school_by_age_plot + work_by_age_plot + home_by_age_plot+ all_by_age_plot+
        plot_layout(nrow = 4)&
        theme_bw(base_family = 'Times New Roman')+
        theme(strip.background = element_blank(),
              axis.ticks.y = element_blank(),
              axis.text.y = element_blank(),
              axis.text = element_text(color = 'black'))&
        labs(x = NULL,
             y = NULL)

# mean contacts in household by age-group over time
home_by_age_data <- bind_rows(contact_by_age_ls)
home_by_age_data$age_group <- as.factor(home_by_age_data$age_group)
facet_label <- c('0-19', '20-29', '30-39', '40-49', '50-59', '60+') 
facet_hide <- levels(home_by_age_data$age_group)
names(facet_label) <- facet_hide
home_by_age_plot <- home_by_age_data %>% 
        mutate(days = as.numeric(days),
               scenario = 'Household') %>%
        ggplot(aes(x = days, y = mean_contact))+
        geom_line(color = '#771434FF', size = 0.5)+
        geom_ribbon(aes(ymin = lower, ymax = upper), fill = '#D2A6B4FF', alpha = 0.6)+
        facet_grid(scenario~age_group, 
                   labeller = labeller(age_group = facet_label))+
        scale_x_continuous(limits = c(0,20), breaks = seq(0, 20,5))+
        scale_y_continuous(limits = c(0,16), breaks = seq(0,16,4))
mcnt_phased <- s_phased_p + w_phased_p + h_phased_p + all_phased_p+
        plot_layout(nrow = 4)&
        theme_bw(base_family = 'Times New Roman')+
        theme(strip.background = element_blank(),
              panel.grid = element_blank(),
              axis.text = element_text(color = 'black'),
              plot.title = element_text(size = 10, hjust = 0.5))&
        annotate('rect', xmin = 6-0.5, ymin = -Inf,
                 xmax = 7+0.5, ymax = Inf, fill = "grey", alpha = 0.25) &
        annotate('rect', xmin = 13-0.5, ymin = -Inf,
                 xmax = 14+0.5, ymax = Inf, fill = "grey", alpha = 0.25) &
        annotate('text', x = 6.5, y = 0,
                 label = 'Weekend', size = 3, vjust = 0)&
        annotate('text', x = 13.5, y = 0,
                 label = 'Weekend', size = 3, vjust = 0)&
        labs(x = NULL,
             y = NULL,
             title = 'Overall')
# (mcnt_phased | mcnt_by_age) + plot_layout(ncol = 2, widths = c(1,2))+
#         plot_annotation(caption = 'Days since outbreak',
#                         theme = theme(plot.caption = element_text(hjust = 0.5, size = 10)))


add_global_label(((mcnt_phased | mcnt_by_age) + 
                          plot_layout(ncol = 2, widths = c(1,2))+
                          plot_annotation(caption = 'Days since outbreak',
                                          theme = theme(plot.caption = element_text(hjust = 0.5, 
                                                                                    size = 11)))),
                 Ylab = "Mean Contacts",
                 Ygap = 0.02)




# ggsave('plot_mcnt_over_time.pdf', height = 10, width = 15,dpi = 300)


# 每日直接除得的接触度
# cnt_facet_days <- qz_close_contact %>% 
#         group_by(day_since_outbreak) %>% 
#         summarise(cnt_per_day = length(unique(raw_cont_id)) / length(unique(raw_part_id)) / 2)
# ggplot(data = cnt_facet_days, aes(x = day_since_outbreak, y = cnt_per_day))+
#         geom_line()+
#         geom_point()
# # 每日的SAR
# sar_facet_days <- qz_close_contact %>% 
#         group_by(day_since_outbreak) %>% 
#         summarise(sar_per_day = sum(is_case) / length(unique(raw_cont_id)))
# sar_facet_days %>% filter(day_since_outbreak %in% 0:20) %>% 
#         ggplot(aes(x = day_since_outbreak, y = sar_per_day))+
#         geom_line()+
#         geom_point()

