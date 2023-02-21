pacman::p_load(
        openxlsx,
        tidyverse,
        stringr,
        socialmixr,
        scales
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

