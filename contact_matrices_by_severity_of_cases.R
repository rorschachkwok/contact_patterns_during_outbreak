pacman::p_load(
        openxlsx,
        tidyverse,
        epitrix,
        stringr,
        socialmixr,
        scales
)

max_eigen <- function(m) {
        eig <- eigen(m)
        max_eig <- eig$values[[1]]
        max_eig
}
qz_close_contact <- readRDS('data/qz_close_contact.rds')
qz_close_contact <- qz_close_contact %>% 
        mutate(across(.cols = contains('age'), 
                      .fns  = as.integer))

participants <- qz_close_contact %>% 
        distinct(part_id, .keep_all = T) %>% 
        select(part_id, part_age) 

contacts <- qz_close_contact %>% 
        select(cont_id, cnt_age_exact, part_id)

# import population data
pop <- read.xlsx('data/pop.xlsx', sheet = 2)
pop <- pop %>% 
        mutate(across(.cols = everything(),
                      .fns = as.integer))

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

