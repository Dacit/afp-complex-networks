source('../data/setup.R')

degree_ind_raw_df <- data.frame(read.csv(file = '../data/csv/degree_distribution_individual.csv'))
degree_ind_raw_num <- degree_ind_raw_df %>%
  distinct(name) %>%
  pull(name) %>%
  length()

degree_ind_df <- degree_ind_raw_df %>%
  filter(!is.na(count)) %>%
  group_by(name) %>%
  filter(n() > 10) %>%
  ungroup() %>%
  mutate(k = degree, n = count)
degree_ind_num <- degree_ind_df %>%
  distinct(name) %>%
  pull(name) %>%
  length()

if (fresh) {
  degree_ind_df %>%
    group_by(name) %>%
    group_modify(~.x %>% fit_power_law()) %>%
    write.csv('../data/csv/gen/power_fits.csv', row.names = FALSE)
}
degree_ind_res_df <- read.csv('../data/csv/gen/power_fits.csv') %>% filter(name %in% (degree_ind_df %>% pull(name)))

stats_df <- data.frame(read.csv(file = '../data/csv/stats.csv'))
degree_ind_frac_df <- stats_df %>%
  left_join(degree_ind_res_df, by = 'name') %>%
  mutate(frac = nodes / sum(nodes)) %>%
  filter(!is.na(alpha)) %>%
  mutate(alpha_frac = alpha * frac)

degree_ind_alpha <- degree_ind_frac_df %>%
  pull(alpha_frac) %>%
  sum()
degree_ind_pl_pref_num <- degree_ind_res_df %>%
  filter(comp > 0) %>%
  count()
