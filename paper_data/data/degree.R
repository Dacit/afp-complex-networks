source('../data/setup.R')

degree_full_df <- data.frame(read.csv(file = '../data/csv/indegree_distribution.csv')) %>%
  mutate(k = k_in)

num_full_nodes <- degree_full_df %>% pull(n) %>% sum()

degree_full_obs_df <- degree_full_df %>%
  filter(k > 0) %>%
  expand_obs() %>%
  pull(obs)

if (fresh) {
  degree_full_pl <- powerlaw$Fit(degree_full_obs_df, xmax = num_full_nodes, discrete = TRUE)
  degree_full_xmin <- degree_full_pl$power_law$xmin
  write.csv(degree_full_xmin, '../data/csv/gen/degree_full_xmin', row.names = FALSE)
} else {
  degree_full_xmin <- the(read.csv('../data/csv/gen/degree_full_xmin'))
  degree_full_pl <- powerlaw$Fit(degree_full_obs_df, xmin = degree_full_xmin, xmax = num_full_nodes, discrete = TRUE)
}

degree_full_alpha <- degree_full_pl$power_law$alpha

degree_full_pl_ln_r <- degree_full_pl$distribution_compare('power_law', 'lognormal', normalized_ratio = FALSE)[[1]]

linetype <- 'solid'
linesize <- 0.8