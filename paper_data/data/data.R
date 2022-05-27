source('../data/degree.R')
source('../data/degree_individual.R')
source('../data/metrics.R')

evaluation_df <- data.frame(read.csv(file = '../data/csv/evaluation.csv'))
stats_df <- data.frame(read.csv(file = '../data/csv/stats.csv'))
entity_metrics_df <- data.frame(read.csv(file = '../data/csv/entity_metrics.csv.gz'))
average_path_length_undir_df <- data.frame(read.csv(file = '../data/csv/average_path_length.csv'))

# Eval stats
num_answers <- evaluation_df %>%
  distinct(name) %>%
  count() %>%
  the()

num_graphs <- stats_df %>%
  filter(nodes > 0 | edges > 0) %>%
  distinct(name) %>%
  count() %>%
  the()
# Graph stats individual
num_nodes <- stats_df %>% pull(nodes) %>% sum()
num_edges <- stats_df %>% pull(edges) %>% sum()
# Graph stats total
num_total_nodes <- degree_full_df %>% pull(n) %>% sum()
num_total_edges <- degree_full_df %>%
  mutate(total = k_in * n) %>%
  pull(total) %>%
  sum()

# Small world
total_nodes <- sum(stats_df$nodes)
stats_ext_df <- stats_df %>%
  rowwise() %>%
  mutate(
    p = if (nodes < 2) 1 else edges / (nodes + ((nodes * (nodes - 1)) / 2)),
    frac = nodes / total_nodes,
    cc = p,
    cc_frac = p * frac,
    l = if (nodes * p <= 1) nodes * p else log(nodes) / log(nodes * p),
    l_frac = l * frac
  )
avg_cc <- mean(entity_metrics_df$clustering)
avg_cc_er <- stats_ext_df %>% pull(cc_frac) %>% sum()
avg_l <- average_path_length_undir_df %>%
  filter(!is.na(avg_length)) %>%
  inner_join(stats_ext_df, by = 'name') %>%
  transmute(avg_l_frac = frac * avg_length) %>%
  pull(avg_l_frac) %>%
  sum()
avg_l_er <- stats_ext_df %>% pull(l_frac) %>% sum()

# Prediction
prediction_thm_best_metric_opt <- avg_score_df %>%
  filter(type == 'theorems' |
           type == 'theorems_thm' |
           type == 'theorems_thm_pos') %>%
  slice(which.max(f1))

prediction_best_metric_opt <- avg_score_df %>%
  filter(type == 'concept_pos' | type == 'concept') %>%
  slice(which.max(f1))

prediction_best_metric_opt2 <- avg_score_df %>%
  filter(type == 'concept_pos' | type == 'concept') %>%
  slice(which.max(f2))

prediction_best_metric_max_pre <- avg_score_df %>%
  filter(type == 'concept_pos' & metric == prediction_best_metric_opt %>% the(metric)) %>%
  filter(pre < 1.0) %>%
  slice(which.max(pre))

# Correlation
metric_correlation_mt <- rcorr(as.matrix(entity_metrics_df %>% select(out_degree, eigenvector, in_degree, closeness, betweenness, clustering)), type = 'spearman')
metric_correlation_max_p <- max(metric_correlation_mt$P, na.rm = TRUE)
metric_correlation_n <- metric_correlation_mt$n[1, 1]
data.frame(metric_correlation_mt$r) %>%
  rownames_to_column() %>%
  rowwise() %>%
  mutate_if(is.numeric, spearman_val, metric_correlation_max_p, 0.001, 0.5) %>%
  arrange(factor(rowname, levels = c('in_degree', 'out_degree', 'clustering', 'eigenvector', 'closeness', 'betweenness'))) %>%
  mutate(rowname = recode(rowname,
                          'betweenness' = '$C_p$',
                          'closeness' = '$C_\\textrm{dist}$',
                          'clustering' = '$CC$',
                          'eigenvector' = '$C_\\lambda$',
                          'in_degree' = '$k_\\textrm{in}$',
                          'out_degree' = '$k_\\textrm{out}$')) %>%
  write.csv(file = '../data/csv/gen/metrics_correlation.csv', row.names = FALSE)

# Lint correlation
centrality_metrics_cor_df <- read.csv(file = '../data/csv/gen/centrality_metrics_cor_num.csv')
other_metrics_cor_df <- read.csv(file = '../data/csv/gen/other_metrics_cor_num.csv')
lint_correlation_opt_val <- centrality_metrics_cor_df %>%
  select_if(is.numeric) %>% replace(is.na(.), 0) %>%
  abs() %>% max()
if (!any(centrality_metrics_cor_df == lint_correlation_opt_val)) lint_correlation_opt_val <- -lint_correlation_opt_val
lint_correlation_d_l_opt_val <- max(
  other_metrics_cor_df %>% slice(which.max(d_l)) %>% the(d_l),
  other_metrics_cor_df %>% slice(which.max(d_l_lin)) %>% the(d_l_lin))
lint_correlation_sloc_opt_val <- other_metrics_cor_df %>% slice(which.max(sloc)) %>% the(sloc)
