source('../data/setup.R')

# Load data
detail_stats_df <- data.frame(read.csv(file = '../data/csv/detail_stats.csv')) %>%
  filter(!is.na(in_degree) & !is.nan(in_degree)) %>%
  select(name, theory, sloc)

lint_severity_df <- data.frame(read.csv(file = '../data/csv/lint_count.csv')) %>%
  transmute(lints = info + warn + error, theory = theory)

entity_metrics_df <- data.frame(read.csv(file = '../data/csv/entity_metrics.csv.gz'))

position_metrics_df <- data.frame(read.csv(file = '../data/csv/pos_metrics.csv'))

entry_fractal_dim_df <- data.frame(read.csv(file = '../data/csv/fractal_dimension.csv')) %>%
  group_by(name) %>%
  group_modify(~.x %>% filter(n() > 2)) %>%
  group_modify(~data.frame(d_l = d_l(.x, the(.y)), d_l_lin = d_l_lin(.x, the(.y)))) %>%
  ungroup()

theory_fractal_dim_df <- data.frame(read.csv(file = '../data/csv/theory_fractal_dimension.csv')) %>%
  group_by(theory) %>%
  group_modify(~.x %>% filter(n() > 2)) %>%
  group_modify(~data.frame(d_l = d_l(.x, the(.y)), d_l_lin = d_l_lin(.x, the(.y)))) %>%
  ungroup()

theory_metrics_df <- data.frame(read.csv(file = '../data/csv/theory_metrics.csv'))

# Theory and entity density
theory_entry_df <- detail_stats_df %>%
  select(name, theory)

theory_lint_frequency_df <- detail_stats_df %>%
  left_join(lint_severity_df, by = 'theory') %>% # todo what happens for inner join
  mutate(lints = replace_na(lints, 0)) %>%
  mutate(frequency = lints / sloc)

entry_lint_frequency_df <- theory_lint_frequency_df %>%
  group_by(name) %>%
  summarise(frequency = sum(lints) / sum(sloc), .groups = 'drop')

# Aggregation
agg_metrics <- function(lint_frequency_df, metrics_df, by, by2) {
  mean_df <- metrics_df %>%
    group_by({ { by } }) %>%
    summarise(in_degree = mean(in_degree), out_degree = mean(out_degree), eigenvector = mean(eigenvector),
              closeness = mean(closeness), clustering = mean(clustering), betweenness = mean(betweenness), .groups = 'drop')
  total_df <- metrics_df %>%
    group_by({ { by } }) %>%
    summarise(in_degree = sum(in_degree), out_degree = sum(out_degree), eigenvector = sum(eigenvector),
              closeness = sum(closeness), clustering = sum(clustering), betweenness = sum(betweenness), .groups = 'drop')
  max_df <- metrics_df %>%
    group_by({ { by } }) %>%
    summarise(in_degree = max(in_degree), out_degree = max(out_degree), eigenvector = max(eigenvector),
              closeness = max(closeness), clustering = max(clustering), betweenness = max(betweenness), .groups = 'drop')

  bind_rows(
    lint_frequency_df %>%
      select({ { by } }, frequency) %>%
      left_join(mean_df, by = by2) %>%
      select(-{ { by } }) %>%
      dens_cor() %>%
      mutate(score = 'mean'),
    lint_frequency_df %>%
      select({ { by } }, frequency) %>%
      left_join(total_df, by = by2) %>%
      select(-{ { by } }) %>%
      dens_cor() %>%
      mutate(score = 'total'),
    lint_frequency_df %>%
      select({ { by } }, frequency) %>%
      left_join(max_df, by = by2) %>%
      select(-{ { by } }) %>%
      dens_cor() %>%
      mutate(score = 'max'))
}

entity_entry_metrics_df <- theory_lint_frequency_df %>%
  inner_join(entity_metrics_df, by = 'theory') %>%
  select(-theory, -sloc, -lints)

theory_lint_freq_df <- theory_lint_frequency_df %>%
  select(-name, -sloc, -lints)

metrics_lint_freq_cor_df <- bind_rows(
  agg_metrics(entry_lint_frequency_df, entity_entry_metrics_df, name, 'name') %>% mutate(granularity = 'entry', nodes = 'entity'),
  agg_metrics(entry_lint_frequency_df, position_metrics_df %>% select(-theory), name, 'name') %>% mutate(granularity = 'entry', nodes = 'position'),
  agg_metrics(theory_lint_frequency_df, entity_metrics_df, theory, 'theory') %>% mutate(granularity = 'theory', nodes = 'entity'),
  agg_metrics(theory_lint_frequency_df, position_metrics_df %>% select(-name), theory, 'theory') %>% mutate(granularity = 'theory', nodes = 'position'),
  theory_lint_freq_df %>%
    left_join(theory_metrics_df, by = 'theory') %>%
    select(-theory, -name, -nodes, -edges) %>%
    dens_cor() %>%
    mutate(granularity = 'theory', nodes = 'theory', score = ''),
  theory_lint_freq_df %>%
    left_join(entity_metrics_df, by = 'theory') %>%
    select(-theory) %>%
    dens_cor() %>%
    mutate(granularity = 'entity', nodes = 'entity', score = ''),
  theory_lint_freq_df %>%
    left_join(position_metrics_df, by = 'theory') %>%
    select(-theory, -name, -pos) %>%
    dens_cor() %>%
    mutate(granularity = 'position', nodes = 'position', score = '')
)

metrics_lint_freq_cor_df %>%
  rowwise() %>%
  mutate_if(is.numeric, spearman_val2, 0.25) %>%
  write.csv('../data/csv/gen/centrality_metrics_cor.csv', row.names = FALSE)

metrics_lint_freq_cor_df %>%
  write.csv('../data/csv/gen/centrality_metrics_cor_num.csv', row.names = FALSE, na = '')

# Fractal dimension
fractal_dim_agg_df <- theory_fractal_dim_df %>%
  inner_join(theory_entry_df, by = 'theory') %>%
  group_by(name)

fractal_dim_df <- bind_rows(
  theory_lint_frequency_df %>%
    inner_join(theory_fractal_dim_df, by = 'theory') %>%
    select(-sloc, -lints, -theory, -name) %>%
    dens_cor() %>%
    mutate(score = 'theory', aggregation = ''),
  fractal_dim_agg_df %>%
    summarise(d_l = max(d_l), d_l_lin = max(d_l_lin), .groups = 'drop') %>%
    inner_join(entry_lint_frequency_df, by = 'name') %>%
    select(-name) %>%
    dens_cor() %>%
    mutate(score = 'entry', aggregation = 'max'),
  fractal_dim_agg_df %>%
    summarise(d_l = mean(d_l), d_l_lin = mean(d_l_lin), .groups = 'drop') %>%
    inner_join(entry_lint_frequency_df, by = 'name') %>%
    select(-name) %>%
    dens_cor() %>%
    mutate(score = 'entry', aggregation = 'mean'),
  fractal_dim_agg_df %>%
    summarise(d_l = sum(d_l), d_l_lin = sum(d_l_lin), .groups = 'drop') %>%
    inner_join(entry_lint_frequency_df, by = 'name') %>%
    select(-name) %>%
    dens_cor() %>%
    mutate(score = 'entry', aggregation = 'total'),
  entry_fractal_dim_df %>%
    inner_join(entry_lint_frequency_df, by = 'name') %>%
    select(-name) %>%
    dens_cor() %>%
    mutate(score = 'entry', aggregation = ''))

sloc_df <- bind_rows(
  theory_lint_frequency_df %>%
    select(frequency, sloc) %>%
    dens_cor() %>%
    mutate(score = 'theory', aggregation = ''),
  theory_lint_frequency_df %>%
    group_by(name) %>%
    summarise(frequency = sum(lints) / sum(sloc), sloc = max(sloc)) %>%
    select(-name) %>%
    dens_cor() %>%
    mutate(score = 'entry', aggregation = 'max'),
  theory_lint_frequency_df %>%
    group_by(name) %>%
    summarise(frequency = sum(lints) / sum(sloc), sloc = mean(sloc)) %>%
    select(-name) %>%
    dens_cor() %>%
    mutate(score = 'entry', aggregation = 'mean'),
  theory_lint_frequency_df %>%
    group_by(name) %>%
    summarise(frequency = sum(lints) / sum(sloc), sloc = sum(sloc)) %>%
    select(-name) %>%
    dens_cor() %>%
    mutate(score = 'entry', aggregation = 'total'))

fractal_dim_df %>%
  full_join(sloc_df, by = c('score', 'aggregation')) %>%
  replace_na(list('')) %>%
  rowwise() %>%
  mutate_if(is.numeric, spearman_val2, 0.25) %>%
  write.csv('../data/csv/gen/other_metrics_cor.csv', row.names = FALSE, na = '--')

fractal_dim_df %>%
  full_join(sloc_df, by = c('score', 'aggregation')) %>%
  write.csv('../data/csv/gen/other_metrics_cor_num.csv', row.names = FALSE, na = '')