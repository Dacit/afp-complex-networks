source("../data/metrics.R")

metrics_f1 <- avg_score_df %>%
  filter(n <= 100 & type %in% c("concept", "concept_pos", "theorems", "theorems_thm")) %>%
  mutate(type = recode(type,
                       'concept' = 'Concepts',
                       'concept_pos' = 'Defining Concept Positions',
                       'theorems' = 'Theorems',
                       'theorems_thm' = 'Theorems Dependency Graph')) %>%
  mutate(Metric = recode(metric,
                         'betweenness' = '$C_p$',
                         'closeness' = '$C_\\textrm{dist}$',
                         'clustering' = '$CC$',
                         'eigenvector' = '$C_\\lambda$',
                         'in_degree' = '$k_\\textrm{in}$'))

plot((
  ggplot(metrics_f1, aes(x = n, y = f1, pch = Metric, color = Metric)) +
    geom_point(data = metrics_f1 %>%
      group_by(type, metric) %>%
      slice(which.max(f1))) +
    ylab('$F_1$-Score') +
    xlab('Top-$N$') +
    geom_line() +
    facet_wrap(~type) +
    theme_pubr(legend = 'right')) %>%
       change_palette(palette = 'lancet')
)