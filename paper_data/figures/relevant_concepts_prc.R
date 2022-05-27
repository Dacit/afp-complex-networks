source("../data/metrics.R")

relevant_concepts_prc <- avg_score_df %>%
  filter(type == 'concept_pos') %>%
  mutate(Metric = recode(metric,
                         'betweenness' = '$C_p$',
                         'closeness' = '$C_\\textrm{dist}$',
                         'clustering' = '$CC$',
                         'eigenvector' = '$C_\\lambda$',
                         'in_degree' = '$k_\\textrm{in}$'))

library(pals)
relevant_concepts_prc_label <- relevant_concepts_prc %>%
  filter(metric == 'in_degree' & (n %in% c(8, 14)))

plot(
  (relevant_concepts_prc %>%
    ggplot(aes(x = rec, y = pre, label = n, pch = Metric, color = Metric)) +
    geom_line() +
    geom_point(alpha = 0.8) +
    geom_text_repel(
      data = relevant_concepts_prc_label,
      aes(label = sprintf('$n = %d$', n)),
      min.segment.length = unit(0, 'lines'),
      nudge_y = 0.1,
      color = 'black',
      segment.color = 'grey',
      family = 'LM Roman 10') +
    xlab('Recall') +
    ylab('Precision') +
    coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
    theme_pubr(legend = 'right')) %>%
    change_palette(palette = 'lancet')
)