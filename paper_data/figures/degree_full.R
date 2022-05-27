source('../data/degree.R')

# Dist
degree_full_sum_deg <- degree_full_df %>%
  filter(k >= degree_full_xmin) %>%
  summarise(total = sum(n)) %>%
  the(total)


degree_full_dist_line_xmax <- 10^((-1 / degree_full_alpha) * log10(zeta(degree_full_alpha, shift = degree_full_xmin) / degree_full_sum_deg))
degree_full_dist_line_xs <- seq(degree_full_xmin, degree_full_dist_line_xmax, length.out = 2)
degree_full_dist_line_ys <- degree_full_sum_deg * ((degree_full_dist_line_xs^(-degree_full_alpha)) / zeta(degree_full_alpha, shift = degree_full_xmin))
degree_full_dist_xmax <- degree_full_df %>% pull(k) %>% max()
degree_full_dist_ymax <- degree_full_df %>%
  filter(n > 0 & k > 0) %>%
  pull(n) %>%
  max()
degree_full_dist_plot <- degree_full_df %>%
  filter(n > 0 & k > 0) %>%
  ggplot(aes(x = k, y = n)) +
  geom_point(alpha = 0.5, shape = 1) +
  coord_equal(ylim = c(1, degree_full_dist_ymax), xlim = c(1, degree_full_dist_xmax)) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  geom_line(data = data.frame(x = degree_full_dist_line_xs, y = degree_full_dist_line_ys), aes(x = x, y = y, color = '1'), size = linesize, linetype = linetype) +
  annotation_logticks() +
  scale_color_manual(values = '#ED0000FF') +
  labs(color = NULL) +
  xlab('$k_\\textrm{in}$') +
  ylab('Frequency') +
  theme_pubr()

# Density
degree_full_ln = dislnorm$new(degree_full_obs_df)
#degree_full_ln$xmin <- estimate_xmin(degree_full_ln)
degree_full_ln$xmin <- 14
degree_full_ln$pars <- estimate_pars(degree_full_ln)

degree_full_pois = dispois$new(degree_full_obs_df)
#degree_full_pois$xmin <- estimate_xmin(degree_full_pois)
degree_full_pois$xmin <- 943
degree_full_pois$pars <- estimate_pars(degree_full_pois)

degree_full_exp = disexp$new(degree_full_obs_df)
#degree_full_exp$xmin <- suppressWarnings(estimate_xmin(degree_full_exp))
degree_full_exp$xmin <- 1
degree_full_exp$pars <- estimate_pars(degree_full_exp)

degree_full_plot_df <- plot(displ$new(degree_full_obs_df), draw = F)
degree_full_density_ymin <- degree_full_plot_df %>% pull(y) %>% min()
degree_full_density_xmax <- degree_full_plot_df %>% pull(x) %>% max()
degree_full_density_plot <- ggplot(degree_full_plot_df) +
  geom_point(aes(x = x, y = y), shape = 1) +
  coord_equal(ylim = c(degree_full_density_ymin, 1), xlim = c(1, degree_full_density_xmax)) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", function(x) sprintf('$10^{%d}$', x))) +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks() +
  geom_line(data = lines2(degree_full_obs_df, degree_full_plot_df, degree_full_pl$power_law), aes(x = x, y = y, color = '1'), size = linesize, linetype = linetype) +
  geom_line(data = lines(degree_full_ln, draw = F) %>% filter(y > 0), aes(x = x, y = y, color = '2'), size = linesize, linetype = linetype) +
  geom_line(data = lines(degree_full_exp, draw = F) %>% filter(y > 0), aes(x = x, y = y, color = '3'), size = linesize, linetype = linetype) +
  geom_line(data = lines(degree_full_pois, draw = F) %>% filter(y > 0), aes(x = x, y = y, color = '4'), size = linesize, linetype = linetype) +
  labs(color = NULL) +
  xlab('$k_\\textrm{in}$') +
  ylab('$\\Pr[K\\geq k_\\textrm{in}]$') +
  scale_color_manual(values = c('#ED0000FF', '#42B540FF', '#0099B4FF', '#925E9FFF'), labels = c('Scale-free', 'Log-normal', 'Exponential', 'Poisson')) +
  theme_pubr()

plot(
  suppressWarnings(
    ggarrange(
      degree_full_dist_plot, degree_full_density_plot, ncol = 2, common.legend = TRUE,
      legend.grob = get_legend(degree_full_density_plot), legend = 'top', align = 'h')))