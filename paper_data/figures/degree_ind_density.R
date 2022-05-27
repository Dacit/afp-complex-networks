source('../data/degree_individual.R')

cdf <- function(obs) {
  m_pl <- displ$new(obs)
  plot(m_pl, draw = F)
}

line <- function(full_data) {
  xmax <- sum(full_data %>% pull(n))
  obs <- full_data %>%
    filter(k > 0) %>%
    expand_obs() %>%
    pull(obs)
  m_pl <- powerlaw$Fit(obs, xmax = xmax, discrete = TRUE)
  cdf <- data.frame(cdf(obs))

  lines2(obs, cdf, m_pl$power_law)
}

set.seed(42)
degree_ind_pl_pref <- degree_ind_res_df %>%
  filter(!is.na(comp)) %>%
  sample_n(size = 20) %>%
  pull(name)

shorten <- function(str, n) {
  if (nchar(str) > n) paste0(substr(str, 1, (n - 2)), '\\textellipsis')
  else str
}

degree_ind_pl_pref_cdfs <- degree_ind_df %>%
  filter(name %in% degree_ind_pl_pref) %>%
  filter(!is.na(degree) & !is.na(count)) %>%
  filter(degree > 0) %>%
  group_by(name) %>%
  group_modify(~.x %>%
    expand_obs() %>%
    pull(obs) %>%
    cdf()) %>%
  rowwise() %>%
  mutate(name_tex = shorten(gsub('_', ' ', name), 12)) %>%
  ungroup()

degree_ind_pld_pref_lines <- degree_ind_df %>%
  filter(name %in% degree_ind_pl_pref) %>%
  filter(!is.na(degree) & !is.na(count)) %>%
  group_by(name) %>%
  group_modify(~.x %>% line()) %>%
  rowwise() %>%
  mutate(name_tex = shorten(gsub('_', ' ', name), 12)) %>%
  ungroup()

plot(
  (
    ggplot(data = degree_ind_pl_pref_cdfs, aes(x = x, y = y)) +
      geom_point(alpha = 0.8, shape = 1) +
      scale_y_log10(labels = trans_format("log10", function(x) sprintf('$10^{%d}$', x))) +
      scale_x_log10(labels = trans_format("log10", math_format(10^.x))) +
      geom_line(data = degree_ind_pld_pref_lines %>% filter(y > 0), colour = "red", size = 0.8) +
      facet_wrap(~name_tex, nrow = 4, ncol = 5) +
      xlab('$k_\\textrm{in}$') +
      ylab('$\\Pr[K\\geq k_\\textrm{in}]$') +
      coord_equal(xlim = c(1, degree_ind_pl_pref_cdfs %>% pull(x) %>% max()), ylim = c(degree_ind_pl_pref_cdfs %>% pull(y) %>% min(), 1)) +
      theme_pubr()) %>%
    change_palette(palette = 'lancet')
)
