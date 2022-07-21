suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(tibble))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(ggpubr))
suppressPackageStartupMessages(library(poweRlaw))
suppressPackageStartupMessages(library(VGAM))
suppressPackageStartupMessages(library(extrafont))
suppressPackageStartupMessages(library(tikzDevice))
suppressPackageStartupMessages(library(tinytex))
suppressPackageStartupMessages(library(ggrepel))
suppressPackageStartupMessages(library(scales))
suppressPackageStartupMessages(library(Hmisc))
suppressPackageStartupMessages(library(corrplot))
suppressPackageStartupMessages(library(ggcorrplot))
suppressPackageStartupMessages(library(rlang))
suppressPackageStartupMessages(library(reticulate))
suppressPackageStartupMessages(library(gridExtra))
suppressPackageStartupMessages(library(jsonlite))
use_python('/usr/bin/python')
powerlaw <- import('powerlaw')

set.seed(42)
fresh <- TRUE

the <- function(.data, .row) {
  if (count(.data) == 1) {
    single <- .data %>% slice_head(n = 1)
    if (missing(.row)) single %>% pull() %>% head()
    else single %>% pull(var = { { .row } }) %>% head()
  } else simpleError("Expected single row")
}

expand_obs <- function(data) {
  data %>%
    rowwise() %>%
    group_modify(~data.frame(obs = rep(.x$k, .x$n)))
}

fit_power_law <- function(data) {
  obs <- data %>% filter(k > 0) %>% expand_obs() %>% pull(obs)
  if (count(data) < 10 |
    all(data == median(obs)) |
    max(obs) < 3) data.frame(alpha = NA, comp = NA)
  else {
    xmax <- sum(data %>% pull(n))
    m_pl <- powerlaw$Fit(obs, xmax = xmax, discrete = TRUE)

    data.frame(alpha = m_pl$power_law$alpha, comp = m_pl$distribution_compare('power_law', 'lognormal', normalized_ratio = FALSE)[[1]])
  }
}

d_l_lin <- function(data, name) {
  -lm(log(data$n_b) ~ log(data$l_b))$coefficients[[2]]
}

d_l <- function(data, name) {
  obs <- data %>%
    transmute(k = l_b, n = n_b) %>%
    expand_obs()
  p_l <- displ$new(obs$obs)
  p_l$xmin <- estimate_xmin(p_l)
  p_l$pars
}

scor <- function(x, y, type, p) {
  if (cor.test(x, y, method = type, use = "complete.obs", exact = FALSE)$p.value >= p) NaN
  else cor(x, y, method = type, use = "complete.obs")
}

dens_cor <- function(data, type = 'spearman', p = 0.01) {
  density <- data %>% pull(frequency)
  data %>%
    select(-frequency) %>%
    summarise(across(everything(), ~scor(density, .x, type, p)))
}

num_apa <- function(value, places) {
  if (value < 0) sprintf('\\shortminus %s', substring(sprintf('%.2f', round(-value, places)), 2))
  else substring(sprintf('%.2f', round(value, places)), 2)
}

spearman_apa <- function(s, p, n) {
  if (p < 0.001) sprintf('$s(%i)=%s$, $p<.001$', int(n - 2), num_apa(s, 2))
  else sprintf('$s(%i)=%s$, $p=%s$', int(n - 2), num_apa(s, 2), num_apa(p, 3))
}

spearman_val <- function(s, p, p_level = 0.01, bold_level = 1) {
  if (p > p_level) '--'
  else if (s == 1.00) ''
  else if (abs(s) > bold_level) sprintf('$\\bf{%s}$', num_apa(s, 2))
  else sprintf('$%s$', num_apa(s, 2))
}

spearman_val2 <- function(s, bold_level = 1) {
  if (is.nan(s)) '--'
  else if (is.na(s)) ''
  else if (abs(s) > bold_level) sprintf('$\\bf{%s}$', num_apa(s, 2))
  else sprintf('$%s$', num_apa(s, 2))
}

lines2 <- function(obs, obs_cdf, model) {
  xmin <- model$xmin
  frame_fac <- obs_cdf %>% filter(x == xmin) %>% the(y)
  xs <- 10^seq(log10(min(obs)), log10(max(obs)), length.out = 100)
  xs <- xs[xs >= xmin & xs <= model$xmax]
  ys <- model$ccdf(xs) * frame_fac
  data.frame(x = xs, y = ys) %>% filter(y > 0 & x > 0)
}

theme_pubr <- function(legend = c("top", "bottom", "left", "right", "none")) {
  base_size <- 10
  base_family <- 'LM Roman 10'
  half_line <- base_size / 2
  if (!is.numeric(legend)) legend <- match.arg(legend)
  panel.border <- element_blank()
  axis.line <- element_line(colour = "black", size = 0.5)
  plot.margin <- margin(0, half_line, half_line, half_line)
  .theme <- theme_bw(base_size = base_size, base_family = base_family) %+replace% theme(panel.border = panel.border, panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = axis.line, axis.text = element_text(color = "black"), legend.key = element_blank(), strip.background = element_rect(colour = "black"), plot.margin = plot.margin, legend.position = legend, complete = TRUE)
  .theme <- .theme + theme(axis.title.x = element_text(margin = margin(t = half_line)), axis.title.y = element_text(margin = margin(r = half_line)))
  .theme
}