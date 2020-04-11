#d <- pipelinedata %>% dplyr::filter(dsp %in% c("ALEX", "CARLOS", "DAN-A", "HANS", "NATHAN", "ROGER"))

m_lme <- lme4::glmer(formula = close ~  (1|year/dsp) - 1, data = d, family = binomial)
coef(m_lme)$`dsp:year`[[1]][1:5]
