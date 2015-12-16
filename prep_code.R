log_model1 <- glm(uncontrolled ~ lbdglusi, data = bgDF[, c(1, 6)], family = "binomial") 



min_val <- with(bgDF, min(lbdglusi))
max_val <- with(bgDF, max(lbdglusi))


bgDF$logistic_pred <- seq(from = min_val, to = max_val, length.out = 333) %>% 
  data.frame(lbdglusi = ., uncontrolled = 1) %>%
  predict(log_model1, newdata = ., type = "response")

bgDF$maxmin_seq <- seq(from = min_val, to = max_val, length.out = 333)


with(bgDF, plot(lbdglusi, jitter(uncontrolled)))

with(bgDF, lines(maxmin_seq, logistic_pred))

tmp <- sort(unique(bgDF$lbdglusi))
lapply(tmp, table())