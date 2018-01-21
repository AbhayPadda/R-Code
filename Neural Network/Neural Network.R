library(neuralnet)
data("infert")
set.seed(1988)

nn <- neuralnet(case~ age+parity+induced+spontaneous, data='infert', hidden=2, err.fct = "ca", linear.output = FALSE)

