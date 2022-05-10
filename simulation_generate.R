# Generate simulated data
library(tsintermittent)
IDI = c(1.00, 1.32, 2.00, 4.00)
CV2 = c(0.25, 0.49, 1.00, 2.00)
obs = c(84, 108, 132, 156)

dataset_84 = c()
for (i in 1:length(IDI)) {
  for (j in 1:length(CV2)) {
    dataset <- t(simID(n = 1000, obs = obs[1]
                       , idi = IDI[i], cv2 = CV2[j]))
    dataset_84 = rbind(dataset_84, dataset)
  }
}

dataset_108 = c()
for (i in 1:length(IDI)) {
  for (j in 1:length(CV2)) {
    dataset <- t(simID(n = 1000, obs = obs[2]
                       , idi = IDI[i], cv2 = CV2[j]))
    dataset_108 = rbind(dataset_108, dataset)
  }
}

dataset_132 = c()
for (i in 1:length(IDI)) {
  for (j in 1:length(CV2)) {
    dataset <- t(simID(n = 1000, obs = obs[3]
                       , idi = IDI[i], cv2 = CV2[j]))
    dataset_132 = rbind(dataset_132, dataset)
  }
}

dataset_156 = c()
for (i in 1:length(IDI)) {
  for (j in 1:length(CV2)) {
    dataset <- t(simID(n = 1000, obs = obs[4]
                       , idi = IDI[i], cv2 = CV2[j]))
    dataset_156 = rbind(dataset_156, dataset)
  }
}



# Plot the simulated text set
idi = c()
cv2 = c()
for (i in 1:length(dataset_simulation_test)) {
  idi = c(idi, dataset_simulation_test[[i]]$IDI)
  cv2 = c(cv2, dataset_simulation_test[[i]]$cv2)
}
simulation_data = data.frame(IDI = idi, CV2 = cv2)

library(ggplot2)
ggplot(simulation_data, aes(x=log(IDI), y=log(CV2))) +
  geom_hex(bins = 50, aes(alpha=log(..count..)),fill="#000000")+
  xlim(-0.5, 2.5) +
  ylim(-3,3)+
  geom_vline(xintercept = log(1.32), color="red",linetype = 2) +
  geom_hline(yintercept = log(0.49), color="red",linetype = 2)+
  labs(x = expression(log(IDI)), y=expression(log(CV^2)))



