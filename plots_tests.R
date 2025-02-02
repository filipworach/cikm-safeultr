library(ggplot2)
library(dplyr)

# results n=400 for  CLTR\ ------------------------------------------------
exposureIPS <- c(.695,
                .672,
                .656,
                .669,
                .679,
                .668,
                .679,
                .661,
                .647,
                .680)

exposureCRM <- c(	0.704894566,
                  0.700143625,
                  0.707225319,
                  0.705619375,
                  0.703226373,
                  0.699424544,
                  0.705115144,
                  0.703475931,
                  0.701013709,
                  0.705429772)

actionIPS <- c(0.680123071,
               0.661412701,
               0.66675727,
               0.684847329,
               0.669258413,
               0.669459601,
               0.643395905,
               0.659052269,
               0.655492174,
               0.67500819)

actionCRM <- c(0.706754364,
               0.709363791,
               0.702973735,
               0.702011838,
               0.71061066,
               0.70589656,
               0.7054039,
               0.710188515,
               0.706091934,
               0.711076517)

results.full <- data.frame(
  exposureIPS = exposureIPS,
  exposureCRM = exposureCRM,
  actionIPS = actionIPS,
  actionCRM = actionCRM)

t.test(exposureIPS, exposureCRM, paired=TRUE)
t.test(actionIPS, actionCRM, paired=TRUE)

results <- data.frame(
  "model" = c("exposureIPS", "exposureCRM", "actionIPS", "actionCRM"),
  "mean" = c(mean(exposureIPS), mean(exposureCRM), mean(actionIPS), mean(actionCRM)),
  "variance" = c(var(exposureIPS), var(exposureCRM), var(actionIPS), var(actionCRM)),
  "Two-sample_t-test_p-value" = c(t.test(exposureIPS, actionIPS, paired=TRUE)$p.value, t.test(actionCRM, exposureCRM, paired=TRUE)$p.value))

# plot IPS results

results.IPS <- data.frame(
  exposureIPS = exposureIPS,
  actionIPS = actionIPS)

results.CRM <- data.frame(
  exposureCRM = exposureCRM,
  actionCRM = actionCRM)

data_long_ips <- results.IPS %>%
  tidyr::pivot_longer(cols = c(exposureIPS, actionIPS), names_to = "series", values_to = "value")

data_long_crm <- results.CRM %>%
  tidyr::pivot_longer(cols = c(exposureCRM, actionCRM), names_to = "series", values_to = "value")

means_ips <- data_long_ips %>%
  group_by(series) %>%
  summarise(mean_value = mean(value))

means_crm <- data_long_crm %>%
  group_by(series) %>%
  summarise(mean_value = mean(value))


plot_ips <- ggplot(data_long_ips, aes(x = value, fill = series)) +
  geom_density(alpha = 0.5) +
  geom_vline(data = means_ips, aes(xintercept = mean_value, color = series), linetype = "dashed", size = 1) +
  labs(title = "Density Plots of ExposureIPS and ActionIPS", x = "Value", y = "Density") +
  scale_fill_manual(values = c("blue", "red")) +
  scale_color_manual(values = c("blue", "red")) +
  theme_minimal()

plot_crm <- ggplot(data_long_crm, aes(x = value, fill = series)) +
  geom_density(alpha = 0.5) +
  geom_vline(data = means_crm, aes(xintercept = mean_value, color = series), linetype = "dashed", size = 1) +
  labs(title = "Density Plots of ExposureCRM and ActionCRM", x = "Value", y = "Density") +
  scale_fill_manual(values = c("blue", "red")) +
  scale_color_manual(values = c("blue", "red")) +
  theme_minimal()


# MSLR30K -----------------------------------------------------------------

exposureIPS <- c(0.245139823,
                 0.250329868,
                 0.251592250,
                 0.245926894,
                 0.253395489)

exposureCRM <- c(	0.239205801,
                  0.109825550,
                  0.245983610,
                  0.245037801,
                  0.255963508)

actionIPS <- c(0.116241769,
               0.224363336,
               0.104873727,
               0.154011796,
               0.105091362)

actionCRM <- c(0.256307796,
               0.247015348,
               0.228442772,
               0.247227330,
               0.249322336)
results.full.MLSR30K <- data.frame(
  exposureIPS = exposureIPS,
  exposureCRM = exposureCRM,
  actionIPS = actionIPS,
  actionCRM = actionCRM)

results.MLSR30K <- data.frame(
  "model" = c("exposureIPS", "exposureCRM", "actionIPS", "actionCRM"),
  "mean" = c(mean(exposureIPS), mean(exposureCRM), mean(actionIPS), mean(actionCRM)),
  "variance" = c(var(exposureIPS), var(exposureCRM), var(actionIPS), var(actionCRM)),
  "Two-sample_t-test_p-value" = c(t.test(exposureIPS, actionIPS, paired=TRUE)$p.value, t.test(actionCRM, exposureCRM, paired=TRUE)$p.value))

results.IPS <- data.frame(
  exposureIPS = exposureIPS,
  actionIPS = actionIPS)

results.CRM <- data.frame(
  exposureCRM = exposureCRM,
  actionCRM = actionCRM)


data_long_ips <- results.IPS %>%
  tidyr::pivot_longer(cols = c(exposureIPS, actionIPS), names_to = "series", values_to = "value")

data_long_crm <- results.CRM %>%
  tidyr::pivot_longer(cols = c(exposureCRM, actionCRM), names_to = "series", values_to = "value")

means_ips <- data_long_ips %>%
  group_by(series) %>%
  summarise(mean_value = mean(value))

means_crm <- data_long_crm %>%
  group_by(series) %>%
  summarise(mean_value = mean(value))


plot_ips <- ggplot(data_long_ips, aes(x = value, fill = series)) +
  geom_density(alpha = 0.5) +
  geom_vline(data = means_ips, aes(xintercept = mean_value, color = series), linetype = "dashed", size = 1) +
  labs(title = "Density Plots of ExposureIPS and ActionIPS", x = "Value", y = "Density") +
  scale_fill_manual(values = c("blue", "red")) +
  scale_color_manual(values = c("blue", "red")) +
  theme_minimal()

plot_crm <- ggplot(data_long_crm, aes(x = value, fill = series)) +
  geom_density(alpha = 0.5) +
  geom_vline(data = means_crm, aes(xintercept = mean_value, color = series), linetype = "dashed", size = 1) +
  labs(title = "Density Plots of ExposureCRM and ActionCRM", x = "Value", y = "Density") +
  scale_fill_manual(values = c("blue", "red")) +
  scale_color_manual(values = c("blue", "red")) +
  theme_minimal()

