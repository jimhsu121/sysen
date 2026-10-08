library(ggplot2)
library(dplyr)

fmea = data.frame(
  failure_mode = c("pump_noise", "door_ajar",
                   "filter_clog", "light_out"),
  severity = c(4, 2, 3, 5),
  occurrence = c(3, 4, 2, 1),
  detection = c(2, 3, 4, 5)
) %>%
  mutate(criticality =
    severity * occurrence * detection)

ggplot() +
  geom_point(
    data = fmea,
    mapping = aes(
      x = occurrence,
      y = severity,
      color = failure_mode,
      size = criticality)) +
  labs(x = "Occurrence",
       y = "Severity",
       title = "Where does risk sit?")


