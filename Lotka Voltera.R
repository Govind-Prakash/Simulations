# Install and load the deSolve package
if (!requireNamespace("deSolve", quietly = TRUE)) {
  install.packages("deSolve")
}
library(deSolve)
install.packages("ggplot2")
# Define the Lotka-Volterra model
lotka_volterra <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    dx <- alpha * x - beta * x * y
    dy <- delta * x * y - gamma * y
    list(c(dx, dy))
  })
}

# Initial conditions
state <- c(x = 40, y = 9)  # Initial populations of prey and predators

# Parameters
parameters <- c(alpha = 0.1, beta = 0.02, delta = 0.01, gamma = 0.1)

# Time sequence for simulation
times <- seq(0, 200, by = 1)

# Solve the ODE
output <- ode(y = state, times = times, func = lotka_volterra, parms = parameters)

# Convert output to a data frame
output <- as.data.frame(output)

# Plot the results
library(ggplot2)
ggplot(output, aes(x = time)) +
  geom_line(aes(y = x, color = "Prey Population")) +
  geom_line(aes(y = y, color = "Predator Population")) +
  labs(
    title = "Lotka-Volterra Predator-Prey Model",
    x = "Time",
    y = "Population"
  ) +
  scale_color_manual(name = "Legend", values = c("Prey Population" = "blue", "Predator Population" = "red")) +
  theme_minimal()

