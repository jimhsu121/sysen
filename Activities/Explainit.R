# Exploding phones: 1 fail in 30 days, calculate hazard rate between 
m   <- 30
lambda <- 1 / m

# Set up a time sequence and a small time step
deltat <- 1
t      <- seq(from = 0, to = 100, by = 1)

# Build the table of F(t), F(t+dt), and f(t)
phones <- tibble(
  t          = t,
  F_t        = pexp(t, rate = lambda),
  F_t_deltat = pexp(t + deltat, rate = lambda),
  f_t        = (F_t_deltat - F_t) / deltat
)

phones


