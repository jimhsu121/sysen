# Practice It: Block Diagrams -- Recitation 4
# Reliability of a system, from the reliabilities of its parts.

# R(t) for one component with failure rate lambda, exponential lifespan
r = function(t, lambda){ exp(-1 * lambda * t) }

# Blocks in SERIES: everything must work, so multiply.
series = function(...){ prod(c(...)) }

# Blocks in PARALLEL: the block works unless ALL of them fail.
parallel = function(...){ 1 - prod(1 - c(...)) }

t = 1000   # the time on the picture

# Two parallel pairs, given as reliabilities at t = 1000.
r_w1 = 0.93
r_w2 = 0.93
r_a = 0.99
r_c = 0.90

# YOUR TURN: collapse EACH pair with parallel(), then put the two blocks in series().
r_w_block = parallel(r_w1, r_w2)
r_system  = series(r_a, r_w_block, r_c)
r_system

