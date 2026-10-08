mycoffee = bind_rows(
  # Make first data.frame
  data.frame(
    # Containing these vectors style and price
    style = c("latte", "cappuccino", "americano"),
    price = c(5, 4, 3)),
  # Make second data.frame
  data.frame(
    # Containing these vectors style, price, and shop
    style = c("coffee", "hot cocoa"),
             price = c(3, 2),
             shop = c("Gimme Coffee", "Starbucks"))
)
# Notice how they stack, 
# but in first data.frame values,
# shop gets filled with NA, 
# since it wasn't in first dataframe
mycoffee
mycoffee = mycoffee %>%
  # Add a new vector (must be of same length as data.frame)
  # vector is number of those drinks purchased
  mutate(purchased = c(5, 4, 10, 2, 1))
mycoffee
mycoffee %>%
  # Summarize data.frame into one row
  summarize(
    # Calculate mean price of drinks
    mean_price = mean(price),
    # Calculate total drinks purchased
    total_purchased = sum(purchased))
