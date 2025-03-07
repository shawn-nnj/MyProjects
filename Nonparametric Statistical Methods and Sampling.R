library(stats)

#GAME ATTENDANCE
# Input the data
attendance <- c(6210, 3150, 2700, 3012, 4875,
                3540, 6127, 2581, 2642, 2573,
                2792, 2800, 2500, 3700, 6030,
                5437, 2758, 3490, 2851, 2720)

# Set the hypothesized median
H0 <- 3000

# Perform the sign test
sign_test <- binom.test(sum(attendance > H0), length(attendance), p = 0.5, alternative = "two.sided")

# Output the results
sign_test

#LOTTERY TICKET SALES
# Set the hypothesized median
H0 <- 200

# Number of days sampled
n <- 40

# Number of days with fewer than 200 tickets sold
num_days_below_200 <- 15

# Perform the sign test
sign_test <- binom.test(num_days_below_200, n, p = 0.5, alternative = "less")

# Output the results
sign_test

#LENGTHS OF PRISON SENTENCES
# Input the data
males <- c(8, 12, 6, 14, 22, 27, 32, 24, 26, 19, 15, 13)
females <- c(7, 5, 2, 3, 21, 26, 30, 9, 4, 17, 23, 12, 11, 16)

# Perform the Wilcoxon rank sum test
wilcox_test <- wilcox.test(males, females, alternative = "two.sided")

# Output the results
wilcox_test

#WINNING BASEBALL GAMES
# Input the data for NL Eastern Division
NL_wins <- c(89, 96, 88, 101, 90, 91, 92, 96, 108, 100, 95)

# Input the data for AL Eastern Division
AL_wins <- c(108, 86, 91, 97, 100, 102, 95, 104, 95, 89, 88, 101)

# Perform the Wilcoxon rank sum test
wilcox_test <- wilcox.test(NL_wins, AL_wins, alternative = "two.sided")

# Output the results
wilcox_test

#MATHEMATICS LITERACY SCORES
# Mathematics literacy scores for each region
western_hemisphere <- c(527, 406, 474, 381, 411)
europe <- c(520, 510, 513, 548, 496)
eastern_asia <- c(523, 547, 547, 391, 549)

# Combine scores into a list
scores <- list(WesternHemisphere = western_hemisphere,
               Europe = europe,
               EasternAsia = eastern_asia)

# Perform Kruskal-Wallis test
kruskal.test(scores)

#SUBWAY AND COMMUTER RAIL PASSENGERS
# Daily passenger trips data for subways and rail across cities
subway_trips <- c(845, 494, 425, 313, 108, 41)
rail_trips <- c(39, 291, 142, 103, 33, 38)

# Perform Spearman rank correlation test
cor_test <- cor.test(subway_trips, rail_trips, method = "spearman")

# Print the results
print(cor_test)

#PRIZES IN CARAMEL CORN BOXES
# Function to simulate buying boxes until all four prizes are collected
simulate_boxes_until_all_prizes <- function() {
  # List of four different prizes
  prizes <- c("Prize A", "Prize B", "Prize C", "Prize D")
  
  # Initialize an empty vector to track collected prizes
  collected_prizes <- character(0)
  
  # Initialize counter for number of boxes bought
  num_boxes <- 0
  
  # Continue buying boxes until all four prizes are collected
  while(length(unique(collected_prizes)) < 4) {
    # Randomly select a prize and add it to collected prizes
    new_prize <- sample(prizes, 1)
    collected_prizes <- c(collected_prizes, new_prize)
    
    # Increment the number of boxes bought
    num_boxes <- num_boxes + 1
  }
  
  return(num_boxes)
}

# Set the number of simulations (repetitions)
num_simulations <- 40

# Vector to store the number of boxes needed for each simulation
boxes_needed <- numeric(num_simulations)

# Perform multiple simulations and store the results
for (i in 1:num_simulations) {
  boxes_needed[i] <- simulate_boxes_until_all_prizes()
}

# Calculate the average number of boxes needed
average_boxes_needed <- mean(boxes_needed)

# Print the average number of boxes needed to collect all four prizes
cat("Average number of boxes needed to collect all four prizes:", average_boxes_needed, "\n")

#LOTTERY WINNER
# Function to simulate buying tickets until all letters to spell "BIG" are obtained
simulate_tickets_until_big <- function() {
  # Initialize counters for letters obtained
  b_obtained <- FALSE
  i_obtained <- FALSE
  g_obtained <- FALSE
  
  # Initialize counter for number of tickets bought
  num_tickets <- 0
  
  # Continue buying tickets until all letters B, I, G are obtained
  while (!(b_obtained & i_obtained & g_obtained)) {
    # Randomly determine if the ticket contains letter B (60% chance)
    has_b <- runif(1) <= 0.6
    
    # Randomly determine if the ticket contains letter I (30% chance)
    has_i <- runif(1) <= 0.3
    
    # Randomly determine if the ticket contains letter G (10% chance)
    has_g <- runif(1) <= 0.1
    
    # Update letter obtained statuses
    if (has_b && !b_obtained) b_obtained <- TRUE
    if (has_i && !i_obtained) i_obtained <- TRUE
    if (has_g && !g_obtained) g_obtained <- TRUE
    
    # Increment the number of tickets bought
    num_tickets <- num_tickets + 1
  }
  
  return(num_tickets)
}

# Set the number of simulations (repetitions)
num_simulations <- 30

# Vector to store the number of tickets needed for each simulation
tickets_needed <- numeric(num_simulations)

# Perform multiple simulations and store the results
for (i in 1:num_simulations) {
  tickets_needed[i] <- simulate_tickets_until_big()
}

# Calculate the average number of tickets needed
average_tickets_needed <- mean(tickets_needed)

# Print the average number of tickets needed to spell "BIG"
cat("Average number of tickets needed to spell 'BIG':", average_tickets_needed, "\n")

