# Simulations

set.seed(1)

## Number of free throws made out of 20:

# rbinom() simulates draws from a binomial distribution.
rbinom(20, size = 1, prob = 0.8)
rbinom(1, size = 20, prob = 0.8)
rbinom(20, size = 20, prob = 0.8)

simulation_data <- tibble(res = rbinom(1000, size = 20, prob = 0.8))
simulation_data <- simulation_data %>% count(res) %>% mutate(prop = n / sum(n))
ggplot(simulation_data) + 
  geom_bar(aes(x = res, y = prop), stat = "identity") +
  labs(x = "Number of successful free throws", y = "Proportion of trials")

# dbinom() gives you exact probabilities.
dbinom(11, size = 20, prob = 0.8)
true_probabilities <- tibble(res = 8:20, prob = dbinom(8:20, size = 20, prob = 0.8))
ggplot(simulation_data) + 
  geom_bar(aes(x = res, y = prop), stat = "identity") +
  labs(x = "Number of successful free throws", y = "Proportion of trials") +
  geom_point(aes(x = res, y = prob), data = true_probabilities, size = 4)


## Consecutive free throws made before first missed throw:

rnbinom(20, size = 1, prob = 1 - 0.8)

simulation_data <- tibble(res = rnbinom(1000, size = 1, prob = 1 - 0.8))
simulation_data <- simulation_data %>% count(res) %>% mutate(prop = n / sum(n))
ggplot(simulation_data) + 
  geom_bar(aes(x = res, y = prop), stat = "identity") +
  labs(x = "Number of successful free throws", y = "Proportion of trials")

true_probabilities <- tibble(res = 0:40, prob = dnbinom(0:40, size = 1, prob = 1 - 0.8))
ggplot(simulation_data) + 
  geom_bar(aes(x = res, y = prop), stat = "identity") +
  labs(x = "Number of successful free throws", y = "Proportion of trials") +
  geom_point(aes(x = res, y = prob), data = true_probabilities, size = 4)


# Other simulation functions: rpois(), rnorm(), rt() for Poisson, normal, and t
# True probabilities:         dpois(), dnorm(), dt()


# I won't teach probability rules because we will instead take advantage of R functions
#   and simulations.


# Example 1. Rosencrantz and Guildenstern:
dbinom(183, size = 183, prob = 0.5) # About 1e50 molecules make up the Earth.


# Example 2. Monty Hall problem.
ntrials <- 10000

# Set up results vectors.
does_first_guess_win <- logical(ntrials)
does_switch_win <- logical(ntrials)
for (i in 1:ntrials) {
  door_with_car <- sample(1:3, size = 1)
  doors_with_goats <- setdiff(1:3, door_with_car)
  my_guess <- sample(1:3, size = 1)
  if (my_guess == door_with_car) {
    # Monty opens a different door.
    door_opened <- sample(doors_with_goats, size = 1)
  } else {
    # I picked one of the doors with a goat, so Monty has to open the other one.
    door_opened <- setdiff(doors_with_goats, my_guess)
  }
  doors_remaining <- setdiff(1:3, door_opened)
  switch_guess <- setdiff(doors_remaining, my_guess)
  does_first_guess_win[i] <- (my_guess == door_with_car)
  does_switch_win[i] <- (switch_guess == door_with_car)
}
sum(does_first_guess_win)
sum(does_switch_win)


# Uncertainty. See slides.

ntrials <- 1000
sample_means <- numeric(ntrials)
for (i in 1:ntrials) {
  sample_means[i] <- mean(rnorm(300, mean = -1))
}
ggplot(tibble(x = sample_means)) + geom_histogram(aes(x = x))

# I can use the quantile() function to get a precise interval. Say I want to be 90% sure:
quantile(sample_means, probs = c(.05, .95))
quantile(sample_means, probs = .95) - quantile(sample_means, probs = .05)
quantile(sample_means, probs = .995) - quantile(sample_means, probs = .005)


# Bootstrap:
set.seed(1)
simulation_data <- rnorm(300) - 1
mean(simulation_data)
sample_means <- numeric(ntrials)
for (i in 1:ntrials) {
  bootstrap_sample <- sample(simulation_data, size = 300, replace = TRUE)
  sample_means[i] <- mean(bootstrap_sample)
}
quantile(sample_means, probs = c(.05, .95))
quantile(sample_means, probs = .95) - quantile(sample_means, probs = .05)


# Baseball example. https://www.retrosheet.org/boxesetc/2020/Y_2020.htm

innings <- read_csv("./data/runs_by_inning.csv")

NYY_runs_scored <- innings %>%
  filter(Team == "NYY", Type == "Produced") %>%
  count(Runs) %>%
  mutate(Prop = n / sum(n))
ggplot(NYY_runs_scored) +
  geom_bar(aes(x = Runs, y = Prop), stat = "identity") +
  labs(x = "Runs Scored", y = "Proportion of innings")

prop_no_runs <- NYY_runs_scored %>%
  filter(Runs == 0) %>%
  pull(Prop)
poisson_probabilities <- tibble(
  Runs = 0:7, Prob = dpois(0:7, lambda = log(1 / prop_no_runs))
)
ggplot(NYY_runs_scored) + 
  geom_bar(aes(x = Runs, y = Prop), stat = "identity") +
  labs(x = "Runs Scored", y = "Proportion of innings") +
  geom_point(aes(x = Runs, y = Prob), data = poisson_probabilities, size = 4)


ntrials <- 100
season_results <- numeric(ntrials)
NYY_runs_scored <- innings %>% filter(Team == "NYY", Type == "Produced") %>% pull(Runs)
NYY_runs_allowed <- innings %>% filter(Team == "NYY", Type == "Allowed") %>% pull(Runs)
for (i in 1:ntrials) {
  game_results <- character(60)
  for (game in 1:60) {
    runs_scored_by_inning <- sample(NYY_runs_scored, size = 9, replace = TRUE)
    runs_allowed_by_inning <- sample(NYY_runs_allowed, size = 9, replace = TRUE)
    runs_scored <- sum(runs_scored_by_inning)
    runs_allowed <- sum(runs_allowed_by_inning)
    # Extra innings:
    while (runs_scored == runs_allowed) {
      runs_scored <- runs_scored + sample(NYY_runs_scored, size = 1)
      runs_allowed <- runs_allowed + sample(NYY_runs_allowed, size = 1)
    }
    if (runs_scored > runs_allowed) {
      game_results[game] <- "W"
    } else {
      game_results[game] <- "L"
    }
  }
  season_results[i] <- sum(game_results == "W")
}
ggplot(tibble(W = season_results)) +
  geom_histogram(aes(x = W), binwidth = 1, center = 20) +
  labs(x = "Number of wins", y = "Number of simulations") +
  geom_vline(xintercept = 33, linetype = "dashed")
quantile(season_results, c(0.1, 0.9))


ALEast_teams <- c("BOS", "NYY", "TB", "BAL", "TOR")
all_season_results <- matrix(nrow = 5, ncol = ntrials)
rownames(all_season_results) <- ALEast_teams
for (team in ALEast_teams) {
  season_results <- numeric(ntrials)
  team_runs_scored <- innings %>% filter(Team == team, Type == "Produced") %>% pull(Runs)
  team_runs_allowed <- innings %>% filter(Team == team, Type == "Allowed") %>% pull(Runs)
  for (i in 1:ntrials) {
    game_results <- character(60)
    for (game in 1:60) {
      runs_scored_by_inning <- sample(team_runs_scored, size = 9, replace = TRUE)
      runs_allowed_by_inning <- sample(team_runs_allowed, size = 9, replace = TRUE)
      runs_scored <- sum(runs_scored_by_inning)
      runs_allowed <- sum(runs_allowed_by_inning)
      # Extra innings:
      while (runs_scored == runs_allowed) {
        runs_scored <- runs_scored + sample(team_runs_scored, size = 1)
        runs_allowed <- runs_allowed + sample(team_runs_allowed, size = 1)
      }
      if (runs_scored > runs_allowed) {
        game_results[game] <- "W"
      } else {
        game_results[game] <- "L"
      }
    }
    season_results[i] <- sum(game_results == "W")
  }
  all_season_results[team, ] <- season_results
}

who_wins <- character(ntrials)
for (i in 1:ntrials) {
  standings <- all_season_results[, i]
  division_leaders <- which(standings == max(standings))
  if (length(division_leaders) == 1) {
    who_wins[i] <- names(division_leaders)
  } else {
    who_wins[i] <- sample(names(division_leaders), size = 1)
  }
}
table(who_wins)
