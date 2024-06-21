#Functions to generate data, calculate truncation threshold and calculate posterior for
#effective order

my_path = "/Users/noemisavelkoul/Desktop/bayesian_var" # path to directory containing R code and Stan program,
# starting in current working directory
source(paste(my_path, "/functions.R", sep=""))

#load cmdstanr
library(cmdstanr)
library(stringr)


# ==== GENERATE DATA ====

# Define parameters
num_pmf <- 10  # Number of posterior mass functions to generate
N <- 1000  # Number of observations, change this value as needed

# Define possible values for m and p based on the paper
m_values <- c(1, 3, 5, 7)
p_values <- c(7, 12, 30)

# Create data directory if it does not exist
output_folder <- "data"
if (!dir.exists(output_folder)) {
  dir.create(output_folder)
}

for (m in m_values) {
  for (p in p_values) {
    
    # Create a new Excel workbook
    wb <- createWorkbook()
    
    for (i in 1:num_pmf) {
      experiment <- i 
      
      # Define parameters for data generation > original is mu = 0, Sigma = I
      Sigma <- diag(1, m)
      mu <- rep(0, m)
      
      data <- generate_data_set(N = N, m = m, p = p, Sigma = Sigma, mu = mu)
      
      # Save the dataset to the Excel workbook
      addWorksheet(wb, paste0("Dataset_", i))
      writeData(wb, sheet = paste0("Dataset_", i), data$y)
    }
    
    # Save the workbook to a file
    saveWorkbook(wb, file.path(output_folder, paste0("Data_m_", m, "_p_", p, ".xlsx")), overwrite = TRUE)
  }
}


# ==== RUN SIMULATION ====

library(openxlsx)
library(cmdstanr)
library(stringr)
library(tictoc)

# Define possible values for m and p based on the paper
m_values <- c(1, 3, 5, 7)
p_values <- c(7, 12, 30)

# Define the number of PMFs (adjust according to your data)
num_pmf <- 10

# Function to calculate and print elapsed time
print_time <- function(start_time, prefix = "") {
  elapsed <- Sys.time() - start_time
  print(paste(prefix, "Elapsed time:", round(elapsed, 2), "seconds"))
}

# Start overall timer
overall_start <- Sys.time()

# Iterate over all combinations of m and p
for (m in m_values) {
  for (p in p_values) {
    # Initialize an empty list to store the PMFs for the current combination of m and p
    pmf_list <- vector("list", num_pmf)
    
    # Define the path to the Excel file
    file_path <- paste0("/Users/noemisavelkoul/Desktop/bayesian_var_git/data/Data_m_", m, "_p_", p, ".xlsx")
    
    # Load the Excel workbook
    wb <- loadWorkbook(file_path)
    
    # Start timer for current (m, p) combination
    start_time <- Sys.time()
    print(paste("Processing combination m =", m, ", p =", p))
    
    for (i in 1:num_pmf) {
      # Access the dataset from the appropriate sheet
      dataset <- read.xlsx(wb, sheet = paste0("Dataset_", i))
      
      # Access the specific experiment data
      y <- as.matrix(dataset)
      
      # Mean center the data
      y <- t(apply(y, 1, function(x) x - colMeans(y)))
      
      # Correct dimensions when m == 1
      if (m == 1) y <- t(y)
      
      # Compile Stan program
      file <- paste0(my_path, "/multiplicative_gamma.stan")
      mod <- cmdstan_model(file)
      
      p_max <- p + 4 
      
      data <- list(m = m, p = p_max, N = N, y = y, n_miss = 0, ind_miss = c(), df = m + 4, scale_diag = 1, scale_offdiag = 0, a1 = 2.5, a2 = 3, a = 6)
      
      # Run HMC with 4 chains on 4 parallel cores, with 1000 iterations of warmup and 4000 sampling iterations
      output <- mod$sample(data = data, chains = 4, parallel_chains = 4, iter_warmup = 1000, iter_sampling = 4000)
      
      # Extract MCMC output as 3-dimensional array of form: [iterations, chains, parameters]
      draws_arr <- output$draws()
      draws_arr <- unclass(draws_arr)
      
      # Extract samples of P matrices
      nchains <- 4
      P <- NULL
      for (j in 1:nchains) {
        P <- rbind(P, draws_arr[, j, which(stringr::str_detect(dimnames(draws_arr)[[3]], "P\\["))])
      }
      
      # Calculate truncation threshold
      threshold <- calculate_threshold(m, N = N, beta = 0.99)
      
      # Calculate posterior mass for effective order p*
      posterior_mass <- eff_order(P = P, pmax = p_max, threshold = threshold, singval = FALSE)
      
      # Store the PMF in the list
      pmf_list[[i]] <- posterior_mass$pmf
      
      # Print progress for each PMF
      print(paste("Processed PMF", i, "out of", num_pmf, "for combination m =", m, ", p =", p))
    }
    
    # Combine PMFs into a data frame for the current combination of m and p
    pmf_data <- data.frame(k = 0:p_max)
    for (I in 1:num_pmf) {
      pmf_data[[paste0("probability_", I)]] <- pmf_list[[I]]
    }
    
    # Define the path to save the PMF data
    save_path <- paste0("/Users/noemisavelkoul/Desktop/bayesian_var_git/probabilities/pmf_data_m", m, "_p", p, ".RData")
    
    # Save the PMF data to the specified folder
    save(pmf_data, file = save_path)
    
    # Print elapsed time for the current combination
    print_time(start_time, prefix = paste("Combination m =", m, ", p =", p, "-"))
  }
}

# Print overall elapsed time
print_time(overall_start, prefix = "Overall -")