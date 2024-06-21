# Load necessary libraries
library(openxlsx)

# Source the functions required for data generation
source("functions.R")  # Adjust the path to your actual functions.R file

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
