library(ggplot2)
library(reshape2)
library(readxl) 

# ==== MCMC trace plots ====

#' Create Trace Plots for MCMC Output
#'
#' This function generates trace plots for each parameter in the MCMC output.
#' The trace plots help to assess the convergence and mixing of the MCMC chains.
#'
#' @param mcmc_output A 3-dimensional array containing the MCMC samples. The dimensions should be iterations x chains x parameters.
#' @param output_path The path where the trace plots will be saved.
#' @param burn_in The number of initial iterations to be marked as burn-in. Default is 200.
#' @param light_palette A logical value indicating whether to use a light color palette. Default is FALSE.
#' @param legend_inside A logical value indicating whether to place the legend inside the plot. Default is FALSE.
#' @param dashed A logical value indicating whether to use different line types (e.g., dashed) for different chains. Default is FALSE.
#' @param figure_title A logical value indicating whether to include a title in the plots. Default is FALSE.
#'
#' @return This function does not return a value. It saves the trace plots to the specified output path.
create_trace_plots <- function(mcmc_output, output_path, burn_in = 200, light_palette = TRUE, legend_inside = FALSE, dashed = FALSE, figure_title = FALSE, actual_values_path = NULL) {
  # Load the actual values if the path is provided
  if (!is.null(actual_values_path)) {
    actual_values <- read_excel(actual_values_path)
    print(actual_values)
  }
  
  # Define color palettes
  dark_palette <- c("#666666", "#ff7f00", "#0000FF", "#000000")
  light_palette_colors <- c("#0014a8", "#FF9B00", "#68BB59", "#DDA0DD")
  
  # Select the color palette
  custom_colors <- if (light_palette) light_palette_colors else dark_palette
  
  # Define line types if dashed is TRUE
  line_types <- if (dashed) c("solid", "dashed", "dotted", "dotdash") else rep("solid", 4)
  
  # Get the parameter names
  parameter_names <- dimnames(mcmc_output)[[3]]
  
  # Identify unique prefixes/categories for parameters
  parameter_categories <- unique(gsub("\\[.*\\]", "", parameter_names))
  
  # Create folders for each category if they do not exist
  for (category in parameter_categories) {
    category_path <- file.path(output_path, category)
    if (!dir.exists(category_path)) {
      dir.create(category_path, recursive = TRUE)
    }
  }
  
  # Loop through all parameters and create trace plots
  for (parameter_index in 1:length(parameter_names)) {
    parameter_name <- parameter_names[parameter_index]
    
    # Extract the samples for the specified parameter across all chains
    samples <- mcmc_output[, , parameter_index]
    
    # Convert to a data frame for ggplot2
    mcmc_samples_df <- as.data.frame(samples)
    colnames(mcmc_samples_df) <- paste0("Chain_", 1:ncol(mcmc_samples_df))
    mcmc_samples_df$Iteration <- 1:nrow(mcmc_samples_df)
    
    # Melt the data frame for ggplot2
    mcmc_melted <- melt(mcmc_samples_df, id.vars = "Iteration")
    
    # Create trace plot using ggplot2
    trace_plot <- ggplot(mcmc_melted, aes(x = Iteration, y = value, color = variable, linetype = variable)) +
      geom_line(size = 0.25, alpha = 1) +  # Thinner lines with transparency
      labs(
        title = if (figure_title) paste("Trace Plot for Parameter", parameter_name) else NULL,
        x = "Iteration",
        y = "Sample Value"
      ) +
      geom_vline(xintercept = burn_in, color = "red", linetype = 5, size = 0.5) +  # Dashed burn-in line
      scale_color_manual(values = custom_colors) +  # Custom colors
      scale_linetype_manual(values = line_types) +  # Line types
      theme(
        panel.background = element_rect(fill = "white"),           # White background
        panel.grid.major = element_line(color = "grey80"),         # Major grid lines
        panel.grid.minor = element_line(color = "grey90"),         # Minor grid lines
        panel.border = element_rect(color = "black", fill = NA),   # Border around the plot
        axis.ticks.length = unit(-0.15, "cm"),                     # Length of the ticks
        axis.text.x = element_text(margin = margin(t = 5)),        # Margin for axis text
        axis.text.y = element_text(margin = margin(r = 5)),        # Margin for axis text
        legend.position = if (legend_inside) c(0.85, 0.85) else "bottom",  # Legend inside or at the bottom
        legend.title = element_blank(),                            # No legend title
        legend.background = element_rect(fill = "white", color = "black"),  # Legend background
        legend.key = element_rect(fill = "white", color = "black") # Legend keys
      ) +
      guides(color = guide_legend(override.aes = list(alpha = 1)))  # Adjust legend line transparency
    
    # Add a horizontal line if the actual value exists
    if (!is.null(actual_values_path)) {
      actual_value <- actual_values$parameter_values[which(actual_values$parameter_names == parameter_name)]
      if (length(actual_value) > 0) {
        trace_plot <- trace_plot + geom_hline(yintercept = actual_value, color = "blue", linetype = "dashed")
      }
    }
    
    # Determine the category for the parameter
    category <- gsub("\\[.*\\]", "", parameter_name)
    
    # Save the plot in the corresponding folder
    ggsave(filename = file.path(output_path, category, paste0("trace_plot_", parameter_name, ".png")),
           plot = trace_plot,
           width = 10, height = 6)
  }
}

# ==== MCMC autocorrelation plots ====

#' Create Autocorrelation Plots for MCMC Output
#'
#' This function generates autocorrelation plots for each parameter in the MCMC output.
#' The autocorrelation plots help to assess the dependence of the samples.
#'
#' @param mcmc_output A 3-dimensional array containing the MCMC samples. The dimensions should be iterations x chains x parameters.
#' @param output_path The path where the autocorrelation plots will be saved.
#' @param lags The maximum number of lags to include in the autocorrelation plot. Default is 50.
#' @param light_palette A logical value indicating whether to use a light color palette. Default is FALSE.
#' @param figure_title A logical value indicating whether to include a title in the plots. Default is FALSE.
#' @param legend_inside A logical value indicating whether to place the legend inside the plot. Default is FALSE.
#'
#' @return This function does not return a value. It saves the autocorrelation plots to the specified output path.
create_autocorr_plot <- function(mcmc_output, output_path, lags = 50, light_palette = TRUE, figure_title = FALSE, legend_inside = FALSE) {
  # Define color palettes
  dark_palette <- c("#666666", "#ff7f00", "#0000FF", "#000000")
  light_palette_colors <- c("#0014a8", "#FF9B00", "#68BB59", "#DDA0DD")
  
  # Select the color palette
  custom_colors <- if (light_palette) light_palette_colors else dark_palette
  
  # Get the number of parameters
  num_parameters <- dim(mcmc_output)[3]
  parameter_names <- dimnames(mcmc_output)[[3]]
  
  # Identify unique prefixes/categories for parameters
  parameter_categories <- unique(gsub("\\[.*\\]", "", parameter_names))
  
  # Create folders for each category if they do not exist
  for (category in parameter_categories) {
    category_path <- file.path(output_path, category)
    if (!dir.exists(category_path)) {
      dir.create(category_path, recursive = TRUE)
    }
  }
  
  for (parameter_index in 1:num_parameters) {
    # Extract the samples for the specified parameter across all chains
    samples <- mcmc_output[, , parameter_index]
    
    # Compute autocorrelations
    autocorr_list <- lapply(1:ncol(samples), function(chain) {
      acf(samples[, chain], plot = FALSE, lag.max = lags)$acf
    })
    
    # Convert to a data frame for ggplot2
    autocorr_df <- data.frame(
      Lag = 0:lags,
      do.call(cbind, autocorr_list)
    )
    colnames(autocorr_df)[-1] <- paste0("Chain_", 1:ncol(samples))
    
    # Melt the data frame for ggplot2
    autocorr_melted <- melt(autocorr_df, id.vars = "Lag")
    
    # 99% confidence bands for autocorrelation (approx. +-2/sqrt(N))
    n <- nrow(samples)
    conf_level <- 0.99
    conf_band <- qnorm((1 + conf_level) / 2) / sqrt(n)
    
    # Create autocorrelation plot using ggplot2
    autocorr_plot <- ggplot(autocorr_melted, aes(x = Lag, y = value, color = variable)) +
      geom_line(size = 0.5, alpha = 0.7) +  # Thinner lines with transparency
      geom_hline(yintercept = c(-conf_band, conf_band), linetype = "longdash", color = "red") +  # 99% confidence bands
      scale_x_continuous(limits = c(0, lags), expand = c(0, 0)) +  # Ensure plot ends at max lag and starts at 0
      labs(
        title = if (figure_title) paste("Autocorrelation Plot for Parameter", parameter_names[parameter_index]) else NULL,
        x = "Lag",
        y = "Autocorrelation"
      ) +
      scale_color_manual(values = custom_colors) +  # Custom colors
      theme(
        panel.background = element_rect(fill = "white"),           # White background
        panel.grid.major = element_line(color = "grey80"),         # Major grid lines
        panel.grid.minor = element_line(color = "grey90"),         # Minor grid lines
        panel.border = element_rect(color = "black", fill = NA),   # Border around the plot
        axis.ticks.length = unit(-0.25, "cm"),                     # Length of the ticks, outside the plot
        axis.ticks = element_line(color = "black"),                # Tick marks color
        axis.text.x = element_text(margin = margin(t = 10)),       # Margin for axis text
        axis.text.y = element_text(margin = margin(r = 10)),       # Margin for axis text
        legend.position = if (legend_inside) c(0.85, 0.85) else "bottom",  # Legend inside or at the bottom
        legend.title = element_blank(),                            # No legend title
        legend.background = element_rect(fill = "white", color = "black"),  # Legend background
        legend.key = element_rect(fill = "white", color = "black"), # Legend keys
        legend.box = if (!legend_inside) "vertical" else NULL,      # Ensure the legend box is vertical when outside
        legend.direction = if (!legend_inside) "horizontal" else "vertical" # Horizontal legend when outside
      ) +
      guides(color = guide_legend(override.aes = list(alpha = 1)))  # Adjust legend line transparency
    
    # Determine the category for the parameter
    category <- gsub("\\[.*\\]", "", parameter_names[parameter_index])
    
    # Save the plot in the corresponding folder
    ggsave(filename = file.path(output_path, category, paste0("autocorr_plot_param_", parameter_names[parameter_index], ".png")),
           plot = autocorr_plot,
           width = 10, height = 6)
  }
}



# ==== USE CASE ====

output_path <- "/Users/noemisavelkoul/Desktop/bayesian_var_git/Plots"
create_trace_plots(draws_arr_wu, output_path)

output_path <- "/Users/noemisavelkoul/Desktop/bayesian_var_git/Plots"
create_autocorr_plot(draws_arr_wu, output_path)

# path -- > "/Users/noemisavelkoul/Desktop/data/Parameters_m_1_p_4.xlsx"







