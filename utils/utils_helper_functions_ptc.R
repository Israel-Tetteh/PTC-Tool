
#' Get needed columns for analysis
#'
#' @param x A dataframe
#'
#' @returns A dataframe with selected columns from the 3rd column to the last column
#' @noRd
#' 
Get_needed_cols <- function(x = NULL) {
  obtained_cols <- x[3:ncol(x)]
  # convert rest of the columns to numeric.
  obtained_cols[3:ncol(obtained_cols)] <- lapply(obtained_cols[3:ncol(obtained_cols)], as.numeric)

  return(obtained_cols)
  
} 


#' Read datasheet files for further analysis
#'
#' @param file_path File path to the datasheet
#'
#' @returns A dataframe with required columns needed for analysis, with standardized column names
#' @noRd
#' 
read_datasheet <- function(file_path) {
  # Determine file extension
  ext <- tools::file_ext(file_path)
  
  # Read file based on extension, adding an error handling script.
  tryCatch({
    if (ext %in% c("xlsx", "xls")) {
      a <- readxl::read_excel(file_path,
                              skip = 3,
                              .name_repair = "minimal"
      )
    } else if (ext == "csv") {
      a <- read.csv(file_path, stringsAsFactors = FALSE, skip = 3)
    } else {
      stop("Unsupported file format. Please upload an Excel (.xlsx, .xls) or CSV file.")
    }
  })
  
  # Refit names.
  colnames(a)[1:4] <- c('Date','Week','Treatment_ID','Replicate_Number')
  a <- a[1 : which(colnames(a) == '')[1] -1] # remove empty columns
  a <- Get_needed_cols(x = a) # get needed columns.
  
  # Replace spaces with underscores
  colnames(a) <- sapply(colnames(a), function(x) gsub(" ", "_", x))
  # Return result.
  return(a)
  
}
 

#' Generate a summary of data with error handling
#'
#' @param x A dataframe or matrix containing the datasheet
#'
#' @returns A dataframe summarizing the number of observations, missing values, and data types for each column
#' @noRd
#' 
dat_sum <- function(x) {
  tryCatch(
    {
      colname_data <- colnames(x)
      # Check if column names exist
      if (is.null(colname_data)) {
        colname_data <- paste0("Column", 1:ncol(x))
        warning("Data has no column names. Using generic names.")
      }
      # Create output data frame
      data_fram <- as.data.frame(
        matrix(
          nrow = length(colname_data), ncol = 4,
          dimnames = list(NULL, c(
            "Column_Names", "Number_of_Entries",
            "Missing_Values", "Data_Type"
          ))
        )
      )
      data_fram[, 1] <- colname_data
      # For loop with more robust counting
      for (i in seq_len(ncol(x))) {
        # Count non-NA values
        data_fram[i, 2] <- sum(!is.na(x[[i]]))
        # Count NA values
        data_fram[i, 3] <- sum(is.na(x[[i]]))
        # Get data type
        data_fram[i, 4] <- class(x[[i]])[1]
      }
      return(data_fram)
    },
    error = function(e) {
      # Handle errors and provide informative message
      message("Error in dat_sum function: ", e$message)
      return(data.frame(Error = paste("Failed to summarize data:", e$message)))
    }
  )
}



#' Get column names for user display
#'
#' @param x A dataframe or matrix
#'
#' @returns A character vector containing the column names
#' @noRd
#' 
get_colnames <- function(x = NULL) {
  result <- colnames(x) |> as.vector()
  return(result)
}


#' Trim data by removing rows with missing values
#'
#' @param data A dataframe containing the dataset
#' @param res_name A character string specifying the response variable column name
#'
#' @returns A dataframe with Treatment_ID, Replicate_Number, and the specified response variable, with all rows containing NA values removed
#' @noRd
#' 
na_trim <- function(data = NULL,
                    res_name = NULL) {
  # Deal with one input at a time. returned table here is going to be the useful.
  subset_data <- data[, c("Treatment_ID", "Replicate_Number", res_name)]
  data_trimmed <- na.omit(subset_data)
  return(data_trimmed)  
}



#' Compute descriptive statistics by treatment group
#'
#' @param data A dataframe containing the trimmed dataset with Treatment_ID column
#' @param nam_col A character string specifying the column name for which to calculate statistics
#'
#' @returns A dataframe with descriptive statistics (mean, standard deviation, and standard error) grouped by Treatment_ID
#' @noRd
#' 
compute_descriptive_stats <- function(data = NULL, nam_col = NULL) {
  data[["Treatment_ID"]] <- forcats::as_factor(data[["Treatment_ID"]])
  Result <- data %>%
    group_by(.data[["Treatment_ID"]]) %>%
    summarise(
      Mean_score = round(x = mean(.data[[nam_col]], na.rm = TRUE), digits = 2),
      Standard_Deviation = round(x = sd(.data[[nam_col]], na.rm = TRUE), digits = 2),
      Standard_error = round(x = sd(.data[[nam_col]], na.rm = TRUE) / sqrt(n()), digits = 2)
    )
  return(Result)
}



#' Partition analysis results into a structured list
#'
#' @param descript_stat A dataframe containing descriptive statistics
#' @param anova_result A list containing ANOVA results, either non-parametric test results or parametric ANOVA with post-hoc tests
#'
#' @returns A list containing descriptive statistics and appropriate statistical test results. For non-parametric tests, returns descriptive statistics and non-parametric results. For parametric tests, returns descriptive statistics, ANOVA results, Tukey test results, and LSD test results.
#' @noRd
#' 
list_result <- function(descript_stat, anova_result) {
  if (length(names(anova_result)) == 1) {
    result <- list(descript_stat = descript_stat, non_parametric = anova_result[["non_parametric"]])
  } else {
    result <- list(
      descript_stat = descript_stat,
      Anova_result = anova_result[["Anova_result"]],
      Tukey_result = anova_result[["Tukey_result"]],
      LSD_result = anova_result[["LSD_result"]]
    )
  }
  return(result)
}


#' Test ANOVA assumptions using Shapiro-Wilk and Levene's tests
#'
#' @param data A dataframe containing the dataset with Treatment_ID column
#' @param nam_col A character string specifying the column name for the response variable to test
#'
#' @returns A list containing results from the Shapiro-Wilk normality test and Levene's test for homogeneity of variance. Issues warnings if assumptions are violated (p < 0.05 for both tests).
#' @noRd
#' 
shap.lev_test_func <- function(data, nam_col) {
  # Shapiro test
  shapiro_result <- shapiro.test(data[[nam_col]])
  # Levene's test
  Treatment_ID <- data[["Treatment_ID"]]
  nam_col <- data[[nam_col]]
  levenes_result <- car::leveneTest(aov(formula = nam_col ~ Treatment_ID, data = data))
  if (shapiro_result$p.value < 0.05 & levenes_result$`Pr(>F)`[1] < 0.05) {
    warning("We entreat you transform the data before proceeding!\n")
    warning("You can verify from the Normality Test (Shapiro Test) and Levene's Test (Homogeneity of Variance) below:\n\n")
  }
  # Return list with shapiro and levenes test results
  return(list(shapiro_result = shapiro_result, levenes_result = levenes_result))
}
 


#' Automatically normalize data using Yeo-Johnson power transformation
#'
#' @param data A dataframe containing the dataset with Treatment_ID column
#' @param nam_col A character string specifying the column name for the response variable to transform
#' @param alpha Significance level for assumption tests (default: 0.05)
#' @param max_iter Maximum number of lambda values to test (default: 20)
#'
#' @returns A list containing transformation status, lambda value used, assumption test results, and the dataframe with a new transformed column. If assumptions are met without transformation, returns original data. If transformation fails to meet assumptions, suggests non-parametric testing.
#' @noRd
#' 
auto_normalize <- function(data, nam_col, alpha = 0.05, max_iter = 20) {
  # Function to check ANOVA assumptions
  check_anova_assumptions <- function(response, group) {
    group <- as.factor(group)
    model <- aov(response ~ group)
    res <- residuals(model)
    
    # Normality of residuals
    shapiro_p <- shapiro.test(res)$p.value
    
    # Homogeneity of variances
    levene_p <- car::leveneTest(response ~ group)$`Pr(>F)`[1]
    
    return(list(shapiro_p = shapiro_p, levene_p = levene_p))
  }
  
  # Initial assumption check
  assumptions <- check_anova_assumptions(data[[nam_col]], data[['Treatment_ID']])
  
  if (assumptions$shapiro_p > alpha & assumptions$levene_p > alpha) {
    message("Assumptions met without transformation ✅")
    data[[paste0(nam_col, "_transformed")]] <- data[[nam_col]]
    return(list(
      status = "No transformation needed",
      lambda_used = NA,
      assumptions = assumptions,
      data = data
    ))
  }
  
  # Estimate best lambda
  lambda_opt <- car::powerTransform(data[[nam_col]], family = "yjPower")$lambda
  
  # Try lambdas in a sequence around estimated lambda
  lambda_seq <- seq(lambda_opt - 0.5, lambda_opt + 0.5, length.out = max_iter)
  
  for (lambda in lambda_seq) {
    transformed <- car::yjPower(data[[nam_col]], lambda)
    assumptions <- check_anova_assumptions(transformed, data[['Treatment_ID']])
    
    if (assumptions$shapiro_p > alpha & assumptions$levene_p > alpha) {
      message(sprintf("Assumptions met with lambda = %.3f ✅", lambda))
      data[[paste0(nam_col, "_transformed")]] <- transformed
      
      return(list(
        status = "Transformed",
        lambda_used = lambda,
        assumptions = assumptions,
        data = data
      ))
    }
  }
  
  # If no lambda succeeded
  message("Assumptions not met after transformations — consider non-parametric test ❗")
  transformed <- car::yjPower(data[[nam_col]], lambda_opt)
  data[[paste0(nam_col, "_transformed")]] <- transformed
  
  return(list(
    status = "Non-parametric suggested",
    lambda_used = lambda_opt,
    assumptions = assumptions,
    data = data
  ))
}


 

#' Partition and reformat LSD test results for display
#'
#' @param lsd_result A list containing LSD test results with components: groups, means, and statistics
#'
#' @returns A dataframe combining LSD statistics, means, and group classifications in a formatted layout with separating columns for better readability. Numeric values are rounded to 3 decimal places.
#' @noRd
#' 
lsd_partioner <- function(lsd_result) {
  lsd_groups <- tibble::rownames_to_column(lsd_result[["groups"]], var = "Treatments")
  lsd_means <- tibble::rownames_to_column(lsd_result[["means"]], var = "Treatments")
  # Change column names for lsd_means
  colnames(lsd_means)[2] <- "Means"
  # Get indices of numeric columns
  numeric_cols <- sapply(lsd_means, is.numeric)
  # Apply rounding only to numeric columns
  lsd_means[numeric_cols] <- round(lsd_means[numeric_cols], digits = 3)
  lsd_statistics <- t(lsd_result[["statistics"]]) |> as.data.frame()
  lsd_statistics <- round(x = lsd_statistics, digits = 3)
  colnames(lsd_statistics) <- "Value"
  lsd_statistics <- tibble::rownames_to_column(lsd_statistics, var = "Statistic")
  # Adjust length of statistics to the others
  if (nrow(lsd_statistics) < nrow(lsd_means)) {
    diff <- nrow(lsd_means) - nrow(lsd_statistics)
    i <- 1
    repeat{
      lsd_statistics <- rbind(lsd_statistics, rep(NA, ncol(lsd_statistics)))
      if (i == diff) break
      i <- i + 1
    }
    
    final_output <- cbind(lsd_statistics, '', lsd_means, '', lsd_groups)
    # Adjust the length of others to statistics if its different
  } else if (nrow(lsd_statistics) > nrow(lsd_means)) {
    diff <- nrow(lsd_statistics) - nrow(lsd_means)
    i <- 1
    repeat{
      lsd_means <- rbind(lsd_means, rep(NA, ncol(lsd_means)))
      lsd_groups <- rbind(lsd_groups, rep(NA, ncol(lsd_groups)))
      if (i == diff) break
      i <- i + 1
    }
    final_output <- cbind.data.frame(lsd_statistics, '', lsd_means, '', lsd_groups)
  } else {
    final_output <- cbind(lsd_statistics, '', lsd_means, '', lsd_groups)
  }
  return(final_output)
}


#' Assign significance symbols based on p-values
#'
#' @param p A numeric p-value or NA
#'
#' @returns A character string representing significance level 
#' @noRd
#' 
get_significance <- function(p) {
  if (is.na(p)) {
    return("NA")
  } else if (p <= 0.001) {
    return("***")
  } else if (p <= 0.01) {
    return("**")
  } else if (p <= 0.05) {
    return("*")
  } else if (p <= 0.1) {
    return(".")
  } else {
    return("ns")
  }
}



#' Perform parametric ANOVA tests with post-hoc comparisons
#'
#' @param data A list containing transformation results with components: 'assumptions' (test results), 'data' (dataframe), 'lambda_used' (transformation parameter), and 'status' (transformation status)
#'
#' @returns A list containing either non-parametric test results (if normality assumption fails) or parametric test results including ANOVA results, Tukey HSD post-hoc tests, and LSD test results with back-transformed means when applicable
#' @noRd
#' 
anova_function <- function(data) {
  holdnames <- names(data)

  s <- data[['assumptions']][['shapiro_p']]

  if (s < 0.05) {
    non_parametric <- non_para_test(data = data[['data']])
    return(list(non_parametric = non_parametric))
  } else {
    
    data1 <- data[['data']]


    colname_data <- colnames(data1)
    nam_col <- colname_data[length(colname_data)]

    Treatment_ID <- data1[[colname_data[1]]] |> as.factor() # get col for treatment ID
    response_variable <- data1[[nam_col]] # get col for response variable

    # Anova section
    Anova_result <- stats::aov(formula = response_variable ~ Treatment_ID, data = data1) |> broom::tidy()
    # Standardizing results
    Anova_result$sumsq <- round(Anova_result$sumsq, digits = 3)
    Anova_result$meansq <- round(Anova_result$meansq, digits = 3)
    Anova_result$statistic[1] <- formatC(x = Anova_result[['statistic']][1], digits = 3,format = 'f')
    Anova_result$p.value[1] <- formatC(x = Anova_result[['p.value']][1], digits = 5,format = 'f')

    # get anova raw model
    anova_model <- stats::aov(formula = response_variable ~ Treatment_ID, data = data1)

    # Tukey section
    Tukey_result <- stats::TukeyHSD(anova_model) |> broom::tidy()
    Tukey_result$estimate <- round(Tukey_result$estimate, digits = 3)
    Tukey_result$conf.low <- round(Tukey_result$conf.low, digits = 3)
    Tukey_result$conf.high <- round(Tukey_result$conf.high, digits = 3)
    Tukey_result$adj.p.value <- formatC(x = Tukey_result[['adj.p.value']], digits = 4,format = 'f')
    
    # Append significance.
    Tukey_result$significance <- sapply(as.numeric(Tukey_result[['adj.p.value']]), get_significance)
    
    # LSD section
    Lsd_result <- agricolae::LSD.test(y = anova_model, trt = "Treatment_ID")
    
    # Back transform.
    # Subset just groups.
    Lsd_test_groups <- Lsd_result$groups
    
    # Back transform.
    if(is.null(data[['lambda_used']])){
      
      Lsd_test_groups$back_transformed <-  Lsd_test_groups[[1]]
      
    } else{
      
      Lsd_test_groups$back_transformed <- back_transform_yeo(Lsd_test_groups[[1]] ,lambda = data[['lambda_used']]) |> round(digits = 3)
      
    }
    # replace groups with new dataframe
    Lsd_result$groups <- Lsd_test_groups
    # change name
    colnames(Lsd_result$groups)[1] <- 'Mean'
    
    LSD_result <- lsd_partioner(lsd_result = Lsd_result) # Combine subsets as one.
    

    return(list(Anova_result = Anova_result, Tukey_result = Tukey_result, LSD_result = LSD_result))
  }
}

 
#' Perform non-parametric Kruskal-Wallis test with post-hoc comparisons
#'
#' @param data A dataframe where the first column is the treatment/group variable and the third column is the response variable
#'
#' @returns A dataframe combining Kruskal-Wallis test results and Dunn's post-hoc test results with Bonferroni correction. The first row contains the overall test statistics, and subsequent rows contain pairwise comparison results.
#' @noRd
#' 
non_para_test <- function(data) {
  col_name <- colnames(data)
  Treatment <- col_name[1]
  Parameter <- col_name[3]

  Non_parametric_test <- kruskal.test(data[[Parameter]] ~ data[[Treatment]]) |> broom::tidy()
  # Convert to standard form.
  Non_parametric_test[2] <- formatC(x = Non_parametric_test[[2]], digits = 5,format = 'f')

  Non_parametric_test[1] <- formatC(x = Non_parametric_test[[1]], digits = 5,format = 'f' )

  names(Non_parametric_test)[3] <- "df" # assign df column name

  # Perform post hoc tests.
  fomula <- paste(Parameter,Treatment,sep = '~') |> as.formula()
  
  Pairwise.wilcox_test <- rstatix::dunn_test(data = data,formula = fomula , p.adjust.method = "bonferroni") 
  # format p to make sure values are readable.
  Pairwise.wilcox_test["p"] <- formatC(Pairwise.wilcox_test[["p"]],digits = 5,format = 'f')
  
  Pairwise.wilcox_test['p.adj'] <- formatC(Pairwise.wilcox_test[['p.adj']],digits = 5,format = 'f')
  Pairwise.wilcox_test$statistic <- round(Pairwise.wilcox_test$statistic,digits = 4)
 
   # Dataframe to hold results
  datta <- as.data.frame(matrix(nrow = nrow(Pairwise.wilcox_test), ncol = 13))
  datta[1, 1:4] <- Non_parametric_test 
  datta[5:ncol(datta)] <- Pairwise.wilcox_test

  colnames(datta) <- c(colnames(Non_parametric_test), colnames(Pairwise.wilcox_test))
  return(datta)
  
}


#' Interpret Yeo-Johnson transformation results
#'
#' @param data A list containing 'lambda_used' and 'assumptions' with 'shapiro_p' value
#'
#' @returns Character vector with interpretation of transformation results and normality assessment
#' @noRd
interpret_yeojohnson <- function(data) {

  lambda <- data[['lambda_used']]
  p_value <- data[['assumptions']][['shapiro_p']]

  if (p_value < 0.05) {
    result <- c(" Normality & Homogenity of Variance test, Failed!❌ ", " Data does not meet the assumptions of Anova.",  " Best to use Non-parametric tests✅ ")
    return(noquote(result))
  } else if (p_value > 0.05) {
    result <- c()

    result <- c(result, "🔍 Analyzing Yeo-Johnson Transformation")
    result <- c(result, "──────────────────────────────────────────────")
    result <- c(result, paste("Lambda (λ):", lambda))

    # Basic transformation meaning
    interpretation <- if (lambda == 1) {
      "No transformation (linear scale, identity)"
    } else if (lambda == 0) {
      "Log transformation for y ≥ 0; quadratic-like for y < 0"
    } else if (lambda == 2) {
      "Square transformation for y ≥ 0; log transform for y < 0"
    } else if (lambda > 1) {
      "Stretches higher values more (like squaring); helps left-skew"
    } else if (lambda > 0 & lambda < 1) {
      "Mild transformation (e.g., square root); good for moderate right-skew"
    } else if (lambda < 0) {
      "Strong compression (inverse-like); used for severe right-skew"
    } else {
      "Unusual lambda value; custom interpretation may be needed"
    }

    result <- c(result, "📌 Interpretation:")
    result <- c(result, paste("  ", interpretation))

    # Normality test (p-value)
    if (!is.na(p_value)) {
      result <- c(result, "──────────────────────────────────────────────")
      result <- c(result, paste("P-value from normality test:", round(p_value, 5)))

      if (p_value < 0.05) {
        result <- c(result, "📉 The transformation significantly improved normality (p < 0.05).")
      } else if (p_value < 0.10) {
        result <- c(result, "⚠️ Some improvement in normality (borderline case, p ~ 0.1).")
      } else {
        result <- c(result, "✅ No significant difference — data may have been already close to normal.")
      }
    }

    return(paste(noquote(result)))
  }
}


#' Generate barplot for discrete variables
#'
#' @param data A dataframe containing 'Treatment_ID', 'Mean_score', and 'Standard_error' columns
#' @param x_title Character. Title for x-axis (default: "x axis title")
#' @param y_title Character. Title for y-axis (default: "y axis title")
#' @param Title Character. Main plot title (default: "Title of graph")
#' @param axis_title_size Numeric. Font size for axis titles (default: 14)
#' @param axis_text_size Numeric. Font size for axis text (default: 13)
#' @param plot_title_size Numeric. Font size for plot title (default: 16)
#' @param bar_color Character. Color for bars (default: "cornflowerblue")
#'
#' @returns A ggplot2 object containing a bar plot with error bars
#' @noRd
plot_graph <- function(data,
                       # segment = 5,
                       x_title = "x axis title",
                       y_title = "y axis title",
                       Title = "Title of graph",
                       axis_title_size = 14,
                       axis_text_size = 13,
                       plot_title_size = 16,
                       # max_value = 30,
                       bar_color = "cornflowerblue" # Ensure default color
) {
  plot_result <- ggplot2::ggplot(data = data, mapping = aes(x = .data[["Treatment_ID"]], y = Mean_score)) +
    ggplot2::geom_bar(stat = "identity", fill = bar_color, width = 0.7, colour = "black") +
    ggplot2::geom_errorbar(mapping = aes(ymin = Mean_score - Standard_error, ymax = Mean_score + Standard_error), width = 0.2) +
    ggplot2::theme_classic(base_line_size = 1) +
    # geom_text(label = data[['Mean_score']],nudge_x = 0.2 ,nudge_y = -0.25,colour = 'darkred' ,size = 6,fontface = 'bold' )+
    ggplot2::labs(x = x_title, y = y_title, title = Title) +
    ggplot2::theme(
      plot.title = element_text(face = "bold", size = plot_title_size, hjust = 0.5),
      axis.title = element_text(face = "bold", size = axis_title_size),
      axis.text = element_text(colour = "black", size = axis_text_size),
      axis.ticks = element_line(linewidth = 1)
    )

  return(plot_result)
}



#' Generate boxplot for data visualization
#'
#' @param data A dataframe containing 'Treatment_ID' column and the specified response variable
#' @param x_title Character. Title for x-axis (default: "Treatment")
#' @param y_title Character. Title for y-axis (default: "Mean_plant height")
#' @param Title Character. Main plot title (default: "Tissue Culture Experiment")
#' @param axis_title_size Numeric. Font size for axis titles (default: 14)
#' @param axis_text_size Numeric. Font size for axis text (default: 13)
#' @param plot_title_size Numeric. Font size for plot title (default: 16)
#' @param nam_col Character. Column name for y-axis variable (default: "User selected Column")
#' @param bar_colour Character vector. Colors for boxplots (default: c("skyblue", "grey", "orange", "pink", "lightgreen", "lightyellow", "yellow"))
#'
#' @returns A ggplot2 object containing a boxplot
#' @noRd
plot_boxplot <- function(data,
                         x_title = "Treatment",
                         y_title = "Mean_plant height",
                         Title = "Tissue Culture Experiment",
                         axis_title_size = 14,
                         axis_text_size = 13,
                         plot_title_size = 16,
                         nam_col = "User selected Column",
                         bar_colour = c("skyblue", "grey", "orange", "pink", "lightgreen", "lightyellow", "yellow")) {
  plot_result <- ggplot2::ggplot(data, aes(x = factor(x = .data[["Treatment_ID"]], levels = unique(.data[["Treatment_ID"]])), y = .data[[nam_col]])) +
    ggplot2::geom_boxplot(fill = bar_colour, color = "black", width = 0.7, outlier.shape = 16, outlier.size = 3) +
    ggplot2::theme_classic(base_line_size = 1) +
    ggplot2::labs(x = x_title, y = y_title, title = Title) +
    ggplot2::theme(
      plot.title = element_text(face = "bold", size = plot_title_size, hjust = 0.5),
      axis.title = element_text(face = "bold", size = axis_title_size),
      axis.text = element_text(color = "black", size = axis_text_size),
      axis.ticks = element_line(linewidth = 1)
    )
  return(plot_result)
}


#----------------------- Functions to Generate Datasheet -----------------------
# Function to generate Single Factor Treatment data sheet
generate_single_factor_sheet <- function(experiment_title, researcher, collection_frequency, num_records, treatments, replicates_per_treatment, parameters) {
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Sheet1")

  # Write experiment title and details
  openxlsx::mergeCells(wb, sheet = 1, cols = 1:2, rows = 1)
  openxlsx::writeData(wb, sheet = 1, x = c("Experiment Title :", experiment_title), startCol = 1, startRow = 1, colNames = FALSE)

  openxlsx::mergeCells(wb, sheet = 1, cols = 4:5, rows = 1)
  openxlsx::writeData(wb, sheet = 1, x = c("Researcher's Name :", researcher), startCol = 4, startRow = 1, colNames = FALSE)

  openxlsx::mergeCells(wb, sheet = 1, cols = 7:8, rows = 1)
  openxlsx::writeData(wb, sheet = 1, x = c("Collection Frequency (Weeks):", collection_frequency), startCol = 7, startRow = 1, colNames = FALSE)

  # Write table headers
  openxlsx::writeData(wb, sheet = 1, x = "Date", startRow = 4, startCol = 1, colNames = FALSE)
  openxlsx::writeData(wb, sheet = 1, x = "Week", startRow = 4, startCol = 2, colNames = FALSE)
  openxlsx::writeData(wb, sheet = 1, x = "Treatment_ID", startRow = 4, startCol = 3, colNames = FALSE)
  openxlsx::writeData(wb, sheet = 1, x = "Replicate_Number", startRow = 4, startCol = 4, colNames = FALSE)

  for (i in 1:length(parameters)) {
    openxlsx::writeData(wb, sheet = 1, x = parameters[i], startCol = i + 4, startRow = 4, colNames = FALSE)
  }

  # Translate collection frequency to weeks
  frequency_in_weeks <- as.numeric(gsub(" weeks?", "", collection_frequency))

  data <- data.frame(Date = as.Date(character()), Week = numeric(), Treatment_ID = character(), Replicate_Number = integer())
  for (param in parameters) {
    data[[param]] <- numeric()
  }

  row_start <- 5
  for (record in 1:num_records) {
    current_date <- Sys.Date() + (frequency_in_weeks * (record - 1) * 7)
    week_number <- frequency_in_weeks * record
    for (treatment in treatments) {
      for (replicate in 1:replicates_per_treatment) {
        new_row <- data.frame(Date = current_date, Week = week_number, Treatment_ID = treatment, Replicate_Number = replicate)
        for (param in parameters) {
          new_row[[param]] <- NA
        }
        data <- rbind(data, new_row)
        openxlsx::writeData(wb, sheet = 1, x = current_date, startCol = 1, startRow = row_start, colNames = FALSE)
        openxlsx::writeData(wb, sheet = 1, x = week_number, startCol = 2, startRow = row_start, colNames = FALSE)
        openxlsx::writeData(wb, sheet = 1, x = treatment, startCol = 3, startRow = row_start, colNames = FALSE)
        openxlsx::writeData(wb, sheet = 1, x = replicate, startCol = 4, startRow = row_start, colNames = FALSE)
        row_start <- row_start + 1
      }
    }
  }

  file <- tempfile(fileext = ".xlsx")
  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)

  return(list(file = file, data = data))
}



# Function to generate Factorial Treatment data sheet
generate_factorial_sheet <- function(experiment_title, researcher, collection_frequency, num_records, replicates_per_treatment, parameters, num_factors, treatments) {
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Sheet1")

  # Write experiment title and details
  openxlsx::mergeCells(wb, sheet = 1, cols = 1:2, rows = 1)
  openxlsx::writeData(wb, sheet = 1, x = c("Experiment Title :", experiment_title), startCol = 1, startRow = 1, colNames = FALSE)

  openxlsx::mergeCells(wb, sheet = 1, cols = 4:5, rows = 1)
  openxlsx::writeData(wb, sheet = 1, x = c("Researcher's Name :", researcher), startCol = 4, startRow = 1, colNames = FALSE)

  openxlsx::mergeCells(wb, sheet = 1, cols = 7:8, rows = 1)
  openxlsx::writeData(wb, sheet = 1, x = c("Collection Frequency (Weeks):", collection_frequency), startCol = 7, startRow = 1, colNames = FALSE)

  # Write headers
  openxlsx::writeData(wb, sheet = 1, x = "Date", startRow = 4, startCol = 1, colNames = FALSE)
  openxlsx::writeData(wb, sheet = 1, x = "Week", startRow = 4, startCol = 2, colNames = FALSE)

  # Write headers for each treatment factor
  for (i in 1:num_factors) {
    openxlsx::writeData(wb, sheet = 1, x = paste0("Treatment_Factor ", i), startRow = 4, startCol = i + 2, colNames = FALSE)
  }

  # Write headers for replicate and parameters
  openxlsx::writeData(wb, sheet = 1, x = "Replicate", startRow = 4, startCol = num_factors + 3, colNames = FALSE)
  for (i in 1:length(parameters)) {
    openxlsx::writeData(wb, sheet = 1, x = parameters[i], startRow = 4, startCol = i + num_factors + 3, colNames = FALSE)
  }

  # Translate collection frequency to weeks
  frequency_in_weeks <- as.numeric(gsub(" weeks?", "", collection_frequency))

  row_start <- 5
  data <- data.frame(Date = as.Date(character()), Week = integer(), Replicate = integer())
  for (i in 1:num_factors) {
    data[[paste0("Treatment_Factor ", i)]] <- character()
  }
  for (param in parameters) {
    data[[param]] <- numeric()
  }

  # Generate combinations of treatments for all factors
  treatment_combinations <- expand.grid(treatments, stringsAsFactors = FALSE)

  for (record in 1:num_records) {
    current_date <- Sys.Date() + (frequency_in_weeks * (record - 1) * 7)
    week_number <- frequency_in_weeks * record
    for (i in 1:nrow(treatment_combinations)) {
      for (replicate in 1:replicates_per_treatment) {
        new_row <- data.frame(
          Date = current_date, Week = week_number, Replicate = replicate
        )

        # Add treatment combinations
        for (j in 1:num_factors) {
          new_row[[paste0("Treatment_Factor ", j)]] <- treatment_combinations[i, j]
        }

        # Initialize parameter columns with NA
        for (param in parameters) {
          new_row[[param]] <- NA
        }

        data <- rbind(data, new_row)

        # Write to Excel
        openxlsx::writeData(wb, sheet = 1, x = current_date, startCol = 1, startRow = row_start, colNames = FALSE)
        openxlsx::writeData(wb, sheet = 1, x = week_number, startCol = 2, startRow = row_start, colNames = FALSE)

        for (j in 1:num_factors) {
          openxlsx::writeData(wb, sheet = 1, x = treatment_combinations[i, j], startCol = j + 2, startRow = row_start, colNames = FALSE)
        }

        openxlsx::writeData(wb, sheet = 1, x = replicate, startCol = num_factors + 3, startRow = row_start, colNames = FALSE)
        row_start <- row_start + 1
      }
    }
  }

  file <- tempfile(fileext = ".xlsx")
  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)

  return(list(file = file, data = data))
}



# 
# Standard_form <- function(x, digits = 3) {
#   # Take out NA's
#   x <- x[!is.na(x) & !is.null(x)]
# 
#   # Empty list to store result.
#   list_obj <- vector(mode = "list", length = length(x))
# 
#   check_num <- seq_len(9) # who numbers to compare to and render as default
# 
#   for (variable in 1:length(x)) {
#     # if(any(x[variable] %in% check_num)) list_obj[[variable]] <- x[variable]
# 
#     # Format the number with scientific notation
#     base <- formatC(x[variable], format = "e", digits = digits)
# 
#     # Parse into components
#     parts <- strsplit(x = base, split = "e") |> unlist() # vectorise it, by default it returns a list
#     numr <- parts[1]
#     exponent <- as.numeric(parts[2])
# 
#     # Create pretty format with proper symbols
#     if (exponent == 0) {
#       list_obj[[variable]] <- numr
#     } else {
#       # Use Unicode for superscript numbers
#       # Don't bother about this, you could use ms word to generate this, copy and paste
#       # Or you can use unicode in R, wrap them in paste or cat and later copy and paste, hahah
#       sup_chars <- c("⁰", "¹", "²", "³", "⁴", "⁵", "⁶", "⁷", "⁸", "⁹", "⁻")
# 
#       # Convert exponent to superscript characters
#       exp_str <- as.character(abs(exponent))
#       exp_chars <- strsplit(exp_str, "")[[1]]
# 
#       sup_exp <- paste(sapply(exp_chars, function(d) sup_chars[as.numeric(d) + 1]), collapse = "")
# 
#       if (exponent < 0) sup_exp <- paste0(sup_chars[11], sup_exp)
# 
#       list_obj[[variable]] <- (paste0(numr, " × 10", sup_exp)) # else
#     }
#   }
# 
#   return(unlist(list_obj)) # unlist and return as object.
# }



#' Generate trend data from Excel or CSV files
#'
#' @param file_path Character. Path to the input file (Excel or CSV format)
#'
#' @returns A dataframe with standardized column names where the first four columns are 'Date', 'Week', 'Treatment_ID', and 'Replicate_Number'. Spaces in column names are replaced with underscores.
#' @noRd
#' 
Generate_trend_data <- function(file_path){
  # Determine file extension
  ext <- tools::file_ext(file_path)
  
  # Read file based on extension, adding an error handling script.
  tryCatch({
    if (ext %in% c("xlsx", "xls")) {
      a <- readxl::read_excel(file_path,
                              skip = 3,
                              .name_repair = "minimal"
      )
    } else if (ext == "csv") {
      a <- read.csv(file_path, stringsAsFactors = FALSE, skip = 3)
    } else {
      stop("Unsupported file format. Please upload an Excel (.xlsx, .xls) or CSV file.")
    }
  })
  
  # Refit names.
  colnames(a)[1:4] <- c('Date','Week','Treatment_ID','Replicate_Number')
  
  # Replace spaces with underscores
  colnames(a) <- sapply(colnames(a), function(x) gsub(" ", "_", x))
  
  return(a)
  
}



#' Generate trend plot over weeks by treatment
#'
#' @param data A dataframe containing 'Treatment_ID', 'Week', and the specified parameter column
#' @param parameter Character. Name of the parameter column to plot
#' @param smooth Logical. If TRUE, generates smooth trend lines; if FALSE, generates point-and-line plot (default: TRUE)
#'
#' @returns A ggplot2 object showing parameter trends over weeks grouped by treatment
#' @noRd
#' 
Generate_trends <- function(data, parameter,smooth = TRUE ){
  #Remove column names with nothing.
  data <-  data[!(colnames(data) %in% '')] 
  # Convert parameter column to numeric
  data[[parameter]] <- data[[parameter]] |> as.numeric()
  # cleaned data
  data <-  na.omit(data[c('Treatment_ID', 'Week', parameter)])
  
  na_null_res <- data %>%
    group_by(Treatment_ID, Week) %>%
    summarise(
      parameter_1 = mean(.data[[parameter]], na.rm = TRUE),
      .groups = "drop"
    )
  # If user chooses smooth trends
  if(smooth){
    p <- ggplot(na_null_res, aes(x = Week, y = parameter_1, color = Treatment_ID)) +
      geom_smooth(se = FALSE) +
      theme_classic() +
      labs(
        title = paste0(gsub('_',' ',parameter), " Trend Over Weeks by Treatment"),
        y = paste("Mean", gsub('_',' ',parameter)),
        x = "Week"
      ) +
      theme(
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        axis.title = element_text(face = "bold", size = 14),
        axis.text = element_text(colour = "black", size = 13),
        legend.text = element_text(colour = "black", size = 13),
        legend.title = element_text(face = "bold", size = 14)
      ) 
  }else{
    p <- ggplot(na_null_res, aes(x = Week, y = parameter_1, color = Treatment_ID)) +
      geom_point(size = 3) +
      geom_line(linewidth = 0.7) +
      theme_classic() +
      labs(
        title = paste0(gsub('_',' ',parameter), " Trend Over Weeks by Treatment"),
        y = paste("Mean", gsub('_',' ',parameter)),
        x = "Week"
      ) +
      theme(
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        axis.title = element_text(face = "bold", size = 14),
        axis.text = element_text(colour = "black", size = 13),
        legend.text = element_text(colour = "black", size = 13),
        legend.title = element_text(face = "bold", size = 14)
      ) 
  }
  
  
  return(p)
}