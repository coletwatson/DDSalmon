# DDSalmon
Density dependence analysis for Bristol Bay Sockeye Salmon
##Density Dependence analysis of Sockeye Salmon in Bristol Bay AK (Modeling)
#Cole Watson November 5, 2024

#Load in packages
#Dpylr = data manipulation, ggplot2 = plot graphics, lme4 = mixed models, tibble = simple data frames, patchwork = plot orientation
library(dplyr)
library(ggplot2)
library(lme4)
library(tibble)
library(patchwork)

#Read in data
complete <- read.csv("Excel Files/CompleteData.csv")

#Getcombinations of OceanAge and sex
combos <- expand.grid(OceanAge = unique(complete$OceanAge),
                                    sex = unique(complete$sex))

#Lists to store models and plots
models <- list()
plots <- list()

#Loop through combinations of ocean age and sex for models and plots
for (i in 1:nrow(combos)) {
  #Get current combination of ocean age and sex
  age <- combos$OceanAge[i]
  sex <- combos$sex[i]
  
  #Filter data for the specific combination of ocean age and sex
  subset <- subset(complete, OceanAge == age & sex == sex)
  
  #Fit the model
  if (nrow(subset) > 1) {
    modname <- paste0("mod_", age, "_", sex)
    models[[modname]] <- glm(Length ~ BBrun.m, data = subset)
    
    #Create a plot for combinations of ocean age and sex
    plot <- ggplot(subset, aes(x = BBrun.m, y = Length)) +
      geom_point(size = 1, alpha = 0.5) +
      geom_smooth(method = "lm", formula = y ~ x, se = TRUE, color = 'blue') +
      labs(x = "Total Run (millions of fish)", y = "Length (mm)", 
           title = paste(age, "Ocean", sex)) +
      xlim(10, 90) +
      ylim(250, 600)
    
    #store plots
    plotname <- paste0(age, "Ocean", sex)
    plots[[plotname]] <- plot
  }
}

#combine plots
combined <- wrap_plots(plots, ncol = 2)  # Adjust ncol for layout preference
print(combined)

#save combined
ggsave("General Linear Models.png", combined, path = "File Outputs/General Linear Plots", width = 12, height = 8)

#store slope/intercepts
coef <- data.frame(OceanAge = integer(),
                              sex = character(),
                              Intercept = numeric(),
                              Slope = numeric())

#Loop models for coefficients
for (i in 1:nrow(combos)) {
  age <- combos$OceanAge[i]
  sex <- combos$sex[i]
  modname <- paste0("mod_", age, "_", sex)
  
  if (!is.null(models[[modname]])) {
    #Intercept/slope for Length ~ BBrun.m
    coefficients <- coef(models[[modname]])
    intercept <- coefficients[1]
    slope <- coefficients[2]
    
    #Put coefficents in dataframe
    coef <- rbind(coef,
                             data.frame(OceanAge = age, sex = sex, 
                                        Intercept = intercept, Slope = slope))
  }
}

print(coef)
