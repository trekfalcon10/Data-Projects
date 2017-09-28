# Load all packages
library(ggplot2)
library(reshape2)
library(dplyr)
library(ggthemes)
library(rio)
library(haven)

# convert SPSS to csv
adult <- convert("/Volumes/Backup_Chris/Dropbox/R_Projects/ADULT.sav", "ADULT.csv")

#read in csv file
adult <- read.csv(adult, stringsAsFactors = FALSE)

#Reduce number of variables to required 10
adult_reduced <- select(adult, RBMI, BMI_P, RACEHPR2, SRSEX, 
                        SRAGE_P, MARIT2, AB1, ASTCUR, AB51, POVLL)
adult <- adult_reduced

# Get subset of races included
included_races <- subset(adult, subset = ((adult$RACEHPR2 != "2") & 
                (adult$RACEHPR2 != "3") & (adult$RACEHPR2 != "7")))

adult <- included_races

# Remove individual aboves 84
adult <- subset(adult, subset = (adult$SRAGE_P <= 84))

# Remove individuals with a BMI below 16 and above or equal to 52
adult <- subset(adult, subset = (adult$BMI_P >= 16 & adult$BMI_P < 52))

# Relabel the race variable:
adult$RACEHPR2 <- factor(adult$RACEHPR2, labels = c("Latino", "Asian", "African American", "White"))

# Relabel the BMI categories variable:
adult$RBMI <- factor(adult$RBMI, labels = c("Under-weight", "Normal-weight", "Over-weight", "Obese"))

#Relabel the POVLL variable:
adult$POVLL <- factor(adult$POVLL, labels = 
              c("0-99% FPL", "100-199%", "200-299%", "300% +"))

# Script generalized into a function
mosaicGG <- function(data, X, FILL) {
  
  # Proportions in raw data
  DF <- as.data.frame.matrix(table(data[[X]], data[[FILL]]))
  DF$groupSum <- rowSums(DF)
  DF$xmax <- cumsum(DF$groupSum)
  DF$xmin <- DF$xmax - DF$groupSum
  DF$X <- row.names(DF)
  DF$groupSum <- NULL
  DF_melted <- melt(DF, id = c("X", "xmin", "xmax"), variable.name = "FILL")
  library(dplyr)
  DF_melted <- DF_melted %>%
    group_by(X) %>%
    mutate(ymax = cumsum(value/sum(value)),
           ymin = ymax - value/sum(value))
  
  # Chi-sq test
  results <- chisq.test(table(data[[FILL]], data[[X]])) # fill and then x
  resid <- melt(results$residuals)
  names(resid) <- c("FILL", "X", "residual")
  
  # Merge data
  DF_all <- merge(DF_melted, resid)
  
  # Positions for labels
  DF_all$xtext <- DF_all$xmin + (DF_all$xmax - DF_all$xmin)/2
  index <- DF_all$xmax == max(DF_all$xmax)
  DF_all$ytext <- DF_all$ymin[index] + (DF_all$ymax[index] - DF_all$ymin[index])/2
  
  # plot:
  g <- ggplot(DF_all, aes(ymin = ymin,  ymax = ymax, xmin = xmin,
                          xmax = xmax, fill = residual)) +
    geom_rect(col = "white") +
    geom_text(aes(x = xtext, label = X),
              y = 1, size = 3, angle = 90, hjust = 1, show.legend = FALSE) +
    geom_text(aes(x = max(xmax),  y = ytext, label = FILL),
              size = 3, hjust = 1, show.legend = FALSE) +
    scale_fill_gradient2("Residuals") +
    scale_x_continuous("Individuals", expand = c(0,0)) +
    scale_y_continuous("Proportion", expand = c(0,0)) +
    theme_tufte() +
    theme(legend.position = "bottom")
  print(g)
}

# BMI described by age
mosaicGG(adult, "SRAGE_P", "RBMI")

# Poverty described by age
mosaicGG(adult, "SRAGE_P", "POVLL")

# mtcars: am described by cyl
mosaicGG(mtcars, "cyl", "am")

# Vocab: vocabulary described by education
library(car)
mosaicGG(Vocab, "education", "vocabulary")
