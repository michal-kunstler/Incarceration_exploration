library(dplyr)

data <- read.csv("Ankieta-wyniki_do_wczytania.csv", sep = ";", header = TRUE)

colnames(data)

data2 <- select(data, -Decyzja.Klasyfikacyjna, -Artykuł.Skazania, -X)

for (col in names(data2)) {
  data2[[col]] <- substr(data2[[col]], 1, 1)
  data2[[col]] <- as.factor(data2[[col]])
}

# Load required packages
library(ggcorrplot)
library(reshape2)
library(dplyr)
library(vcd)

#https://r-charts.com/part-whole/pie-chart-percentages-ggplot2/
# Convert factors to characters
data2[] <- lapply(data2, as.character)

# Compute Cramer's V
cor_matrix <- lapply(data2, function(x) lapply(data2, function(y) assocstats(table(x, y))$cramer))

# Convert to data frame
cor_df <- melt(do.call(rbind, cor_matrix))
names(cor_df) <- c("Variable1", "Variable2", "CramersV")

colnames(cor_df)[3:29] <- unique(as.character(cor_df$Variable1))

# Filter diagonal and lower triangle
cor_df <- cor_df %>%
  filter(Variable2 == "Korzyść.majątkowa") %>% 
  filter(row_number(Variable1) >= row_number(Variable2)) %>% 
  select(-Variable2)

rownames(cor_df) <- unique(as.character(cor_df$Variable1))

cor_df <- cor_df %>% select(-Variable1)
for (col in names(cor_df)) {
  cor_df[[col]] <- as.numeric(cor_df[[col]])
}


# Create correlation plot
ggcorrplot(cor_df, lab =TRUE)

len(colnames(data2))

plots <- list()
for(i in 1:length(colnames(data2))){
  # Data transformation
  df <- data2 %>% 
    group_by(!!as.name(colnames(data2)[i])) %>% # Variable to be transformed
    filter(!!as.name(colnames(data2)[i]) != "") %>% 
    filter(!!as.name(colnames(data2)[i]) != " ") %>% 
    count() %>% 
    ungroup() %>% 
    mutate(perc = `n` / sum(`n`)) %>% 
    arrange(perc) %>%
    mutate(labels = scales::percent(perc)) 
  

    plt <- eval(substitute(ggplot(df, aes(x = "", y = perc, fill = !!as.name(colnames(data2)[i]))) +
          geom_col() +
          geom_text(aes(label = labels),
                    position = position_stack(vjust = 0.5)) +
          coord_polar(theta = "y"), list(i=i)))
  
    print(i)
    print(plt)
    plots[[as.name(colnames(data2)[i])]] <- plt
    
    ggsave(path = "/Users/mkunstler/Desktop/wykresy_bernie", filename = paste(colnames(data2)[i], ".png"))
}



#install.packages("MASS")
library(MASS)


data3 <- data2 %>% select(-Zakład)

# Convert the column to an ordered factor
data3[["widzenia"]] <- factor(data3[["widzenia"]], ordered = TRUE)
data3[["liczba.telefonów"]] <- factor(data3[["liczba.telefonów"]], ordered = TRUE)
data3$środki.odurzające <- as.factor(data3$środki.odurzające)
data3$szczepienie <- as.factor(data3$szczepienie)
data3$długośćwykonywanej.kary <- as.factor(data3$długośćwykonywanej.kary)
data3$pozostały.okres.kary <- as.factor(data3$pozostały.okres.kary)
data3$wykształcenie <- as.factor(data3$wykształcenie)
data3$wiek.obecnie <- as.factor(data3$wiek.obecnie)

dummies <- model.matrix(~ 0 + data3[["środki.odurzające"]])
colnames(dummies) <- paste0("środki.odurzające", ".", substr(colnames(dummies), nchar(colnames(dummies)), nchar(colnames(dummies))))
data3 <- cbind(data3, dummies)
data3[["środki.odurzające"]] <- NULL
data3 <- data3 %>% select(-środki.odurzające.S)

dummies <- model.matrix(~ 0 + data3[["szczepienie"]])
colnames(dummies) <- paste0("szczepienie", ".", substr(colnames(dummies), nchar(colnames(dummies)), nchar(colnames(dummies))))
data3 <- cbind(data3, dummies)
data3[["szczepienie"]] <- NULL
data3 <- data3 %>% select(-`szczepienie.]`)

dummies <- model.matrix(~ 0 + data3[["długośćwykonywanej.kary"]])
colnames(dummies) <- paste0("długośćwykonywanej.kary", ".", substr(colnames(dummies), nchar(colnames(dummies)), nchar(colnames(dummies))))
data3 <- cbind(data3, dummies)
data3[["długośćwykonywanej.kary"]] <- NULL
data3 <- data3 %>% select(-długośćwykonywanej.kary.E)

dummies <- model.matrix(~ 0 + data3[["pozostały.okres.kary"]])
colnames(dummies) <- paste0("pozostały.okres.kary", ".", substr(colnames(dummies), nchar(colnames(dummies)), nchar(colnames(dummies))))
data3 <- cbind(data3, dummies)
data3[["pozostały.okres.kary"]] <- NULL
data3 <- data3 %>% select(-pozostały.okres.kary.E)

dummies <- model.matrix(~ 0 + data3[["wykształcenie"]])
colnames(dummies) <- paste0("wykształcenie", ".", substr(colnames(dummies), nchar(colnames(dummies)), nchar(colnames(dummies))))
data3 <- cbind(data3, dummies)
data3[["wykształcenie"]] <- NULL
data3 <- data3 %>% select(-wykształcenie.H)

dummies <- model.matrix(~ 0 + data3[["wiek.obecnie"]])
colnames(dummies) <- paste0("wiek.obecnie", ".", substr(colnames(dummies), nchar(colnames(dummies)), nchar(colnames(dummies))))
data3 <- cbind(data3, dummies)
data3[["wiek.obecnie"]] <- NULL
data3 <- data3 %>% select(-wiek.obecnie.E)

data4 <- data3[, !sapply(data3, is.character)]

# model na widzenia w zależności od długości wykonywanej kary
# Fit the model
myModel1 <- polr(widzenia ~ długośćwykonywanej.kary.A + długośćwykonywanej.kary.B + długośćwykonywanej.kary.C + długośćwykonywanej.kary.D, data = data4, method = "probit")
# Summarize the model results
summary(myModel1)

# model na widzenia w zależności od pozostałej wykonywanej kary
# Fit the model
myModel2 <- polr(widzenia ~ pozostały.okres.kary.A + pozostały.okres.kary.B + pozostały.okres.kary.C + pozostały.okres.kary.D, data = data4, method = "probit")
# Summarize the model results
summary(myModel2)

# model na widzenia w zależności od wykształcenia
# Fit the model
myModel3 <- polr(widzenia ~ wykształcenie.A + wykształcenie.B + wykształcenie.C + wykształcenie.D + wykształcenie.E + wykształcenie.F + wykształcenie.G, data = data4, method = "probit")
# Summarize the model results
summary(myModel3)

# Fit the model
myModel3b <- polr(widzenia ~ wykształcenie.A + wykształcenie.B + wykształcenie.C + wykształcenie.DEFG, data = data4, method = "probit")
# Summarize the model results
summary(myModel3b)

# model na liczba.telefonów w zależności od długości wykonywanej kary
# Fit the model

myModel4 <- polr(liczba.telefonów ~ długośćwykonywanej.kary.A + długośćwykonywanej.kary.B + długośćwykonywanej.kary.C + długośćwykonywanej.kary.D, data = data4, method = "probit")
# Summarize the model results
summary(myModel4)

# model na liczba.telefonów w zależności od pozostałej wykonywanej kary
# Fit the model
myModel5 <- polr(liczba.telefonów ~ pozostały.okres.kary.A + pozostały.okres.kary.B + pozostały.okres.kary.C + pozostały.okres.kary.D, data = data4, method = "probit")
# Summarize the model results
summary(myModel5)

# model na liczba.telefonów w zależności od wykształcenia
# Fit the model
myModel6 <- polr(liczba.telefonów ~ wykształcenie.A + wykształcenie.B + wykształcenie.C + wykształcenie.D + wykształcenie.E + wykształcenie.F + wykształcenie.G, data = data4, method = "probit")
# Summarize the model results
summary(myModel6)

# Fit the model
myModel6b <- polr(liczba.telefonów ~ wykształcenie.A + wykształcenie.B + wykształcenie.C + wykształcenie.DEFG, data = data4, method = "probit")
# Summarize the model results
summary(myModel6b)



# model na środki.odurzające w zależności od wykształcenia
data5 <- data4 %>% filter(środki.odurzające.B == 1 | środki.odurzające.C == 1)
# Fit the model
myModel7 <- glm( środki.odurzające.B ~ wykształcenie.A + wykształcenie.B + wykształcenie.C + wykształcenie.D + wykształcenie.E + wykształcenie.F + wykształcenie.G, data = data5, family = binomial)
# Summarize the model results
summary(myModel7)

# Fit the model
myModel7b <- glm( środki.odurzające.B ~ wykształcenie.A + wykształcenie.B + wykształcenie.C + wykształcenie.DEFG, data = data5, family = binomial)
# Summarize the model results
summary(myModel7b)

# model na szczepienia w zależności od wykształcenia
data5 <- data4 %>% filter(szczepienie.A == 1 | szczepienie.B == 1)
# Fit the model
myModel8 <- glm( szczepienie.A ~ wykształcenie.A + wykształcenie.B + wykształcenie.C + wykształcenie.D + wykształcenie.E + wykształcenie.F + wykształcenie.G, data = data5, family = binomial)
# Summarize the model results
summary(myModel8)

myModel8b <- glm( szczepienie.A ~ wykształcenie.A + wykształcenie.B + wykształcenie.C + wykształcenie.DEFG, data = data5, family = binomial)
# Summarize the model results
summary(myModel8b)

# Fit the model
myModel9 <- glm( środki.odurzające.B ~ wiek.obecnie.A + wiek.obecnie.B + wiek.obecnie.C + wiek.obecnie.D , data = data5, family = binomial)
# Summarize the model results
summary(myModel9)

data4$wykształcenie.DEFG <- data4$wykształcenie.D + data4$wykształcenie.E + data4$wykształcenie.F + data4$wykształcenie.G


#widzenia ~ długośćwykonywanej.kary + pozostały.okres.kary + wykształcenie
#liczba.telefonów ~ długośćwykonywanej.kary + pozostały.okres.kary + wykształcenie
#środki.odurzające ~ wykształcenie
#szczepienie (A - tak, B - nie) ~ wykształcenie <- tutaj chcemy zwykłą regresję logistyczną
