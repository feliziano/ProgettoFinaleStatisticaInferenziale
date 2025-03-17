# Caricamento delle librerie necessarie
library(ggplot2)
library(dplyr)
library(tidyr)
library(caret)
library(MASS)
library(car)

# Lettura dei dati
data <- read.csv("neonati.csv")

# Pulizia dei dati
data <- data %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>%
  drop_na()

data <- data %>%
  mutate(Tipo.parto_num = ifelse(Tipo.parto == "Ces", 1, 0))

# Analisi descrittiva
summary(data)
descriptive_stats <- data %>% summarise_all(funs(mean, sd, min, max))
print(descriptive_stats)

# Test statistici
cesareo_table <- table(data$Tipo.parto_num, data$Ospedale)
chisq.test(cesareo_table)

t.test(data$Peso, mu = 3200)
t.test(data$Lunghezza, mu = 50)

t.test(Peso ~ Sesso, data = data)
t.test(Lunghezza ~ Sesso, data = data)

# Creazione del modello di regressione
model <- lm(Peso ~ Anni.madre + N.gravidanze + Fumatrici + 
              Gestazione + Lunghezza + Cranio + 
              Tipo.parto + Ospedale + Sesso, data = data)

# Selezione del modello ottimale (AIC)
stepwise_model <- stepAIC(model, direction = "both")
summary(stepwise_model)

# Diagnostica del modello
par(mfrow = c(2,2))
plot(stepwise_model)

# Validazione del modello con train-test split
set.seed(123)
train_index <- createDataPartition(data$Peso, p = 0.7, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

stepwise_model_train <- stepAIC(model, direction = "both", data = train_data)
predictions_test <- predict(stepwise_model_train, newdata = test_data)

rmse_test <- sqrt(mean((test_data$Peso - predictions_test)^2))
r2_test <- cor(test_data$Peso, predictions_test)^2

cat("RMSE (Test):", rmse_test, "\nR-squared (Test):", r2_test, "\n")

# Analisi comparativa tra ospedali
anova_result <- aov(Peso ~ Ospedale, data = data)
summary(anova_result)
tukey_result <- TukeyHSD(anova_result)
print(tukey_result)

# Visualizzazioni
ggplot(data, aes(x = Gestazione, y = Peso)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm", col = "blue") + 
  ggtitle("Relazione tra durata della gravidanza e peso del neonato")

ggplot(data, aes(x = as.factor(Fumatrici), y = Peso)) + 
  geom_boxplot(fill = "lightblue") + 
  scale_x_discrete(labels = c("0" = "Non fumatrice", "1" = "Fumatrice")) +  # Cambia le etichette
  ggtitle("Effetto del fumo materno sul peso del neonato") +
  xlab("Fumo materno") +  # Aggiungi un'etichetta all'asse x
  theme_minimal()  # Opzionale: migliora l'aspetto del grafico

ggplot(data, aes(x = as.factor(Ospedale), y = Peso)) + 
  geom_boxplot(fill = "lightgreen") + 
  ggtitle("Distribuzione del peso neonatale per ospedale")+
  xlab("Ospedale")

ggplot(data, aes(x = Sesso, y = Lunghezza)) + 
  geom_boxplot(fill = "lightpink") + 
  ggtitle("Distribuzione della lunghezza neonatale per sesso")

# Heatmap delle correlazioni
cor_matrix <- cor(data %>% dplyr::select(Anni.madre, N.gravidanze, Fumatrici, Gestazione, Peso, Lunghezza, Cranio))
heatmap(as.matrix(cor_matrix), col = heat.colors(10), margins = c(5,5))

library(reshape2)

# Trasforma la matrice di correlazione in un formato "lungo" per ggplot2
cor_melted <- melt(cor_matrix)

# Crea la heatmap con ggplot2
ggplot(cor_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +  # Crea i riquadri colorati
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, 
                       limits = c(-1, 1), name = "Correlazione") +  # Definisci la scala di colore e la legenda
  labs(title = "Heatmap delle correlazioni", x = "", y = "") +  # Aggiungi titoli
  theme_minimal() +  # Usa un tema minimalista
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Ruota le etichette dell'asse x
