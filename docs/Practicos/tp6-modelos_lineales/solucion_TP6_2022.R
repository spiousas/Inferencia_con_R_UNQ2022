# 1 ####
library(tidyverse)
#install.packages("openintro")
library(openintro)

View(starbucks)

starbucks %>% count(type)

model_starbucks_fat <- lm(calories ~ fat, data = starbucks)
model_starbucks_null <- lm(calories ~ 1, data = starbucks)
#model_starbucks_fat <- starbucks %>% lm(calories ~ fat, data = .)
model_starbucks_fat
model_starbucks_null
summary(model_starbucks_fat)

starbucks %>% ggplot(aes(x = fat,
           y = calories)) +
  geom_point() + theme_minimal() +
  geom_abline(intercept = model_starbucks_fat$coefficients[1],
              slope = model_starbucks_fat$coefficients[2],
              color = "red")

starbucks %>% ggplot(aes(x = fat,
                         y = calories)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)

# calories = 183.73 + 11.27 * fat

resid(model_starbucks_fat)

starbucks <- starbucks %>% mutate(residuals = resid(model_starbucks_fat))

starbucks %>%
  ggplot(aes(x = fat,
             y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")

anova(model_starbucks_fat, model_starbucks_null)

183.73 + 11.27 * 19
predict.lm(model_starbucks_fat, tibble(fat = c(10, 12, 19, 25)))

#model_starbucks_full <- lm(calories ~ fat + carb + protein, data = starbucks)

model_starbucks_fat

# Outliers
starbucks_out1 <- starbucks %>%
  dplyr::select(c("calories", "fat")) %>%
  bind_rows(tibble(fat = 25, calories = 150))

starbucks_out1 %>%
  ggplot(aes(x = fat,
             y = calories)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)

model_starbucks_out1 <- starbucks_out1 %>%
  lm(calories ~ fat, .)
model_starbucks_out1

starbucks_out2 <- starbucks %>%
  dplyr::select(c("calories", "fat")) %>%
  rbind(tibble(fat = 50, calories = 150))

starbucks_out2 %>%
  ggplot(aes(x = fat,
             y = calories)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)

model_starbucks_out2 <- starbucks_out2 %>%
  lm(calories ~ fat, .)
model_starbucks_out2

starbucks_out3 <- starbucks %>%
  dplyr::select(c("calories", "fat")) %>%
  rbind(tibble(fat = 50, calories = 700))

starbucks_out3 %>%
  ggplot(aes(x = fat,
             y = calories)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)

model_starbucks_out3 <- starbucks_out3 %>%
  lm(calories ~ fat, .)
model_starbucks_out3

# 2 #####
library(palmerpenguins)
library(car)
penguins %>% drop_na() %>%
  ggplot(aes(x = bill_depth_mm,
             y = body_mass_g,
             color = species)) +
    geom_point()

model_basic <- lm(body_mass_g ~ bill_depth_mm, data = penguins)
model_basic

Anova(lm(body_mass_g ~ species , data = penguins), type = 3)


model_penguins <- lm(body_mass_g ~ species, data = penguins %>% filter(species %in% c("Adelie", "Chinstrap")))
model_penguins

model_penguins <- lm(body_mass_g ~ bill_depth_mm + species, data = penguins)
model_penguins

# body_mass_g = -1007.28 + bill_depth_mm * 256.61 + 13.38 * (species == "Chinstrap") + 2238.67 * (species == "Gentoo")
-1007.28 + 18 * 256.61 + 13.38 * 1 + 2238.67 * 0

anova(model_penguins, model_basic)
Anova(model_penguins, type = 3)
summary(model_penguins)

model_penguins_interact <- lm(body_mass_g ~ bill_depth_mm + species + bill_depth_mm:species, data = penguins)
model_penguins_interact

model_penguins_interact <- lm(body_mass_g ~ bill_depth_mm * species, data = penguins)
model_penguins_interact

# body_mass_g = -283.28  + bill_depth_mm * 217.15  +
#               247.06 * (species == "Chinstrap") +
#               -175.71 * (species == "Gentoo") +
#               -12.53 * (species == "Chinstrap") * bill_depth_mm +
#               152.29 * (species == "Gentoo") * bill_depth_mm

# body_mass_g = -283.28  + 18 * 217.15  +
#               247.06 * 1 +
#               -175.71 * 0 +
#               -12.53 * 1 * 18 +
#               152.29 * 0 * 18

# body_mass_g = -283.28  + 18 * 217.15  + 247.06 + -12.53 * 18
# body_mass_g = -283.28+247.06  + 18 * (217.15-12.53)
-283.28+247.06  + 18 * (217.15-12.53)

# 3 ####
set.seed(4)
data_dientes <- tibble(Dientes = round(rnorm(2000) * 10+50),
                       Peso = (500 +  1 * Dientes) + rnorm(2000) * 30)
data_dientes %>% ggplot(aes(x = Dientes,
           y = Peso)) +
  geom_point()

model_dientes <- data_dientes %>%
  lm(Peso ~ Dientes, .)

library(parameters)
model_parameters(model_dientes)

set.seed(4)
CI_in <- tibble(index = 1:200,
                low = 0,
                high = 0,
                isin = FALSE)
CI_in

for (i in 1:200) {
  data_sample <- data_dientes %>% sample_n(size = 20)
  model_sample <- lm(Peso ~ Dientes, data_sample)
  CI_in$low[i] <- model_parameters(model_sample)$CI_low[2]
  CI_in$high[i] <- model_parameters(model_sample)$CI_high[2]
  CI_in$isin[i] <- between(1, model_parameters(model_sample)$CI_low[2], model_parameters(model_sample)$CI_high[2])
  print(paste0(i, " low:", CI_in$low[i], " high:", CI_in$high[i], "IN:", CI_in$isin[i]))
}

CI_in %>% count(isin)

set.seed(14)
CI_in_50 <- tibble(index = 1:200,
                   low = 0,
                   high = 0,
                   isin = FALSE)
for (i in 1:200) {
  data_sample <- data_dientes %>% sample_n(size = 50)
  model_sample <- lm(Peso ~ Dientes, data_sample)
  CI_in_50$low[i] <- model_parameters(model_sample)$CI_low[2]
  CI_in_50$high[i] <- model_parameters(model_sample)$CI_high[2]
  CI_in_50$isin[i] <- between(1, model_parameters(model_sample)$CI_low[2], model_parameters(model_sample)$CI_high[2])
  print(paste0(i, " low:", CI_in_50$low[i], " high:", CI_in_50$high[i], "IN:", CI_in_50$isin[i]))
}

CI_in_50 %>% count(isin)

ggplot() +
  geom_ribbon(data = CI_in, aes(x = index, ymin = low, ymax = high), fill = "#1380A1", alpha = 0.6) +
  geom_ribbon(data = CI_in_50, aes(x = index, ymin = low, ymax = high), fill = "#ED6A5A", alpha = 0.6) +
  geom_hline(yintercept = 1, color = "black", linetype = "dashed")

# 4 ####
library(MASS)
cats

model_cats <- lm(Hwt ~ Bwt * Sex, data = cats)
model_cats

cats %>% ggplot(aes(x = Bwt,
           y = Hwt,
           color = Sex)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  theme_minimal()

model_cats

# Bwt = 2.981 + Bwt * 2.636 - 4.165 * (Sex==M) + 1.676 * Bwt * (Sex==M)

2.981 + 3 * 2.636 - 4.165 * 1 + 1.676 * 3 * 1
2.981-4.165+ 3 * (2.636+1.676)
2.981 + 2 * 2.636

summary(model_cats)
Anova(model_cats, type = 3)

model_aov = aov(bill_length_mm ~ species, penguins)
anova(model_aov)  # Dedicated ANOVA function
model_test = lm(bill_length_mm ~ species, penguins)  # As in-your-face linear model
Anova(model_test, type = 3)

# 5 ####
library(openintro)
births14

model_births <- lm(weight ~ weeks : mage, data = births14)
summary(model_births)

Anova(model_births, type = 3)

# weight = -3.818059 + weeks * 0.264509 + mage * 0.015518 + 0.369806 * (sex==male) +
#          -0.429281 * (habit == smoker) + 0.018222 *visits

