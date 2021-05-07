
# Load the tidyverse
library(tidyverse)

# Import the dataset and convert variables
pokedex <- read_csv("datasets/pokedex.csv", 
                    col_types = cols(name = col_factor(), 
                                     type = col_factor(),
                                     is_legendary = col_factor()))

# Look at the first six rows
head(pokedex)

# Examine the structure
str(pokedex)

library(testthat)
library(IRkernel.testthat)

soln_pokedex <- read_csv("datasets/pokedex.csv", 
                         col_types = cols(name = col_factor(), 
                                          type = col_factor(),
                                          is_legendary = col_factor()))

run_tests({
    
    test_that("packages are loaded", {
        expect_true("tidyverse" %in% .packages(), info = "Did you load the `tidyverse` package?")
    })
    
    test_that("data is loaded and formatted correctly", {
        expect_equal(pokedex, soln_pokedex,
                         info = "Did you convert `type` and `is_legendary` to factors using `col_factor()`?")
    })
})

# Prepare the data
legendary_pokemon <- pokedex %>% 
  count(is_legendary) %>% 
  mutate(prop = n / nrow(pokedex))

# Print the data frame
legendary_pokemon

soln_legendary_pokemon <- soln_pokedex %>% 
  count(is_legendary) %>% 
  mutate(prop = n / nrow(soln_pokedex))

run_tests({
    
    test_that("data is prepared correctly", {
        
        expect_equal(colnames(legendary_pokemon)[1], colnames(soln_legendary_pokemon)[1],
                         info = "Did you pass `is_legendary` to the `count()` function?")
        
        expect_equal(legendary_pokemon[3], soln_legendary_pokemon[3],
                     info = "Did you divide `n` by `nrow(pokedex)`?")
    
  })
  
})

# Prepare the plot
legend_by_heightweight_plot <- pokedex %>% 
  ggplot(aes(x = height_m, y = weight_kg)) +
  geom_point(aes(color = is_legendary), size = 2) +
  geom_text(aes(label = ifelse(height_m > 7.5|weight_kg > 600, as.character(name), '')),
            vjust = 0, hjust = 0) +
  geom_smooth(method = "lm", se = FALSE, col = "black", linetype = "dashed") +
  expand_limits(x = 16) +
  labs(title = "Legendary Pokemon by height and weight",
       x = "Height (m)",
       y = "Weight (kg)") +
  guides(color = guide_legend(title = "Pokemon status")) +
  scale_color_manual(labels = c("Non-Legendary", "Legendary"),
                     values = c("#F8766D", "#00BFC4"))

# Print the plot
legend_by_heightweight_plot

soln_legend_by_heightweight_plot <- soln_pokedex %>% 
  ggplot(aes(x = height_m, y = weight_kg)) +
  geom_point(aes(color = is_legendary), size = 2) +
  geom_text(aes(label = ifelse(height_m > 7.5|weight_kg > 600, as.character(name), '')), vjust = 0, hjust = 0) +
  geom_smooth(method = "lm", se = FALSE, col = "black", linetype = "dashed") +
  expand_limits(x = 16) +
  labs(title = "Legendary Pokemon by height and weight",
       x = "Height (m)",
       y = "Weight (kg)") +
  guides(color = guide_legend(title = "Pokemon status")) +
  scale_color_manual(labels = c("Non-Legendary", "Legendary"),
                     values = c("#F8766D", "#00BFC4"))
run_tests({
  
  test_that("the mappings are correct", {

        expect_equal(deparse(legend_by_heightweight_plot$mapping$x),
                         deparse(soln_legend_by_heightweight_plot$mapping$x),
                         info = 'The `x` aesthetic is incorrect. Did you map it to `height_m`?')

        expect_equal(deparse(legend_by_heightweight_plot$mapping$y),
                         deparse(soln_legend_by_heightweight_plot$mapping$y),
                         info = 'The `y` aesthetic is incorrect. Did you map it to `weight_kg`?')

        expect_equal(legend_by_heightweight_plot$layers[[1]]$mapping,
                         soln_legend_by_heightweight_plot$layers[[1]]$mapping,
                         info = "The `col` aesthetic in `geom_point()` is incorrect. Did you map it to `is_legendary`?")
    
  })
  
  test_that("the conditional labels are correct", {
    
        expect_true(str_detect(legend_by_heightweight_plot$labels$label, "height_m > 7.5"),
                     info = "The conditional labels are incorrect. Did you specify that `height_m` should be greater than `7.5` in the first argument of the `ifelse()` function?")

        expect_true(str_detect(legend_by_heightweight_plot$labels$label, "weight_kg > 600"),
                    info = "The conditional labels are incorrect. Did you specify that `weight_kg` should be greater than `600` in the first argument of the `ifelse()` function?")

        expect_true(str_detect(legend_by_heightweight_plot$labels$label, "\\|"),
                    info = "The conditional labels are incorrect. Did you remember to use the conditional 'OR' operator (|)?")
    
  })
    
    test_that("the plot limits are correct", {
        
        expect_equal(ggplot_build(legend_by_heightweight_plot)$data[[4]],
                         ggplot_build(soln_legend_by_heightweight_plot)$data[[4]],
                         info = "The plot limits are incorrect. Did you expand the limit of the `x`-axis to 16?")
        
    })
  
})

# Prepare the data
legend_by_type <- pokedex %>% 
    group_by(type) %>% 
    mutate(is_legendary = as.numeric(is_legendary) - 1) %>% 
    summarise(prop_legendary = mean(is_legendary)) %>% 
    ungroup() %>% 
    mutate(type = fct_reorder(type, prop_legendary))

# Prepare the plot
legend_by_type_plot <- legend_by_type %>% 
    ggplot(aes(x = type, y = prop_legendary, fill = prop_legendary)) + 
    geom_col() +
    labs(title = "Legendary Pokemon by type") +
    coord_flip() +
    guides(fill = FALSE)

# Print the plot
legend_by_type_plot

soln_legend_by_type <- soln_pokedex %>% 
    group_by(type) %>% 
    mutate(is_legendary = as.numeric(is_legendary) - 1) %>% 
    summarise(prop_legendary = mean(is_legendary)) %>% 
    ungroup() %>% 
    mutate(type = fct_reorder(type, prop_legendary))

soln_legend_by_type_plot <- soln_legend_by_type %>% 
    ggplot(aes(x = type, y = prop_legendary, fill = prop_legendary)) + 
    geom_col() +
    labs(title = "Legendary Pokemon by type") +
    coord_flip() +
    guides(fill = FALSE)

run_tests({
  
  test_that("the data preparation is correct", {
      
      expect_equal(group_vars(legend_by_type), group_vars(soln_legend_by_type),
                   info = "Did you group by `type`?")
      
      expect_equal(legend_by_type$prop_legendary, soln_legend_by_type$prop_legendary,
                   info = "Did you set `prop_legendary` equal to the mean of `is_legendary`?")
      
      expect_equal(legend_by_type$type, soln_legend_by_type$type,
                  info = "Did you the `fct_reorder()` function to order `type` by `prop_legendary`?")

    
  })
    
    test_that("the plot preparation is correct", {
        
      expect_equal(deparse(legend_by_type_plot$mapping$y),
                       deparse(soln_legend_by_type_plot$mapping$y),
                       info = 'The `y` aesthetic is incorrect. Did you map it to `prop_legendary`?')
        
      expect_equal(deparse(legend_by_type_plot$mapping$fill),
                       deparse(soln_legend_by_type_plot$mapping$fill),
                       info = 'The `fill` aesthetic is incorrect. Did you map it to `prop_legendary`?')
    
  })
  
})

# Prepare the data
legend_by_stats <- pokedex  %>% 
  select(is_legendary, attack, sp_attack, defense, sp_defense, hp, speed)  %>% 
  gather(key = "fght_stats", value = "value", -is_legendary) 

# Prepare the plot
legend_by_stats_plot <- legend_by_stats %>% 
 ggplot(aes(x = is_legendary, y = value, fill = is_legendary)) +
 geom_boxplot(varwidth = TRUE) +
 facet_wrap(~fght_stats) +
 labs(title = "Pokemon fight statistics",
        x = "Legendary status") +
 guides(fill = FALSE)

# Print the plot
legend_by_stats_plot

soln_legend_by_stats <- soln_pokedex  %>% 
  select(is_legendary, attack, sp_attack, defense, sp_defense, hp, speed)  %>% 
  gather(key = "fght_stats", value = "value", -is_legendary) 

soln_legend_by_stats_plot <- soln_legend_by_stats %>% 
 ggplot(aes(x = is_legendary, y = value, fill = is_legendary)) +
 geom_boxplot(varwidth = TRUE) +
 facet_wrap(~fght_stats) +
 labs(title = "Pokemon fight statistics",
        x = "Legendary status") +
 guides(fill = FALSE)

run_tests({
  
  test_that("the data preparation is correct", {
      
      expect_equal(colnames(legend_by_stats)[1], colnames(soln_legend_by_stats)[1],
                       info = "Did you exclude `is_legendary` from the `gather()` process by placing it after the `-` sign?")
      
      expect_equal(colnames(legend_by_stats)[2:3], colnames(soln_legend_by_stats)[2:3],
                      info = "Did you set `\"fght_stats\"` as the `key` and `\"value\"` as the `value` when using `gather()`?")
    
  })
    
    test_that("the plot preparation is correct", {
        
      expect_equal(legend_by_stats_plot$facet$params$facets, soln_legend_by_stats_plot$facet$params$facets,
                        info = "Did you use the `facet_wrap()` function to facet by `fght_stats?`")
           
  })
})

# Set seed for reproducibility
set.seed(1234)

# Save number of rows in dataset
n <- nrow(pokedex)

# Generate 60% sample of rows
sample_rows <- sample(n, 0.6 * n)

# Create training set
pokedex_train <- pokedex  %>% 
  filter(row_number() %in% sample_rows)

# Create test set
pokedex_test <- pokedex  %>% 
  filter(!row_number() %in% sample_rows)

set.seed(1234)

soln_n <- nrow(soln_pokedex)
soln_sample_rows <- sample(soln_n, 0.6 * soln_n)

soln_pokedex_train <- soln_pokedex  %>% 
  filter(row_number() %in% soln_sample_rows)

soln_pokedex_test <- soln_pokedex  %>% 
  filter(!row_number() %in% soln_sample_rows)

run_tests({
  
  test_that("the seed is correct", {
      expect_equal(sample_rows, soln_sample_rows,
                       info = "Did you set the seed to `1234`?")
  })
    
    test_that("the value for `n` is correct", {
        expect_equal(n, soln_n,
                        info = "The value for `n` is incorrect. Did you save it as the number of rows in `pokedex`?")
  })
    
    test_that("the sample is correct", {
        expect_equal(length(sample_rows), length(soln_sample_rows),
                        info = "Did you generate a 60% sample of rows?")
    })
    
    test_that("the training/test sets are correct", {
        expect_equal(pokedex_train, soln_pokedex_train,
                         info = "The training set is incorrect. Did you filter the `pokedex` using `sample_rows`?")
        
        expect_equal(pokedex_test, soln_pokedex_test,
                         info = "The test set is incorrect. Did you filter the `pokedex` using `sample_rows`?")
    })
  
})

# Load packages and set seed
library(rpart)
library(rpart.plot)
set.seed(1234)

# Fit decision tree
model_tree <- rpart(is_legendary ~ attack + defense + height_m + 
                    hp + sp_attack + sp_defense + speed + type + weight_kg,
                       data = pokedex_train,
                       method = "class",
                       na.action = na.omit)

# Plot decision tree
rpart.plot(model_tree)

set.seed(1234)

soln_model_tree <- rpart(is_legendary ~ attack + defense + height_m + 
                    hp + sp_attack + sp_defense + speed + type + weight_kg,
                       data = soln_pokedex_train,
                       method = "class",
                       na.action = na.omit)

run_tests({
  
  test_that("packages are loaded", {
      expect_true("rpart" %in% .packages(), info = "Did you load the `rpart` package?")
      expect_true("rpart.plot" %in% .packages(), info = "Did you load the `rpart` package?")
  })
    
    test_that("the decision tree is correct", {
        
        expect_true(model_tree[["call"]][["data"]] == "pokedex_train",
                         info = "Did you train the model on `pokedex_train`?")
        
        expect_equal(model_tree[["call"]][["na.action"]], soln_model_tree[["call"]][["na.action"]],
                         info = "Did you set `na.action` to `na.omit`?")
        
    })
  
})

# Load package and set seed
library(randomForest)
set.seed(1234)

# Fit random forest
model_forest <- randomForest(is_legendary ~ attack + defense + height_m + 
                         hp + sp_attack + sp_defense + speed + type + weight_kg,
                         data = pokedex_train,
                         importance = TRUE,
                         na.action = na.omit)

# Print model output
model_forest

random_state <- .Random.seed
set.seed(1234)
soln_random_state <- .Random.seed

soln_model_forest <- randomForest(is_legendary ~ attack + defense + height_m + 
                         hp + sp_attack + sp_defense + speed + type + weight_kg,
                         data = soln_pokedex_train,
                         importance = TRUE,
                         na.action = na.omit)

run_tests({
  
  test_that("packages are loaded", {
      expect_true("randomForest" %in% .packages(), info = "Did you load the `randomForest` package?")
  })
    
    test_that("the random forest is correct", {
        
        expect_true(model_forest[["call"]][["data"]] == "pokedex_train",
                         info = "Did you train the model on `pokedex_train`?")
        
        expect_equal(model_forest[["call"]][["na.action"]], soln_model_forest[["call"]][["na.action"]],
                         info = "Did you set `na.action` to `na.omit`?")
        
    })
  
})

# Load the ROCR package
library(ROCR)

# Create prediction and performance objects for the decision tree
probs_tree <- predict(model_tree, pokedex_test, type = "prob")
pred_tree <- prediction(probs_tree[,2], pokedex_test$is_legendary)
perf_tree <- performance(pred_tree, "tpr", "fpr")

# Create prediction and performance objects for the random forest
probs_forest <- predict(model_forest, pokedex_test, type = "prob")
pred_forest <- prediction(probs_forest[,2], pokedex_test$is_legendary)
perf_forest <- performance(pred_forest, "tpr", "fpr")

# Plot the ROC curves
plot(perf_tree, col = "red", main = "ROC curves")
plot(perf_forest, add = TRUE, col = "blue")
legend(x = "bottomright",  legend = c("Decision Tree", "Random Forest"), fill = c("red", "blue"))

soln_probs_forest <- predict(soln_model_forest, soln_pokedex_test, type = "prob")
soln_pred_forest <- prediction(soln_probs_forest[,2], soln_pokedex_test$is_legendary)
soln_perf_forest <- performance(soln_pred_forest, "tpr", "fpr")

run_tests({
  
  test_that("packages are loaded", {
      expect_true("ROCR" %in% .packages(), info = "Did you load the `ROCR` package?")
  })
    
    test_that("the prediction and performance objects are correct", {
        
        expect_equal(probs_forest, soln_probs_forest,
                         info = "The `probs_forest` object is incorrect. Did you remember to use `model_forest` rather than `model_tree`?")
        
        expect_equal(pred_forest, soln_pred_forest,
                         info = "The `pred_forest` object is incorrect. Did you remember to use `probs_forest` rather than `probs_tree`?")
        
        expect_equal(perf_forest, soln_perf_forest,
                         info = "The `perf_forest` object is incorrect. Did you remember to use `pred_forest` rather than `pred_tree`?")
        
  })
  
})

# Print variable importance measures
importance_forest <- importance(model_forest)
importance_forest

# Create a dotchart of variable importance
varImpPlot_forest <- varImpPlot(model_forest)
varImpPlot_forest

soln_importance_forest <- importance(soln_model_forest)
soln_varImpPlot_forest <- varImpPlot(soln_model_forest)

run_tests({
  
  test_that("the `importance` and `varImpPlot` objects are correct", {
      expect_equal(importance_forest, soln_importance_forest,
                       info = "Did you pass the random forest model to the `importance()` function?")
      expect_equal(varImpPlot_forest, soln_varImpPlot_forest,
                       info = "Did you pass the random forest model to the `varImpPlot()` function?")
  })
  
})

# According to the MeanDecreaseAccuracy plot:

# Q1. Is the `attack` or `defense` variable more important?
answer1 <- "attack"

# Q2. Is the `weight_kg` or `height_m` variable more important?
answer2 <- "weight_kg"

# According to the MeanDecreaseGini plot:

# Q3. Is the `attack` or `defense` variable more important?
answer3 <- "defense"

# Q4. Is the `weight_kg` or `height_m` variable more important?
answer4 <- "weight_kg"

soln_answer1 <- "attack"
soln_answer2 <- "weight_kg"
soln_answer3 <- "defense"
soln_answer4 <- "weight_kg"

run_tests({
  
  test_that("the answers are correct", {
      
      expect_equal(answer1, soln_answer1,
                      info = "The answer to Q1 is wrong. Did you check which variable was higher in the `MeanDecreaseAccuracy` plot?")
      
      expect_equal(answer2, soln_answer2,
                      info = "The answer to Q2 is wrong. Did you check which variable was higher in the `MeanDecreaseAccuracy` plot?")
      
      expect_equal(answer3, soln_answer3,
                      info = "The answer to Q3 is wrong. Did you check which variable was higher in the `MeanDecreaseGini` plot?")
      
      expect_equal(answer4, soln_answer4,
                      info = "The answer to Q4 is wrong. Did you check which variable was higher in the `MeanDecreaseGini` plot?")
      
  })
  
})
