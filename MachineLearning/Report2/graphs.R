library(ggplot2)
library(tidyr)
library(dplyr)
library(hrbrthemes)
library(viridis)

graph_depth_e_evolution <- function(depth_tests){
  deph <- t_deph[-1,] %>% 
    mutate(deph = as.factor(deph)) %>% 
    group_by(deph) %>% 
    mutate(
      'Test error' = mean(test_e),
      'Train error' = mean(train_e)
    ) %>% 
    select(-c(test_e,train_e)) %>% 
    pivot_longer(!deph,names_to = 'type', values_to = 'error') %>% 
    distinct()
  
  # Plot
  deph %>%
    ggplot( aes(x=deph, y=error, group=type, color=type)) +
    geom_line() +
    scale_color_viridis(discrete = TRUE) +
    ggtitle("Train/Test error by maxdeph") +
    theme_ipsum() +
    ylab("Error (missclassification rate, CV = 10)")
}

graph_split_e_evolution <- function(split_tests){
    
  split <- t_split[-1,] %>% 
    mutate(split = as.factor(split)) %>% 
    group_by(split) %>% 
    mutate(
      'Test error' = mean(test_e),
      'Train error' = mean(train_e)
    ) %>% 
    select(-c(test_e,train_e)) %>% 
    pivot_longer(!split,names_to = 'type', values_to = 'error') %>% 
    distinct()
  
  # Plot
  split %>%
    ggplot( aes(x=split, y=error, group=type, color=type)) +
    geom_line() +
    scale_color_viridis(discrete = TRUE) +
    theme_ipsum() +
    xlab("Minimum size of split")+
    ylab("Error (missclassification rate, CV = 10)")+
    scale_x_discrete(breaks = seq(1, 80, by = 10))+
    labs(color = NULL)
}




