## do subspecies ##

if(gen_id ==0){
  
  eval1 <- unlist(lapply(neat_pop, FUN = nn_eval,fun_act=my_fun, input = c(1,0,0) ))
  eval2 <- unlist(lapply(neat_pop, FUN = nn_eval,fun_act=my_fun, input = c(1,0,1) ))
  eval3 <- unlist(lapply(neat_pop, FUN = nn_eval,fun_act=my_fun, input = c(1,1,0) ))
  eval4 <- unlist(lapply(neat_pop, FUN = nn_eval,fun_act=my_fun, input = c(1,1,1) ))
  eval_final <- 4 - (abs(eval1) + abs(1 - eval2) + abs(1-eval3) + abs(eval4))
  eval_final[is.na(eval_final)] <- min(eval_final,na.rm = T) 
  
  pop_rep <- list(neat_pop[[sample(pop_size,1)]])
  species_vec <- get_sub_species(neat_pop, pop_rep, dist_torlerance=3) 
}else{
  tem_ind <- 1:pop_size
  species_list <- unique(neat_pop_species)
  pop_rep <- list()
  for(i in 1:species_list ){
      sample_species_nn <- neat_pop[sample(tem_ind[neat_pop_species == i], 1)  ]
      pop_rep <- c(pop_rep, sample_species_nn)
  }
  species_vec <- get_sub_species(neat_pop, pop_rep, dist_torlerance=3) 
  eval_final <- neat_pop_fitness
  
}
print(summary(eval_final))

## do summary by species ##

summary_df <- data.frame(id= 1:pop_size, fitness_init = eval_final, species = species_vec)
species_summary <- summary_df %>% group_by(species) %>% summarise(species_freq = n(), best_fit = max(fitness_init), worst_fit = min(fitness_init)) %>% as.data.frame()
summary_df <- merge(summary_df, species_summary)
summary_df$fitness_adj <- summary_df$fitness_init/summary_df$species_freq
species_summary <- summary_df %>% group_by(species) %>% summarise(fit_adj_sum = sum(fitness_adj)) %>% as.data.frame()
species_summary$offspring_n <- round(species_summary$fit_adj_sum/sum(species_summary$fit_adj_sum) * pop_size,0)
species_summary$offspring_n[1] <- pop_size - sum(species_summary$offspring_n) + species_summary$offspring_n[1]
species_summary$offspring_n_mutation <- round(species_summary$offspring_n * mutation_only_rate, 0 )
species_summary$offspring_n_crossover <- species_summary$offspring_n - species_summary$offspring_n_mutation - 1
