save.image()
## get best individual in each species and add to next generation

neat_pop_bk <- neat_pop

## get offspring by species 

for( i in species_summary$species){
  
  ## get worse individual and remove it
  
  # worst_id <- summary_df$id[summary_df$fitness_init == summary_df$worst_fit & summary_df$species == i]
  # summary_df <- summary_df[!summary_df$id %in% worst_id, ]
  
  
  ### get crossover offspring
  
  offspring_n_crossover <- species_summary$offspring_n_crossover[species_summary$species == i]
  
  p_pop <- summary_df$id[summary_df$species == i]
  if(length(p_pop) > 1){
    p1 <- sample(p_pop, 
                 replace = T,
                 size = offspring_n_crossover,
                 prob = summary_df$fitness_adj[summary_df$species == i]/sum(summary_df$fitness_adj[summary_df$species == i])
    )
  }else{
    p1 <- rep(p_pop, offspring_n_crossover )
  }
  
  
  p1_finess <- summary_df$fitness_adj[match(p1,summary_df$id)]
  p1 <- neat_pop[p1]
  
  if(length(p_pop) > 1){
    p2 <- sample(p_pop, 
                 replace = T,
                 size = offspring_n_crossover,
                 prob = summary_df$fitness_adj[summary_df$species == i]/sum(summary_df$fitness_adj[summary_df$species == i])
    )
  }else{
    p2 <- rep(p_pop,offspring_n_crossover )
  }
  p2_finess <- summary_df$fitness_adj[match(p2,summary_df$id)]
  p2 <- neat_pop[p2]
  
  
  offspring_mate <- mapply(FUN = nn_mate, nn1 = p1, nn2 = p2,
                           fitness1 = p1_finess, fitness2 =  p2_finess,
                           MoreArgs = list( disable_p = 0.7))
  
  weigth_mutation_vec <- (runif(offspring_n_crossover) <= weight_mutation_rate)
  node_mutation_vec <- (runif(offspring_n_crossover) <= node_mutation_rate)
  link_mutation_vec <- (runif(offspring_n_crossover) <= link_mutation_rate)
  
  ## do mutation for crossover offspring
  offspring_mate[weigth_mutation_vec] <- lapply(offspring_mate[weigth_mutation_vec], FUN = nn_mutation,
         mutation_tracking = mutation_tracking,  
         max_node = max_node, 
         max_marker = max_marker,
         type = 3, 
         scale = 1, 
         p = 0.8 )
  
  new_node_id <- (1:offspring_n_crossover)[node_mutation_vec]
  if(length(new_node_id >0)){
    for(ii in new_node_id){
      
      x <-  nn_mutation(nn = offspring_mate[[ii]], 
                        mutation_tracking = mutation_tracking,
                        max_node = max_node,
                        max_marker = max_marker,
                        type = 2)
      
      offspring_mate[[ii]] <- x$nn
      mutation_tracking = x$mutation_tracking
      max_node = x$max_node
      max_marker= x$max_marker
    }
  }
  
  new_link_id <- (1:offspring_n_crossover)[link_mutation_vec]
  if(length(new_link_id >0)){
    for(ii in new_link_id){
      
      x <-  nn_mutation(nn = offspring_mate[[ii]], 
                        mutation_tracking = mutation_tracking,
                        max_node = max_node,
                        max_marker = max_marker,
                        type = 1)
      
      offspring_mate[[ii]] <- x$nn
      mutation_tracking = x$mutation_tracking
      max_node = x$max_node
      max_marker= x$max_marker
    }
  }  
  
 
  ### get mutation only offspring
  offspring_n_mutation <- species_summary$offspring_n_mutation[species_summary$species == i]
  if(length(p_pop) > 1){
    p <- sample(p_pop, 
                replace = T,
                size = offspring_n_mutation,
                prob = summary_df$fitness_adj[summary_df$species == i]/sum(summary_df$fitness_adj[summary_df$species == i])
    )
  }else{
    p <- rep(p_pop, offspring_n_mutation)
  }
  
  offspring_mutation <- neat_pop[p]
  
  weigth_mutation_vec <- (runif(offspring_n_mutation) <= weight_mutation_rate)
  node_mutation_vec <- (runif(offspring_n_mutation) <= node_mutation_rate)
  link_mutation_vec <- (runif(offspring_n_mutation) <= link_mutation_rate)
  
  ## do mutation for crossover offspring
  offspring_mutation[weigth_mutation_vec] <- lapply(offspring_mutation[weigth_mutation_vec], FUN = nn_mutation,
                                                mutation_tracking = mutation_tracking,  
                                                max_node = max_node, 
                                                max_marker = max_marker,
                                                type = 3, 
                                                scale = 1, 
                                                p = 0.8 )
  
  new_node_id <- (1:offspring_n_mutation)[node_mutation_vec]
  if(length(new_node_id >0)){
    for(ii in new_node_id){
      
      x <-  nn_mutation(nn = offspring_mutation[[ii]], 
                        mutation_tracking = mutation_tracking,
                        max_node = max_node,
                        max_marker = max_marker,
                        type = 2)
      
      offspring_mutation[[ii]] <- x$nn
      mutation_tracking = x$mutation_tracking
      max_node = x$max_node
      max_marker= x$max_marker
    }
  }
  
  new_link_id <- (1:offspring_n_mutation)[link_mutation_vec]
  if(length(new_link_id >0)){
    for(ii in new_link_id){
      
      x <-  nn_mutation(nn = offspring_mutation[[ii]], 
                        mutation_tracking = mutation_tracking,
                        max_node = max_node,
                        max_marker = max_marker,
                        type = 1)
      
      offspring_mutation[[ii]] <- x$nn
      mutation_tracking = x$mutation_tracking
      max_node = x$max_node
      max_marker= x$max_marker
    }
  }  
  
  offspring <- c(offspring_mate, offspring_mutation)
  
  eval1 <- unlist(lapply(offspring, FUN = nn_eval,fun_act=my_fun, input = c(1,0,0) ))
  eval2 <- unlist(lapply(offspring, FUN = nn_eval,fun_act=my_fun, input = c(1,0,1) ))
  eval3 <- unlist(lapply(offspring, FUN = nn_eval,fun_act=my_fun, input = c(1,1,0) ))
  eval4 <- unlist(lapply(offspring, FUN = nn_eval,fun_act=my_fun, input = c(1,1,1) ))
  eval_final <- 4 - (abs(eval1) + abs(1 - eval2) + abs(1-eval3) + abs(eval4))
  
  offspring <- c(offspring, neat_pop[summary_df$id[summary_df$species == i]])
  offspring_fitness <- c(eval_final, summary_df$fitness_init[summary_df$species == i])
  offspring_fitness[is.na(offspring_fitness)] <- min(offspring_fitness,na.rm = T)
  
  offspring_index <- sample( (1:length(offspring)), 
                             size = offspring_n_crossover + offspring_n_mutation,
                             replace = T, 
                             prob = offspring_fitness/sum(offspring_fitness) )
  offspring <- offspring[ offspring_index ]
  offspring_fitness <- offspring_fitness[offspring_index]
  
  best_id <- summary_df$id[summary_df$fitness_init == summary_df$best_fit & summary_df$species == i]
  best_fitness <- max(summary_df$best_fit[summary_df$species == i])
  offspring <- c(neat_pop[best_id][1], offspring)
  offspring_fitness <- c(best_fitness,offspring_fitness)
  
  if(i == 1){
    neat_pop_next <- offspring
    neat_pop_fitness <- offspring_fitness
    neat_pop_species <- rep(i,length(offspring_fitness))
  }else{
    neat_pop_next <- c(neat_pop_next,offspring )
    neat_pop_fitness <- c(neat_pop_fitness,offspring_fitness )
    neat_pop_species <- c(neat_pop_species, rep(i,length(offspring_fitness) ))
  }

}
neat_pop <- neat_pop_next[1:pop_size]
neat_pop_fitness <- neat_pop_fitness[1:pop_size]
neat_pop_species <- neat_pop_species[1:pop_size]
head(neat_pop)
