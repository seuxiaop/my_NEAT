save.image()
## get best individual in each species and add to next generation

neat_pop_bk <- neat_pop

## get offspring by species 

for( i in species_summary$species){
  
  ## get worse individual and remove it

  worst_id <- summary_df$id[summary_df$fitness_init == summary_df$worst_fit & summary_df$species == i]
  n_i <- length(summary_df$id[ summary_df$species == i])
  
  ### reproduce within species ###
  if(n_i == 1){
    index <- summary_df$species == i 
  }else{
    index <- summary_df$species == i & (!summary_df$id %in% worst_id)  
  }
  
  p <- neat_pop[index]
  p_fitness <- summary_df$fitness_adj[index]
  n <- length(p)
  
  if( n > 0 ){
    
    # Do Crossover
    offspring_n_crossover <- species_summary$offspring_n_crossover[species_summary$species == i]
    if(offspring_n_crossover > 0){
      if(n > 1){
        p1 <- sample(1:n, 
               replace = T,
               size = offspring_n_crossover,
               prob = p_fitness)
        p1_finess <- p_fitness[p1]
        p1 <- p[p1]
        
        p2 <- sample(1:n, 
                     replace = T,
                     size = offspring_n_crossover,
                     prob = p_fitness)
        p2_finess <- p_fitness[p2]
        p2 <- p[p2]
        
      }else{
        p1 <- rep(1,offspring_n_crossover)
        
        p1 <- p[p1]
        p1_finess <- rep(p_fitness, offspring_n_crossover)
        
        p2 <- rep(1,offspring_n_crossover)
        p2 <- p[p2]
        p2_finess <- rep(p_fitness, offspring_n_crossover)
      }
      
      offspring_mate <- mapply(FUN = nn_mate, nn1 = p1, nn2 = p2,
                               fitness1 = p1_finess, fitness2 =  p2_finess,
                               MoreArgs = list( disable_p = 0.7))
    }else{
      offspring_mate <- NULL
    }
    
    
    # get mutation offspring
    offspring_n_mutation <- species_summary$offspring_n_mutation[species_summary$species == i]
    if(offspring_n_mutation >0 ){
      if(n > 1){
        index <- sample(1:n, replace = T, size = offspring_n_mutation, prob = p_fitness )
        offspring_mutation <- p[index]
      }else{
        index <- rep(1,offspring_n_mutation)
        offspring_mutation <- p[index]
      }
    }else{
      offspring_mutation <- NULL
    }
    
    # get all offspring
    offspring <- c(offspring_mate,offspring_mutation)
    
  }else{
    offspring <- NULL
  }
  
  n_offspring <- length(offspring)
  
  if(n_offspring > 0){
    
    # Do weight mutation
    weight_mutation_vec <- (runif(n_offspring) <= weight_mutation_rate)
    
    offspring[weight_mutation_vec] <- lapply(offspring[weight_mutation_vec], 
                                             FUN = nn_mutation,
                                             mutation_tracking = mutation_tracking,  
                                             max_node = max_node, 
                                             max_marker = max_marker,
                                             type = 3, 
                                             scale = 1, 
                                             p = 0.8 )
    
    node_mutation_vec <- (runif(n_offspring) <= node_mutation_rate)
    link_mutation_vec <- (runif(n_offspring) <= link_mutation_rate)
    
    ## do node mutation 
    new_node_id <- (1:n_offspring)[node_mutation_vec]
    if(length(new_node_id >0)){
      for(ii in new_node_id){
        
        x <-  nn_mutation(nn = offspring[[ii]], 
                          mutation_tracking = mutation_tracking,
                          max_node = max_node,
                          max_marker = max_marker,
                          type = 2)
        
        offspring[[ii]] <- x$nn
        mutation_tracking = x$mutation_tracking
        max_node = x$max_node
        max_marker= x$max_marker
      }
    }
    
    ## do connection mutation 
    
    new_link_id <- (1:n_offspring)[link_mutation_vec]
    if(length(new_link_id >0)){
      for(ii in new_link_id){
        
        x <-  nn_mutation(nn = offspring[[ii]], 
                          mutation_tracking = mutation_tracking,
                          max_node = max_node,
                          max_marker = max_marker,
                          type = 1)
        
        offspring[[ii]] <- x$nn
        mutation_tracking = x$mutation_tracking
        max_node = x$max_node
        max_marker= x$max_marker
      }
    } 
    
    
    ## get fitness for offspring 
    eval1 <- unlist(lapply(offspring, FUN = nn_eval,fun_act=my_fun, input = c(1,0,0) ))
    eval2 <- unlist(lapply(offspring, FUN = nn_eval,fun_act=my_fun, input = c(1,0,1) ))
    eval3 <- unlist(lapply(offspring, FUN = nn_eval,fun_act=my_fun, input = c(1,1,0) ))
    eval4 <- unlist(lapply(offspring, FUN = nn_eval,fun_act=my_fun, input = c(1,1,1) ))
    eval_final <- 4 - (abs(eval1) + abs(1 - eval2) + abs(1-eval3) + abs(eval4))
    eval_final[is.na(eval_final)] <- -1
    
    offspring_fitness <- eval_final
    
    ## put best nn from parent generation into 
    
    best_id <- summary_df$id[summary_df$fitness_init == summary_df$best_fit & summary_df$species == i ]
    offspring <- c(neat_pop[best_id[1]],offspring)
    offspring_fitness <- c(summary_df$fitness_init[best_id[1]],offspring_fitness)
    
    # offspring <- offspring[1:pop_size]
    # offspring_fitness <- offspring_fitness[1:pop_size]
    
  }else{
    # do nothing
  }

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
