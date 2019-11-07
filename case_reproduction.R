## get best individual in each species and add to next generation

best_id <- summary_df$id[summary_df$fitness_init == summary_df$best_fit]
best_id_species <- summary_df$species[match(best_id, summary_df$id )]
neat_pop_next <- neat_pop[best_id]
new_gen_species <- best_id_species
## get worse individual and remove it

worst_id <- summary_df$id[summary_df$fitness_init == summary_df$worst_fit]
summary_df <- summary_df[!summary_df$id == worst_id, ]

## random select from rest pop and do mutation 

## do weight mutation

mutation_id <- sample(summary_df$id,size=nrow(summary_df) * mutation_rate)

neat_pop[mutation_id] <- lapply(neat_pop[mutation_id], FUN = nn_mutation,
                                mutation_tracking = mutation_tracking,  
                                max_node = max_node, 
                                max_marker = max_marker,
                                type = 3, 
                                scale = 0.05, 
                                p = 0.9 )

## do node and connection mutation

new_link_id <- sample(summary_df$id,size=nrow(summary_df) * new_connection_rate)

for(i in new_link_id){
  
  x <-  nn_mutation(nn = neat_pop[[i]], 
                    mutation_tracking = mutation_tracking,
                    max_node = max_node,
                    max_marker = max_marker,
                    type = 1)
  
  neat_pop[[i]] <- x$nn
  mutation_tracking = x$mutation_tracking
  max_node = x$max_node
  max_marker= x$max_marker
}



new_node_id <- sample(summary_df$id,size=nrow(summary_df) * new_node_rate)

for(i in new_node_id){
  
  x <-  nn_mutation(nn = neat_pop[[i]], 
                    mutation_tracking = mutation_tracking,
                    max_node = max_node,
                    max_marker = max_marker,
                    type = 2)
  
  neat_pop[[i]] <- x$nn
  mutation_tracking = x$mutation_tracking
  max_node = x$max_node
  max_marker= x$max_marker
}

## get offspring number for each species 

for( i in species_summary$species){
  
  offspring_n_mutation <- species_summary$offspring_n_mutation[species_summary$species == i]
  offspring_n_crossover <- species_summary$offspring_n_crossover[species_summary$species == i]
  
  id_offspring_mutation <- sample(summary_df$id[summary_df$species == i], size = offspring_n_mutation)
  neat_pop_next <- c(neat_pop_next,neat_pop[id_offspring_mutation])
  
  new_gen_species <- c(new_gen_species, rep(i, offspring_n_mutation))
  
  p1 <- sample(summary_df$id[summary_df$species == i], size = offspring_n_crossover)
  p2 <- sample(summary_df$id[summary_df$species == i], size = offspring_n_crossover)
  
  neat_pop_next_mate <- mapply(FUN = nn_mate, nn1 = neat_pop[p1], nn2= neat_pop[p2],
                               fitness1 = summary_df$fitness_adj[match(p1,summary_df$id)] , fitness2 =  summary_df$fitness_adj[match(p2,summary_df$id)],
                               MoreArgs = list( disable_p = 0.8))
  neat_pop_next <-  c(neat_pop_next,neat_pop_next_mate)
  
  new_gen_species <- c(new_gen_species, rep(i, offspring_n_crossover))
  
}
neat_pop <- neat_pop_next
