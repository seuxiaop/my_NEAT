#pop_rep <- list(neat_pop[[sample(150,1)]])
get_sub_species <- function(neat_pop, pop_rep, dist_torlerance=3){
  species_vec <- rep(0, length(neat_pop))
  ids <- 1:length(neat_pop)
  n <- length(pop_rep)
  
  for(i in 1:n){
    
    dist_vec <- unlist(lapply(neat_pop, FUN =nn_dist, nn2 = pop_rep[[i]],c1 = 1,c2 = 1,c3 = 0.4))
    index <- dist_vec < dist_torlerance
    in_group_id <- ids[index]
    species_vec[in_group_id] <- i
    ids <- ids[!index]
  }
  
  while(length(ids) >1){
    i <- i + 1
    new_pop_rep <-  neat_pop[[sample(ids,1)]]
    dist_vec <- unlist(lapply(neat_pop[ids], FUN =nn_dist, nn2 = new_pop_rep,c1 = 1,c2 = 1,c3 = 0.4))
    index <- dist_vec < dist_torlerance
    in_group_id <- ids[index]
    species_vec[in_group_id] <- i
    ids <- ids[!index]
  }
  
  species_vec[species_vec==0] <- i + 1
  
  return(species_vec)
}
