add_new_connection <- function(node_df, connect_df,max_node,  mutation_tracking, max_marker){
  
  
  node_list <- get_node_info(connect_df)
  
  
  # random select a node with "in" capacity
  sample_pop <-  node_list$node_id[!(node_list$possible_connection) ==""]
  if(length(sample_pop) == 0){
    return(list(node_df=node_df,
                connect_df = connect_df, 
                mutation_tracking =mutation_tracking ,
                max_node = max_node, 
                max_marker = max_marker 
    ))
  }

  if(length(sample_pop) == 1){
    in_node_id <- sample_pop
  }else{
    in_node_id <- sample(sample_pop   ,1)
  }
  

  # random sample a node with "in" capacity and not yet connnected with the randomly selected in_node_id
  sample_pop <-  unlist(strsplit(node_list$possible_connection[in_node_id],","))
  out_node_id <- sample(length(sample_pop), 1)
  out_node_id <- as.numeric(sample_pop[out_node_id])
  
  # add connection
  innovation_name = paste0("nc",in_node_id,"-" ,out_node_id)  # new connection
  inno_index <- which(mutation_tracking$name == innovation_name)
  
  if(length(inno_index) ==0){
    
    new_marker <- max_marker + 1
    max_marker <- new_marker
    
    new_innovation <- data.frame(
      name = innovation_name, new_node_id = NA,
      new_marker1 = new_marker, In1= in_node_id, Out1 = out_node_id,
      new_marker2 = NA, In2= NA, Out2 = NA
    )
    
    ## update mutation_tracker
    mutation_tracking <- rbind(mutation_tracking,new_innovation )
    
    ## update connect genome 
    new_connect <- data.frame(In = in_node_id, Out = out_node_id, Weight = runif(1), Marker = new_marker, Disabled = "N")
    connect_df <- rbind(connect_df, new_connect)
    
  }else{
    
    ## only update connect genome 
    new_connect <- data.frame(In = in_node_id, Out = out_node_id, Weight = runif(1), Marker = new_marker, Disabled = "N")
    connect_df <- rbind(connect_df, new_connect)
    
  }
  return(list(node_df=node_df,
              connect_df = connect_df, 
              mutation_tracking =mutation_tracking ,
              max_node = max_node, 
              max_marker = max_marker 
  ))
  
}
