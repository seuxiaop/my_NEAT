add_new_connection <- function(node_df, connect_df,  mutation_tracking, max_marker){
  
  
  # random select a node with "in" capacity
  in_sum <- as.data.frame(table(connect_df$In))
  colnames(in_sum) <- c("node_id","current_in")
  in_sum$node_id <- as.numeric(as.character(in_sum$node_id))
  in_sum$max_in <- length(node_df$node_id[node_df$node_type != "sensor"])
  tem_index <- in_sum$node_id %in% node_df$node_id[node_df$node_type =="hidden"]
  in_sum$max_in[tem_index] <- in_sum$max_in[tem_index] - 1
  in_node_id <- sample(in_sum$node_id[in_sum$current_in < in_sum$max_in ], 1 )
  
  # random sample a node with "in" capacity and not yet connnected with the randomly selected in_node_id
  sample_pop <-  connect_df$Out[!connect_df$Out %in%   unique(connect_df$Out[connect_df$In == in_node_id])]
  if(length(sample_pop) == 1){
    out_node_id <- sample_pop
  }else{
    out_node_id <- sample(   sample_pop   ,1)
  }
  
  
  
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
