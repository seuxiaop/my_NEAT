nn_mutation <- function(nn, mutation_tracking,max_node, max_marker, 
                    type = 1, scale = 0.05, p = 0.8){ # 1= add connection, 2= add node, 3= weight change
  if(type == 1){
    return(add_new_connection(nn,max_node,  mutation_tracking, max_marker))
    
  }else if(type == 2){
    return(add_new_node(nn, max_node, mutation_tracking, max_marker))
  }else{
    
    nn <- weight_mutation(nn, scale = scale, p = p)
    
    return(list(nn = nn,
                mutation_tracking =mutation_tracking,
                max_node = max_node, 
                max_marker = max_marker 
    ))
  }
  
  
} 