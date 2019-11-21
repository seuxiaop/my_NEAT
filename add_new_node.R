add_new_node <- function(nn,max_node, mutation_tracking, max_marker){
  
  node_df <- nn$node_df
  connect_df <- nn$connect_df
  
  # add node
  marker_pop <- connect_df$Marker
  if(length(marker_pop) == 0){
    return(list(nn = nn,
                mutation_tracking =mutation_tracking,
                max_node = max_node, 
                max_marker = max_marker ))
    
  }else if(length(marker_pop) == 1){
    index <- marker_pop
  }else{
    index <- sample(marker_pop , 1)  
  }
  index <- which(connect_df$Marker == index )
  connect_df[index, "Disabled"] <- "Y"
  innovation_name = paste0("nn",connect_df$In[index],"-" ,connect_df$Out[index])
  inno_index <- which(mutation_tracking$name == innovation_name)
  if(length(inno_index) ==0 ){
    
    new_innovation <- data.frame(
      name = innovation_name, new_node_id = NA,
      new_marker1 = NA, In1= NA, Out1 = NA,
      new_marker2 = NA, In2= NA, Out2 = NA
    )
    
    ## add node
    new_node <- data.frame(node_id = max_node + 1, node_type = "hidden")
    new_innovation$new_node_id <- new_node$node_id
    
    ## update node genome list
    node_df <- rbind(node_df, new_node)
    
    ##update max_node traker
    max_node <- max_node + 1
    
    ## add connection
    new_marker <- max_marker + 1
    max_marker <- new_marker
    new_connect1 <- data.frame(In = connect_df$In[index], Out = new_node$node_id, Weight = 1, Marker = new_marker, Disabled = "N")
    
    new_innovation$new_marker1 <- new_marker
    new_innovation$In1 <-  connect_df$In[index]
    new_innovation$Out1 <- new_node$node_id
    
    
    new_marker <- max_marker + 1
    max_marker <- new_marker
    new_connect2 <- data.frame(In = new_node$node_id, Out = connect_df$Out[index], Weight = connect_df$Weight[index], Marker = new_marker, Disabled = "N")
    
    new_innovation$new_marker2 <- new_marker
    new_innovation$In2 <-   new_node$node_id
    new_innovation$Out2 <- connect_df$Out[index]
    
    ## update connection genome
    connect_df <- rbind(connect_df, new_connect1, new_connect2)
    
    ## update max_marker traker
    max_marker <- new_marker
    
    ## update mutation_tracker
    mutation_tracking <- rbind(mutation_tracking,new_innovation )
    
    
  }else{
    ## add node
    new_node <- data.frame(node_id = mutation_tracking$new_node_id[inno_index], node_type = "hidden")
    node_df <- rbind(node_df, new_node)
    ## add connection
    new_connect1 <- data.frame(
      In = mutation_tracking$In1[inno_index],
      Out = mutation_tracking$Out1[inno_index], 
      Weight = 1, 
      Marker = mutation_tracking$new_marker1[inno_index], 
      Disabled = "N"
    )
    
    new_connect2 <- data.frame(
      In = mutation_tracking$In2[inno_index],
      Out = mutation_tracking$Out2[inno_index], 
      Weight = connect_df$Weight[index], 
      Marker = mutation_tracking$new_marker2[inno_index], 
      Disabled = "N"
    )
    ## update connction genome
    connect_df <- rbind(connect_df, new_connect1, new_connect2)
  }
  
  
  nn <- list(node_df=node_df,
             connect_df = connect_df)
  
  return(list(nn = nn,
              mutation_tracking =mutation_tracking,
              max_node = max_node, 
              max_marker = max_marker ))
  

}