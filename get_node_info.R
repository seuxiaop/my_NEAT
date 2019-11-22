get_node_info <- function(nn){
  
  node_list <- nn$node_df
  connect_df <- nn$connect_df
  node_list$possible_connection <- NA
  
  for(i in 1:nrow(node_list)){
    current_connected <- connect_df$Out[connect_df$In == node_list$node_id[i] & connect_df$Disabled == "N" ]  #including disabled
    all_possible_connection <- node_list$node_id[node_list$level > node_list$level[i]]
    new_possible_connection <- all_possible_connection[!all_possible_connection %in% current_connected]  
    node_list$possible_connection[i] <- paste(new_possible_connection, sep="", collapse=",")
  }
  
  
  return(node_list)
}
