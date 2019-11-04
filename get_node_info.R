get_node_info <- function(connect_df){
  
  node_list <- data.frame(node_id= unique(c(connect_df$In,connect_df$Out)))
  max_level <- 0
  node_list$level <- max_level
  node_list <- node_list[order(node_list$node_id),]
  sensor_node <- node_list$node_id[!node_list$node_id %in% connect_df$Out]
  tem_connect_df <- connect_df
  tem_index <- tem_connect_df$In %in% sensor_node
  while( sum(tem_index) > 0 ){
    level_index <- unique(tem_connect_df$Out[tem_index & tem_connect_df$Disabled == 'N'])
    node_list$level[level_index] <- max_level + 1
    max_level <- max_level + 1
    tem_index <- tem_connect_df$In %in% level_index
    
  }
 
  node_list$level <- rank(node_list$level, ties.method = "min")
  node_list$level <-as.numeric(as.factor(node_list$level ))
  
  in_summary <- as.data.frame(table(connect_df$In))
  colnames(in_summary) <- c("node_id","in_sum")
  out_summary <- as.data.frame(table(connect_df$Out))
  colnames(out_summary) <- c("node_id","out_sum")
  
  node_list <- merge(node_list, in_summary, all.x = T )
  node_list <- merge(node_list, out_summary, all.x = T)
  node_list[is.na(node_list)] <- 0
  
  node_list$possible_connection <- NA
  for(i in 1:nrow(node_list)){
    current_connected <- connect_df$Out[connect_df$In == i]  #including disabled
    all_possible_connection <- node_list$node_id[node_list$level > node_list$level[i]]
    new_possible_connection <- all_possible_connection[!all_possible_connection %in% current_connected]  
    node_list$possible_connection[i] <- paste(new_possible_connection, sep="", collapse=",")
  }
  
  
  return(node_list)
}
