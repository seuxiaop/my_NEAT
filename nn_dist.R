nn_dist <- function(nn1,nn2,c1 = 1,c2 = 1,c3 = 0.4){
  
  node_df1 <- nn1$node_df
  node_df2 <- nn2$node_df
  connect_df1 <- nn1$connect_df
  connect_df2 <- nn2$connect_df
  
  connect_df <- merge(connect_df1, connect_df2, by="Marker", all.x = T, all.y = T)
  
  n_unmatched <- sum(is.na(connect_df$In.x)) + sum(is.na(connect_df$In.y))
  n_excess <- abs(max(connect_df2$Marker) - max(connect_df1$Marker))
  n_disjoin <- n_unmatched - n_excess
  
  matched  <- (!is.na(connect_df$In.x) ) & (!is.na(connect_df$In.y) )
  w_avg_diff <- mean(abs( connect_df$Weight.x[matched ] - connect_df$Weight.y[matched ]))
  
  n_max <- max(nrow(connect_df1), nrow(connect_df2))
  
  return( c1*n_excess/n_max + c2*n_disjoin/n_max + c3*w_avg_diff )
  
}