#' Intersection plot
#'
#' Compares two networks by plotting in a circular layout the respective
#' networks for only those nodes that are shared across networks.
#'
#'

inter_plot = function(x, y,
                       nodes = NULL,
                       node_col = 'black', node_border = NA,
                       edge_col = rgb(0,0,0,alpha=.5), edge_weight = function(x) if(is.null(x)) 1 else x,
                       r = .05){

  # get node names
  v_x = names(V(x))
  v_y = names(V(y))

  if(is.null(nodes)){
    nodes = unique(c(v_x, v_y))
  }

  # get circular layout
  l = circle_raw(length(nodes),360,1,c(0,0))[-(length(nodes)+1),]
  l_t = circle_raw(length(nodes),360,1.07,c(0,0))[-(length(nodes)+1),]

  # multiply colors
  if(length(node_col) != nrow(l)) node_col = rep(node_col[1], nrow(l))

  # start plot
  plot.new();plot.window(xlim=c(-1,1) * (1 + r + .3) + c(0,3),
                         ylim=c(-1,1) * (1 + r + .3))

  # subset graphs
  g_x = induced_subgraph(x,v_x[v_x %in% nodes])
  g_y = induced_subgraph(y,v_y[v_y %in% nodes])

  print(summary(edge_weight(E(g_x)$weight)))

  # plot edges
  e_x = get.edgelist(g_x)
  e_x = cbind(memnet:::match_cn(e_x[,1],nodes,1:length(nodes)),memnet:::match_cn(e_x[,2],nodes,1:length(nodes)))
  for(i in 1:nrow(e_x)) lines(l[e_x[i,],1],l[e_x[i,],2],col=edge_col,lwd=edge_weight(E(g_x)$weight)[i])

  e_y = get.edgelist(g_y)
  e_y = cbind(memnet:::match_cn(e_y[,1],nodes,1:length(nodes)),memnet:::match_cn(e_y[,2],nodes,1:length(nodes)))
  for(i in 1:nrow(e_y)) lines(l[e_y[i,],1] + 3,l[e_y[i,],2],col=edge_col,lwd=edge_weight(E(g_y)$weight)[i])

  # plot nodes
  for(i in 1:nrow(l)) circle(r,l[i,],col = node_col[i], border = node_border)
  for(i in 1:nrow(l)) circle(r,l[i,] + c(3,0),col = node_col[i], border = node_border)

  # plot text
  ang = seq(0,-360,length = length(nodes))
  for(i in 1:nrow(l)) text(l_t[i,1],l_t[i,2],labels = nodes[i],adj = ifelse(l_t[i,1]>0,0,1),
                           srt=ifelse(l_t[i,1]>0,ang[i],180+ang[i]), cex = .6)
  for(i in 1:nrow(l)) text(l_t[i,1] + 3,l_t[i,2],labels = nodes[i],adj = ifelse(l_t[i,1]>0,0,1),
                           srt=ifelse(l_t[i,1]>0,ang[i],180+ang[i]), cex = .6)

  }







