require(igraph)


watts_strogatz = function(size = 10, max_dist = 2, p = 0, igraph = TRUE){
  
  # get edges
  edges = matrix(ncol = 3, nrow = size * max_dist)
  ind = 0
  for(i in 1:(size-1)){
    for(j in (i+1):size){
      dist = min(c(abs(j - i),abs((j - size)-i)))
      if(dist <= max_dist){
        ind = ind + 1
        edges[ind, ] = c(i, j, dist)
        }
      }
    }
  
  # sort edges by distance
  edges = edges[order(edges[,3],edges[,1],edges[,2]),]
  
  # fill matrix
  net = matrix(0, ncol = size, nrow = size)
  net[edges[,1:2]] = net[edges[,2:1]]  = 1
  
  # rewire
  for(i in 1:nrow(edges)){
    if(runif(1,0,1) < p){
      new = sample(1:size,1)
      if(new != edges[i,1] && net[edges[i,1], new] == 0){
        net[edges[i,1], edges[i,2]] = net[edges[i,2],edges[i,1]] = 0
        net[edges[i,1], new] = net[new, edges[i,1]] = 1
        }
      }
    }
  if(igraph == TRUE) return(graph_from_adjacency_matrix(net, mode = "undirected"))
  net
  }

par(mar=c(0,0,0,0))
g = watts_strogatz(20,2)
plot(g,layout = layout_in_circle(g))


watts_strogatz_fast = function(size = 10, max_dist = 2, p = 0, igraph = TRUE){
  
  # get edges
  net = matrix(0, ncol = size, nrow = size)
  for(i in 0:(size-1)){
    i = (i %% size) + 1
    for(j in i:(size-1)){
      j = (j %% size) + 1
      dist = min(c(abs(j - i),abs((j - size)-i)))
      if(i != j & dist <= max_dist){
        if(runif(1,0,1) > p){
          net[i, j] = net[j, i] = 1
        } else {
          new_j = sample(1:size, 1)
          if(net[i, new_j] == 0){
            net[i, new_j] = net[new_j, i] = 1
          }
        }
      }
    }
  }
  if(igraph == TRUE) return(graph_from_adjacency_matrix(net, mode = "undirected"))
  net
  }
  


rep = 10
ps = exp(seq(log(0.0001), log(1), length = 10))
res = matrix(ncol = 3, nrow = length(ps) * rep)
ind = 0
for(i in 1:length(ps)){
  print(i)
  for(j in 1:rep){
    ind = ind + 1
    net = watts_strogatz_fast(1000, 10, p = ps[i])
    res[ind, 1] = ps[i]
    res[ind, 2] = igraph::average.path.length(net)
    res[ind, 3] = igraph::transitivity(net, type = 'localaverage')
    }
  }

L0 = igraph::average.path.length(watts_strogatz_fast(1000, 10))
C0 = igraph::transitivity(watts_strogatz_fast(1000, 10), type = 'localaverage')

plot(log(ps), tapply(res[,2],res[,1],mean)/L0,ylim=c(0,1))
lines(log(ps), tapply(res[,3],res[,1],mean)/C0)



rep = 1000
ps = exp(seq(log(0.0001), log(1), length = 20))
res = matrix(ncol = 3, nrow = length(ps) * rep)
ind = 0
for(i in 1:length(ps)){
  for(j in 1:rep){
    ind = ind + 1
    net = sample_smallworld(1, 20, 2, p = ps[i])
    res[ind, 1] = ps[i]
    res[ind, 2] = igraph::average.path.length(net)
    res[ind, 3] = igraph::transitivity(net, type = 'localaverage')
  }
}

L0 = igraph::average.path.length(sample_smallworld(1, 20, 2, p = 0))
C0 = igraph::transitivity(sample_smallworld(1, 20, 2, p = 0), type = 'localaverage')

plot(log(ps), tapply(res[,2],res[,1],mean)/L0,ylim=c(0,1))
lines(log(ps), tapply(res[,3],res[,1],mean)/C0)



g = watts_strogatz(20, 2, p = .2)
plot(g, layout = layout_in_circle(g))

mean(degree(watts_strogatz(20, 2, p = 0)))
mean(degree(g))

plot(igraph::graph_from_adjacency_matrix(watts_strogatz(), mode = 'undirected'))


graph = igraph::graph_from_adjacency_matrix(net, mode = 'undirected')
plot(graph, layout = layout_in_circle(graph))



graph = igraph::graph_from_adjacency_matrix(net, mode = 'undirected')
plot(graph, layout = layout_in_circle(graph), color = 'red')



g <- sample_smallworld(2, 3, 3, 0)
g
plot(g, layout = layout_in_circle(g))
