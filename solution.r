#this is assignment one solution code

initializeNodes <- function(curPackage)
{
  rows <- list()
  packx <- curPackage$x
  packy <- curPackage$y
  for (y in 1:10)
  {
    row = list()
    for (x in 1:10)
    {
      row[[x]] <- createNode(x, y, manhattanDistance(x, y, packx, packy))
      #print(row)
    }
    rows[[y]] <- row
  }
  return (rows)
}

createNode <- function(xx, yy, hh)
{
  # Keep parent as just coordinates
  return (list(x = xx, y = yy, f = 0, g = 0, h = hh, parent = NULL))
}

getNeighbors <- function(car, nodes)
{
  x = car$x
  y = car$y
  
  
  # Get left neighbor
  lnx = x - 1
  lny = y
  ln = NULL
  if (lnx >= 1)
  {
    ln <- nodes[[lny]][[lnx]]
  }
  # Get upper neighbor
  unx = x
  uny = y + 1
  un = NULL
  if (uny <= 10)
  {
    un <- nodes[[uny]][[unx]]
  }
  # Get right neighbor
  rnx = x+1
  rny = y
  rn = NULL;
  if (rnx <= 10)
  {
    rn <- nodes[[rny]][[rnx]]
  }
  # Get below neighbor
  dnx = x
  dny = y-1
  dn = NULL;
  if (dny >= 1)
  {
    dn <- nodes[[dny]][[dnx]]
  }
  
  return (list(ln, un, rn, dn))
}

manhattanDistance <- function(x1, y1, x2, y2)
{
  return (abs(x2-x1) + abs(y2 - y1))
}

calculateNeighborsCost <- function(node, nodes, roads)
{
  x = node$x
  y = node$y
  hroads = roads$hroads
  vroads = roads$vroads
  neighbors = getNeighbors(node, nodes)
  
  for (neighbor in neighbors) {
    if (!is.null(neighbor))
    {
      dx <- neighbor$x - x
      dy <- neighbor$y - y
      if (abs(dx) == 1)
      {
        hcost <- hroads[x + dx-1, y]
        neighbor$g <- node$g + hcost
      }
      else if (abs(dy) == 1)
      {
        vcost <- vroads[x, y + dy-1]
        neighbor$g <- node$g + vcost
      }
      neighbor$f <- neighbor$g + neighbor$h
      nodes[[neighbor$y]][[neighbor$x]] <- neighbor
      print(neighbor)
    }
  }
}

getBestNode <- function(frontier, goal)
{
  bestNode <- NULL
  bestF <- Inf
  for (node in frontier)
  {
    if (node$f < bestF)
    {
      bestF <- node$f
      bestNode <- node
    }
    if (node$x == goal$x && node$y == goal$y)
    {
      bestNode <- goal
      break
    }
  }
  return (bestNode)
}

test <- function(roads, car, packages)
{
  # Testing
  nodes <- initializeNodes(list(x = 5, y = 7))
  neighbors <- getNeighbors(car, nodes)
  cx = car$x
  cy = car$y
  curNode <- nodes[[cy]][[cx]]
  calculateNeighborsCost(curNode, nodes, roads)
}
