module G : module type of Graph.Imperative.Graph.Concrete (Coords)
module Search : module type of Graph.Traverse.Bfs (G)
