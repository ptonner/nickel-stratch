let graph = {
  foo = [],
  bar = ["foo"],
  buz = ["foo"],
  baz = ["bar", "buz"],
  qux = ["bar", "baz"],
}
in
let init-state = fun g =>
  {
    visited = [],
    graph = std.record.map (fun _ dps => { mark = false, deps = dps }) g
  }
in
let mark-node | doc "Mark a node as visited, putting into running topological sort"
  = fun state name =>
    let { visited, graph } = state in
    let node = std.record.get name graph in
    state
    |> std.record.update "graph" (std.record.update name (std.record.update "mark" true node) graph)
    |> std.record.update "visited" (std.array.append name visited)
  in
let rec visit | doc "Recursive graph exploration, for topological sorting"
  = fun state name =>
    let { visited, graph } = state in
    let node = std.record.get name graph in
    let { deps, mark } = node in
    if mark then
      state
    else
      let new-state = std.array.fold_left visit state deps in
      mark-node new-state name
  in
let topo-sort | doc "Topologically sort nodes in a graph from dependencies"
  = fun graph node =>
    graph
    |> init-state
    |> (fun s => visit s node)
    |> std.record.get "visited"
  in
{
  order = topo-sort graph "qux"
}
