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
  = fun nodes graph =>
    graph
    |> init-state
    |> (fun s => std.array.fold_left visit s nodes)
    |> std.record.get "visited"
  in
{
  order = topo-sort ["qux"] graph,
  # REF: https://opendsa-server.cs.vt.edu/ODSA/Books/CS3/html/GraphTopsort.html
  tasks =
    topo-sort
      ["J6", "J7"]
      {
        "J7" = ["J5"],
        "J6" = ["J2"],
        "J5" = ["J4", "J2"],
        "J4" = ["J2", "J3"],
        "J3" = ["J1"],
        "J2" = ["J1"],
        "J1" = []
      },
  classes =
    topo-sort
      ["3214", "3604", "3304"]
      {
        "3214" = ["2506"],
        "2506" = ["3114", "2505"],
        "3604" = ["3114"],
        "3304" = ["3114"],
        "3114" = ["2505"],
        "2505" = ["2114"],
        "2114" = ["1114"],
        "1114" = [],
      }
}
