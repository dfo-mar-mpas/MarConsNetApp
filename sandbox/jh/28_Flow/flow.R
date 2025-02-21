library(DiagrammeR)

grViz("
  digraph flowchart {
    graph [layout = dot, rankdir = LR]  # Left to Right

    node [shape = rectangle, style = filled, fillcolor = lightblue]
    network   [label = \"Network Level Objective\"]
    site      [label = \"Site Level Objective\"]
    bin       [label = \"Indicator Bin\"]
    indicator [label = \"Indicator\"]
    data      [label = \"Data\"]
    project   [label = \"Project\"]
    money     [label = \"$\"]

    network -> site
    site -> bin
    bin -> indicator
    indicator -> data
    data -> project
    project -> money
  }
")
