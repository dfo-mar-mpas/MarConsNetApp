# Figure 1: Flow Chart of the action taken when a user applies
# useAdjusted(which=ALL)

library(DiagrammeR)
grViz("digraph flowchart {
      # node definitions with substituted label text
      node [fontname = Helvetica, shape = rectangle, color=black]
      tab1 [label = '@@1']
      node [fontname = Helvetica, shape = rectangle, color=red]
      tab2 [label = '@@2']
      tab3 [label = '@@3']
      node [fontname = Helvetica, shape = rectangle, color=green]
      tab4 [label = '@@4']
      tab5 [label = '@@5']
      node [fontname = Helvetica, shape = rectangle, color=blue]
      tab6 [label = '@@6']
      tab7 [label = '@@7']
      node [fontname = Helvetica, shape = rectangle, color=purple]
      tab8 [label = '@@8']
      tab9 [label = '@@9']




      # edge definitions with the node IDs
      tab1 -> tab2
      tab2 -> tab3
      tab3 -> tab4
      tab4 -> tab5
      tab5 -> tab6
      tab6 -> tab7
      tab7 -> tab8
      tab8 -> tab9


      }

      [1]: 'Conserve and protect all major benthic, demersal (i.e., close to the sea floor) and pelagic (i.e., in the water column) habitats within the MPA, along with their associated physical, chemical, geological and biological properties and processes'
      [2]: 'Indicator 1'
      [3]: 'Indicator 2'
      [4]: 'Indicator 3'
      [5]: 'Indicator 4'
      [6]: 'Indicator 5 '
      [7]: 'Indicator 6'
      [8]: 'Indicator 7'
      [9]: 'Indicator 8'



      ")
