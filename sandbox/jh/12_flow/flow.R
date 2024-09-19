library(DiagrammeR)
grViz("digraph flowchart {
      # node definitions with substituted label text
      node [fontname = Helvetica, shape = rectangle, color=black]
      tab1 [label = '@@1']
      tab2 [label = '@@2']
      tab3 [label = '@@3']
      tab4 [label = '@@4']
      tab5 [label = '@@5']
      tab6 [label = '@@6']
      tab7 [label = '@@7']

      # edge definitions with the node IDs
      tab1 -> tab2
      tab2 -> tab3
      tab3 -> tab4
      tab4 -> tab5 [label='App Indicator not Other']
      tab4 -> tab6 [label='App Indicator = Other']
      tab6 -> tab5
      tab5 -> tab7
      }

      [1]: 'Create a project in the PPT'
      [2]: 'Tag the protected/conservation area in the tags of your project'
      [3]: 'Tag the relevant indicator bin in the tags of your project*'
      [4]: 'Select the relevant App Indicator in the Deliverable type'
      [5]: 'In Deliverable description write relevant database or link to data'
      [6]: 'Contact Jaimie + Remi to discuss how to calculate indicator'
      [7]: 'Put the data in X format'
      ")
