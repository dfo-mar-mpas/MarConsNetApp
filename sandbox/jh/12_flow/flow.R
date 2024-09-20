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
      tab8 [label = '@@8']
      tab9 [label = '@@9']
      tab10 [label = '@@10']

      # edge definitions with the node IDs
      tab1 -> tab2
      tab2 -> tab3
      tab3 -> tab4
      tab4 -> tab10 [label='App Indicator not Other']
      tab4 -> tab7 [label='App Indicator = Other']
      tab7 -> tab10
      tab10 -> tab5
      tab5 -> tab6 [label='No']
      tab5 -> tab8 [label='Yes']
      tab6 -> tab8
      tab8 -> tab9 [color=green]
      }

      [1]: 'Create a project in the PPT'
      [2]: 'Tag the protected/conservation area in the tags of your project'
      [3]: 'Tag the relevant indicator bin in the tags of your project*'
      [4]: 'Select the relevant App Indicator in the Deliverable type'
      [5]: 'Is data in a database?'
      [6]: 'Add to GOC Open Data, other Open Data Platforms, or Sharepoint*'
      [7]: 'Contact Jaimie + Remi to discuss how to calculate new indicator'
      [8]: 'Add database name or link into Deliverable description*'
      [9]: 'Data is added to the App!'
      [10]: 'Put the data in X format'

      ")
