
library(DiagrammeR)


grViz("
digraph process {

  graph [rankdir = TB, nodesep = 0.6]

  node [
  fontname = Helvetica,
  fontsize = 12,
  shape = box,
  style = rounded
]

edge [
  fontname = Helvetica,
  fontsize = 10,
  arrowsize = 0.8
]

Note1 [
  label = 'Power Automate Flow 1 sends an email\\n with an initial interest form once an SME\\nrecord is created in List 1',
  shape = note,
  style = filled,

]

Note2 [
  label = 'Power Automate Flow 2 pulls responses\\nfrom initial form responses to create\\ndataset record',
  shape = note,
  style = filled,

]


Note3 [
  label = 'Power Automate Flow 3 sends an email\\nwith the data collection form once\\nsprint start date is entered in List 2',
  shape = note,
  style = filled,

]

Note4 [
  label = 'Power Automate Flow 4 pulls responses from\\ndata collection form to fill dataset record',
  shape = note,
  style = filled,

]

{
  rank = same
  Note1
  InitialEmail
}
{
  rank = same
  Note2
  DatasetRecord
}

{
  rank = same
  Note3
  SprintStart
}

{
  rank = same
  Note4
  CompleteRecord
}


  CoffeeChat      [label = 'Coffee Chat', shape = oval, style = 'rounded, filled', fillcolor = palegreen ]
  InitialEmail    [label = 'Send Initial Email ', shape = oval, style = 'rounded, filled', fillcolor = lightblue ]
  InterestForm    [label = 'Initial Interest Form Filled?', shape = diamond, style = 'rounded, filled', fillcolor = gold]
  DatasetRecord   [label = 'Dataset Record Created\\nin Microsoft List',  shape = box, style = 'rounded, filled', fillcolor = thistle]
  FollowUp1       [label = 'Follow Up', shape = box, style = 'rounded, filled', fillcolor = thistle]
  SMEAvailable    [label = 'SME Available?', shape = diamond, style = 'rounded, filled', fillcolor = gold]
  ScheduleLater   [label = 'Schedule at Later Date', shape = box, style = 'rounded, filled', fillcolor = thistle]
  SprintStart     [label = 'Set Sprint Start Date', shape = box, style = 'rounded, filled', fillcolor = thistle]
  CollectionEmail [label = 'Send Data Collection Email ', shape = oval, style = 'rounded, filled', fillcolor = lightblue ]
  DataForm        [label = 'Data Collection Form Filled?', shape = diamond, style = 'rounded, filled', fillcolor = gold]
  FollowUp2       [label = 'Follow Up', shape = box, style = 'rounded, filled', fillcolor = thistle]
  CompleteRecord  [label = 'Completes Dataset Record\\nin Microsoft List', shape = box, style = 'rounded, filled', fillcolor = thistle]
  DataInspection  [label = 'Data Inspection', shape = oval, style = 'rounded, filled', fillcolor = lightblue]
  DataAccess      [label = 'Can We Access the Data?', shape = diamond, style = 'rounded, filled', fillcolor = gold]
  MeetSME         [label = 'Meet with SME', shape = box, style = 'rounded, filled', fillcolor = thistle]
  SMEHolder       [label = 'Is the SME the Holder?', shape = diamond, style = 'rounded, filled', fillcolor = gold]
  ContactSME      [label = 'Contact the SME', shape = box, style = 'rounded, filled', fillcolor = thistle]
  ContactHolder   [label = 'Contact the Data Holder', shape = box, style = 'rounded, filled', fillcolor = thistle]
  Sprint          [label = 'Focused Data Ingestion Period', shape = oval, style = 'rounded, filled', fillcolor = lightblue]
  FinalReview     [label = 'Final Review\\nwith SME', shape = oval, style = 'rounded, filled', fillcolor = palegreen]

  CoffeeChat -> InitialEmail
  InitialEmail -> Note1 [style = dashed, arrowhead = none, constraint = false]
  InitialEmail -> InterestForm

  InterestForm -> FollowUp1 [label = 'No']
  FollowUp1 -> InterestForm

  InterestForm -> DatasetRecord [label = 'Yes']

  DatasetRecord -> Note2  [style = dashed, arrowhead = none, constraint = false]
  DatasetRecord -> SMEAvailable

  SMEAvailable -> ScheduleLater [label = 'No']
  SMEAvailable -> SprintStart [label = 'Yes']

  SprintStart -> Note3 [style = dashed, arrowhead = none, constraint = false]
  SprintStart -> CollectionEmail
  CollectionEmail -> DataForm

  DataForm -> FollowUp2 [label = 'No']
  DataForm -> CompleteRecord [label = 'Yes']

  CompleteRecord -> Note4 [style = dashed, arrowhead = none, constraint = false]
  CompleteRecord -> DataInspection
  DataInspection -> DataAccess

  DataAccess -> MeetSME [label = 'Yes']
  DataAccess -> SMEHolder [label = 'No']

  SMEHolder -> ContactSME [label = 'No']
  SMEHolder -> ContactHolder [label = 'Yes']

  ContactSME -> MeetSME
  ContactHolder -> MeetSME

  MeetSME -> Sprint
  Sprint -> FinalReview

}
")
