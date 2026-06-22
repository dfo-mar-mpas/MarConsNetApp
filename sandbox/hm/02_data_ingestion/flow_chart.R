
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
  label = 'Power Automate Flow 1 sends an email\\nwith an initial interest form once an SME record for\\na given topic is marked as entered',
  shape = note,
  style = filled,

]

Note2 [
  label = 'Power Automate Flow 2 pulls responses\\nfrom initial form responses to create\\nrecord(s) in Microsoft List 2',
  shape = note,
  style = filled,

]


Note3 [
  label = 'Power Automate Flow 3 sends an\\nemail with the data collection form once sprint\\nstart date is entered in List 2',
  shape = note,
  style = filled,

]

Note4 [
  label = 'Power Automate Flow 4 pulls responses from\\ndata collection form to populate associated dataset record\\nin Microsoft List 2',
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

{
  rank = same
  ContactSME
  ContactHolder
}

  CoffeeChat      [label = 'Coffee Chat', shape = oval, style = 'rounded, filled', fillcolor = palegreen ]
  SMERecord       [label = 'Create SME record in Microsoft List 1', shape = oval, style = 'rounded, filled', fillcolor = lightblue ]
  SMEsEntered     [label = 'Are all SMEs for that topic entered?', shape = diamond, style = 'rounded, filled', fillcolor = gold]
  InitialEmail    [label = 'Initial Interest Email Sent', shape = box, style = 'rounded, filled', fillcolor = thistle]
  FollowUp1       [label = 'Follow Up', shape = box, style = 'rounded, filled', fillcolor = thistle]
  InterestForm    [label = 'Initial Interest Form Filled?', shape = diamond, style = 'rounded, filled', fillcolor = gold]
  DatasetRecord   [label = 'Dataset Record Created',  shape = box, style = 'rounded, filled', fillcolor = thistle]
  FollowUp2       [label = 'Follow Up', shape = box, style = 'rounded, filled', fillcolor = thistle]
  SMEAvailable    [label = 'SME Available?', shape = diamond, style = 'rounded, filled', fillcolor = gold]
  ScheduleLater   [label = 'Schedule at Later Date', shape = box, style = 'rounded, filled', fillcolor = thistle]
  SprintStart     [label = 'Set Sprint Start Date', shape = box, style = 'rounded, filled', fillcolor = thistle]
  CollectionEmail [label = 'Send Data Collection Email ', shape = oval, style = 'rounded, filled', fillcolor = lightblue ]
  DataForm        [label = 'Data Collection Form Filled?', shape = diamond, style = 'rounded, filled', fillcolor = gold]
  FollowUp3       [label = 'Follow Up', shape = box, style = 'rounded, filled', fillcolor = thistle]
  CompleteRecord  [label = 'Completes Dataset Record\\nin Microsoft List', shape = box, style = 'rounded, filled', fillcolor = thistle]
  DataInspection  [label = 'Data Inspection', shape = oval, style = 'rounded, filled', fillcolor = lightblue]
  DataAccess      [label = 'Can we Access the Data ?', shape = diamond, style = 'rounded, filled', fillcolor = gold]
  MeetSME         [label = 'Meet with SME', shape = box, style = 'rounded, filled', fillcolor = thistle]
  SMEHolder       [label = 'Is the SME the Holder?', shape = diamond, style = 'rounded, filled', fillcolor = gold]
  ContactSME      [label = 'Contact the SME', shape = box, style = 'rounded, filled', fillcolor = thistle]
  ContactHolder   [label = 'Contact the Data Holder', shape = box, style = 'rounded, filled', fillcolor = thistle]
  Sprint          [label = 'Focused Data Ingestion Period', shape = oval, style = 'rounded, filled', fillcolor = lightblue]
  FinalReview     [label = 'Final Review\\nwith SME', shape = oval, style = 'rounded, filled', fillcolor = palegreen]


  CoffeeChat -> SMERecord

  SMERecord -> SMEsEntered

  SMEsEntered -> InitialEmail [label = 'Yes']
  SMEsEntered -> FollowUp1 [label = 'No']
  FollowUp1 -> SMEsEntered

  InitialEmail -> Note1 [style = dashed, arrowhead = none, constraint = false]
  InitialEmail -> InterestForm

  InterestForm -> FollowUp2 [label = 'No']
  FollowUp2 -> InterestForm

  InterestForm -> DatasetRecord [label = 'Yes']

  DatasetRecord -> Note2  [style = dashed, arrowhead = none, constraint = false]
  DatasetRecord -> SMEAvailable

  SMEAvailable -> ScheduleLater [label = 'No']
  SMEAvailable -> SprintStart [label = 'Yes']

  SprintStart -> Note3 [style = dashed, arrowhead = none, constraint = false]
  SprintStart -> CollectionEmail
  CollectionEmail -> DataForm

  DataForm -> FollowUp3 [label = 'No']
  DataForm -> CompleteRecord [label = 'Yes']

  CompleteRecord -> Note4 [style = dashed, arrowhead = none, constraint = false]
  CompleteRecord -> DataInspection
  DataInspection -> DataAccess

  DataAccess -> ContactSME  [label = 'Yes']
  DataAccess ->  SMEHolder [label = 'No']

  SMEHolder -> ContactSME [label = 'Yes']
  SMEHolder -> ContactHolder [label = 'No']

  ContactHolder -> MeetSME
  ContactSME -> MeetSME

  MeetSME -> Sprint
  Sprint -> FinalReview

}
")
