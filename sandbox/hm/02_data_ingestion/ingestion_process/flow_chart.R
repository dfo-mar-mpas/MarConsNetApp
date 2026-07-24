
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
  label = 'Power Automate Flow 1 sends an email\\nwith a data collection form once an SME record for\\na given topic is marked as entered',
  shape = note,
  style = filled,

]

Note2 [
  label = 'Power Automate Flow 2 pulls responses\\nfrom initial form responses to create\\nrecord(s) in Microsoft List 2',
  shape = note,
  style = filled,

]


{
  rank = same
  Note1
  CollectionEmail
}
{
  rank = same
  Note2
  DatasetRecord
}


{
  rank = same
  ContactSME
  ContactHolder
}

  CoffeeChat      [label = 'Coffee Chat', shape = oval, style = 'rounded, filled', fillcolor = palegreen ]
  SMERecord       [label = 'Create SME record in Microsoft List 1', shape = oval, style = 'rounded, filled', fillcolor = lightblue ]
  SMEsEntered     [label = 'Are all SMEs for that topic entered?', shape = diamond, style = 'rounded, filled', fillcolor = gold]
  CollectionEmail [label = 'Data Colllection Email Sent', shape = box, style = 'rounded, filled', fillcolor = thistle]
  FollowUp1       [label = 'Follow Up', shape = box, style = 'rounded, filled', fillcolor = thistle]
  CollectionForm  [label = 'Data Collection Form Filled?', shape = diamond, style = 'rounded, filled', fillcolor = gold]
  DatasetRecord   [label = 'Dataset Record Created in Microsoft List 2',  shape = oval, style = 'rounded, filled', fillcolor = lightblue]
  FollowUp2       [label = 'Follow Up', shape = box, style = 'rounded, filled', fillcolor = thistle]
  SMEAvailable    [label = 'SME Available?', shape = diamond, style = 'rounded, filled', fillcolor = gold]
  ScheduleLater   [label = 'Schedule at Later Date', shape = box, style = 'rounded, filled', fillcolor = thistle]
  SprintStart     [label = 'Set Sprint Start Date', shape = box, style = 'rounded, filled', fillcolor = thistle]
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

  SMEsEntered -> CollectionEmail [label = 'Yes']
  SMEsEntered -> FollowUp1 [label = 'No']
  FollowUp1 -> SMEsEntered

  CollectionEmail -> Note1 [style = dashed, arrowhead = none, constraint = false]
  CollectionEmail -> CollectionForm

  CollectionForm -> FollowUp2 [label = 'No']
  FollowUp2 -> CollectionForm

  CollectionForm -> DatasetRecord [label = 'Yes']

  DatasetRecord -> Note2  [style = dashed, arrowhead = none, constraint = false]
  DatasetRecord -> SMEAvailable

  SMEAvailable -> ScheduleLater [label = 'No']
  SMEAvailable -> SprintStart [label = 'Yes']

  SprintStart -> DataInspection
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
