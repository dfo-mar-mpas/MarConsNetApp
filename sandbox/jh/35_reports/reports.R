mpas <- MPAs$NAME_E
rmd_file <- system.file("data", "report.Rmd", package = "MarConsNetApp")
output_dir <- file.path(Sys.getenv("OneDriveCommercial"),"MarConsNetTargets","data", "reports")

for (i in seq_along(mpas)) {
  state <- list()
  params <- list()
  input <- list()
  state$mpas <- mpas[i]
  params$mpas <- mpas[i]
  input$mpas <- mpas[i]
  output_file <- file.path(paste0(output_dir,"/", make.names(paste0(names=mpas[i], ".html"))))
  render(input=rmd_file, output_file = output_file, output_format = "html_document", params = params, envir = new.env())
}



# MARITIMES
rmd_file <- system.file("data", "network_report.Rmd", package = "MarConsNetApp")
output_dir <- file.path(Sys.getenv("OneDriveCommercial"),"MarConsNetTargets","data", "reports")

state <- list()
params <- list()
input <- list()
state$mpas <- "Maritimes"
params$mpas <- "Maritimes"
input$mpas <- "Maritimes"
output_file <- file.path(paste0(output_dir,"/", make.names(paste0(names="Maritimes", ".html"))))
render(input=rmd_file, output_file = output_file, output_format = "html_document", params = params, envir = new.env())

