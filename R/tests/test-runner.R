source(file.path(getwd(), 'R/tests/dummy-variable.spec.R'))
source(file.path(getwd(), 'R/tests/project-builder.spec.R'))

testGetCodeForDummyVariable()
testBuildProjectFiles()