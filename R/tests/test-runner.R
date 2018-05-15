source(file.path(getwd(), 'R/tests/dummy-variable.spec.R'))
source(file.path(getwd(), 'R/tests/project-builder.spec.R'))
source(file.path(getwd(), 'R/tests/interactions.spec.R'))

testGetCodeForDummyVariable()
testBuildProjectFiles()
testGetCodeForInteractions()