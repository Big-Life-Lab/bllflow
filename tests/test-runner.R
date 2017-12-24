source(file.path(getwd(), 'tests/dummy-variable.spec.R'))
source(file.path(getwd(), 'tests/project-builder.spec.R'))

testGetCodeForDummyVariable()
testBuildProjectFiles()