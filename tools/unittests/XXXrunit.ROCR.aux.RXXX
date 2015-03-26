library(RUnit)
myTestSuite <- defineTestSuite("ROCR test suite", "tests","runit.aux.r")
isValidTestSuite(myTestSuite)
testData <- runTestSuite(myTestSuite)

printTextProtocol(testData, showDetails=TRUE)
printHTMLProtocol(testData, "tests/testresults.html")
