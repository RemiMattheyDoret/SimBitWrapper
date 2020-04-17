### runSimulation
runSimBit = function(executable, path, lineNumber)
{
    stopifnot(class(executable) == "character")
    stopifnot(length(executable) == 1)
    stopifnot(class(path) == "character")
    stopifnot(length(path) == 1)
    system(paste(executable, "f", path, lineNumber))
}
