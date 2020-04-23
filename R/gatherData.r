
gatherData = function(paramGrid, columnNameOutputFile = NULL, fun = defaultFunctionForGatherData)
{

	if (class(paramGrid) != "data.frame"))
	{
		stop("paramGrid is not a data.frame")
	}

	if (is.null(columnNameOutputFile))
	{
		columnNameOutputFile = "outputFile"
	}

	outputsGathered = data.frame()
	for (row in 1:nrow(paramGrid))
	{
		path = paramGrid[[columnNameOutputFile]][row]
		if (class(path) == "factor")
		{
			path = paste(path)
		} else
		{
			if (class(path) != "character")
			{
				stop(paste0("At row ", row, " of the column ", columnNameOutputFile, " of paramGrid, found a value that is not a character, not a factor. It is a ", class(path), "."))
			}
		}

		outputsGathered[row,] = fun(path)
	}


	return (cbind(paramGrid, outputsGathered))
}
