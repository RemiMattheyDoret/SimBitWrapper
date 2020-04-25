
gatherData = function(paramGrid, columnNameOutputFile = NULL, fun = defaultFunctionForGatherData)
{
	if (class(paramGrid) != "data.frame")
	{
		stop(paste0("paramGrid is not a data.frame but a ", class(paramGrid), "."))
	}

	if (is.null(columnNameOutputFile))
	{
		columnNameOutputFile = "outputFile"
		if (!(columnNameOutputFile %in% names(paramGrid)))
		{
			stop(paste0("The column name ", columnNameOutputFile, " (default argument) does not exists in the paramGrid"))
		}
	}

	if (nrow(paramGrid) == 0)
	{
		stop("The parameter paramGrid is empty!")
	}

	if (!(columnNameOutputFile %in% names(paramGrid)))
	{
		stop(paste0("The column name ", columnNameOutputFile, " does not exists in the paramGrid"))
	}


	firstFileData = fun(paramGrid[[columnNameOutputFile]][1])
	outputsGathered = as.data.frame(matrix(ncol = ncol(firstFileData), nrow=nrow(paramGrid)))
	names(outputsGathered) = names(firstFileData)
	outputsGathered[1,] = firstFileData

	if (nrow(paramGrid) > 1)
	{
		for (row in 2:nrow(paramGrid))
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
	}


	return (cbind(paramGrid, outputsGathered))
}
