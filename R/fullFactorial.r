fullFactorial = function(..., outputFilePrefix = NULL)
{
	input = list(...)
	#print(input)
	Names = rep("",length(input))
	for (big in 1:length(input))
	{
		if (class(input[[big]]) == "list")
		{
			name = names(input[[big]])[1]
		} else
		{
			name = names(input)[big]
		}
			
		if (length(name)<1) stop("error: Name missing")
		if (length(name)>1) stop("error: length(name)>1")
		Names[big] = name
	}
	#print(paste0("Names = ", paste(Names, collapse=" ")))

	FirstCol = list()
	for (elem in input)
	{
		#print(paste0("elem = ", paste(elem, collapse=" ")))
		#print(paste0("class of elem = ", class(elem)))
		if (class(elem)=="list")
		{
			FirstCol[[length(FirstCol)+1]] = elem[[1]]
		} else
		{
			FirstCol[[length(FirstCol)+1]] = elem
		}
	}

	#print(FirstCol)
	SGrid = expand.grid(FirstCol)
	Cols = FirstCol
	for (big in 1:length(input))
	{
		if (class(input[[big]]) == "list")
		{
			for (small in 2:length(input[[big]]))
			{
				Cols[[big]] = input[[big]][[small]]
				name = names(input[[big]][small])
				if (length(name)<1) stop("error: Name missing")
				if (length(name)>1) stop("error: length(name)>1")
				Names[length(Names)+1] = name
				SGrid = cbind(SGrid,expand.grid(Cols)[,big])
			}
		}
	}
	names(SGrid) = Names

	if (!is.null(outputFilePrefix))
	{
		SGrid$outputFile = paste0(outputFilePrefix, 1:nrow(SGrid))
	}

	return(SGrid)
}
