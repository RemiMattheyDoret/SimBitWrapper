fullFactorial = function(..., simIDprefix = NULL)
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
			
		#if (length(name)<1) stop("error: Name missing")
		#if (length(name)>1) stop("error: length(name)>1")
		Names[big] = name
	}
	#print(Names)
	

	FirstCol = list()
	for (elem in input)
	{
		if (class(elem)=="list")
		{
			FirstCol[[length(FirstCol)+1]] = elem[[1]]
		} else
		{
			FirstCol[[length(FirstCol)+1]] = elem
		}
	}

	SGrid = expand.grid(FirstCol)
	Cols = FirstCol
	for (big in 1:length(input))
	{
		if (length(input[[big]]) > 1)
		{
			for (small in 2:length(input[[big]]))
			{
				#print(paste0("big = ", big))
				#print(paste0("small = ", small))
				Cols[[big]] = input[[big]][[small]]
				name = names(input[[big]][small])
				if (length(name)<1) stop("error: Name missing")
				if (length(name)>1) stop("error: length(name)>1")
				if (any(Names == name)) stop(paste0("Name '", name, "' is present more than once"))
				Names[length(Names)+1] = name
				#print("Current cols:")
				#print(Cols)
				#print("adding column:")
				#print(expand.grid(Cols)[,big])
				SGrid[[name]] = expand.grid(Cols)[,big]
				#SGrid = cbind(SGrid,expand.grid(Cols)[,big]) This does not work if using list of lists
			}
		}
	}
	names(SGrid) = Names

	if (!is.null(simIDprefix))
	{
		SGrid$simulationID = paste0(simIDprefix, 1:nrow(SGrid))
	}

	return(SGrid)
}



# Old much simpler full factorial

# fullFactorial = function(..., outputFilePrefix = NULL)
# {
# 	input = list(...)
# 	print(input)
	
# 	SGrid = expand.grid(input)

# 	if (!is.null(outputFilePrefix))
# 	{
# 		SGrid$outputFile = paste0(outputFilePrefix, 1:nrow(SGrid))
# 	}

# 	return(SGrid)
# }