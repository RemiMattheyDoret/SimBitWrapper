defaultFunctionForGatherData = function(prefix)
{
	path = associateFilePathFromPrefix(prefix)

	d = data.table::fread(file, header=TRUE)
	if (nrow(d) > 1)
	{
		d = d[-(1:(nrow(d)-1)),]
	}
	return(d)
}
