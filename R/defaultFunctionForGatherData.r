defaultFunctionForGatherData = function(path)
{
	directory = sub("/[^/]+$","",path)
	pattern   = paste0(sub(".*/.*[^/]/+","",path), "\\.")
	file = list.files(path=directory, pattern=pattern)
	if (length(file) == 0)
	{
		stop(paste0("In the default function 'fun' of 'gatherData', tried to find a single file associate with path '", path, "'. (dir:", directory, " | pattern:", pattern, ") but none was found."))
	}
	if (length(file) > 1)
	{
		file = file[!grepl("*\\.*[^.]", file)]
		stop(paste0("In the default function 'fun' of 'gatherData', tried to find a single file associate with path '", path, "'. (dir:", directory, " | pattern:", pattern, ") but it found ", length(file), " files."))
	}

	d = data.table::fread(file, header=TRUE)
	if (nrow(d) > 1)
	{
		stop(paste0("The default 'fun' argument to 'gatherData' assumes that each file has headers and only one row of data. The file ", path, " has ", nrow(d), " rows though!"))
	}
	return(d)
}
