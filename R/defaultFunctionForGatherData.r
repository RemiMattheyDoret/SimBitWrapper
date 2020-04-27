defaultFunctionForGatherData = function(path)
{
	directory = sub("/[^/]+$","",path)
	pattern   = paste0(sub(".*/.*[^/]/+","",path), "\\.")
	file = paste0(directory, "/",list.files(path=directory, pattern=pattern))
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
		d = d[-(1:(nrow(d)-1)),]
	}
	return(d)
}
