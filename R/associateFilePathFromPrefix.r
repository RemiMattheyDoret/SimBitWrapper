associateFilePathFromPrefix = function(prefix)
{
	directory = sub("/[^/]+$","",prefix)
	pattern   = paste0(sub(".*/.*[^/]/+","",prefix), "\\.")
	path = paste0(directory, "/",list.files(path=directory, pattern=pattern))
	if (length(path) == 0)
	{
		stop(paste0("In 'associateFilePathFromPrefix', (maybe called from the default function 'fun' of 'gatherData'), tried to find a single file associate with prefix '", prefix, "'. (dir:", directory, " | pattern:", pattern, ") but none was found."))
	}
	if (length(path) > 1)
	{
		path = path[!grepl("*\\.*[^.]", path)]
		stop(paste0("In 'associateFilePathFromPrefix', (maybe called from the default function 'fun' of 'gatherData'), tried to find a single file associate with prefix '", prefix, "'. (dir:", directory, " | pattern:", pattern, ") but it found ", length(path), " files."))
	}
	return(path)
}
