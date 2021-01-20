GetParameterGridData = function(SGrid,Parameter_line)
{
	for (v in names(SGrid))
	{
		assign(v,SGrid[v][Parameter_line,], envir = .GlobalEnv)
	}
}
