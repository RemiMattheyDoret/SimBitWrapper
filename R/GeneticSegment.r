
GeneticSegment = R6::R6Class(
    ### Class name
    "GeneticSegment",

    public = list(

        initialize = function(lociType, nbLoci, recRates, muRates, dominanceCoefs, selCoefs, fitnessLine, r_unit = "rate", T1_isMultfit = TRUE, T5_isMultfit = TRUE)
        {
            ############
            ## r_unit ##
            ############
            if (r_unit != "rate" & r_unit != "cM" & r_unit != "M")
            {
                stop(paste0("When initializing a 'GeneticSegment', received recombination rate unit (r_unit) different from 'rate', 'cM' and 'M'. Received '", r_unit, "'. Note that default is 'rate'"))
            }

            private$ddr_unit = r_unit

            ################
            ## is multfit ##
            ################

            if (class(T1_isMultfit) != "logical" | is.na(T1_isMultfit) | class(T5_isMultfit) != "logical" | is.na(T5_isMultfit))
            {
                stop(paste0("When initializing a 'GeneticSegment', received T1_isMultfit and/or T1_isMultfit that are not just either TRUE or FALSE. Note that default is 'TRUE'"))
            }
            private$ddfit_T1_isMultfit = T1_isMultfit
            private$ddfit_T5_isMultfit = T5_isMultfit

            ############
            ## nbLoci ##
            ############
            if (missing(nbLoci))
            {
                stop("When initializing a 'GeneticSegment', argument 'nbLoci' was missing.")
            }
            if (class(nbLoci) != "integer" & class(nbLoci) != "numeric")
            {
                stop(paste0("When initializing a 'GeneticSegment', received 'nbLoci' that is not 'numeric', nor 'integer' (class(nbLoci) = ", class(nbLoci), "'."))
            }
            if (nbLoci <= 0)
            {
                stop(paste0("When initializing a 'GeneticSegment', received 'nbLoci' lower than one (nbLoci = '", nbLoci, "'"))
            }

            ###################
            ## Set loci type ##
            ###################

            TTindex = -1
            if (lociType == "T1")
            {
                private$ddL[length(private$ddL) + 1] = paste("T1", nbLoci)
                TTindex = 1
            } else if (lociType == "T2")
            {
                private$ddL[length(private$ddL) + 1] = paste("T2", nbLoci)
                TTindex = 2
            } else if (lociType == "T3")
            {
                stop("Sorry, T3 type of loci are not (yet) accepted by 'GeneticSegment'. The reason is that I fail to agree with myself as to how to specify mutation rates, phenotypes, fitness landscape and all that.")
                private$ddL[length(private$ddL) + 1] = paste("T3", nbLoci)
                TTindex = 3
            } else if (lociType == "T4")
            {
                private$ddL[length(private$ddL) + 1] = paste("T4", nbLoci)  
                TTindex = 4 
            } else if (lociType == "T5")
            {
                private$ddL[length(private$ddL) + 1] = paste("T5", nbLoci)
                TTindex = 5
            } else if (lociType == "T8")
            {
                private$ddL[length(private$ddL) + 1] = paste("T8", nbLoci)
                TTindex = 6
            }
            stopifnot(TTindex != -1)

            ##############
            ## RecRates ##
            ##############
            if (nbLoci > 1)
            {
                if (missing(recRates))
                {
                    stop("When initializing a 'GeneticSegment', argument 'recRates' was missing.")
                }
                if (class(recRates) != "numeric")
                {
                    stop(paste0("When initializing a 'GeneticSegment', received 'recRates' that is not 'numeric' (class(recRates) = ", class(recRates), "'."))
                }
                if (length(recRates) == 0)
                {
                    stop(paste0("When initializing a 'GeneticSegment', received 'recRates' that is of length 0."))
                }
                if (length(recRates) != 1 & length(recRates) != nbLoci-1)
                {
                    stop(paste0("When initializing a 'GeneticSegment', received 'recRates' that is of length different from 1 and different from the number of loci minus one (nbLoci-1)"))
                }
            } else
            {
                if (!missing(recRates))
                {
                    if (length(recRates) != 0)
                    {
                        stop(paste0("When initializing a 'GeneticSegment', received 'recRates' that is of length different from zero whille there is only one locus."))
                    }
                }
                recRates = numeric()
            }


            if (nbLoci > 1)
            {
                if (length(recRates) == 1)
                {
                    private$ddr[length(private$ddr) + 1] = paste("R", recRates[1], nbLoci-1)
                }
                else
                {
                    stopifnot(length(recRates) == nbLoci - 1)
                    private$ddr[length(private$ddr) + 1] = paste(recRates, collapse=" ")
                }
            }

            #############
            ## muRates ##
            #############
            if (class(muRates) != "numeric")
            {
                stop(paste0("When initializing a 'GeneticSegment', received 'muRates' that is not 'numeric' (class(muRates) = ", class(muRates), "'."))
            }
            if (length(muRates) == 0)
            {
                stop(paste0("When initializing a 'GeneticSegment', received 'muRates' that is of length 0."))
            }
            if (length(muRates) != 1 & length(muRates) != nbLoci)
            {
                stop(paste0("When initializing a 'GeneticSegment', received 'muRates' that is of length different from 1 and different from the number of loci"))
            }


            if (length(muRates) == 1)
            {
                #print(length(private$ddmu[[TTindex]]) + 1)
                #print(private$ddmu[[TTindex]])
                #print(paste("R", muRates[1], nbLoci)) 
                private$ddmu[[TTindex]][length(private$ddmu[[TTindex]]) + 1] = paste("R", muRates[1], nbLoci)
            }
            else
            {
                stopifnot(length(muRates) == nbLoci)
                private$ddmu[[TTindex]][length(private$ddmu[[TTindex]]) + 1] = paste(muRates, collapse=" ")
            }


            ###############
            ## Fitnesses ##
            ###############

            if (!missing(fitnessLine))
            {
                if (!missing(dominanceCoefs) | !missing(selCoefs))
                {
                    stop("One cannot specify a 'fitnessLine' and a dominanceCoefs or a selCoefs at the same time")
                }

                private$ddfit[[TTindex]][length(private$ddfit[[TTindex]]) + 1] = fitnessLine
            } else
            {
                if (!missing(dominanceCoefs))
                {
                    if (length(dominanceCoefs) != 1 & length(dominanceCoefs) != nbLoci)
                    {
                        stop("When initializing a 'GeneticSegment', received a 'dominanceCoefs' which length differs from 1 and from the number of loci")
                    }
                }

                if (!missing(selCoefs))
                {
                    if (length(selCoefs) != 1 & length(selCoefs) != nbLoci)
                    {
                        stop("When initializing a 'GeneticSegment', received a 'selCoefs' which length differs from 1 and from the number of loci")
                    }
                }



                if (lociType == "T1")
                {
                    if (private$ddfit_T1_isMultfit)
                    {
                        if (!missing(dominanceCoefs))
                        {
                            stop("When initializing a 'GeneticSegment', received a 'dominanceCoefs' although you specified to use the multiplicative fitness assumption. Note ddfit_T1_isMultfit is set to TRUE by default.")
                        }
                        if (missing(selCoefs))
                        {
                            private$ddfit[[TTindex]][length(private$ddfit[[TTindex]]) + 1] = paste("R 1",nbLoci)
                        } else
                        {
                            if (length(selCoefs) == 1)
                            {
                                private$ddfit[[TTindex]][length(private$ddfit[[TTindex]]) + 1] = rep(1+selCoefs, nbLoci)
                            } else
                            {
                                private$ddfit[[TTindex]][length(private$ddfit[[TTindex]]) + 1] = 1+selCoefs           
                            }
                        }
                            
                    } else
                    {
                        if (missing(selCoefs))
                        {
                            if (!missing(dominanceCoefs))
                            {
                                stop("When initializing a 'GeneticSegment', received a 'dominanceCoefs' but not 'selCoefs'")
                            }
                            
                            private$ddfit[[TTindex]][length(private$ddfit[[TTindex]]) + 1] = paste("R 1",nbLoci*3)
                        } else
                        {
                            if (missing(dominanceCoefs))
                            {
                                stop("When initializing a 'GeneticSegment', did not receive a 'dominanceCoefs' although you specified a 'selCoefs' and you specified not using the multiplicative fitness assumption.")
                            }

                            if (length(selCoefs) == 1)
                            {
                                if (length(dominanceCoefs) == 1)
                                {
                                    private$ddfit[[TTindex]][length(private$ddfit[[TTindex]]) + 1] = paste(rep(paste(1, 1+dominanceCoefs*selCoefs, 1+selCoefs, collapse=" "), nbLoci), collapse=" ")
                                } else
                                {
                                    private$ddfit[[TTindex]][length(private$ddfit[[TTindex]]) + 1] = paste(paste(1, 1+dominanceCoefs*selCoefs, 1+selCoefs), collapse=" ")
                                }
                            } else
                            {
                                if (length(dominanceCoefs) == 1)
                                {
                                    private$ddfit[[TTindex]][length(private$ddfit[[TTindex]]) + 1] = paste(paste(1, 1+dominanceCoefs*selCoefs, 1+selCoefs), collapse=" ")
                                } else
                                {
                                    private$ddfit[[TTindex]][length(private$ddfit[[TTindex]]) + 1] = paste(1, 1+dominanceCoefs*selCoefs, 1+selCoefs, collapse=" ")
                                }
                            }                   
                        }
                    }
                        
                } else if (lociType == "T2")
                {
                    if (!missing(dominanceCoefs))
                    {
                        stop("When initializing a 'GeneticSegment', received a 'dominanceCoefs' although you with T2 loci, you are forced to accept the multiplicative fitness assumption.")
                    }
                    if (missing(selCoefs))
                    {
                        private$ddfit[[TTindex]][length(private$ddfit[[TTindex]]) + 1] = paste("R 1",nbLoci)
                    } else
                    {
                        if (length(selCoefs) == 1)
                        {
                            private$ddfit[[TTindex]][length(private$ddfit[[TTindex]]) + 1] = paste(rep(1+selCoefs, nbLoci), collapse = " ")
                        } else
                        {
                            private$ddfit[[TTindex]][length(private$ddfit[[TTindex]]) + 1] = paste(1+selCoefs, collapse=" ")
                        }
                    }
                } else if (lociType == "T3")
                {
                    abort()
                } else if (lociType == "T4")
                {
                    if (!missing(dominanceCoefs) & !missing(selCoefs))
                    {
                        stop("When initializing a 'GeneticSegment', a dominance coefficients and/or a selection coefficient for loci of type 4. Loci of type 4 are fundamentally neutral and those entries should be left undefined (arguments should be missing).")
                    }
                    
                } else if (lociType == "T5")
                {
                    if (private$ddfit_T5_isMultfit)
                    {
                        if (!missing(dominanceCoefs))
                        {
                            stop("When initializing a 'GeneticSegment', received a 'dominanceCoefs' although you specified to use the multiplicative fitness assumption. Note ddfit_T5_isMultfit is set to TRUE by default.")
                        }
                        if (missing(selCoefs))
                        {
                            private$ddfit[[TTindex]][length(private$ddfit[[TTindex]]) + 1] = paste("R 1",nbLoci)
                        } else
                        {
                            if (length(selCoefs) == 1)
                            {
                                private$ddfit[[TTindex]][length(private$ddfit[[TTindex]]) + 1] = paste(rep(1+selCoefs, nbLoci), collapse = " ")
                            } else
                            {
                                private$ddfit[[TTindex]][length(private$ddfit[[TTindex]]) + 1] = paste(1+selCoefs, collapse=" ")
                            }
                        }
                            
                    } else
                    {
                        if (missing(selCoefs))
                        {
                            if (!missing(dominanceCoefs))
                            {
                                stop("When initializing a 'GeneticSegment', received a 'dominanceCoefs' but not 'selCoefs'")
                            }
                            
                            private$ddfit[[TTindex]][length(private$ddfit[[TTindex]]) + 1] = paste("R 1",nbLoci*2)
                        } else
                        {
                            if (missing(dominanceCoefs))
                            {
                                stop("When initializing a 'GeneticSegment', did not receive a 'dominanceCoefs' although you specified a 'selCoefs' and you specified not using the multiplicative fitness assumption.")
                            }

                            if (length(selCoefs) == 1)
                            {
                                if (length(dominanceCoefs) == 1)
                                {
                                    private$ddfit[[TTindex]][length(private$ddfit[[TTindex]]) + 1] = paste(rep(paste(1+dominanceCoefs*selCoefs, 1+selCoefs, collapse=" "), nbLoci), collapse=" ")
                                } else
                                {
                                    private$ddfit[[TTindex]][length(private$ddfit[[TTindex]]) + 1] = paste(paste(1+dominanceCoefs*selCoefs, 1+selCoefs), collapse=" ")
                                }
                            } else
                            {
                                if (length(dominanceCoefs) == 1)
                                {
                                    private$ddfit[[TTindex]][length(private$ddfit[[TTindex]]) + 1] = paste(paste(1+dominanceCoefs*selCoefs, 1+selCoefs), collapse=" ")
                                } else
                                {
                                    private$ddfit[[TTindex]][length(private$ddfit[[TTindex]]) + 1] = paste(1+dominanceCoefs*selCoefs, 1+selCoefs, collapse=" ")
                                }
                            }                   
                        }
                    }
                } else if (lociType == "T8")
                {
                    if (!missing(dominanceCoefs))
                    {
                        stop("When initializing a 'GeneticSegment', received a 'dominanceCoefs' although you with T8 loci, you are forced to accept the multiplicative fitness assumption.")
                    }
                    if (missing(selCoefs))
                    {
                        private$ddfit[[TTindex]][length(private$ddfit[[TTindex]]) + 1] = paste("R 1",nbLoci)
                    } else
                    {
                        if (length(selCoefs) == 1)
                        {
                            private$ddfit[[TTindex]][length(private$ddfit[[TTindex]]) + 1] = paste(rep(1+selCoefs, nbLoci), collapse = " ")
                        } else
                        {
                            #print(private$ddfit[[TTindex]])
                            #print(length(private$ddfit[[TTindex]]))
                            #print(1+selCoefs)
                            private$ddfit[[TTindex]][length(private$ddfit[[TTindex]]) + 1] = paste(1+selCoefs, collapse=" ")
                        }
                    }
                }
            }
        },

        append = function(other, distanceAmongGeneticSequences = 0)
        {
            if (class(distanceAmongGeneticSequences) != "numeric" & class(distanceAmongGeneticSequences) != "integer")
            {
                stop(paste0("In function 'append' (eventually called from SimBitWrapper::merge), received a distanceAmongGeneticSequences of class '", class(distanceAmongGeneticSequences), " while it was expectedd a numeric"))
            }
            if (length(distanceAmongGeneticSequences) != 1)
            {
                stop(paste0("In function 'append' (eventually called from SimBitWrapper::merge), received a  distanceAmongGeneticSequences that contains more than (or less than) one element."))
            }
            if (distanceAmongGeneticSequences < 0)
            {
                stop(paste0("In function 'append' (eventually called from SimBitWrapper::merge), received a negative distanceAmongGeneticSequences ('", distanceAmongGeneticSequences, "')."))
            }

            if (private$ddr_unit != other$get_ddr_unit())
            {
                stop("In function 'append', the two genetic segments have different unit of recombination rate. Default is 'rate'.")
            }

            if (private$ddfit_T5_isMultfit != other$get_ddfit_T5_isMultfit())
            {
                stop("In function 'append', the two genetic segments have different 'T5_isMultfit'. Default is 'TRUE'.")
            }


            if (private$ddfit_T1_isMultfit != other$get_ddfit_T1_isMultfit())
            {
                stop("In function 'append', the two genetic segments have different 'T1_isMultfit'. Default is 'TRUE'.")
            }

            
            private$ddr[length(private$ddr) + 1] = paste(distanceAmongGeneticSequences)

            private$ddr[(length(private$ddr) + 1) : (length(private$ddr) + length(other$get_ddr()))] = other$get_ddr()
            
            private$ddL[(length(private$ddL) + 1) : (length(private$ddL) + length(other$get_ddL()))] = other$get_ddL()
            for (i in 1:6)
            {
                if (length(other$get_ddmu()[[i]]) > 0)
                    private$ddmu[[i]][(length(private$ddmu[[i]]) + 1) : (length(private$ddmu[[i]]) + length(other$get_ddmu()[[i]]))] = other$get_ddmu()[[i]]

                if (i != 4 & length(other$get_ddfit()[[i]]) > 0)
                    private$ddfit[[i]][(length(private$ddfit[[i]]) + 1) : (length(private$ddfit[[i]]) + length(other$get_ddfit()[[i]]))] = other$get_ddfit()[[i]]
            }
        },

        finalize = function()
        {
            if (length(private$ddL) == 0)
            {
                stop("In function InputGenetics::finalize, it appears that not a single loci has been received!")
            }

            out = list()

            out[["r"]] = paste(private$ddr_unit, "A", paste(private$ddr, collapse=" "))
            out[["L"]] = paste0(private$ddL, collapse=" ")
            

            for (i in 1:6)
            {
                TT = i
                if (i == 6) TT = 8
                #print(paste0("T", i, "_mu"))
                #print(length(private$ddmu[[i]]))
                #print(private$ddmu[i])
                if (length(private$ddmu[[i]]) > 0)
                    out[[paste0("T", TT, "_mu")]] = paste("A", paste0(private$ddmu[[i]], collapse=" "))

                if (i != 4)
                {
                    if (length(private$ddfit[[i]]) > 0)
                    {
                        if (i == 1)
                        {
                            out[[paste0("T", TT, "_fit")]] = paste(ifelse(private$T1_isMultfit, "multfitA", "A"), paste0(private$ddfit[[i]], collapse=" "))
                        } else if (i == 5)
                        {
                            out[[paste0("T", TT, "_fit")]] = paste(ifelse(private$T5_isMultfit, "multfitA", "A"), paste0(private$ddfit[[i]], collapse=" "))
                        } else
                        {
                            out[[paste0("T", TT, "_fit")]] = paste("A", paste0(private$ddfit[[i]], collapse=" "))
                        }
                    }
                }
            }
   
            return(out)
        },


        get_ddr_unit = function()
        {
            return(private$ddr_unit)
        },
        get_ddfit_T1_isMultfit = function()
        {
            return(private$ddfit_T1_isMultfit)
        },
        get_ddfit_T5_isMultfit = function()
        {
            return(private$ddfit_T5_isMultfit)
        },

        get_ddr = function()
        {
            return(private$ddr)
        },
        get_ddL = function()
        {
            return(private$ddL)
        },
        get_ddmu = function()
        {
            return(private$ddmu)
        },
        get_ddfit = function()
        {
            return(private$ddfit)
        }
    ),

    private = list(
        ## dd stands for dash-dash or double-dash

        ddr_unit = NULL,
        ddfit_T1_isMultfit = NULL,
        ddfit_T5_isMultfit = NULL,

        ddr = character(),
        ddL = character(),
        ddmu = list(
            T1 = character(),
            T2 = character(),
            T3 = character(),
            T4 = character(),
            T5 = character(),
            T8 = character()
        ),
        ddfit = list(
            T1 = character(),
            T2 = character(),
            T3 = character(),
            T4 = character(),
            T5 = character(),
            T8 = character()
        )
    )
)


merge = function(listGeneticSegments = list(), distanceAmongGeneticSequences = NULL)
{
    if (distanceAmongGeneticSequences == Inf | distanceAmongGeneticSequences == -Inf)
    {
        stop("In function 'SimBitWrapper::merge', distanceAmongGeneticSequences is non-finite (either Inf or -Inf).")
    }

    if (length(listGeneticSegments) == 0)
    {
        stop("In function 'SimBitWrapper::merge', did not receive any genetic segments")
    }

    out = listGeneticSegments[[1]]$clone()

    if (all(class(out) != "GeneticSegment"))
    {
        stop(paste0("In function 'SimBitWrapper::merge', first object of the list is of class '", class(out),"'. The function 'SimBitWrapper::merge' can only merge objects of type 'GeneticSegment'."))
    }
    
    reference_ddr_unit = out$get_ddr_unit()
    reference_ddfit_T1_isMultfit = out$get_ddfit_T1_isMultfit()
    reference_ddfit_T5_isMultfit = out$get_ddfit_T5_isMultfit()

    if (any(is.null(distanceAmongGeneticSequences) | is.na(distanceAmongGeneticSequences)))
    {
        stop("In function 'SimBitWrapper::merge', distanceAmongGeneticSequences is either missing, NULL or NA.")
    }

    if (length(distanceAmongGeneticSequences) != 1 & length(distanceAmongGeneticSequences) != (length(listGeneticSegments)-1))
    {
        stop(paste("In function 'SimBitWrapper::merge', distanceAmongGeneticSequences is not of length 1 or of the length of the number of geneticSegments received minus 1. Received ",length(listGeneticSegments)," genetic segments but ", length(distanceAmongGeneticSequences)," distances among those elements."))   
    }
    
    for (i in 2:length(listGeneticSegments))
    {
        if (all(class(listGeneticSegments[[i]]) != "GeneticSegment"))
        {
            stop(paste0("In function 'SimBitWrapper::merge', received an object of class '", class(listGeneticSegments[[i]]),"'. The function 'SimBitWrapper::merge' can only merge objects of type 'GeneticSegment'."))
        }


        if (listGeneticSegments[[i]]$get_ddr_unit() != reference_ddr_unit)
        {
            stop("In function 'SimBitWrapper::merge', genetic segments have different unit of recombination rate. Default is 'rate'.")
        }

        if (listGeneticSegments[[i]]$get_ddfit_T1_isMultfit() != reference_ddfit_T1_isMultfit)
        {
            stop("In function 'SimBitWrapper::merge', genetic segments have different 'T1_isMultfit'. Default is 'TRUE'.")
        }

        if (listGeneticSegments[[i]]$get_ddfit_T5_isMultfit() != reference_ddfit_T5_isMultfit)
        {
            stop("In function 'SimBitWrapper::merge', genetic segments have different 'T5_isMultfit'. Default is 'TRUE'.")
        }

        if (length(distanceAmongGeneticSequences) == 1)
        {
            out$append(listGeneticSegments[[i]], distanceAmongGeneticSequences)
        }
        else
        {
            out$append(listGeneticSegments[[i]], distanceAmongGeneticSequences[i-1])
        }
    }
    return(out)
}
