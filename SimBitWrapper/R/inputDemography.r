
InputDemographyPatch = R6::R6Class(
    ### Class name
    "InputDemographyPatch",

    public = list(
        MIGR = NULL,


        initialize = function(s)
        {
            private$testSize(s, "new (initialize)")
            private$SIZE = s
            self$MIGR = list()
        },

        resize = function(s)
        {
            private$testSize(s, "resize")
            private$SIZE = s
        },

        setMigration = function(name, m)
        {
            private$testMigrationRate(m)

            if (name == "")
            {
                stop("In function 'setMigration', received a name for the new patch that is the empty string. Any string is accepted (for as long it has not been used already for another patch) except the empty string")
            }

            if (is.na(m))
            {
                stop("In function 'setMigration', received a migration probability that is NA.")
            }

            if (m < 0.0 | m > 1.0)
            {
                stop(paste0("In function 'setMigration', received a migration probability that is either lower than 0 or greater than 1. Received m = ", m, "."))
            }


            self$MIGR[[name]] = m
        },

        size = function()
        {
            return (private$SIZE)
        }
    ),

    private = list(
        SIZE = NULL,


        testMigrationRate = function(m)
        {
            if (class(m) != "numeric" & class(m) != "integer")
            {
                stop(paste0("Received a migration rate that is of inappropriate class (not an 'integer' and not a 'numeric'). Class received is '", class(m), "'."))
            }
            if (m < 0 | m > 1)
            {
                stop(paste0("Received a migration rate that is either lower than zero or greater than one. Migration rate received is ", m, "."))
            }
        },

        testSize = function(size, fct)
        {
            if (is.na(size))
            {
                stop(paste0("In class 'InputDemography', in function '",fct,"', received NA for the size of the new patch."))
            }
            if (size < 0)
            {
                stop(paste0("In class 'InputDemography', in function '",fct,"', received a negative size for a new patch. Size received is ", size, "."))
            }
            if (size%%1!=0)
            {
                stop(paste0("In class 'InputDemography', in function '",fct,"', received a size that is not an integer. Size received is ", size, "."))
            }
        }
    )
)



InputDemography = R6::R6Class(
    ### Class name
    "InputDemography",


    ### Public members
    public = list( 
        ### Initialize
        initialize = function(isForward = TRUE)
        {
            private$isForward = isForward
            private$hasFinalizedYet = FALSE
            private$generation = 0
            private$patches = list()
            private$allData = list()
            private$nameMap = new.env(hash=TRUE)
            private$entireMigrationInputLines = list()
        },

        ### resize a patch
        resize = function(name, size)
        {
            if (class(name) != "character") name = as.character(name)

            if (is.null(private$patches[[name]]))
            {
                stop(paste0("In class 'InputDemography', in function 'resize'. Patch '",name, "' does not exist in the current population."))
            }

            private$patches[[name]]$resize(size)

        },


        size = function(name)
        {
            if (class(name) != "character") name = as.character(name)

            if (is.null(private$patches[[name]]))
            {
                stop(paste0("In class 'InputDemography', in function 'size'. Patch '",name, "' does not exist in the current population."))
            }

            return(private$patches[[name]]$size())
        },

        ### Set migration rate
        setMigration = function(patchA, patchB, m)
        {
            if (class(patchA) != "character") patchA = as.character(patchA)
            if (class(patchB) != "character") patchB = as.character(patchB)
            
            if (is.null(private$patches[[patchA]]))
            {
                stop(paste0("In class 'InputDemography', in function 'setMigration'. Patch '",patchA, "' does not exist in the current population."))
            }

            if (is.null(private$patches[[patchB]]))
            {
                stop(paste0("In class 'InputDemography', in function 'setMigration'. Patch '",patchB, "' does not exist in the current population."))
            }

            private$patches[[patchA]]$setMigration(patchB, m)

        },

        ### add a Patch
        addPatch = function(name, size)
        {
            if (class(name) != "character")
            {
                name = as.character(name)
            }

            if (name == "")
            {
                stop("In class 'InputDemography', in function 'addPatch', received a name for the new patch that is the empty string. Any string is accepted (for as long it has not been used already for another patch) except the empty string. Note that patch names that are not characters are being coerced as characters.")
            }

            if (!is.null(private$patches[[name]]))
            {
                stop(paste0("In class 'InputDemography', in function 'addPatch'. Patch '",name, "' already exists. You can reuse a patch name if you have removed it previously but no two patches present at the same time can have the same name. Note that patch names that are not characters are being coerced as characters."))   
            }

            if (is.null(private$nameMap[[name]]))
                private$nameMap[[name]] = length(private$nameMap) + 1 # one-based counting

            private$patches[[name]] = InputDemographyPatch$new(size)
        },

        ### delete patch
        removePatch = function(name)
        {
            if (class(name) != "character")
            {
                name = as.character(name)
            }

            if (is.null(private$patches[[name]]))
            {
                stop(paste0("In class 'InputDemography', in function 'removePatch'. Patch to remove named '",name, "' does not exist. Note that patch names that are not characters are being coerced as characters."))   
            }

            # Remove patch
            private$patches[[name]] = NULL

            # Remove migration rates to this patch
            for (patch in private$patches)
            {
                patch$MIGR[[name]] = NULL
            }

            ## Do not remove from the patch name map (private$nameMap)
        },

        ### setGeneration
        setGeneration = function(newGen)
        {
            if (private$generation != 0 | newGen != 0)
            {
                if (newGen < 0 | (private$generation != 0 & newGen <= private$generation))
                {
                    stop("In class 'InputDemography', in function 'setGeneration', received the generation '", newGen, "'. The last generation received was '", private$generation, "'. Generations must be recevied in incremental order. Note that setting the generation 0 is optional.")
                }

                private$moveData()
                private$generation = newGen
            }
        },


        ### fixMigrationInputLine
        setEntireMigrationInputLineForThisGeneration = function(line)
        {
            if (class(line) != "character")
            {
                stop("In function 'setEntireMigrationInputLineForThisGeneration', received an object that is not of type 'character'.")
            }

            if (length(line) > 1)
            {
                line = paste(line, collapse=" ")
            } else if (length(line) == 0)
            {
                stop("In function 'setEntireMigrationInputLineForThisGeneration', received an object of length 0 (not even the empty string).")
            }


            if (line == "")
            {
                stop("In function 'setEntireMigrationInputLineForThisGeneration', received an empty string. Note that, by default, InputDemography creates a dispersal matrix based on the data given to 'setMigration'. If you specify, for a given generation, a line with the function 'setEntireMigrationInputLineForThisGeneration', then this line has precedence (the data given to 'setMigration' will be ignored). Note that this string is not evaluated by InputDemography, in the sense that if it does not make any sense, then it will still be present in the input and it will be SimBit's job to highlight the nonesense.")
            }
            private$entireMigrationInputLines[[paste(private$generation)]] = reduceString(line)
        },


        ### Function that is public but should a priori only be called from Input
        finalize = function()
        {
            if (!private$hasFinalizedYet)
            {
                private$moveData()
            }

            ### PN input
            maxPN = 0
            for (generation in names(private$allData))
            {
                patches = private$allData[[paste(generation)]]
                if (length(patches) == 0)
                {
                    stop(paste0("In InputDemography, error found when trying to write SimBit's input line. Starting at generation ", generation," it seems that there are zero patches in the population. Nothing willl be able to be simulated."))
                }
                if (maxPN < length(patches)) maxPN = length(patches)
            }
            stopifnot(maxPN > 0)
            PN_input = paste(maxPN)


            ### N input
            N_input = character()
            for (generation in names(private$allData))
            {
                patches = private$allData[[paste(generation)]]

                ### Compute stuff
                Ns = integer(maxPN)
                for (patchName in names(patches))
                {
                    id = private$nameMap[[patchName]]
                    stopifnot(id <= length(Ns))
                    Ns[id] = patches[[patchName]]$size()
                }

                ### Prepare input string for generation
                newString = ifelse(length(Ns) > 1 & length(unique(Ns)) == 1, paste("unif", Ns[1]), paste("A", reduceString(as.character(Ns))))

                ### Add string if it differs from the previous one
                if (length(N_input) == 0)
                {
                    N_input[length(N_input)+1] = paste0("@G", generation)
                    N_input[length(N_input)+1] = newString
                } else if (N_input[length(N_input)] != newString)
                {
                    N_input[length(N_input)+1] = paste0("@G", generation)
                    N_input[length(N_input)+1] = newString  
                }
            }
            N_input = paste(N_input, collapse=" ")
            stopifnot(length(N_input) == 1)




            ### migration input 
            m_input = character()
            for (generation in names(private$allData))
            {
                if (is.null(private$entireMigrationInputLines[[paste(generation)]]))
                {
                    patches = private$allData[[paste(generation)]]

                    ### Compute stuff
                    M = diag(maxPN)
                    for (patchFromName in names(patches))
                    {
                        idFrom = private$nameMap[[patchFromName]]
                        stopifnot(idFrom <= length(Ns))

                        migrInfo = patches[[patchFromName]]$MIGR
                        for (patchToName in names(migrInfo))
                        {
                            idTo = private$nameMap[[patchToName]]
                            m = migrInfo[[patchToName]]

                            stopifnot (m >= 0 | m <= 1)
                            if (idTo == idFrom)
                            {
                                stopifnot(patchFromName == patchToName)
                                stop(paste0("In InputDemography, error found when trying to write SimBit's input line. Starting at generation ", generation," it seems that it received a migration probability from patch ", patchFromName, " (internal one-based index = ", idFrom,") to itself. When using 'InputDemography', probabilities of not migrating should not be set but are computed internally from the probabilities of migrating."))
                            }

                            M[idTo,idFrom] = m
                            M[idFrom,idFrom] = M[idFrom,idFrom] - m
                            if (M[idFrom,idFrom] < 0.0)
                            {
                                stop(paste0("In InputDemography, error found when trying to write SimBit's input line. Starting at generation ", generation," it seems that it received a sum of migration probabilitties greater than 1."))
                            }
                        }
                    }

                    ### Prepare input string for generation
                    newString = paste0("A ", reduceString(M))
                } else
                {
                    newString = private$entireMigrationInputLines[[paste(generation)]]
                }


                ### Add string if it differs from the previous one
                if (length(m_input) == 0)
                {
                    m_input[length(m_input)+1] = paste0("@G", generation)
                    m_input[length(m_input)+1] = newString
                } else if (m_input[length(m_input)] != newString)
                {
                    m_input[length(m_input)+1] = paste0("@G", generation)
                    m_input[length(m_input)+1] = newString  
                }
            }
            m_input = paste0(ifelse(private$isForward, "", "backward "), paste(m_input, collapse=" "))



            ### Make sure we know the data has already been moved
            private$generation = 0
            private$hasFinalizedYet = TRUE

            ### Return stuff
            return(list(
                PN = PN_input,
                N  = N_input,
                m = m_input
            ))
        },


        getCurrentNbPatches = function()
        {
            return(length(private$patches))
        },

        getCurrentPatchNames = function()
        {
            return(names(private$patches))
        },

        getAllPatchNames = function()
        {
            return(names(private$nameMap))
        },


        print_allDataSizes = function()
        {
            for (generation in names(private$allData))
            {
                cat(paste0(generation, ": "))
                patches = private$allData[[paste(generation)]]
                
                for (patchName in names(patches))
                {
                    cat(paste0(patches[[patchName]]$size()), " ")
                }
                cat("\n")
            }
        }
    ),


    private = list(
        hasFinalizedYet = NULL,
        isForward = NULL,
        generation = NULL,
        allData = NULL,
        nameMap = NULL,
        entireMigrationInputLines = NULL,
        patches = NULL,


        moveData = function()
        {
            # private$allData[[paste(private$generation)]] = private$patches # That would shallow copy only
            private$allData[[paste(private$generation)]] = list()
            for (name in names(private$patches))
            {
                private$allData[[paste(private$generation)]][[name]] = private$patches[[name]]$clone(deep=TRUE)
            }
        }
    )
)





# cppFunction('
#     std::string toShortString(NumericVector& x)
#     {
#         std::string r;
#         if (x.size() == 0)
#         {
#             return r;
#         }

#         size_t nbRepeats = 1;
#         double last = x[0];
#         for (size_t i = 1 ; i < x.size() ; ++i)
#         {
#             if (x[i] == last)
#             {
#                 ++nbRepeats;
#             } else
#             {
#                 std::ostringstream ss;
#                 ss<<last;
#                 std::string lastString = ss.str();
#                 if (nbRepeats > 2)
#                 {
#                     r += " REP " + lastString + " " + std::to_string(nbRepeats);
#                 } else
#                 {
#                     if (nbRepeats == 1)
#                         r += " " + lastString;
#                     else if (nbRepeats == 2)
#                         r += " " + lastString + " " + lastString;
#                 }
#                 nbRepeats = 1;
#                 last = x[i];
#             }
#         }
 


#         std::ostringstream ss;
#         ss<<last;
#         std::string lastString; = ss.str();
#         if (nbRepeats > 2)
#         {
#             r += " REP " + lastString + " " + std::to_string(nbRepeats);
#         } else
#         {
#             if (nbRepeats == 1)
#                 r += " " + lastString;
#             else if (nbRepeats == 2)
#                 r += " " + lastString + " " + lastString;
#         }

#         return r;
#     }
# ')

