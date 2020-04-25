


###################
##### R6Class #####
###################



Input = R6::R6Class(
    ### Class name
    "Input",

    ### Private members
    private = list(
        runningThreads = c(),
        data = "",
        shared = {
            env = new.env()
            env$runningThreads = c()
            env$isOtherProcessCheckingThreads = FALSE
            env
        }
        #shared = {
           # env <- new.env()
            #env$optionNames <- c("seed", "random_seed", "printProgress", "nbGens", "nbGenerations", "startAtGeneration", "nbThreads", "S", "species", "nbSubGens", "nbSubGenerations", "PN", "PatchNumber", "N", "patchCapacity", "H", "Habitats", "T5_approximationForNtrl", "T56_approximationForNtrl", "T5_fit", "T5_FitnessEffects", "T5_compressData", "T5_compress", "L", "Loci", "ploidy", "fec", "fecundityForFitnessOfOne", "m", "DispMat", "DispWeightByFitness", "gameteDispersal", "InitialpatchSize", "cloningRate", "selfingRate", "matingSystem", "LogfileType", "sequencingErrorRate", "additiveEffectAmongLoci", "selectionOn", "T1_mu", "T1_MutationRate", "T1_epistasis", "T1_EpistaticFitnessEffects", "T2_mu", "T2_MutationRate", "T4_mu", "T4_MutationRate", "T4_maxAverageNbNodesPerHaplotype", "T5_mu", "T5_MutationRate", "T5_toggleMutsEveryNGeneration", "T5_freqThreshold", "T5_frequencyThresholdForFlippingMeaning", "T3_mu", "T3_MutationRate", "T3_pheno", "T3_PhenotypicEffects", "T3_fit", "T3_FitnessLandscape", "T3_DN", "T3_DevelopmentalNoise", "T1_fit", "T1_FitnessEffects", "T2_fit", "T2_FitnessEffects", "r", "RecombinationRate", "recRateOnMismatch", "FitnessMapInfo", "indTypes", "individualTypes", "resetGenetics", "indIni", "individualInitialization", "T1_ini", "T1_Initial_AlleleFreqs", "T5_ini", "T5_Initial_AlleleFreqs", "GP", "GeneralPath", "T1_vcf_file", "T1_VCF_file", "T1_LargeOutput_file", "T1_AlleleFreq_file", "Log", "Logfile", "Logfile_file", "T1_MeanLD_file", "T1_LongestRun_file", "T1_HybridIndex_file", "T1_ExpectiMinRec_file", "T2_LargeOutput_file", "SaveBinary_file", "T3_LargeOutput_file", "T3_MeanVar_file", "fitness_file", "fitnessSubsetLoci_file", "fitnessStats_file", "T1_FST_file", "T1_FST_info", "extraGeneticInfo_file", "patchSize_file", "extinction_file", "genealogy_file", "coalesce", "shouldGenealogyBeCoalesced", "T4_LargeOutput_file", "T4_vcf_file", "T4_VCF_file", "T4_SFS_file", "T1_SFS_file", "T4_printTree", "T4_coalescenceFst_file", "T5_vcf_file", "T5_VCF_file", "T5_SFS_file", "T5_AlleleFreq_file", "T5_LargeOutput_file", "outputSFSbinSize", "eco", "speciesEcologicalRelationships", "popGrowthModel", "stochasticGrowth", "swapInLifeCycle", "Overwrite", "readPopFromBinary", "DryRun", "centralT1LocusForExtraGeneticInfo", "killOnDemand", "geneticSampling_withWalker", "individualSampling_withWalker")
            #env
        #}
    ),
    # active = list(
    #     runningThreads = function(value)
    #     {
    #         if (missing(value))
    #         {
    #             private$shared$runningThreads
    #         } else
    #         {
    #             private$shared$runningThreads = value
    #         }
    #     },

    #     isOtherProcessCheckingThreads = function(value)
    #     {
    #         if (missing(value))
    #         {
    #             private$shared$isOtherProcessCheckingThreads
    #         } else
    #         {
    #             private$shared$isOtherProcessCheckingThreads = value
    #         }
    #     }
    # ),



    ### Public members
    public = list(
        initialize = function(){},

        ### Print on file
        print = function(path, append = FALSE, oneLine = FALSE)
        {
            stopifnot(class(path) == "character")
            stopifnot(length(path) == 1)

            if (file.exists(path))
            {
                if (!append)
                {
                    file.remove(path)
                    file.create(path)
                }
            } else
            {
                file.create(path)
            }

            if (oneLine)
            {
                sep = " "
            } else
            {
                sep = "\n"
            }

            write(paste(private$data, collapse=sep), path, append=append)
        },


        ### Set option
        set = function(optionName, ..., newline=TRUE)
        {
            stopifnot(class(optionName) == "character")
            stopifnot(length(optionName) == 1)

            # if (!any(optionName == private$shared$optionNames))
            # {
            #     distances = stringdist(optionName, private$shared$optionNames)
            #     alternatives = optionNames[which(order(distances) < 3)]

            #     stop(
            #         paste0(
            #         "\n\tReceived option name ", optionName, " to method 'set' for simulation ID ", private$ID, ".\n\tDid you maybe mean ", alternatives[1], ", ", alternatives[2], ", or ", alternatives[3], "?\n"
            #         )
            #     )
            # }

            entries = list(...)
            for (entry_index in 1:length(entries))
            {
                if (any(class(entries[[entry_index]]) == "data.frame"))
                {
                    entries[[entry_index]] = as.matrix(entries[[entry_index]])   
                }
                entries[[entry_index]] = paste(entries[[entry_index]], collapse=" ")
            }
            entry = paste(entries, collapse=" ")

            private$data[length(private$data)+1] = paste0("--", optionName, " ", entry)
        },

        ### just write
        write = function(text, appendToPrevious=FALSE)
        {
            stopifnot(class(text) == "character")
            stopifnot(length(text) == 1)
            
            if (appendToPrevious)
            {
                private$data[length(private$data)] = paste(private$data[length(private$data)], text)
            } else
            {
                private$data[length(private$data)+1] = text
            }
        },

        ### print options
        #catAllOptions = function()
        #{
        #    print(private$shared$optionNames)
        #},

        ### Print current command
        catCommand = function(oneLine=FALSE)
        {
            if (oneLine)
            {
                cat( paste(private$data, collapse=" ") )
            } else
            {
                cat( paste(private$data, collapse="\n") )
            }
        },

        isAThreadAvailable = function(maxNbThreads)
        {
            while (private$shared$isOtherProcessCheckingThreads)
            {
                Sys.sleep(0.001)
            }
            private$shared$isOtherProcessCheckingThreads = TRUE

            thread_index = 1
            while (thread_index <= length(private$shared$runningThreads))
            {
                thread = private$shared$runningThreads[thread_index]
                if (!thread$is_alive())
                {
                    private$shared$runningThreads = private$shared$runningThreads[-thread_index]
                } else
                {
                    thread_index = thread_index + 1
                }
            }

            r = length(private$shared$runningThreads) < maxNbThreads
            private$shared$isOtherProcessCheckingThreads = FALSE
            return(r)
        },


        run = function(exec = "SimBit", maxNbThreads = 1, sleepTimeInSec = 1, waitEndOfThread = FALSE)
        {
            stopifnot(maxNbThreads > 0)
            stopifnot(sleepTimeInSec >= 0)
    
            while (!self$isAThreadAvailable(maxNbThreads))
            {
                Sys.sleep(sleepTimeInSec)
            }

            newThread = processx::process$new(exec, paste(parameters, collapse=" "))

            #newThread = system(paste(executable, paste(parameters, collapse=" "), "& &> /dev/null; echo $!"), intern = TRUE)

            private$shared$runningThreads = c(private$shared$runningThreads, newThread)

            if (waitEndOfThread)
            {
                while (newThread$is_alive())
                {
                    Sys.sleep(sleepTimeInSec)
                }
            }
        }
    )
)



