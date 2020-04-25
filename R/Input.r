


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
        
    ),
    active = list(
        runningThreads = function(value)
        {
            if (missing(value))
            {
                private$shared$runningThreads
            } else
            {
                private$shared$runningThreads = value
            }
        },

        isOtherProcessCheckingThreads = function(value)
        {
            if (missing(value))
            {
                private$shared$isOtherProcessCheckingThreads
            } else
            {
                private$shared$isOtherProcessCheckingThreads = value
            }
        }
    ),



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
            while (isOtherProcessCheckingThreads)
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

            r = length(private$shared$runningThreads) <  maxNbThreads
            private$shared$isOtherProcessCheckingThreads = FALSE
            return(r)
        },


        run = function(exec = "SimBit", maxNbThreads = 1, sleepTimeInSec = 1)
        {
            stopifnot(maxNbThreads > 0)
            stopifnot(sleepTimeInSec >= 0)
    
            while (!isAThreadAvailable(maxNbThreads))
            {
                Sys.sleep(sleepTimeInSec)
            }

            newThread = processx::process$new(executable, paste(parameters, collapse=" "))

            #newThread = system(paste(executable, paste(parameters, collapse=" "), "& &> /dev/null; echo $!"), intern = TRUE)

            private$shared$runningThreads = c(private$shared$runningThreads, newThread)
        
        }
    )
)




#### Example
# input = Input$new("Test","1","/Users/remi/test")
# input$set("PN", 1)
# input$set("N", "unif 100")
# input$set("L", "T1 8")
# input$set("T1_mu", "unif 1e-7")
# input$set("nbGens", "100")
# input$catCommand(oneLine = TRUE)
# input$print("path/to/file.txt", append=FALSE, oneLine=FALSE)
# runSimBit("SimBit", "path/to/file.txt", "a")
