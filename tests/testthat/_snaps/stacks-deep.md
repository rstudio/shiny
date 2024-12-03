# deep stack capturing

    Code
      formatError(err)
    Output
        [1] Error in onFinally: boom                  
        [2]   57: stop                                
        [3]   56: onFinally [test-stacks-deep.R#XXX]  
        [4]   55: onFulfilled                         
        [5]   53: callback [conditions.R#XXX]         
        [6]   50: <Anonymous>                         
        [7]   49: onFulfilled                         
        [8]   39: handleFulfill                       
        [9]   38: <Anonymous>                         
       [10]   37: execCallbacks                       
       [11]   36: later::run_now                      
       [12]   35: wait_for_it [mock-session.R#XXX]    
       [13]   34: eval [test-stacks-deep.R#XXX]       
       [14]   33: eval                                
       [15]   24: test_code                           
       [16]   23: test_that                           
       [17]   22: eval [test-stacks-deep.R#XXX]       
       [18]   21: eval                                
       [19]   12: test_code                           
       [20]   11: source_file                         
       [21]   10: FUN                                 
       [22]    9: lapply                              
       [23]    3: test_files_serial                   
       [24]    2: test_files                          
       [25]    1: testthat::test_file                 
       [26] From earlier call:                        
       [27]   68: domain$wrapOnFulfilled              
       [28]   67: promiseDomain$onThen                
       [29]   66: action                              
       [30]   59: promise                             
       [31]   58: self$then                           
       [32]   57: promise$finally                     
       [33]   56: finally                             
       [34]   55: onRejected [test-stacks-deep.R#XXX] 
       [35]   53: callback [conditions.R#XXX]         
       [36]   50: <Anonymous>                         
       [37]   49: onRejected                          
       [38]   39: handleReject                        
       [39]   38: <Anonymous>                         
       [40]   37: execCallbacks                       
       [41]   36: later::run_now                      
       [42]   35: wait_for_it [mock-session.R#XXX]    
       [43]   34: eval [test-stacks-deep.R#XXX]       
       [44]   33: eval                                
       [45]   24: test_code                           
       [46]   23: test_that                           
       [47]   22: eval [test-stacks-deep.R#XXX]       
       [48]   21: eval                                
       [49]   12: test_code                           
       [50]   11: source_file                         
       [51]   10: FUN                                 
       [52]    9: lapply                              
       [53]    3: test_files_serial                   
       [54]    2: test_files                          
       [55]    1: testthat::test_file                 
       [56] From earlier call:                        
       [57]   69: domain$wrapOnRejected               
       [58]   68: promiseDomain$onThen                
       [59]   67: action                              
       [60]   60: promise                             
       [61]   59: self$then                           
       [62]   58: promise$catch                       
       [63]   57: catch                               
       [64]   56: %...!%                              
       [65]   55: onFulfilled [test-stacks-deep.R#XXX]
       [66]   53: callback [conditions.R#XXX]         
       [67]   50: <Anonymous>                         
       [68]   49: onFulfilled                         
       [69]   39: handleFulfill                       
       [70]   38: <Anonymous>                         
       [71]   37: execCallbacks                       
       [72]   36: later::run_now                      
       [73]   35: wait_for_it [mock-session.R#XXX]    
       [74]   34: eval [test-stacks-deep.R#XXX]       
       [75]   33: eval                                
       [76]   24: test_code                           
       [77]   23: test_that                           
       [78]   22: eval [test-stacks-deep.R#XXX]       
       [79]   21: eval                                
       [80]   12: test_code                           
       [81]   11: source_file                         
       [82]   10: FUN                                 
       [83]    9: lapply                              
       [84]    3: test_files_serial                   
       [85]    2: test_files                          
       [86]    1: testthat::test_file                 
       [87] From earlier call:                        
       [88]   54: domain$wrapOnFulfilled              
       [89]   53: promiseDomain$onThen                
       [90]   52: action                              
       [91]   45: promise                             
       [92]   44: promise$then                        
       [93]   43: then                                
       [94]   42: %...>%                              
       [95]   34: eval [test-stacks-deep.R#XXX]       
       [96]   33: eval                                
       [97]   24: test_code                           
       [98]   23: test_that                           
       [99]   22: eval [test-stacks-deep.R#XXX]       
      [100]   21: eval                                
      [101]   12: test_code                           
      [102]   11: source_file                         
      [103]   10: FUN                                 
      [104]    9: lapply                              
      [105]    3: test_files_serial                   
      [106]    2: test_files                          
      [107]    1: testthat::test_file                 

---

    Code
      formatError(err, full = TRUE)
    Output
        [1] Error in onFinally: boom                          
        [2]   59: h                                           
        [3]   58: .handleSimpleError                          
        [4]   57: stop                                        
        [5]   56: onFinally [test-stacks-deep.R#XXX]          
        [6]   55: onFulfilled                                 
        [7]   54: withCallingHandlers                         
        [8]   53: callback [conditions.R#XXX]                 
        [9]   52: force                                       
       [10]   51: reenter_promise_domain                      
       [11]   50: <Anonymous>                                 
       [12]   49: onFulfilled                                 
       [13]   48: withVisible                                 
       [14]   47: private$doResolve                           
       [15]   46: withCallingHandlers                         
       [16]   45: doTryCatch                                  
       [17]   44: tryCatchOne                                 
       [18]   43: tryCatchList                                
       [19]   42: base::tryCatch                              
       [20]   41: tryCatch                                    
       [21]   40: resolve                                     
       [22]   39: handleFulfill                               
       [23]   38: <Anonymous>                                 
       [24]   37: execCallbacks                               
       [25]   36: later::run_now                              
       [26]   35: wait_for_it [mock-session.R#XXX]            
       [27]   34: eval [test-stacks-deep.R#XXX]               
       [28]   33: eval                                        
       [29]   32: withCallingHandlers                         
       [30]   31: doTryCatch                                  
       [31]   30: tryCatchOne                                 
       [32]   29: tryCatchList                                
       [33]   28: doTryCatch                                  
       [34]   27: tryCatchOne                                 
       [35]   26: tryCatchList                                
       [36]   25: tryCatch                                    
       [37]   24: test_code                                   
       [38]   23: test_that                                   
       [39]   22: eval [test-stacks-deep.R#XXX]               
       [40]   21: eval                                        
       [41]   20: withCallingHandlers                         
       [42]   19: doTryCatch                                  
       [43]   18: tryCatchOne                                 
       [44]   17: tryCatchList                                
       [45]   16: doTryCatch                                  
       [46]   15: tryCatchOne                                 
       [47]   14: tryCatchList                                
       [48]   13: tryCatch                                    
       [49]   12: test_code                                   
       [50]   11: source_file                                 
       [51]   10: FUN                                         
       [52]    9: lapply                                      
       [53]    8: doTryCatch                                  
       [54]    7: tryCatchOne                                 
       [55]    6: tryCatchList                                
       [56]    5: tryCatch                                    
       [57]    4: with_reporter                               
       [58]    3: test_files_serial                           
       [59]    2: test_files                                  
       [60]    1: testthat::test_file                         
       [61] From earlier call:                                
       [62]   68: domain$wrapOnFulfilled                      
       [63]   67: promiseDomain$onThen                        
       [64]   66: action                                      
       [65]   65: withCallingHandlers                         
       [66]   64: doTryCatch                                  
       [67]   63: tryCatchOne                                 
       [68]   62: tryCatchList                                
       [69]   61: base::tryCatch                              
       [70]   60: tryCatch                                    
       [71]   59: promise                                     
       [72]   58: self$then                                   
       [73]   57: promise$finally                             
       [74]   56: finally                                     
       [75]   55: onRejected [test-stacks-deep.R#XXX]         
       [76]   54: withCallingHandlers                         
       [77]   53: callback [conditions.R#XXX]                 
       [78]   52: force                                       
       [79]   51: reenter_promise_domain                      
       [80]   50: <Anonymous>                                 
       [81]   49: onRejected                                  
       [82]   48: withVisible                                 
       [83]   47: private$doResolve                           
       [84]   46: withCallingHandlers                         
       [85]   45: doTryCatch                                  
       [86]   44: tryCatchOne                                 
       [87]   43: tryCatchList                                
       [88]   42: base::tryCatch                              
       [89]   41: tryCatch                                    
       [90]   40: resolve                                     
       [91]   39: handleReject                                
       [92]   38: <Anonymous>                                 
       [93]   37: execCallbacks                               
       [94]   36: later::run_now                              
       [95]   35: wait_for_it [mock-session.R#XXX]            
       [96]   34: eval [test-stacks-deep.R#XXX]               
       [97]   33: eval                                        
       [98]   32: withCallingHandlers                         
       [99]   31: doTryCatch                                  
      [100]   30: tryCatchOne                                 
      [101]   29: tryCatchList                                
      [102]   28: doTryCatch                                  
      [103]   27: tryCatchOne                                 
      [104]   26: tryCatchList                                
      [105]   25: tryCatch                                    
      [106]   24: test_code                                   
      [107]   23: test_that                                   
      [108]   22: eval [test-stacks-deep.R#XXX]               
      [109]   21: eval                                        
      [110]   20: withCallingHandlers                         
      [111]   19: doTryCatch                                  
      [112]   18: tryCatchOne                                 
      [113]   17: tryCatchList                                
      [114]   16: doTryCatch                                  
      [115]   15: tryCatchOne                                 
      [116]   14: tryCatchList                                
      [117]   13: tryCatch                                    
      [118]   12: test_code                                   
      [119]   11: source_file                                 
      [120]   10: FUN                                         
      [121]    9: lapply                                      
      [122]    8: doTryCatch                                  
      [123]    7: tryCatchOne                                 
      [124]    6: tryCatchList                                
      [125]    5: tryCatch                                    
      [126]    4: with_reporter                               
      [127]    3: test_files_serial                           
      [128]    2: test_files                                  
      [129]    1: testthat::test_file                         
      [130] From earlier call:                                
      [131]   69: domain$wrapOnRejected                       
      [132]   68: promiseDomain$onThen                        
      [133]   67: action                                      
      [134]   66: withCallingHandlers                         
      [135]   65: doTryCatch                                  
      [136]   64: tryCatchOne                                 
      [137]   63: tryCatchList                                
      [138]   62: base::tryCatch                              
      [139]   61: tryCatch                                    
      [140]   60: promise                                     
      [141]   59: self$then                                   
      [142]   58: promise$catch                               
      [143]   57: catch                                       
      [144]   56: %...!%                                      
      [145]   55: onFulfilled [test-stacks-deep.R#XXX]        
      [146]   54: withCallingHandlers                         
      [147]   53: callback [conditions.R#XXX]                 
      [148]   52: force                                       
      [149]   51: reenter_promise_domain                      
      [150]   50: <Anonymous>                                 
      [151]   49: onFulfilled                                 
      [152]   48: withVisible                                 
      [153]   47: private$doResolve                           
      [154]   46: withCallingHandlers                         
      [155]   45: doTryCatch                                  
      [156]   44: tryCatchOne                                 
      [157]   43: tryCatchList                                
      [158]   42: base::tryCatch                              
      [159]   41: tryCatch                                    
      [160]   40: resolve                                     
      [161]   39: handleFulfill                               
      [162]   38: <Anonymous>                                 
      [163]   37: execCallbacks                               
      [164]   36: later::run_now                              
      [165]   35: wait_for_it [mock-session.R#XXX]            
      [166]   34: eval [test-stacks-deep.R#XXX]               
      [167]   33: eval                                        
      [168]   32: withCallingHandlers                         
      [169]   31: doTryCatch                                  
      [170]   30: tryCatchOne                                 
      [171]   29: tryCatchList                                
      [172]   28: doTryCatch                                  
      [173]   27: tryCatchOne                                 
      [174]   26: tryCatchList                                
      [175]   25: tryCatch                                    
      [176]   24: test_code                                   
      [177]   23: test_that                                   
      [178]   22: eval [test-stacks-deep.R#XXX]               
      [179]   21: eval                                        
      [180]   20: withCallingHandlers                         
      [181]   19: doTryCatch                                  
      [182]   18: tryCatchOne                                 
      [183]   17: tryCatchList                                
      [184]   16: doTryCatch                                  
      [185]   15: tryCatchOne                                 
      [186]   14: tryCatchList                                
      [187]   13: tryCatch                                    
      [188]   12: test_code                                   
      [189]   11: source_file                                 
      [190]   10: FUN                                         
      [191]    9: lapply                                      
      [192]    8: doTryCatch                                  
      [193]    7: tryCatchOne                                 
      [194]    6: tryCatchList                                
      [195]    5: tryCatch                                    
      [196]    4: with_reporter                               
      [197]    3: test_files_serial                           
      [198]    2: test_files                                  
      [199]    1: testthat::test_file                         
      [200] From earlier call:                                
      [201]   54: domain$wrapOnFulfilled                      
      [202]   53: promiseDomain$onThen                        
      [203]   52: action                                      
      [204]   51: withCallingHandlers                         
      [205]   50: doTryCatch                                  
      [206]   49: tryCatchOne                                 
      [207]   48: tryCatchList                                
      [208]   47: base::tryCatch                              
      [209]   46: tryCatch                                    
      [210]   45: promise                                     
      [211]   44: promise$then                                
      [212]   43: then                                        
      [213]   42: %...>%                                      
      [214]   41: withCallingHandlers [test-stacks-deep.R#XXX]
      [215]   40: domain$wrapSync [conditions.R#XXX]          
      [216]   39: promises::with_promise_domain               
      [217]   38: captureStackTraces [conditions.R#XXX]       
      [218]   37: as.promise                                  
      [219]   36: catch                                       
      [220]   35: %...!%                                      
      [221]   34: eval [test-stacks-deep.R#XXX]               
      [222]   33: eval                                        
      [223]   32: withCallingHandlers                         
      [224]   31: doTryCatch                                  
      [225]   30: tryCatchOne                                 
      [226]   29: tryCatchList                                
      [227]   28: doTryCatch                                  
      [228]   27: tryCatchOne                                 
      [229]   26: tryCatchList                                
      [230]   25: tryCatch                                    
      [231]   24: test_code                                   
      [232]   23: test_that                                   
      [233]   22: eval [test-stacks-deep.R#XXX]               
      [234]   21: eval                                        
      [235]   20: withCallingHandlers                         
      [236]   19: doTryCatch                                  
      [237]   18: tryCatchOne                                 
      [238]   17: tryCatchList                                
      [239]   16: doTryCatch                                  
      [240]   15: tryCatchOne                                 
      [241]   14: tryCatchList                                
      [242]   13: tryCatch                                    
      [243]   12: test_code                                   
      [244]   11: source_file                                 
      [245]   10: FUN                                         
      [246]    9: lapply                                      
      [247]    8: doTryCatch                                  
      [248]    7: tryCatchOne                                 
      [249]    6: tryCatchList                                
      [250]    5: tryCatch                                    
      [251]    4: with_reporter                               
      [252]    3: test_files_serial                           
      [253]    2: test_files                                  
      [254]    1: testthat::test_file                         

# deep stack culling

    Code
      stacktrace
    Output
        [1] Error in onFulfilled: boom                                                  
        [2]   56: stop                                                                  
        [3]   55: onFulfilled [test-stacks-deep.R#XXX]                                  
        [4]   53: callback [conditions.R#XXX]                                           
        [5]   50: <Anonymous>                                                           
        [6]   49: onFulfilled                                                           
        [7]   39: handleFulfill                                                         
        [8]   38: <Anonymous>                                                           
        [9]   37: execCallbacks                                                         
       [10]   36: later::run_now                                                        
       [11]   35: wait_for_it [mock-session.R#XXX]                                      
       [12]   34: eval [test-stacks-deep.R#XXX]                                         
       [13]   33: eval                                                                  
       [14]   24: test_code                                                             
       [15]   23: test_that                                                             
       [16]   22: eval [test-stacks-deep.R#XXX]                                         
       [17]   21: eval                                                                  
       [18]   12: test_code                                                             
       [19]   11: source_file                                                           
       [20]   10: FUN                                                                   
       [21]    9: lapply                                                                
       [22]    3: test_files_serial                                                     
       [23]    2: test_files                                                            
       [24]    1: testthat::test_file                                                   
       [25] From earlier call:                                                          
       [26]   69: domain$wrapOnFulfilled                                                
       [27]   68: promiseDomain$onThen                                                  
       [28]   67: action                                                                
       [29]   60: promise                                                               
       [30]   59: promise$then                                                          
       [31]   58: then                                                                  
       [32]   57: %...>%                                                                
       [33]   56: J__ [test-stacks-deep.R#XXX]                                          
       [34]   55: onFulfilled                                                           
       [35]   53: callback [conditions.R#XXX]                                           
       [36]   50: <Anonymous>                                                           
       [37]   49: onFulfilled                                                           
       [38]   39: handleFulfill                                                         
       [39]   38: <Anonymous>                                                           
       [40]   37: execCallbacks                                                         
       [41]   36: later::run_now                                                        
       [42]   35: wait_for_it [mock-session.R#XXX]                                      
       [43]   34: eval [test-stacks-deep.R#XXX]                                         
       [44]   33: eval                                                                  
       [45]   24: test_code                                                             
       [46]   23: test_that                                                             
       [47]   22: eval [test-stacks-deep.R#XXX]                                         
       [48]   21: eval                                                                  
       [49]   12: test_code                                                             
       [50]   11: source_file                                                           
       [51]   10: FUN                                                                   
       [52]    9: lapply                                                                
       [53]    3: test_files_serial                                                     
       [54]    2: test_files                                                            
       [55]    1: testthat::test_file                                                   
       [56] From earlier call:                                                          
       [57]   69: domain$wrapOnFulfilled                                                
       [58]   68: promiseDomain$onThen                                                  
       [59]   67: action                                                                
       [60]   60: promise                                                               
       [61]   59: promise$then                                                          
       [62]   58: then                                                                  
       [63]   57: %...>%                                                                
       [64]   56: I__ [test-stacks-deep.R#XXX]                                          
       [65]   55: onFulfilled                                                           
       [66]   53: callback [conditions.R#XXX]                                           
       [67]   50: <Anonymous>                                                           
       [68]   49: onFulfilled                                                           
       [69]   39: handleFulfill                                                         
       [70]   38: <Anonymous>                                                           
       [71]   37: execCallbacks                                                         
       [72]   36: later::run_now                                                        
       [73]   35: wait_for_it [mock-session.R#XXX]                                      
       [74]   34: eval [test-stacks-deep.R#XXX]                                         
       [75]   33: eval                                                                  
       [76]   24: test_code                                                             
       [77]   23: test_that                                                             
       [78]   22: eval [test-stacks-deep.R#XXX]                                         
       [79]   21: eval                                                                  
       [80]   12: test_code                                                             
       [81]   11: source_file                                                           
       [82]   10: FUN                                                                   
       [83]    9: lapply                                                                
       [84]    3: test_files_serial                                                     
       [85]    2: test_files                                                            
       [86]    1: testthat::test_file                                                   
       [87] [ reached getOption("shiny.deepstacktrace") -- omitted 7 more stack traces ]
       [88] From earlier call:                                                          
       [89]   55: domain$wrapOnFulfilled                                                
       [90]   54: promiseDomain$onThen                                                  
       [91]   53: action                                                                
       [92]   46: promise                                                               
       [93]   45: promise$then                                                          
       [94]   44: then                                                                  
       [95]   43: %...>%                                                                
       [96]   42: A__ [test-stacks-deep.R#XXX]                                          
       [97]   34: eval [test-stacks-deep.R#XXX]                                         
       [98]   33: eval                                                                  
       [99]   24: test_code                                                             
      [100]   23: test_that                                                             
      [101]   22: eval [test-stacks-deep.R#XXX]                                         
      [102]   21: eval                                                                  
      [103]   12: test_code                                                             
      [104]   11: source_file                                                           
      [105]   10: FUN                                                                   
      [106]    9: lapply                                                                
      [107]    3: test_files_serial                                                     
      [108]    2: test_files                                                            
      [109]    1: testthat::test_file                                                   

