# deep stack capturing

    Code
      formatError(err)
    Output
        [1] Error in onFinally: boom                  
        [2]     : stop                                
        [3]     : onFinally [test-stacks-deep.R#XXX]  
        [4]     : onFulfilled                         
        [5]     : callback [conditions.R#XXX]         
        [6]     : <Anonymous>                         
        [7]     : onFulfilled                         
        [8]     : handleFulfill                       
        [9]     : <Anonymous>                         
       [10]     : execCallbacks                       
       [11]     : later::run_now                      
       [12]     : wait_for_it [mock-session.R#XXX]    
       [13]     : eval [test-stacks-deep.R#XXX]       
       [14]     : eval                                
       [15]     : test_code                           
       [16]     : test_that                           
       [17]     : eval [test-stacks-deep.R#XXX]       
       [18]     : eval                                
       [19]     : test_code                           
       [20]     : source_file                         
       [21]     : FUN                                 
       [22]     : lapply                              
       [23]     : test_files_serial                   
       [24]     : test_files                          
       [25] From earlier call:                        
       [26]     : domain$wrapOnFulfilled              
       [27]     : promiseDomain$onThen                
       [28]     : action                              
       [29]     : promise                             
       [30]     : self$then                           
       [31]     : promise$finally                     
       [32]     : finally                             
       [33]     : onRejected [test-stacks-deep.R#XXX] 
       [34]     : callback [conditions.R#XXX]         
       [35]     : <Anonymous>                         
       [36]     : onRejected                          
       [37]     : handleReject                        
       [38]     : <Anonymous>                         
       [39]     : execCallbacks                       
       [40]     : later::run_now                      
       [41]     : wait_for_it [mock-session.R#XXX]    
       [42]     : eval [test-stacks-deep.R#XXX]       
       [43]     : eval                                
       [44]     : test_code                           
       [45]     : test_that                           
       [46]     : eval [test-stacks-deep.R#XXX]       
       [47]     : eval                                
       [48]     : test_code                           
       [49]     : source_file                         
       [50]     : FUN                                 
       [51]     : lapply                              
       [52]     : test_files_serial                   
       [53]     : test_files                          
       [54] From earlier call:                        
       [55]     : domain$wrapOnRejected               
       [56]     : promiseDomain$onThen                
       [57]     : action                              
       [58]     : promise                             
       [59]     : self$then                           
       [60]     : promise$catch                       
       [61]     : catch                               
       [62]     : %...!%                              
       [63]     : onFulfilled [test-stacks-deep.R#XXX]
       [64]     : callback [conditions.R#XXX]         
       [65]     : <Anonymous>                         
       [66]     : onFulfilled                         
       [67]     : handleFulfill                       
       [68]     : <Anonymous>                         
       [69]     : execCallbacks                       
       [70]     : later::run_now                      
       [71]     : wait_for_it [mock-session.R#XXX]    
       [72]     : eval [test-stacks-deep.R#XXX]       
       [73]     : eval                                
       [74]     : test_code                           
       [75]     : test_that                           
       [76]     : eval [test-stacks-deep.R#XXX]       
       [77]     : eval                                
       [78]     : test_code                           
       [79]     : source_file                         
       [80]     : FUN                                 
       [81]     : lapply                              
       [82]     : test_files_serial                   
       [83]     : test_files                          
       [84] From earlier call:                        
       [85]     : domain$wrapOnFulfilled              
       [86]     : promiseDomain$onThen                
       [87]     : action                              
       [88]     : promise                             
       [89]     : promise$then                        
       [90]     : then                                
       [91]     : %...>%                              
       [92]     : eval [test-stacks-deep.R#XXX]       
       [93]     : eval                                
       [94]     : test_code                           
       [95]     : test_that                           
       [96]     : eval [test-stacks-deep.R#XXX]       
       [97]     : eval                                
       [98]     : test_code                           
       [99]     : source_file                         
      [100]     : FUN                                 
      [101]     : lapply                              
      [102]     : test_files_serial                   
      [103]     : test_files                          

---

    Code
      formatError(err, full = TRUE)
    Output
        [1] Error in onFinally: boom                          
        [2]     : h                                           
        [3]     : .handleSimpleError                          
        [4]     : stop                                        
        [5]     : onFinally [test-stacks-deep.R#XXX]          
        [6]     : onFulfilled                                 
        [7]     : withCallingHandlers                         
        [8]     : callback [conditions.R#XXX]                 
        [9]     : force                                       
       [10]     : reenter_promise_domain                      
       [11]     : <Anonymous>                                 
       [12]     : onFulfilled                                 
       [13]     : withVisible                                 
       [14]     : private$doResolve                           
       [15]     : withCallingHandlers                         
       [16]     : doTryCatch                                  
       [17]     : tryCatchOne                                 
       [18]     : tryCatchList                                
       [19]     : base::tryCatch                              
       [20]     : tryCatch                                    
       [21]     : resolve                                     
       [22]     : handleFulfill                               
       [23]     : <Anonymous>                                 
       [24]     : execCallbacks                               
       [25]     : later::run_now                              
       [26]     : wait_for_it [mock-session.R#XXX]            
       [27]     : eval [test-stacks-deep.R#XXX]               
       [28]     : eval                                        
       [29]     : withCallingHandlers                         
       [30]     : doTryCatch                                  
       [31]     : tryCatchOne                                 
       [32]     : tryCatchList                                
       [33]     : doTryCatch                                  
       [34]     : tryCatchOne                                 
       [35]     : tryCatchList                                
       [36]     : tryCatch                                    
       [37]     : test_code                                   
       [38]     : test_that                                   
       [39]     : eval [test-stacks-deep.R#XXX]               
       [40]     : eval                                        
       [41]     : withCallingHandlers                         
       [42]     : doTryCatch                                  
       [43]     : tryCatchOne                                 
       [44]     : tryCatchList                                
       [45]     : doTryCatch                                  
       [46]     : tryCatchOne                                 
       [47]     : tryCatchList                                
       [48]     : tryCatch                                    
       [49]     : test_code                                   
       [50]     : source_file                                 
       [51]     : FUN                                         
       [52]     : lapply                                      
       [53]     : doTryCatch                                  
       [54]     : tryCatchOne                                 
       [55]     : tryCatchList                                
       [56]     : tryCatch                                    
       [57]     : with_reporter                               
       [58]     : test_files_serial                           
       [59]     : test_files                                  
       [60] From earlier call:                                
       [61]     : domain$wrapOnFulfilled                      
       [62]     : promiseDomain$onThen                        
       [63]     : action                                      
       [64]     : withCallingHandlers                         
       [65]     : doTryCatch                                  
       [66]     : tryCatchOne                                 
       [67]     : tryCatchList                                
       [68]     : base::tryCatch                              
       [69]     : tryCatch                                    
       [70]     : promise                                     
       [71]     : self$then                                   
       [72]     : promise$finally                             
       [73]     : finally                                     
       [74]     : onRejected [test-stacks-deep.R#XXX]         
       [75]     : withCallingHandlers                         
       [76]     : callback [conditions.R#XXX]                 
       [77]     : force                                       
       [78]     : reenter_promise_domain                      
       [79]     : <Anonymous>                                 
       [80]     : onRejected                                  
       [81]     : withVisible                                 
       [82]     : private$doResolve                           
       [83]     : withCallingHandlers                         
       [84]     : doTryCatch                                  
       [85]     : tryCatchOne                                 
       [86]     : tryCatchList                                
       [87]     : base::tryCatch                              
       [88]     : tryCatch                                    
       [89]     : resolve                                     
       [90]     : handleReject                                
       [91]     : <Anonymous>                                 
       [92]     : execCallbacks                               
       [93]     : later::run_now                              
       [94]     : wait_for_it [mock-session.R#XXX]            
       [95]     : eval [test-stacks-deep.R#XXX]               
       [96]     : eval                                        
       [97]     : withCallingHandlers                         
       [98]     : doTryCatch                                  
       [99]     : tryCatchOne                                 
      [100]     : tryCatchList                                
      [101]     : doTryCatch                                  
      [102]     : tryCatchOne                                 
      [103]     : tryCatchList                                
      [104]     : tryCatch                                    
      [105]     : test_code                                   
      [106]     : test_that                                   
      [107]     : eval [test-stacks-deep.R#XXX]               
      [108]     : eval                                        
      [109]     : withCallingHandlers                         
      [110]     : doTryCatch                                  
      [111]     : tryCatchOne                                 
      [112]     : tryCatchList                                
      [113]     : doTryCatch                                  
      [114]     : tryCatchOne                                 
      [115]     : tryCatchList                                
      [116]     : tryCatch                                    
      [117]     : test_code                                   
      [118]     : source_file                                 
      [119]     : FUN                                         
      [120]     : lapply                                      
      [121]     : doTryCatch                                  
      [122]     : tryCatchOne                                 
      [123]     : tryCatchList                                
      [124]     : tryCatch                                    
      [125]     : with_reporter                               
      [126]     : test_files_serial                           
      [127]     : test_files                                  
      [128] From earlier call:                                
      [129]     : domain$wrapOnRejected                       
      [130]     : promiseDomain$onThen                        
      [131]     : action                                      
      [132]     : withCallingHandlers                         
      [133]     : doTryCatch                                  
      [134]     : tryCatchOne                                 
      [135]     : tryCatchList                                
      [136]     : base::tryCatch                              
      [137]     : tryCatch                                    
      [138]     : promise                                     
      [139]     : self$then                                   
      [140]     : promise$catch                               
      [141]     : catch                                       
      [142]     : %...!%                                      
      [143]     : onFulfilled [test-stacks-deep.R#XXX]        
      [144]     : withCallingHandlers                         
      [145]     : callback [conditions.R#XXX]                 
      [146]     : force                                       
      [147]     : reenter_promise_domain                      
      [148]     : <Anonymous>                                 
      [149]     : onFulfilled                                 
      [150]     : withVisible                                 
      [151]     : private$doResolve                           
      [152]     : withCallingHandlers                         
      [153]     : doTryCatch                                  
      [154]     : tryCatchOne                                 
      [155]     : tryCatchList                                
      [156]     : base::tryCatch                              
      [157]     : tryCatch                                    
      [158]     : resolve                                     
      [159]     : handleFulfill                               
      [160]     : <Anonymous>                                 
      [161]     : execCallbacks                               
      [162]     : later::run_now                              
      [163]     : wait_for_it [mock-session.R#XXX]            
      [164]     : eval [test-stacks-deep.R#XXX]               
      [165]     : eval                                        
      [166]     : withCallingHandlers                         
      [167]     : doTryCatch                                  
      [168]     : tryCatchOne                                 
      [169]     : tryCatchList                                
      [170]     : doTryCatch                                  
      [171]     : tryCatchOne                                 
      [172]     : tryCatchList                                
      [173]     : tryCatch                                    
      [174]     : test_code                                   
      [175]     : test_that                                   
      [176]     : eval [test-stacks-deep.R#XXX]               
      [177]     : eval                                        
      [178]     : withCallingHandlers                         
      [179]     : doTryCatch                                  
      [180]     : tryCatchOne                                 
      [181]     : tryCatchList                                
      [182]     : doTryCatch                                  
      [183]     : tryCatchOne                                 
      [184]     : tryCatchList                                
      [185]     : tryCatch                                    
      [186]     : test_code                                   
      [187]     : source_file                                 
      [188]     : FUN                                         
      [189]     : lapply                                      
      [190]     : doTryCatch                                  
      [191]     : tryCatchOne                                 
      [192]     : tryCatchList                                
      [193]     : tryCatch                                    
      [194]     : with_reporter                               
      [195]     : test_files_serial                           
      [196]     : test_files                                  
      [197] From earlier call:                                
      [198]     : domain$wrapOnFulfilled                      
      [199]     : promiseDomain$onThen                        
      [200]     : action                                      
      [201]     : withCallingHandlers                         
      [202]     : doTryCatch                                  
      [203]     : tryCatchOne                                 
      [204]     : tryCatchList                                
      [205]     : base::tryCatch                              
      [206]     : tryCatch                                    
      [207]     : promise                                     
      [208]     : promise$then                                
      [209]     : then                                        
      [210]     : %...>%                                      
      [211]     : withCallingHandlers [test-stacks-deep.R#XXX]
      [212]     : domain$wrapSync [conditions.R#XXX]          
      [213]     : promises::with_promise_domain               
      [214]     : captureStackTraces [conditions.R#XXX]       
      [215]     : as.promise                                  
      [216]     : catch                                       
      [217]     : %...!%                                      
      [218]     : eval [test-stacks-deep.R#XXX]               
      [219]     : eval                                        
      [220]     : withCallingHandlers                         
      [221]     : doTryCatch                                  
      [222]     : tryCatchOne                                 
      [223]     : tryCatchList                                
      [224]     : doTryCatch                                  
      [225]     : tryCatchOne                                 
      [226]     : tryCatchList                                
      [227]     : tryCatch                                    
      [228]     : test_code                                   
      [229]     : test_that                                   
      [230]     : eval [test-stacks-deep.R#XXX]               
      [231]     : eval                                        
      [232]     : withCallingHandlers                         
      [233]     : doTryCatch                                  
      [234]     : tryCatchOne                                 
      [235]     : tryCatchList                                
      [236]     : doTryCatch                                  
      [237]     : tryCatchOne                                 
      [238]     : tryCatchList                                
      [239]     : tryCatch                                    
      [240]     : test_code                                   
      [241]     : source_file                                 
      [242]     : FUN                                         
      [243]     : lapply                                      
      [244]     : doTryCatch                                  
      [245]     : tryCatchOne                                 
      [246]     : tryCatchList                                
      [247]     : tryCatch                                    
      [248]     : with_reporter                               
      [249]     : test_files_serial                           
      [250]     : test_files                                  

# deep stack culling

    Code
      stacktrace
    Output
        [1] Error in onFulfilled: boom                                                  
        [2]     : stop                                                                  
        [3]     : onFulfilled [test-stacks-deep.R#XXX]                                  
        [4]     : callback [conditions.R#XXX]                                           
        [5]     : <Anonymous>                                                           
        [6]     : onFulfilled                                                           
        [7]     : handleFulfill                                                         
        [8]     : <Anonymous>                                                           
        [9]     : execCallbacks                                                         
       [10]     : later::run_now                                                        
       [11]     : wait_for_it [mock-session.R#XXX]                                      
       [12]     : eval [test-stacks-deep.R#XXX]                                         
       [13]     : eval                                                                  
       [14]     : test_code                                                             
       [15]     : test_that                                                             
       [16]     : eval [test-stacks-deep.R#XXX]                                         
       [17]     : eval                                                                  
       [18]     : test_code                                                             
       [19]     : source_file                                                           
       [20]     : FUN                                                                   
       [21]     : lapply                                                                
       [22]     : test_files_serial                                                     
       [23]     : test_files                                                            
       [24] From earlier call:                                                          
       [25]     : domain$wrapOnFulfilled                                                
       [26]     : promiseDomain$onThen                                                  
       [27]     : action                                                                
       [28]     : promise                                                               
       [29]     : promise$then                                                          
       [30]     : then                                                                  
       [31]     : %...>%                                                                
       [32]     : J__ [test-stacks-deep.R#XXX]                                          
       [33]     : onFulfilled                                                           
       [34]     : callback [conditions.R#XXX]                                           
       [35]     : <Anonymous>                                                           
       [36]     : onFulfilled                                                           
       [37]     : handleFulfill                                                         
       [38]     : <Anonymous>                                                           
       [39]     : execCallbacks                                                         
       [40]     : later::run_now                                                        
       [41]     : wait_for_it [mock-session.R#XXX]                                      
       [42]     : eval [test-stacks-deep.R#XXX]                                         
       [43]     : eval                                                                  
       [44]     : test_code                                                             
       [45]     : test_that                                                             
       [46]     : eval [test-stacks-deep.R#XXX]                                         
       [47]     : eval                                                                  
       [48]     : test_code                                                             
       [49]     : source_file                                                           
       [50]     : FUN                                                                   
       [51]     : lapply                                                                
       [52]     : test_files_serial                                                     
       [53]     : test_files                                                            
       [54] From earlier call:                                                          
       [55]     : domain$wrapOnFulfilled                                                
       [56]     : promiseDomain$onThen                                                  
       [57]     : action                                                                
       [58]     : promise                                                               
       [59]     : promise$then                                                          
       [60]     : then                                                                  
       [61]     : %...>%                                                                
       [62]     : I__ [test-stacks-deep.R#XXX]                                          
       [63]     : onFulfilled                                                           
       [64]     : callback [conditions.R#XXX]                                           
       [65]     : <Anonymous>                                                           
       [66]     : onFulfilled                                                           
       [67]     : handleFulfill                                                         
       [68]     : <Anonymous>                                                           
       [69]     : execCallbacks                                                         
       [70]     : later::run_now                                                        
       [71]     : wait_for_it [mock-session.R#XXX]                                      
       [72]     : eval [test-stacks-deep.R#XXX]                                         
       [73]     : eval                                                                  
       [74]     : test_code                                                             
       [75]     : test_that                                                             
       [76]     : eval [test-stacks-deep.R#XXX]                                         
       [77]     : eval                                                                  
       [78]     : test_code                                                             
       [79]     : source_file                                                           
       [80]     : FUN                                                                   
       [81]     : lapply                                                                
       [82]     : test_files_serial                                                     
       [83]     : test_files                                                            
       [84] [ reached getOption("shiny.deepstacktrace") -- omitted 7 more stack traces ]
       [85] From earlier call:                                                          
       [86]     : domain$wrapOnFulfilled                                                
       [87]     : promiseDomain$onThen                                                  
       [88]     : action                                                                
       [89]     : promise                                                               
       [90]     : promise$then                                                          
       [91]     : then                                                                  
       [92]     : %...>%                                                                
       [93]     : A__ [test-stacks-deep.R#XXX]                                          
       [94]     : eval [test-stacks-deep.R#XXX]                                         
       [95]     : eval                                                                  
       [96]     : test_code                                                             
       [97]     : test_that                                                             
       [98]     : eval [test-stacks-deep.R#XXX]                                         
       [99]     : eval                                                                  
      [100]     : test_code                                                             
      [101]     : source_file                                                           
      [102]     : FUN                                                                   
      [103]     : lapply                                                                
      [104]     : test_files_serial                                                     
      [105]     : test_files                                                            

