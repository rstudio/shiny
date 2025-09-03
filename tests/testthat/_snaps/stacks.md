# integration tests

    Code
      df
    Output
         num                          call                 loc
      1   68                             A   [test-stacks.R#3]
      2   67                             B   [test-stacks.R#7]
      3   66                  <reactive:C>  [test-stacks.R#11]
      4   44                             C                    
      5   43                   renderTable  [test-stacks.R#18]
      6   42                          func                    
      7   41                         force                    
      8   40                   withVisible                    
      9   39           withCallingHandlers                    
      10  38               domain$wrapSync                    
      11  37 promises::with_promise_domain                    
      12  36            captureStackTraces                    
      13  32                      tryCatch                    
      14  31                            do                    
      15  30                  hybrid_chain                    

---

    Code
      df
    Output
         num                                     call                 loc
      1   71                                        h                    
      2   70                       .handleSimpleError                    
      3   69                                     stop                    
      4   68                                        A   [test-stacks.R#3]
      5   67                                        B   [test-stacks.R#7]
      6   66                             <reactive:C>  [test-stacks.R#11]
      7   65                         ..stacktraceon..                    
      8   64                                    .func                    
      9   63                              withVisible                    
      10  62                      withCallingHandlers                    
      11  61                              contextFunc                    
      12  60                              env$runWith                    
      13  59                      withCallingHandlers                    
      14  58                          domain$wrapSync                    
      15  57            promises::with_promise_domain                    
      16  56                       captureStackTraces                    
      17  55                                    force                    
      18  54         with_reactive_update_ospan_async                    
      19  53                                    force                    
      20  52                          domain$wrapSync                    
      21  51            promises::with_promise_domain                    
      22  50                       withReactiveDomain                    
      23  49                          domain$wrapSync                    
      24  48            promises::with_promise_domain                    
      25  47                                  ctx$run                    
      26  46                        self$.updateValue                    
      27  45                        ..stacktraceoff..                    
      28  44                                        C                    
      29  43                              renderTable  [test-stacks.R#18]
      30  42                                     func                    
      31  41                                    force                    
      32  40                              withVisible                    
      33  39                      withCallingHandlers                    
      34  38                          domain$wrapSync                    
      35  37            promises::with_promise_domain                    
      36  36                       captureStackTraces                    
      37  35                               doTryCatch                    
      38  34                              tryCatchOne                    
      39  33                             tryCatchList                    
      40  32                                 tryCatch                    
      41  31                                       do                    
      42  30                             hybrid_chain                    
      43  29                               renderFunc                    
      44  28 renderTable({     C() }, server = FALSE)                    
      45  27                         ..stacktraceon..  [test-stacks.R#17]
      46  26                              contextFunc                    
      47  25                              env$runWith                    
      48  24                      withCallingHandlers                    
      49  23                          domain$wrapSync                    
      50  22            promises::with_promise_domain                    
      51  21                       captureStackTraces                    
      52  20                                    force                    
      53  19         with_reactive_update_ospan_async                    
      54  18                                    force                    
      55  17                          domain$wrapSync                    
      56  16            promises::with_promise_domain                    
      57  15                       withReactiveDomain                    
      58  14                          domain$wrapSync                    
      59  13            promises::with_promise_domain                    
      60  12                                  ctx$run                    
      61  11                        ..stacktraceoff..                    
      62  10                                  isolate                    
      63   9                      withCallingHandlers  [test-stacks.R#16]
      64   8                          domain$wrapSync                    
      65   7            promises::with_promise_domain                    
      66   6                       captureStackTraces                    
      67   5                               doTryCatch  [test-stacks.R#15]
      68   4                              tryCatchOne                    
      69   3                             tryCatchList                    
      70   2                                 tryCatch                    
      71   1                                      try                    

