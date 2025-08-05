# integration tests

    Code
      df
    Output
        num                call                 loc
      1  64                   A   [test-stacks.R#3]
      2  63                   B   [test-stacks.R#7]
      3  62        <reactive:C>  [test-stacks.R#11]
      4  42                   C                    
      5  41         renderTable  [test-stacks.R#18]
      6  40                func                    
      7  39               force                    
      8  38         withVisible                    
      9  37 withCallingHandlers                    

---

    Code
      df
    Output
         num                                     call                 loc
      1   67                                        h                    
      2   66                       .handleSimpleError                    
      3   65                                        f                    
      4   64                                        A   [test-stacks.R#3]
      5   63                                        B   [test-stacks.R#7]
      6   62                             <reactive:C>  [test-stacks.R#11]
      7   61                         ..stacktraceon..                    
      8   60                                    .func                    
      9   59                              withVisible                    
      10  58                      withCallingHandlers                    
      11  57                              contextFunc                    
      12  56                              env$runWith                    
      13  55                      withCallingHandlers                    
      14  54                          domain$wrapSync                    
      15  53            promises::with_promise_domain                    
      16  52                       captureStackTraces                    
      17  51                                    force                    
      18  50                          domain$wrapSync                    
      19  49            promises::with_promise_domain                    
      20  48                       withReactiveDomain                    
      21  47                          domain$wrapSync                    
      22  46            promises::with_promise_domain                    
      23  45                                  ctx$run                    
      24  44                        self$.updateValue                    
      25  43                        ..stacktraceoff..                    
      26  42                                        C                    
      27  41                              renderTable  [test-stacks.R#18]
      28  40                                     func                    
      29  39                                    force                    
      30  38                              withVisible                    
      31  37                      withCallingHandlers                    
      32  36                          domain$wrapSync                    
      33  35            promises::with_promise_domain                    
      34  34                       captureStackTraces                    
      35  33                               doTryCatch                    
      36  32                              tryCatchOne                    
      37  31                             tryCatchList                    
      38  30                                 tryCatch                    
      39  29                                       do                    
      40  28                             hybrid_chain                    
      41  27                               renderFunc                    
      42  26 renderTable({     C() }, server = FALSE)                    
      43  25                         ..stacktraceon..  [test-stacks.R#17]
      44  24                              contextFunc                    
      45  23                              env$runWith                    
      46  22                      withCallingHandlers                    
      47  21                          domain$wrapSync                    
      48  20            promises::with_promise_domain                    
      49  19                       captureStackTraces                    
      50  18                                    force                    
      51  17                          domain$wrapSync                    
      52  16            promises::with_promise_domain                    
      53  15                       withReactiveDomain                    
      54  14                          domain$wrapSync                    
      55  13            promises::with_promise_domain                    
      56  12                                  ctx$run                    
      57  11                        ..stacktraceoff..                    
      58  10                                  isolate                    
      59   9                      withCallingHandlers  [test-stacks.R#16]
      60   8                          domain$wrapSync                    
      61   7            promises::with_promise_domain                    
      62   6                       captureStackTraces                    
      63   5                               doTryCatch  [test-stacks.R#15]
      64   4                              tryCatchOne                    
      65   3                             tryCatchList                    
      66   2                                 tryCatch                    
      67   1                                      try                    

---

    Code
      df
    Output
         num                call                 loc
      1   69         <Anonymous>                    
      2   68     signalCondition                    
      3   67        signal_abort                    
      4   66        rlang::abort                    
      5   65                   f                    
      6   64                   A   [test-stacks.R#3]
      7   63                   B   [test-stacks.R#7]
      8   62        <reactive:C>  [test-stacks.R#11]
      9   42                   C                    
      10  41         renderTable  [test-stacks.R#18]
      11  40                func                    
      12  39               force                    
      13  38         withVisible                    
      14  37 withCallingHandlers                    

---

    Code
      df
    Output
         num                                     call                 loc
      1   69                              <Anonymous>                    
      2   68                          signalCondition                    
      3   67                             signal_abort                    
      4   66                             rlang::abort                    
      5   65                                        f                    
      6   64                                        A   [test-stacks.R#3]
      7   63                                        B   [test-stacks.R#7]
      8   62                             <reactive:C>  [test-stacks.R#11]
      9   61                         ..stacktraceon..                    
      10  60                                    .func                    
      11  59                              withVisible                    
      12  58                      withCallingHandlers                    
      13  57                              contextFunc                    
      14  56                              env$runWith                    
      15  55                      withCallingHandlers                    
      16  54                          domain$wrapSync                    
      17  53            promises::with_promise_domain                    
      18  52                       captureStackTraces                    
      19  51                                    force                    
      20  50                          domain$wrapSync                    
      21  49            promises::with_promise_domain                    
      22  48                       withReactiveDomain                    
      23  47                          domain$wrapSync                    
      24  46            promises::with_promise_domain                    
      25  45                                  ctx$run                    
      26  44                        self$.updateValue                    
      27  43                        ..stacktraceoff..                    
      28  42                                        C                    
      29  41                              renderTable  [test-stacks.R#18]
      30  40                                     func                    
      31  39                                    force                    
      32  38                              withVisible                    
      33  37                      withCallingHandlers                    
      34  36                          domain$wrapSync                    
      35  35            promises::with_promise_domain                    
      36  34                       captureStackTraces                    
      37  33                               doTryCatch                    
      38  32                              tryCatchOne                    
      39  31                             tryCatchList                    
      40  30                                 tryCatch                    
      41  29                                       do                    
      42  28                             hybrid_chain                    
      43  27                               renderFunc                    
      44  26 renderTable({     C() }, server = FALSE)                    
      45  25                         ..stacktraceon..  [test-stacks.R#17]
      46  24                              contextFunc                    
      47  23                              env$runWith                    
      48  22                      withCallingHandlers                    
      49  21                          domain$wrapSync                    
      50  20            promises::with_promise_domain                    
      51  19                       captureStackTraces                    
      52  18                                    force                    
      53  17                          domain$wrapSync                    
      54  16            promises::with_promise_domain                    
      55  15                       withReactiveDomain                    
      56  14                          domain$wrapSync                    
      57  13            promises::with_promise_domain                    
      58  12                                  ctx$run                    
      59  11                        ..stacktraceoff..                    
      60  10                                  isolate                    
      61   9                      withCallingHandlers  [test-stacks.R#16]
      62   8                          domain$wrapSync                    
      63   7            promises::with_promise_domain                    
      64   6                       captureStackTraces                    
      65   5                               doTryCatch  [test-stacks.R#15]
      66   4                              tryCatchOne                    
      67   3                             tryCatchList                    
      68   2                                 tryCatch                    
      69   1                                      try                    

