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
      3   65                                     stop                    
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

