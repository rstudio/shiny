# integration tests

    Code
      df
    Output
        num                call                 loc
      1  70                   A   [test-stacks.R#3]
      2  69                   B   [test-stacks.R#7]
      3  68        <reactive:C>  [test-stacks.R#11]
      4  45                   C                    
      5  44         renderTable  [test-stacks.R#18]
      6  43                func                    
      7  42               force                    
      8  41         withVisible                    
      9  40 withCallingHandlers                    

---

    Code
      df
    Output
         num                                     call                 loc
      1   73                                        h                    
      2   72                       .handleSimpleError                    
      3   71                                     stop                    
      4   70                                        A   [test-stacks.R#3]
      5   69                                        B   [test-stacks.R#7]
      6   68                             <reactive:C>  [test-stacks.R#11]
      7   67                         ..stacktraceon..                    
      8   66                                    .func                    
      9   65                              withVisible                    
      10  64                      withCallingHandlers                    
      11  63                              contextFunc                    
      12  62                              env$runWith                    
      13  61                      withCallingHandlers                    
      14  60                          domain$wrapSync                    
      15  59            promises::with_promise_domain                    
      16  58                       captureStackTraces                    
      17  57                                    force                    
      18  56                with_existing_ospan_async                    
      19  55         with_reactive_update_ospan_async                    
      20  54                                    force                    
      21  53                          domain$wrapSync                    
      22  52            promises::with_promise_domain                    
      23  51                       withReactiveDomain                    
      24  50                          domain$wrapSync                    
      25  49            promises::with_promise_domain                    
      26  48                                  ctx$run                    
      27  47                        self$.updateValue                    
      28  46                        ..stacktraceoff..                    
      29  45                                        C                    
      30  44                              renderTable  [test-stacks.R#18]
      31  43                                     func                    
      32  42                                    force                    
      33  41                              withVisible                    
      34  40                      withCallingHandlers                    
      35  39                          domain$wrapSync                    
      36  38            promises::with_promise_domain                    
      37  37                       captureStackTraces                    
      38  36                               doTryCatch                    
      39  35                              tryCatchOne                    
      40  34                             tryCatchList                    
      41  33                                 tryCatch                    
      42  32                                       do                    
      43  31                             hybrid_chain                    
      44  30                               renderFunc                    
      45  29 renderTable({     C() }, server = FALSE)                    
      46  28                         ..stacktraceon..  [test-stacks.R#17]
      47  27                              contextFunc                    
      48  26                              env$runWith                    
      49  25                      withCallingHandlers                    
      50  24                          domain$wrapSync                    
      51  23            promises::with_promise_domain                    
      52  22                       captureStackTraces                    
      53  21                                    force                    
      54  20                with_existing_ospan_async                    
      55  19         with_reactive_update_ospan_async                    
      56  18                                    force                    
      57  17                          domain$wrapSync                    
      58  16            promises::with_promise_domain                    
      59  15                       withReactiveDomain                    
      60  14                          domain$wrapSync                    
      61  13            promises::with_promise_domain                    
      62  12                                  ctx$run                    
      63  11                        ..stacktraceoff..                    
      64  10                                  isolate                    
      65   9                      withCallingHandlers  [test-stacks.R#16]
      66   8                          domain$wrapSync                    
      67   7            promises::with_promise_domain                    
      68   6                       captureStackTraces                    
      69   5                               doTryCatch  [test-stacks.R#15]
      70   4                              tryCatchOne                    
      71   3                             tryCatchList                    
      72   2                                 tryCatch                    
      73   1                                      try                    

