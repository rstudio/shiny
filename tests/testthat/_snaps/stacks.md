# integration tests

    Code
      df
    Output
         num                                     call                 loc
      1   70                                        A   [test-stacks.R#3]
      2   69                                        B   [test-stacks.R#7]
      3   68                             <reactive:C>  [test-stacks.R#11]
      4   46                                        C                    
      5   45                              renderTable  [test-stacks.R#18]
      6   44                                     func                    
      7   28 renderTable({     C() }, server = FALSE)                    
      8   10                                  isolate                    
      9    9                      withCallingHandlers  [test-stacks.R#16]
      10   8                          domain$wrapSync                    
      11   7            promises::with_promise_domain                    
      12   6                       captureStackTraces                    
      13   2                                 tryCatch                    
      14   1                                      try                    
      15   0                               causeError  [test-stacks.R#14]

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
      18  56                   with_otel_span_context                    
      19  55                                    force                    
      20  54                          domain$wrapSync                    
      21  53            promises::with_promise_domain                    
      22  52                       withReactiveDomain                    
      23  51                          domain$wrapSync                    
      24  50            promises::with_promise_domain                    
      25  49                                  ctx$run                    
      26  48                        self$.updateValue                    
      27  47                        ..stacktraceoff..                    
      28  46                                        C                    
      29  45                              renderTable  [test-stacks.R#18]
      30  44                                     func                    
      31  43                         ..stacktraceon..                    
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
      45  29                        ..stacktraceoff..                    
      46  28 renderTable({     C() }, server = FALSE)                    
      47  27                         ..stacktraceon..  [test-stacks.R#17]
      48  26                              contextFunc                    
      49  25                              env$runWith                    
      50  24                      withCallingHandlers                    
      51  23                          domain$wrapSync                    
      52  22            promises::with_promise_domain                    
      53  21                       captureStackTraces                    
      54  20                                    force                    
      55  19                   with_otel_span_context                    
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
      74   0                               causeError  [test-stacks.R#14]

