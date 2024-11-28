# integration tests

    Code
      df
    Output
        num                call loc category
      1  64                   A  3]     user
      2  63                   B  7]     user
      3  62        <reactive:C> 11]     user
      4  42                   C         user
      5  41         renderTable 18]     user
      6  40                func         user
      7  39               force             
      8  38         withVisible             
      9  37 withCallingHandlers         user

---

    Code
      df
    Output
         num                                     call loc category
      1   67                                        h             
      2   66                       .handleSimpleError             
      3   65                                     stop             
      4   64                                        A  3]     user
      5   63                                        B  7]     user
      6   62                             <reactive:C> 11]     user
      7   61                         ..stacktraceon..         user
      8   60                                    .func         user
      9   59                              withVisible             
      10  58                      withCallingHandlers         user
      11  57                              contextFunc         user
      12  56                              env$runWith         user
      13  55                      withCallingHandlers         user
      14  54                          domain$wrapSync         user
      15  53            promises::with_promise_domain             
      16  52                       captureStackTraces         user
      17  51                                    force         user
      18  50                          domain$wrapSync         user
      19  49            promises::with_promise_domain             
      20  48                       withReactiveDomain         user
      21  47                          domain$wrapSync         user
      22  46            promises::with_promise_domain             
      23  45                                  ctx$run         user
      24  44                        self$.updateValue         user
      25  43                        ..stacktraceoff..         user
      26  42                                        C         user
      27  41                              renderTable 18]     user
      28  40                                     func         user
      29  39                                    force             
      30  38                              withVisible             
      31  37                      withCallingHandlers         user
      32  36                          domain$wrapSync         user
      33  35            promises::with_promise_domain             
      34  34                       captureStackTraces         user
      35  33                               doTryCatch         user
      36  32                              tryCatchOne             
      37  31                             tryCatchList             
      38  30                                 tryCatch             
      39  29                                       do         user
      40  28                             hybrid_chain         user
      41  27                               renderFunc         user
      42  26 renderTable({     C() }, server = FALSE)         user
      43  25                         ..stacktraceon.. 17]     user
      44  24                              contextFunc         user
      45  23                              env$runWith         user
      46  22                      withCallingHandlers         user
      47  21                          domain$wrapSync         user
      48  20            promises::with_promise_domain             
      49  19                       captureStackTraces         user
      50  18                                    force         user
      51  17                          domain$wrapSync         user
      52  16            promises::with_promise_domain             
      53  15                       withReactiveDomain         user
      54  14                          domain$wrapSync         user
      55  13            promises::with_promise_domain             
      56  12                                  ctx$run         user
      57  11                        ..stacktraceoff..         user
      58  10                                  isolate         user
      59   9                      withCallingHandlers 16]     user
      60   8                          domain$wrapSync         user
      61   7            promises::with_promise_domain             
      62   6                       captureStackTraces         user
      63   5                               doTryCatch 15]     user
      64   4                              tryCatchOne             
      65   3                             tryCatchList             
      66   2                                 tryCatch             
      67   1                                      try             

