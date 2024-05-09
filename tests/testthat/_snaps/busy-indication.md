# useBusyIndicators()

    Code
      tagList(useBusyIndicators(), useBusyIndicators(spinners = FALSE),
      useBusyIndicators(pulse = FALSE), useBusyIndicators(spinners = FALSE, pulse = FALSE),
      )
    Output
      <script>document.documentElement.dataset.shinyBusySpinners = 'true';
      document.documentElement.dataset.shinyBusyPulse = 'true';</script>
      <script>delete document.documentElement.dataset.shinyBusySpinners;
      document.documentElement.dataset.shinyBusyPulse = 'true';</script>
      <script>document.documentElement.dataset.shinyBusySpinners = 'true';
      delete document.documentElement.dataset.shinyBusyPulse;</script>
      <script>delete document.documentElement.dataset.shinyBusySpinners;
      delete document.documentElement.dataset.shinyBusyPulse;</script>

# busyIndicatorOptions()

    Code
      tagList(busyIndicatorOptions(), busyIndicatorOptions(spinner_type = "bars"),
      busyIndicatorOptions(spinner_type = "pulse"), busyIndicatorOptions(
        spinner_type = "dots"), busyIndicatorOptions(spinner_type = tmpsvg),
      busyIndicatorOptions(spinner_color = "red"), busyIndicatorOptions(spinner_size = "10px"),
      busyIndicatorOptions(spinner_delay = "1s"), busyIndicatorOptions(spinner_color = "red",
        spinner_selector = NA), busyIndicatorOptions(pulse_background = "blue"),
      busyIndicatorOptions(pulse_height = "10px"), busyIndicatorOptions(pulse_speed = "1s"),
      busyIndicatorOptions(spinner_color = "red", spinner_size = "10px",
        spinner_delay = "1s", pulse_background = "blue", pulse_height = "10px",
        pulse_speed = "1s"))
    Output
      <style>:root {--shiny-spinner-type:url('spinners/bars.svg');}</style>
      <style>:root {--shiny-spinner-type:url('spinners/pulse.svg');}</style>
      <style>:root {--shiny-spinner-type:url('spinners/dots.svg');}</style>
      <style>:root {--shiny-spinner-type:url('data:image/svg+xml;base64,PHN2Zz48L3N2Zz4K');}</style>
      <style>:root {--shiny-spinner-color:#FF0000;}</style>
      <style>:root {--shiny-spinner-size:10px;}</style>
      <style>:root {--shiny-spinner-delay:1s;}</style>
      --shiny-spinner-color:#FF0000;
      <style>:root {--shiny-pulse-background:blue;}</style>
      <style>:root {--shiny-pulse-height:10px;}</style>
      <style>:root {--shiny-pulse-speed:1s;}</style>
      <style>:root {--shiny-spinner-color:#FF0000;--shiny-spinner-size:10px;--shiny-spinner-delay:1s;}</style>
      <style>:root {--shiny-pulse-background:blue;--shiny-pulse-height:10px;--shiny-pulse-speed:1s;}</style>

