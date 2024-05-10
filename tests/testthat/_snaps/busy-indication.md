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
      tagList(busy_indicator_options(), busy_indicator_options(spinner_type = "bars"),
      busy_indicator_options(spinner_type = "pulse"), busy_indicator_options(
        spinner_type = "dots"), busy_indicator_options(spinner_color = "red"),
      busy_indicator_options(spinner_size = "10px"), busy_indicator_options(
        spinner_delay = "1s"), busy_indicator_options(pulse_background = "blue"),
      busy_indicator_options(pulse_height = "10px"), busy_indicator_options(
        pulse_speed = "1s"), busy_indicator_options(spinner_color = "red",
        spinner_size = "10px", spinner_delay = "1s", pulse_background = "blue",
        pulse_height = "10px", pulse_speed = "1s"))
    Output
      <style id="bslib-spinner-opts-3885">:has(> #bslib-spinner-opts-3885) {--shiny-spinner-url:url('spinners/bars.svg');}</style>
      <style id="bslib-spinner-opts-3885">:has(> #bslib-spinner-opts-3885) {--shiny-spinner-url:url('spinners/pulse.svg');}</style>
      <style id="bslib-spinner-opts-3885">:has(> #bslib-spinner-opts-3885) {--shiny-spinner-url:url('spinners/dots.svg');}</style>
      <style id="bslib-spinner-opts-3885">:has(> #bslib-spinner-opts-3885) {--shiny-spinner-color:#FF0000;}</style>
      <style id="bslib-spinner-opts-3885">:has(> #bslib-spinner-opts-3885) {--shiny-spinner-size:10px;}</style>
      <style id="bslib-spinner-opts-3885">:has(> #bslib-spinner-opts-3885) {--shiny-spinner-delay:1s;}</style>
      <style>:root {--shiny-pulse-background:blue;}</style>
      <style>:root {--shiny-pulse-height:10px;}</style>
      <style>:root {--shiny-pulse-speed:1s;}</style>
      <style id="bslib-spinner-opts-3885">:has(> #bslib-spinner-opts-3885) {--shiny-spinner-color:#FF0000;--shiny-spinner-size:10px;--shiny-spinner-delay:1s;}</style>
      <style>:root {--shiny-pulse-background:blue;--shiny-pulse-height:10px;--shiny-pulse-speed:1s;}</style>

# Can provide svg file for busyIndicatorOptions(spinner_type)

    Code
      busy_indicator_options(spinner_type = tmpsvg)
    Output
      [[1]]
      <style id="bslib-spinner-opts-3885">:has(> #bslib-spinner-opts-3885) {--shiny-spinner-url:url('data:image/svg+xml;base64,PHN2Zz48L3N2Zz4K');}</style>
      
      attr(,"class")
      [1] "card_item" "list"     

