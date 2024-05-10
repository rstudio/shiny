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
        spinner_type = "dots"), busyIndicatorOptions(spinner_color = "red"),
      busyIndicatorOptions(spinner_size = "10px"), busyIndicatorOptions(
        spinner_delay = "1s"), busyIndicatorOptions(pulse_background = "blue"),
      busyIndicatorOptions(pulse_height = "10px"), busyIndicatorOptions(pulse_speed = "1s"),
      busyIndicatorOptions(spinner_color = "red", spinner_size = "10px",
        spinner_delay = "1s", pulse_background = "blue", pulse_height = "10px",
        pulse_speed = "1s"))
    Output
      <style id="spinner-options-606810">:has(> #spinner-options-606810) {--shiny-spinner-url:url('spinners/bars.svg');}</style>
      <style id="spinner-options-331475">:has(> #spinner-options-331475) {--shiny-spinner-url:url('spinners/pulse.svg');}</style>
      <style id="spinner-options-162876">:has(> #spinner-options-162876) {--shiny-spinner-url:url('spinners/dots.svg');}</style>
      <style id="spinner-options-614193">:has(> #spinner-options-614193) {--shiny-spinner-color:#FF0000;}</style>
      <style id="spinner-options-732154">:has(> #spinner-options-732154) {--shiny-spinner-size:10px;}</style>
      <style id="spinner-options-353895">:has(> #spinner-options-353895) {--shiny-spinner-delay:1s;}</style>
      <style>:root {--shiny-pulse-background:blue;}</style>
      <style>:root {--shiny-pulse-height:10px;}</style>
      <style>:root {--shiny-pulse-speed:1s;}</style>
      <style id="spinner-options-437161">:has(> #spinner-options-437161) {--shiny-spinner-color:#FF0000;--shiny-spinner-size:10px;--shiny-spinner-delay:1s;}</style>
      <style>:root {--shiny-pulse-background:blue;--shiny-pulse-height:10px;--shiny-pulse-speed:1s;}</style>

# Can provide svg file for busyIndicatorOptions(spinner_type)

    Code
      busyIndicatorOptions(spinner_type = tmpsvg)
    Output
      [[1]]
      <style id="spinner-options-606810">:has(> #spinner-options-606810) {--shiny-spinner-url:url('data:image/svg+xml;base64,PHN2Zz48L3N2Zz4K');}</style>
      
      attr(,"class")
      [1] "card_item" "list"     

