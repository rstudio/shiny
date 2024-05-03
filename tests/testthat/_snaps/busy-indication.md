# useBusyIndicators()

    Code
      res
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
      res
    Output
      <style>:root {--shiny-spinner-color: #FF0000}</style>
      <style>:root {--shiny-spinner-size: 10px}</style>
      <style>:root {--shiny-spinner-delay: 1s}</style>
      <style>:root {--shiny-pulse-background: blue}</style>
      <style>:root {--shiny-pulse-height: 10px}</style>
      <style>:root {--shiny-pulse-speed: 1s}</style>
      <style>:root {--shiny-spinner-color: #FF0000;--shiny-spinner-size: 10px;--shiny-spinner-delay: 1s}</style>
      <style>:root {--shiny-pulse-background: blue;--shiny-pulse-height: 10px;--shiny-pulse-speed: 1s}</style>

