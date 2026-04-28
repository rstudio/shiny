# downloadButton snapshot (enabled = 'auto')

    Code
      downloadButton("dl", "Download")
    Output
      <a aria-disabled="true" class="btn btn-default shiny-download-link disabled" download href="" id="dl" tabindex="-1" target="_blank">
        <i class="fas fa-download" role="presentation" aria-label="download icon"></i>
        Download
      </a>

# downloadButton snapshot (enabled = FALSE)

    Code
      downloadButton("dl", "Download", enabled = FALSE)
    Output
      <a aria-disabled="true" class="btn btn-default shiny-download-link disabled" data-shiny-disable-auto-enable download href="" id="dl" tabindex="-1" target="_blank">
        <i class="fas fa-download" role="presentation" aria-label="download icon"></i>
        Download
      </a>

# downloadButton snapshot (enabled = TRUE)

    Code
      downloadButton("dl", "Download", enabled = TRUE)
    Output
      <a id="dl" class="btn btn-default shiny-download-link" href="" target="_blank" download data-shiny-disable-auto-enable>
        <i class="fas fa-download" role="presentation" aria-label="download icon"></i>
        Download
      </a>

# downloadLink snapshot (enabled = 'auto')

    Code
      downloadLink("dl", "Download")
    Output
      <a aria-disabled="true" class="shiny-download-link disabled" download href="" id="dl" tabindex="-1" target="_blank">Download</a>

# downloadLink snapshot (enabled = FALSE)

    Code
      downloadLink("dl", "Download", enabled = FALSE)
    Output
      <a aria-disabled="true" class="shiny-download-link disabled" data-shiny-disable-auto-enable download href="" id="dl" tabindex="-1" target="_blank">Download</a>

# downloadLink snapshot (enabled = TRUE)

    Code
      downloadLink("dl", "Download", enabled = TRUE)
    Output
      <a id="dl" class="shiny-download-link" href="" target="_blank" download data-shiny-disable-auto-enable>Download</a>

