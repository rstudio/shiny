# downloadButton snapshot (autoEnable = TRUE)

    Code
      downloadButton("dl", "Download")
    Output
      <a id="dl" class="btn btn-default shiny-download-link disabled" href="" target="_blank" download aria-disabled="true" tabindex="-1">
        <i class="fas fa-download" role="presentation" aria-label="download icon"></i>
        Download
      </a>

# downloadButton snapshot (autoEnable = FALSE)

    Code
      downloadButton("dl", "Download", autoEnable = FALSE)
    Output
      <a id="dl" class="btn btn-default shiny-download-link disabled" href="" target="_blank" download aria-disabled="true" data-ignore-update tabindex="-1">
        <i class="fas fa-download" role="presentation" aria-label="download icon"></i>
        Download
      </a>

# downloadLink snapshot (autoEnable = TRUE)

    Code
      downloadLink("dl", "Download")
    Output
      <a id="dl" class="shiny-download-link disabled" href="" target="_blank" download aria-disabled="true" tabindex="-1">Download</a>

# downloadLink snapshot (autoEnable = FALSE)

    Code
      downloadLink("dl", "Download", autoEnable = FALSE)
    Output
      <a id="dl" class="shiny-download-link disabled" href="" target="_blank" download aria-disabled="true" data-ignore-update tabindex="-1">Download</a>

