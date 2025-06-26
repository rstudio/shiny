# Action button allows icon customization

    Code
      actionButton("foo", "Click me")
    Output
      <button id="foo" type="button" class="btn btn-default action-button">Click me</button>

---

    Code
      actionButton("foo", "Click me", icon = icon("star"))
    Output
      <button id="foo" type="button" class="btn btn-default action-button">
        <i class="far fa-star" role="presentation" aria-label="star icon"></i>
        <span class="shiny-icon-separator"></span>
        Click me
      </button>

