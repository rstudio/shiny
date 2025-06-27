# Action button allows icon customization

    Code
      actionButton("foo", "Click me")
    Output
      <button id="foo" type="button" class="btn btn-default action-button">
        <span class="action-label">Click me</span>
      </button>

---

    Code
      actionButton("foo", "Click me", icon = icon("star"))
    Output
      <button id="foo" type="button" class="btn btn-default action-button">
        <span class="action-icon">
          <i class="far fa-star" role="presentation" aria-label="star icon"></i>
        </span>
        <span class="action-label">Click me</span>
      </button>

