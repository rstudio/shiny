# tabsetPanel() markup is correct

    Code
      default
    Output
      <div class="tabbable">
        <ul class="nav nav-tabs" data-tabsetid="4785">
          <li class="active">
            <a href="#tab-4785-1" data-toggle="tab" data-bs-toggle="tab" data-value="A">A</a>
          </li>
          <li>
            <a href="#tab-4785-2" data-toggle="tab" data-bs-toggle="tab" data-value="B">
              <i aria-label="github icon" class="fab fa-github fa-fw" role="presentation"></i>
              B
            </a>
          </li>
          <li class="dropdown">
            <a href="#" class="dropdown-toggle" data-toggle="dropdown" data-bs-toggle="dropdown" data-value="Menu">
              Menu
              <b class="caret"></b>
            </a>
            <ul class="dropdown-menu" data-tabsetid="1502">
              <li>
                <a href="#tab-1502-1" data-toggle="tab" data-bs-toggle="tab" data-value="C">C</a>
              </li>
            </ul>
          </li>
        </ul>
        <div class="tab-content" data-tabsetid="4785">
          <div class="tab-pane active" data-value="A" id="tab-4785-1">a</div>
          <div class="tab-pane" data-value="B" data-icon-class="fab fa-github fa-fw" id="tab-4785-2">b</div>
          <div class="tab-pane" data-value="C" id="tab-1502-1">c</div>
        </div>
      </div>

---

    Code
      pills
    Output
      <div class="tabbable">
        <ul class="nav nav-pills" data-tabsetid="4785">
          <li>
            <a href="#tab-4785-1" data-toggle="tab" data-bs-toggle="tab" data-value="A">A</a>
          </li>
          <li class="active">
            <a href="#tab-4785-2" data-toggle="tab" data-bs-toggle="tab" data-value="B">
              <i aria-label="github icon" class="fab fa-github fa-fw" role="presentation"></i>
              B
            </a>
          </li>
          <li class="dropdown">
            <a href="#" class="dropdown-toggle" data-toggle="dropdown" data-bs-toggle="dropdown" data-value="Menu">
              Menu
              <b class="caret"></b>
            </a>
            <ul class="dropdown-menu" data-tabsetid="1502">
              <li>
                <a href="#tab-1502-1" data-toggle="tab" data-bs-toggle="tab" data-value="C">C</a>
              </li>
            </ul>
          </li>
        </ul>
        <div class="content-header"></div>
        <div class="tab-content" data-tabsetid="4785">
          <div class="tab-pane" data-value="A" id="tab-4785-1">a</div>
          <div class="tab-pane active" data-icon-class="fab fa-github fa-fw" data-value="B" id="tab-4785-2">b</div>
          <div class="tab-pane" data-value="C" id="tab-1502-1">c</div>
        </div>
        <div class="content-footer"></div>
      </div>

---

    Code
      bslib_tags(x)
    Output
      <div class="tabbable">
        <ul class="nav nav-tabs" data-tabsetid="4785">
          <li class="nav-item">
            <a href="#tab-4785-1" data-toggle="tab" data-bs-toggle="tab" data-value="A" class="nav-link active">A</a>
          </li>
          <li class="nav-item">
            <a href="#tab-4785-2" data-toggle="tab" data-bs-toggle="tab" data-value="B" class="nav-link">
              <i aria-label="github icon" class="fab fa-github fa-fw" role="presentation"></i>
              B
            </a>
          </li>
          <li class="dropdown nav-item">
            <a href="#" class="dropdown-toggle nav-link" data-toggle="dropdown" data-bs-toggle="dropdown" data-value="Menu">
              Menu
              <b class="caret"></b>
            </a>
            <ul class="dropdown-menu" data-tabsetid="1502">
              <li>
                <a href="#tab-1502-1" data-toggle="tab" data-bs-toggle="tab" data-value="C" class="dropdown-item">C</a>
              </li>
            </ul>
          </li>
        </ul>
        <div class="tab-content" data-tabsetid="4785">
          <div class="tab-pane active" data-value="A" id="tab-4785-1">a</div>
          <div class="tab-pane" data-value="B" data-icon-class="fab fa-github fa-fw" id="tab-4785-2">b</div>
          <div class="tab-pane" data-value="C" id="tab-1502-1">c</div>
        </div>
      </div>

---

    Code
      bslib_tags(x)
    Output
      <div class="tabbable">
        <ul class="nav nav-pills" data-tabsetid="4785">
          <li class="nav-item">
            <a href="#tab-4785-1" data-toggle="tab" data-bs-toggle="tab" data-value="A" class="nav-link">A</a>
          </li>
          <li class="nav-item">
            <a href="#tab-4785-2" data-toggle="tab" data-bs-toggle="tab" data-value="B" class="nav-link active">
              <i aria-label="github icon" class="fab fa-github fa-fw" role="presentation"></i>
              B
            </a>
          </li>
          <li class="dropdown nav-item">
            <a href="#" class="dropdown-toggle nav-link" data-toggle="dropdown" data-bs-toggle="dropdown" data-value="Menu">
              Menu
              <b class="caret"></b>
            </a>
            <ul class="dropdown-menu" data-tabsetid="1502">
              <li>
                <a href="#tab-1502-1" data-toggle="tab" data-bs-toggle="tab" data-value="C" class="dropdown-item">C</a>
              </li>
            </ul>
          </li>
        </ul>
        <div class="content-header"></div>
        <div class="tab-content" data-tabsetid="4785">
          <div class="tab-pane" data-value="A" id="tab-4785-1">a</div>
          <div class="tab-pane active" data-icon-class="fab fa-github fa-fw" data-value="B" id="tab-4785-2">b</div>
          <div class="tab-pane" data-value="C" id="tab-1502-1">c</div>
        </div>
        <div class="content-footer"></div>
      </div>

---

    Code
      dropdown_active
    Output
      <div class="tabbable">
        <ul class="nav nav-tabs" data-tabsetid="4785">
          <li>
            <a href="#tab-4785-1" data-toggle="tab" data-bs-toggle="tab" data-value="A">A</a>
          </li>
          <li>
            <a href="#tab-4785-2" data-toggle="tab" data-bs-toggle="tab" data-value="B">
              <i aria-label="github icon" class="fab fa-github fa-fw" role="presentation"></i>
              B
            </a>
          </li>
          <li class="dropdown active">
            <a href="#" class="dropdown-toggle" data-toggle="dropdown" data-bs-toggle="dropdown" data-value="Menu">
              Menu
              <b class="caret"></b>
            </a>
            <ul class="dropdown-menu" data-tabsetid="1502">
              <li class="active">
                <a href="#tab-1502-1" data-toggle="tab" data-bs-toggle="tab" data-value="C">C</a>
              </li>
            </ul>
          </li>
        </ul>
        <div class="tab-content" data-tabsetid="4785">
          <div class="tab-pane" data-value="A" id="tab-4785-1">a</div>
          <div class="tab-pane" data-value="B" data-icon-class="fab fa-github fa-fw" id="tab-4785-2">b</div>
          <div class="tab-pane active" data-value="C" id="tab-1502-1">c</div>
        </div>
      </div>

# navbarPage() markup is correct

    Code
      nav_page
    Output
      <body class="bslib-page-navbar">
        <nav class="navbar navbar-default navbar-static-top" role="navigation" data-bs-theme="light">
          <div class="container-fluid">
            <div class="navbar-header">
              <span class="navbar-brand">Title</span>
            </div>
            <ul class="nav navbar-nav nav-underline" data-tabsetid="4785">
              <li class="active">
                <a href="#tab-4785-1" data-toggle="tab" data-bs-toggle="tab" data-value="A">A</a>
              </li>
              <li>
                <a href="#tab-4785-2" data-toggle="tab" data-bs-toggle="tab" data-value="B">
                  <i aria-label="github icon" class="fab fa-github fa-fw" role="presentation"></i>
                  B
                </a>
              </li>
              <li class="dropdown">
                <a href="#" class="dropdown-toggle" data-toggle="dropdown" data-bs-toggle="dropdown" data-value="Menu">
                  Menu
                  <b class="caret"></b>
                </a>
                <ul class="dropdown-menu" data-tabsetid="1502">
                  <li>
                    <a href="#tab-1502-1" data-toggle="tab" data-bs-toggle="tab" data-value="C">C</a>
                  </li>
                </ul>
              </li>
            </ul>
          </div>
        </nav>
        <div class="container-fluid">
          <div class="tab-content" data-tabsetid="4785">
            <div class="tab-pane active" data-value="A" id="tab-4785-1">a</div>
            <div class="tab-pane" data-value="B" data-icon-class="fab fa-github fa-fw" id="tab-4785-2">b</div>
            <div class="tab-pane" data-value="C" id="tab-1502-1">c</div>
          </div>
        </div>
      </body>

---

    Code
      bslib_tags(x)
    Output
      <body class="bslib-page-navbar">
        <nav class="navbar navbar-default navbar-static-top" role="navigation" data-bs-theme="light">
          <div class="container-fluid">
            <div class="navbar-header">
              <span class="navbar-brand">Title</span>
            </div>
            <ul class="nav navbar-nav nav-underline" data-tabsetid="4785">
              <li class="nav-item">
                <a href="#tab-4785-1" data-toggle="tab" data-bs-toggle="tab" data-value="A" class="nav-link active">A</a>
              </li>
              <li class="nav-item">
                <a href="#tab-4785-2" data-toggle="tab" data-bs-toggle="tab" data-value="B" class="nav-link">
                  <i aria-label="github icon" class="fab fa-github fa-fw" role="presentation"></i>
                  B
                </a>
              </li>
              <li class="dropdown nav-item">
                <a href="#" class="dropdown-toggle nav-link" data-toggle="dropdown" data-bs-toggle="dropdown" data-value="Menu">
                  Menu
                  <b class="caret"></b>
                </a>
                <ul class="dropdown-menu" data-tabsetid="1502">
                  <li>
                    <a href="#tab-1502-1" data-toggle="tab" data-bs-toggle="tab" data-value="C" class="dropdown-item">C</a>
                  </li>
                </ul>
              </li>
            </ul>
          </div>
        </nav>
        <div class="container-fluid">
          <div class="tab-content" data-tabsetid="4785">
            <div class="tab-pane active" data-value="A" id="tab-4785-1">a</div>
            <div class="tab-pane" data-value="B" data-icon-class="fab fa-github fa-fw" id="tab-4785-2">b</div>
            <div class="tab-pane" data-value="C" id="tab-1502-1">c</div>
          </div>
        </div>
      </body>

# String input is handled properly

    Code
      nav_list
    Output
      <div class="row">
        <div class="col-sm-4 well">
          <ul class="nav nav-pills nav-stacked" data-tabsetid="4785">
            <li class="navbar-brand">A header</li>
            <li class="active">
              <a href="#tab-4785-2" data-toggle="tab" data-bs-toggle="tab" data-value="A">A</a>
            </li>
            <li>
              <a href="#tab-4785-3" data-toggle="tab" data-bs-toggle="tab" data-value="B">
                <i aria-label="github icon" class="fab fa-github fa-fw" role="presentation"></i>
                B
              </a>
            </li>
            <li class="dropdown">
              <a href="#" class="dropdown-toggle" data-toggle="dropdown" data-bs-toggle="dropdown" data-value="Menu">
                Menu
                <b class="caret"></b>
              </a>
              <ul class="dropdown-menu" data-tabsetid="1502">
                <li>
                  <a href="#tab-1502-1" data-toggle="tab" data-bs-toggle="tab" data-value="C">C</a>
                </li>
              </ul>
            </li>
          </ul>
        </div>
        <div class="col-sm-8">
          <div class="tab-content" data-tabsetid="4785">
            <div class="tab-pane active" data-value="A" id="tab-4785-2">a</div>
            <div class="tab-pane" data-value="B" data-icon-class="fab fa-github fa-fw" id="tab-4785-3">b</div>
            <div class="tab-pane" data-value="C" id="tab-1502-1">c</div>
          </div>
        </div>
      </div>

---

    Code
      bslib_tags(x)
    Output
      <div class="row">
        <div class="col-sm-4 well">
          <ul class="nav nav-pills nav-stacked" data-tabsetid="4785">
            <li class="navbar-brand">A header</li>
            <li class="nav-item">
              <a href="#tab-4785-2" data-toggle="tab" data-bs-toggle="tab" data-value="A" class="nav-link active">A</a>
            </li>
            <li class="nav-item">
              <a href="#tab-4785-3" data-toggle="tab" data-bs-toggle="tab" data-value="B" class="nav-link">
                <i aria-label="github icon" class="fab fa-github fa-fw" role="presentation"></i>
                B
              </a>
            </li>
            <li class="dropdown nav-item">
              <a href="#" class="dropdown-toggle nav-link" data-toggle="dropdown" data-bs-toggle="dropdown" data-value="Menu">
                Menu
                <b class="caret"></b>
              </a>
              <ul class="dropdown-menu" data-tabsetid="1502">
                <li>
                  <a href="#tab-1502-1" data-toggle="tab" data-bs-toggle="tab" data-value="C" class="dropdown-item">C</a>
                </li>
              </ul>
            </li>
          </ul>
        </div>
        <div class="col-sm-8">
          <div class="tab-content" data-tabsetid="4785">
            <div class="tab-pane active" data-value="A" id="tab-4785-2">a</div>
            <div class="tab-pane" data-value="B" data-icon-class="fab fa-github fa-fw" id="tab-4785-3">b</div>
            <div class="tab-pane" data-value="C" id="tab-1502-1">c</div>
          </div>
        </div>
      </div>

# Shiny.tag input produces a warning

    Code
      tab_tags
    Output
      <div class="tabbable">
        <ul class="nav nav-tabs" data-tabsetid="4785">
          <li>
            <a href="#tab-4785-1" data-toggle="tab" data-bs-toggle="tab" disabled></a>
          </li>
          <li class="active">
            <a href="#tab-4785-2" data-toggle="tab" data-bs-toggle="tab" data-value="A">A</a>
          </li>
          <li>
            <a href="#tab-4785-3" data-toggle="tab" data-bs-toggle="tab" data-value="B">
              <i aria-label="github icon" class="fab fa-github fa-fw" role="presentation"></i>
              B
            </a>
          </li>
          <li class="dropdown">
            <a href="#" class="dropdown-toggle" data-toggle="dropdown" data-bs-toggle="dropdown" data-value="Menu">
              Menu
              <b class="caret"></b>
            </a>
            <ul class="dropdown-menu" data-tabsetid="1502">
              <li>
                <a href="#tab-1502-1" data-toggle="tab" data-bs-toggle="tab" data-value="C">C</a>
              </li>
            </ul>
          </li>
        </ul>
        <div class="tab-content" data-tabsetid="4785">
          <div id="tab-4785-1">A div</div>
          <div class="tab-pane active" data-value="A" id="tab-4785-2">a</div>
          <div class="tab-pane" data-value="B" data-icon-class="fab fa-github fa-fw" id="tab-4785-3">b</div>
          <div class="tab-pane" data-value="C" id="tab-1502-1">c</div>
        </div>
      </div>

