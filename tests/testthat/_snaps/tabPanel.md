# tabsetPanel() markup is correct

    Code
      default
    Output
      <div class="tabbable">
        <ul class="nav nav-tabs" data-tabsetid="4785">
          <li class="active">
            <a href="#tab-4785-1" data-toggle="tab" data-value="A">A</a>
          </li>
          <li>
            <a href="#tab-4785-2" data-toggle="tab" data-value="B">
              <i class=" fab fa-github fa-fw" role="presentation" aria-label=" icon"></i>
              B
            </a>
          </li>
          <li class="dropdown">
            <a href="#" class="dropdown-toggle" data-toggle="dropdown" data-value="Menu">
              Menu
              <b class="caret"></b>
            </a>
            <ul class="dropdown-menu" data-tabsetid="1502">
              <li>
                <a href="#tab-1502-1" data-toggle="tab" data-value="C">C</a>
              </li>
            </ul>
          </li>
        </ul>
        <div class="tab-content" data-tabsetid="4785">
          <div class="tab-pane active" data-value="A" id="tab-4785-1">a</div>
          <div class="tab-pane" data-value="B" data-icon-class="fab fa-github" id="tab-4785-2">b</div>
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
            <a href="#tab-4785-1" data-toggle="tab" data-value="A">A</a>
          </li>
          <li class="active">
            <a href="#tab-4785-2" data-toggle="tab" data-value="B">
              <i class=" fab fa-github fa-fw" role="presentation" aria-label=" icon"></i>
              B
            </a>
          </li>
          <li class="dropdown">
            <a href="#" class="dropdown-toggle" data-toggle="dropdown" data-value="Menu">
              Menu
              <b class="caret"></b>
            </a>
            <ul class="dropdown-menu" data-tabsetid="1502">
              <li>
                <a href="#tab-1502-1" data-toggle="tab" data-value="C">C</a>
              </li>
            </ul>
          </li>
        </ul>
        <div class="content-header"></div>
        <div class="tab-content" data-tabsetid="4785">
          <div class="tab-pane" data-value="A" id="tab-4785-1">a</div>
          <div class="tab-pane active" data-icon-class="fab fa-github" data-value="B" id="tab-4785-2">b</div>
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
            <a href="#tab-4785-1" data-toggle="tab" data-value="A" class="nav-link active">A</a>
          </li>
          <li class="nav-item">
            <a href="#tab-4785-2" data-toggle="tab" data-value="B" class="nav-link">
              <i class=" fab fa-github fa-fw" role="presentation" aria-label=" icon"></i>
              B
            </a>
          </li>
          <li class="dropdown">
            <a href="#" class="dropdown-toggle" data-toggle="dropdown" data-value="Menu">
              Menu
              <b class="caret"></b>
            </a>
            <ul class="dropdown-menu" data-tabsetid="1502">
              <li class="nav-item">
                <a href="#tab-1502-1" data-toggle="tab" data-value="C" class="dropdown-item">C</a>
              </li>
            </ul>
          </li>
        </ul>
        <div class="tab-content" data-tabsetid="4785">
          <div class="tab-pane active" data-value="A" id="tab-4785-1">a</div>
          <div class="tab-pane" data-value="B" data-icon-class="fab fa-github" id="tab-4785-2">b</div>
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
            <a href="#tab-4785-1" data-toggle="tab" data-value="A" class="nav-link">A</a>
          </li>
          <li class="nav-item">
            <a href="#tab-4785-2" data-toggle="tab" data-value="B" class="nav-link active">
              <i class=" fab fa-github fa-fw" role="presentation" aria-label=" icon"></i>
              B
            </a>
          </li>
          <li class="dropdown">
            <a href="#" class="dropdown-toggle" data-toggle="dropdown" data-value="Menu">
              Menu
              <b class="caret"></b>
            </a>
            <ul class="dropdown-menu" data-tabsetid="1502">
              <li class="nav-item">
                <a href="#tab-1502-1" data-toggle="tab" data-value="C" class="dropdown-item">C</a>
              </li>
            </ul>
          </li>
        </ul>
        <div class="content-header"></div>
        <div class="tab-content" data-tabsetid="4785">
          <div class="tab-pane" data-value="A" id="tab-4785-1">a</div>
          <div class="tab-pane active" data-icon-class="fab fa-github" data-value="B" id="tab-4785-2">b</div>
          <div class="tab-pane" data-value="C" id="tab-1502-1">c</div>
        </div>
        <div class="content-footer"></div>
      </div>

# navbarPage() markup is correct

    Code
      nav_page
    Output
      <nav class="navbar navbar-default navbar-static-top" role="navigation">
        <div class="container-fluid">
          <div class="navbar-header">
            <span class="navbar-brand">Title</span>
          </div>
          <ul class="nav navbar-nav" data-tabsetid="4785">
            <li class="active">
              <a href="#tab-4785-1" data-toggle="tab" data-value="A">A</a>
            </li>
            <li>
              <a href="#tab-4785-2" data-toggle="tab" data-value="B">
                <i class=" fab fa-github fa-fw" role="presentation" aria-label=" icon"></i>
                B
              </a>
            </li>
            <li class="dropdown">
              <a href="#" class="dropdown-toggle" data-toggle="dropdown" data-value="Menu">
                Menu
                <b class="caret"></b>
              </a>
              <ul class="dropdown-menu" data-tabsetid="1502">
                <li>
                  <a href="#tab-1502-1" data-toggle="tab" data-value="C">C</a>
                </li>
              </ul>
            </li>
          </ul>
        </div>
      </nav>
      <div class="container-fluid">
        <div class="tab-content" data-tabsetid="4785">
          <div class="tab-pane active" data-value="A" id="tab-4785-1">a</div>
          <div class="tab-pane" data-value="B" data-icon-class="fab fa-github" id="tab-4785-2">b</div>
          <div class="tab-pane" data-value="C" id="tab-1502-1">c</div>
        </div>
      </div>

---

    Code
      bslib_tags(x)
    Output
      <nav class="navbar navbar-default navbar-static-top" role="navigation">
        <div class="container-fluid">
          <div class="navbar-header">
            <span class="navbar-brand">Title</span>
          </div>
          <ul class="nav navbar-nav" data-tabsetid="4785">
            <li class="nav-item">
              <a href="#tab-4785-1" data-toggle="tab" data-value="A" class="nav-link active">A</a>
            </li>
            <li class="nav-item">
              <a href="#tab-4785-2" data-toggle="tab" data-value="B" class="nav-link">
                <i class=" fab fa-github fa-fw" role="presentation" aria-label=" icon"></i>
                B
              </a>
            </li>
            <li class="dropdown">
              <a href="#" class="dropdown-toggle" data-toggle="dropdown" data-value="Menu">
                Menu
                <b class="caret"></b>
              </a>
              <ul class="dropdown-menu" data-tabsetid="1502">
                <li class="nav-item">
                  <a href="#tab-1502-1" data-toggle="tab" data-value="C" class="dropdown-item">C</a>
                </li>
              </ul>
            </li>
          </ul>
        </div>
      </nav>
      <div class="container-fluid">
        <div class="tab-content" data-tabsetid="4785">
          <div class="tab-pane active" data-value="A" id="tab-4785-1">a</div>
          <div class="tab-pane" data-value="B" data-icon-class="fab fa-github" id="tab-4785-2">b</div>
          <div class="tab-pane" data-value="C" id="tab-1502-1">c</div>
        </div>
      </div>

# String input is handled properly

    Code
      nav_list
    Output
      <div class="row">
        <div class="col-sm-4 well">
          <ul class="nav nav-pills nav-stacked" data-tabsetid="4785">
            <li class="navbar-brand">A header</li>
            <li class="active">
              <a href="#tab-4785-2" data-toggle="tab" data-value="A">A</a>
            </li>
            <li>
              <a href="#tab-4785-3" data-toggle="tab" data-value="B">
                <i class=" fab fa-github fa-fw" role="presentation" aria-label=" icon"></i>
                B
              </a>
            </li>
            <li class="dropdown">
              <a href="#" class="dropdown-toggle" data-toggle="dropdown" data-value="Menu">
                Menu
                <b class="caret"></b>
              </a>
              <ul class="dropdown-menu" data-tabsetid="1502">
                <li>
                  <a href="#tab-1502-1" data-toggle="tab" data-value="C">C</a>
                </li>
              </ul>
            </li>
          </ul>
        </div>
        <div class="col-sm-8">
          <div class="tab-content" data-tabsetid="4785">
            <div class="tab-pane active" data-value="A" id="tab-4785-2">a</div>
            <div class="tab-pane" data-value="B" data-icon-class="fab fa-github" id="tab-4785-3">b</div>
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
              <a href="#tab-4785-2" data-toggle="tab" data-value="A" class="nav-link active">A</a>
            </li>
            <li class="nav-item">
              <a href="#tab-4785-3" data-toggle="tab" data-value="B" class="nav-link">
                <i class=" fab fa-github fa-fw" role="presentation" aria-label=" icon"></i>
                B
              </a>
            </li>
            <li class="dropdown">
              <a href="#" class="dropdown-toggle" data-toggle="dropdown" data-value="Menu">
                Menu
                <b class="caret"></b>
              </a>
              <ul class="dropdown-menu" data-tabsetid="1502">
                <li class="nav-item">
                  <a href="#tab-1502-1" data-toggle="tab" data-value="C" class="dropdown-item">C</a>
                </li>
              </ul>
            </li>
          </ul>
        </div>
        <div class="col-sm-8">
          <div class="tab-content" data-tabsetid="4785">
            <div class="tab-pane active" data-value="A" id="tab-4785-2">a</div>
            <div class="tab-pane" data-value="B" data-icon-class="fab fa-github" id="tab-4785-3">b</div>
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
          <li class="active">
            <a href="#tab-4785-1" data-toggle="tab"></a>
          </li>
          <li>
            <a href="#tab-4785-2" data-toggle="tab" data-value="A">A</a>
          </li>
          <li>
            <a href="#tab-4785-3" data-toggle="tab" data-value="B">
              <i class=" fab fa-github fa-fw" role="presentation" aria-label=" icon"></i>
              B
            </a>
          </li>
          <li class="dropdown">
            <a href="#" class="dropdown-toggle" data-toggle="dropdown" data-value="Menu">
              Menu
              <b class="caret"></b>
            </a>
            <ul class="dropdown-menu" data-tabsetid="1502">
              <li>
                <a href="#tab-1502-1" data-toggle="tab" data-value="C">C</a>
              </li>
            </ul>
          </li>
        </ul>
        <div class="tab-content" data-tabsetid="4785">
          <div class="active" id="tab-4785-1">A div</div>
          <div class="tab-pane" data-value="A" id="tab-4785-2">a</div>
          <div class="tab-pane" data-value="B" data-icon-class="fab fa-github" id="tab-4785-3">b</div>
          <div class="tab-pane" data-value="C" id="tab-1502-1">c</div>
        </div>
      </div>

