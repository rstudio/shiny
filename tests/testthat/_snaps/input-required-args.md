# missing required input args produce informative errors

    Code
      textInput("id")
    Condition
      Error in `textInput()`:
      ! `textInput()` is missing required argument: `label`.

---

    Code
      textInput()
    Condition
      Error in `textInput()`:
      ! `textInput()` is missing required arguments: `inputId` and `label`.

---

    Code
      textAreaInput("id")
    Condition
      Error in `textAreaInput()`:
      ! `textAreaInput()` is missing required argument: `label`.

---

    Code
      passwordInput("id")
    Condition
      Error in `passwordInput()`:
      ! `passwordInput()` is missing required argument: `label`.

---

    Code
      numericInput("id", "Label")
    Condition
      Error in `numericInput()`:
      ! `numericInput()` is missing required argument: `value`.

---

    Code
      checkboxInput("id")
    Condition
      Error in `checkboxInput()`:
      ! `checkboxInput()` is missing required argument: `label`.

---

    Code
      checkboxGroupInput("id")
    Condition
      Error in `checkboxGroupInput()`:
      ! `checkboxGroupInput()` is missing required argument: `label`.

---

    Code
      radioButtons("id")
    Condition
      Error in `radioButtons()`:
      ! `radioButtons()` is missing required argument: `label`.

---

    Code
      selectInput("id", "Label")
    Condition
      Error in `selectInput()`:
      ! `selectInput()` is missing required argument: `choices`.

---

    Code
      varSelectInput("id", "Label")
    Condition
      Error in `varSelectInput()`:
      ! `varSelectInput()` is missing required argument: `data`.

---

    Code
      dateInput("id")
    Condition
      Error in `dateInput()`:
      ! `dateInput()` is missing required argument: `label`.

---

    Code
      dateRangeInput("id")
    Condition
      Error in `dateRangeInput()`:
      ! `dateRangeInput()` is missing required argument: `label`.

---

    Code
      fileInput("id")
    Condition
      Error in `fileInput()`:
      ! `fileInput()` is missing required argument: `label`.

---

    Code
      sliderInput("id", "Label", min = 1, max = 10)
    Condition
      Error in `sliderInput()`:
      ! `sliderInput()` is missing required argument: `value`.

---

    Code
      sliderInput("id")
    Condition
      Error in `sliderInput()`:
      ! `sliderInput()` is missing required arguments: `label`, `min`, `max`, and `value`.

---

    Code
      actionButton("id")
    Condition
      Error in `actionButton()`:
      ! `actionButton()` is missing required argument: `label`.

---

    Code
      actionLink("id")
    Condition
      Error in `actionLink()`:
      ! `actionLink()` is missing required argument: `label`.

