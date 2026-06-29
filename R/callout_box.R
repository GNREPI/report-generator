


Row {data-height=150}
-----------------------------------------------------------------------

  ### Call Out Box 1

  ```{r}
# Content for the first column (e.g., a valueBox or text)
valueBox(42, caption = "New Cases", icon = "fa-user-plus")
Row {data-height=150}
-----------------------------------------------------------------------

  ### Call Out Box 1

  ```{r}
# Content for the first column (e.g., a valueBox or text)
valueBox(42, caption = "New Cases", icon = "fa-user-plus")

# Content for the second column
valueBox(95, caption = "Recovery Rate", icon = "fa-heart", color = "success")


valueBox(42, caption = "Gender", icon = "fa-user-plus")

valueBox(42, caption = "Gender", icon = "fa-user-plus")
```

### 1. `valueBox()`
This is the function itself. It tells R to create a colorful, high-impact rectangular box designed to show a "hero" number or status.

### 2. `42` (The Value)
* **What it is:** The primary number or text displayed in large font.
* **Note:** This can be a hardcoded number like `42`, or a variable from your data like `total_cases`. If you pass a string (e.g., `"42%"`), it will display exactly that.

### 3. `caption = "Gender"`
* **What it is:** The label that appears directly below the large value.
* **Purpose:** It provides context for what the number represents. In your example, it tells the user that the number 42 relates to the "Gender" metric.

### 4. `icon = "fa-user-plus"`
* **What it is:** The small graphic that appears in the background or corner of the box.
* **"fa-":** This prefix stands for **Font Awesome**, a popular icon library bundled with `flexdashboard`.
* **"user-plus":** This is the specific icon name (an outline of a person with a plus sign).
* **Other common icons:** `"fa-pencil"`, `"fa-envelope"`, `"fa-warning"`.

---

  ### Bonus: Optional Arguments
  You didn't include these, but they are very helpful for styling:

* **`color = "primary"`**: Sets the background color. You can use standard colors like `"info"`, `"success"` (green), `"warning"` (orange), `"danger"` (red), or a specific hex code like `"#800000"`.
* **`href = "#section-id"`**: Turns the box into a clickable link that can navigate to another page in your dashboard.

### Example in a code chunk:
```r
### Total Male Cases
```{r}
male_count <- sum(combo$gender == "Male", na.rm = TRUE)

valueBox(
  value = male_count,
  caption = "Total Male Cases",
  icon = "fa-mars",
  color = "info"
)