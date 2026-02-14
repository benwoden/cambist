library(testthat)
library(shiny)
library(shinyjs)

# Source app.R — creates `ui` and `server` in this environment
# Override shinyApp to prevent launch
shinyApp <- function(...) invisible(NULL)
source(file.path(dirname(getwd()), "app.R"), local = FALSE)

# ------------------------------------------------------------------
# Test 1: Off-by-one — "Next" button should allow reaching all pairs
# ------------------------------------------------------------------
test_that("Next button allows reaching the last pair of items", {
  testServer(server, {
    text_data(c("A", "B", "C", "D"))
    current_pos(1)
    closed_book(FALSE)

    # Click next twice: 1->2->3
    session$setInputs(nextBtn = 1)
    session$setInputs(nextBtn = 2)

    # Position 3 means text1=C, text2=D — the last valid pair
    expect_equal(current_pos(), 3)
    expect_equal(output$text1, "C")
    expect_equal(output$text2, "D")
  })
})

test_that("Next button stops at the last valid pair", {
  testServer(server, {
    text_data(c("A", "B", "C"))
    current_pos(1)
    closed_book(FALSE)

    # Click next many times — should stop at position 2 (text1=B, text2=C)
    session$setInputs(nextBtn = 1)
    session$setInputs(nextBtn = 2)
    session$setInputs(nextBtn = 3)
    session$setInputs(nextBtn = 4)

    expect_equal(current_pos(), 2)
    expect_equal(output$text1, "B")
    expect_equal(output$text2, "C")
  })
})

# ------------------------------------------------------------------
# Test 2: CSV parsing — handles quoted fields and multi-row CSVs
# ------------------------------------------------------------------
test_that("CSV upload handles quoted fields with commas", {
  testServer(server, {
    tmp <- tempfile(fileext = ".csv")
    writeLines('"hello, world","foo","bar, baz"', tmp)

    session$setInputs(file = list(
      name = "test.csv",
      size = file.info(tmp)$size,
      type = "text/csv",
      datapath = tmp
    ))

    expect_equal(length(text_data()), 3)
    expect_true("hello, world" %in% text_data())
    expect_true("bar, baz" %in% text_data())

    unlink(tmp)
  })
})

test_that("CSV upload handles multi-row files", {
  testServer(server, {
    tmp <- tempfile(fileext = ".csv")
    writeLines(c("item1,item2", "item3,item4"), tmp)

    session$setInputs(file = list(
      name = "test.csv",
      size = file.info(tmp)$size,
      type = "text/csv",
      datapath = tmp
    ))

    # Should have 4 items total across both rows
    expect_equal(length(text_data()), 4)

    unlink(tmp)
  })
})

# ------------------------------------------------------------------
# Test 3: Buttons before file upload should not error
# ------------------------------------------------------------------
test_that("Next button before upload does nothing and doesn't error", {
  testServer(server, {
    session$setInputs(nextBtn = 1)

    expect_null(text_data())
    expect_equal(current_pos(), 1)
    expect_equal(output$text1, "No data loaded")
  })
})

test_that("Reset button before upload does nothing and doesn't error", {
  testServer(server, {
    session$setInputs(reset = 1)

    expect_null(text_data())
    expect_equal(output$debug, "No data loaded")
  })
})

test_that("Close Book button before upload does nothing and doesn't error", {
  testServer(server, {
    session$setInputs(closeBook = 1)

    expect_null(text_data())
    expect_false(closed_book())
  })
})

# ------------------------------------------------------------------
# Test 4: Close Book cannot be triggered multiple times
# ------------------------------------------------------------------
test_that("Close Book only advances once even when clicked multiple times", {
  testServer(server, {
    text_data(c("A", "B", "C", "D"))
    current_pos(1)
    closed_book(FALSE)

    # First close book click: should advance to 2
    session$setInputs(closeBook = 1)
    expect_equal(current_pos(), 2)
    expect_true(closed_book())

    # Additional clicks should NOT advance further
    session$setInputs(closeBook = 2)
    session$setInputs(closeBook = 3)

    expect_equal(current_pos(), 2)
    expect_true(closed_book())
  })
})

# ------------------------------------------------------------------
# Test 5: Single item edge case
# ------------------------------------------------------------------
test_that("Single item displays correctly", {
  testServer(server, {
    text_data(c("OnlyOne"))
    current_pos(1)
    closed_book(FALSE)

    expect_equal(output$text1, "OnlyOne")
  })
})

test_that("Next does not advance with a single item", {
  testServer(server, {
    text_data(c("OnlyOne"))
    current_pos(1)
    closed_book(FALSE)

    session$setInputs(nextBtn = 1)
    expect_equal(current_pos(), 1)
  })
})

test_that("Close Book works with a single item without advancing", {
  testServer(server, {
    text_data(c("OnlyOne"))
    current_pos(1)
    closed_book(FALSE)

    session$setInputs(closeBook = 1)
    expect_true(closed_book())
    expect_equal(current_pos(), 1)
    expect_equal(output$text1, "OnlyOne")
  })
})

test_that("Two items show as a valid pair", {
  testServer(server, {
    text_data(c("First", "Second"))
    current_pos(1)
    closed_book(FALSE)

    expect_equal(output$text1, "First")
    expect_equal(output$text2, "Second")

    # Next should not advance — already at the only valid pair
    session$setInputs(nextBtn = 1)
    expect_equal(current_pos(), 1)
  })
})
