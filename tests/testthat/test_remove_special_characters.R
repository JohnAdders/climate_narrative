library(climate.narrative)
testthat::test_that("function remove_special_characters produces correct result", {

  # Tests that the function produces exactly expected results.

  # Construct test data - artificial examples
  test_strings <- c(
    "abc",
    "abc_def g.h i[j",
    "klm_nOP_q(Rs)"
  )
  correct_res <- data.frame(
    no_camel = c(
      "abc",
      "abcdefghi[j",
      "klmnOPq(Rs)"
    ),
    camel = c(
      "abc",
      "abcDefGHI[j",
      "klmNopQ(rs)"
    )
  )
  correct_res$default <- correct_res$camel

  # Call the function.
  res_no_camel <- remove_special_characters(test_strings, FALSE)
  res_camel <- remove_special_characters(test_strings, TRUE)
  res_default <- remove_special_characters(test_strings)
  fn_res <- data.frame(
    no_camel = res_no_camel,
    camel = res_camel,
    default = res_default
  )

  # Compare test data vs function outputs.
  expect_equal(fn_res, correct_res)
})
