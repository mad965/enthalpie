test_that("compter_atomes compte correctement les atomes", {
  expect_equal(compter_atomes("CH4", "C"), 1L)
  expect_equal(compter_atomes("CH4", "H"), 4L)
  expect_equal(compter_atomes("C2H5OH", "C"), 2L)
  expect_equal(compter_atomes("C2H5OH", "H"), 6L)  # 5 + 1 du OH
  expect_equal(compter_atomes("C2H5OH", "O"), 1L)
  expect_equal(compter_atomes("C6H12O6", "O"), 6L)
  expect_equal(compter_atomes("C12H22O11", "C"), 12L)
  expect_equal(compter_atomes("CO2", "C"), 1L)
  expect_equal(compter_atomes("CO2", "O"), 2L)
  expect_equal(compter_atomes("CH4", "N"), 0L)  # élément absent
})