context("build_query")

test_that("build_query works", {
  expect_equal(build_query('a'),
               'a{*}')
  expect_equal(build_query('a', list(foo = 'bar')),
               'a{foo:bar}')
  expect_equal(build_query('a', list(foo = 'bar'), 'zzz'),
               'a{foo:bar}by{zzz}')
  expect_equal(build_query('system.disk.await', list(role = 'db', env = 'production'), c('host', 'device')),
               'system.disk.await{role:db,env:production}by{host,device}')
})
