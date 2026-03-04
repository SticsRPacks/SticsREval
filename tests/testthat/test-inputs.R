# ===========================================================================
# Helpers
# ===========================================================================

make_usms_species <- function(
  usms = c("usm1", "usm2"),
  species = c("wheat", "maize")
) {
  data.frame(usm = usms, species = species)
}

# ===========================================================================
# Tests: load_workspace_sim
# ===========================================================================

test_that(
  "load_workspace_sim calls run_simulations when run_simulations = TRUE",
  {
    mock_run_sim  <- mock(list())
    mock_save_sim <- mock(NULL)

    stub(load_workspace_sim, "run_simulations", mock_run_sim)
    stub(load_workspace_sim, "save_sim",        mock_save_sim)
    stub(load_workspace_sim, "is_debug",        function() FALSE)

    load_workspace_sim(
      data_dir        = tempdir(),
      usms_species    = make_usms_species(),
      rotations       = NULL,
      workspace       = "/ws",
      run_simulations = TRUE,
      stics_exe       = "/stics",
      parallel        = FALSE,
      cores           = NA
    )

    expect_called(mock_run_sim, 1)
  }
)

test_that("load_workspace_sim passes correct arguments to run_simulations", {
  mock_run_sim  <- mock(list())
  mock_save_sim <- mock(NULL)

  stub(load_workspace_sim, "run_simulations", mock_run_sim)
  stub(load_workspace_sim, "save_sim",        mock_save_sim)
  stub(load_workspace_sim, "is_debug",        function() FALSE)

  load_workspace_sim(
    data_dir        = tempdir(),
    usms_species    = make_usms_species(c("usm1", "usm2")),
    rotations       = list(c("usm1", "usm2")),
    workspace       = "/ws",
    run_simulations = TRUE,
    stics_exe       = "/stics",
    parallel        = TRUE,
    cores           = 2
  )

  args <- mock_args(mock_run_sim)[[1]]
  expect_equal(args$stics_exe,  "/stics")
  expect_equal(args$workspace,  "/ws")
  expect_equal(args$usm_names,  c("usm1", "usm2"))
  expect_equal(args$successive, list(c("usm1", "usm2")))
  expect_equal(args$parallel,   TRUE)
  expect_equal(args$cores,      2)
})

test_that(
  "load_workspace_sim calls SticsRFiles::get_sim when run_simulations = FALSE",
  {
    mock_get_sim  <- mock(list())
    mock_save_sim <- mock(NULL)

    stub(load_workspace_sim, "SticsRFiles::get_sim", mock_get_sim)
    stub(load_workspace_sim, "save_sim",             mock_save_sim)
    stub(load_workspace_sim, "is_debug",             function() FALSE)

    load_workspace_sim(
      data_dir        = tempdir(),
      usms_species    = make_usms_species(),
      rotations       = NULL,
      workspace       = "/ws",
      run_simulations = FALSE,
      stics_exe       = NA,
      parallel        = FALSE,
      cores           = NA
    )

    expect_called(mock_get_sim, 1)
  }
)

test_that("load_workspace_sim passes correct arguments to get_sim", {
  mock_get_sim  <- mock(list())
  mock_save_sim <- mock(NULL)

  stub(load_workspace_sim, "SticsRFiles::get_sim", mock_get_sim)
  stub(load_workspace_sim, "save_sim",             mock_save_sim)
  stub(load_workspace_sim, "is_debug",             function() FALSE)

  load_workspace_sim(
    data_dir        = tempdir(),
    usms_species    = make_usms_species(c("usm1", "usm2")),
    rotations       = NULL,
    workspace       = "/ws",
    run_simulations = FALSE,
    stics_exe       = NA,
    parallel        = FALSE,
    cores           = NA
  )

  args <- mock_args(mock_get_sim)[[1]]
  expect_equal(args$workspace, "/ws")
  expect_equal(args$usm,       c("usm1", "usm2"))
  expect_equal(args$parallel,  FALSE)
})

test_that("load_workspace_sim calls save_sim with sim result", {
  fake_sim      <- list(usm1 = data.frame(x = 1))
  mock_save_sim <- mock(NULL)

  stub(load_workspace_sim, "run_simulations",      mock(fake_sim))
  stub(load_workspace_sim, "save_sim",             mock_save_sim)
  stub(load_workspace_sim, "is_debug",             function() FALSE)

  usms_species <- make_usms_species()
  load_workspace_sim(
    data_dir        = tempdir(),
    usms_species    = usms_species,
    rotations       = NULL,
    workspace       = "/ws",
    run_simulations = TRUE,
    stics_exe       = "/stics",
    parallel        = FALSE,
    cores           = NA
  )

  args <- mock_args(mock_save_sim)[[1]]
  expect_equal(args[[2]], fake_sim)
  expect_equal(args[[3]], usms_species)
})

# ===========================================================================
# Tests: load_workspace_obs
# ===========================================================================

test_that(
  "load_workspace_obs calls SticsRFiles::get_obs with correct arguments",
  {
    mock_get_obs  <- mock(list())
    mock_save_obs <- mock(NULL)

    stub(load_workspace_obs, "SticsRFiles::get_obs", mock_get_obs)
    stub(load_workspace_obs, "save_obs",             mock_save_obs)
    stub(load_workspace_obs, "is_debug",             function() FALSE)

    load_workspace_obs(
      data_dir     = tempdir(),
      usms_species = make_usms_species(c("usm1", "usm2")),
      workspace    = "/ws",
      parallel     = FALSE,
      cores        = NA
    )

    expect_called(mock_get_obs, 1)
    args <- mock_args(mock_get_obs)[[1]]
    expect_equal(args$workspace, "/ws")
    expect_equal(args$usm,       c("usm1", "usm2"))
    expect_equal(args$parallel,  FALSE)
  }
)

test_that(
  "load_workspace_obs calls save_obs with obs result and usms_species",
  {
    fake_obs      <- list(usm1 = data.frame(y = 1))
    mock_save_obs <- mock(NULL)

    stub(load_workspace_obs, "SticsRFiles::get_obs", mock(fake_obs))
    stub(load_workspace_obs, "save_obs",             mock_save_obs)
    stub(load_workspace_obs, "is_debug",             function() FALSE)

    usms_species <- make_usms_species()
    load_workspace_obs(
      data_dir     = tempdir(),
      usms_species = usms_species,
      workspace    = "/ws",
      parallel     = FALSE,
      cores        = NA
    )

    args <- mock_args(mock_save_obs)[[1]]
    expect_equal(args[[2]], fake_obs)
    expect_equal(args[[3]], usms_species)
  }
)

test_that("load_workspace_obs uses unique usms from usms_species", {
  mock_get_obs  <- mock(list())
  mock_save_obs <- mock(NULL)

  stub(load_workspace_obs, "SticsRFiles::get_obs", mock_get_obs)
  stub(load_workspace_obs, "save_obs",             mock_save_obs)
  stub(load_workspace_obs, "is_debug",             function() FALSE)

  usms_species <- data.frame(
    usm = c("usm1", "usm1", "usm2"),
    species = c("wheat", "maize", "wheat")
  )
  load_workspace_obs(
    data_dir     = tempdir(),
    usms_species = usms_species,
    workspace    = "/ws",
    parallel     = FALSE,
    cores        = NA
  )

  args <- mock_args(mock_get_obs)[[1]]
  expect_equal(args$usm, c("usm1", "usm2"))
})

# ===========================================================================
# Tests: extract_species_from_usms
# ===========================================================================

test_that(
  "extract_species_from_usms returns a data frame with usm and species columns",
  {
    stub(
      extract_species_from_usms,
      "parallelizable_loop",
      function(n, par, cores, fn) {
        lapply(seq_len(n), fn)
      }
    )
    stub(
      extract_species_from_usms,
      "SticsRFiles::get_plant_txt",
      function(workspace) {
        list(codeplante = "wheat")
      }
    )

    result <- extract_species_from_usms(
      usms      = c("usm1", "usm2"),
      workspace = "/ws",
      parallel  = FALSE,
      cores     = NA
    )

    expect_s3_class(result, "data.frame")
    expect_true("usm"     %in% names(result))
    expect_true("species" %in% names(result))
  }
)

test_that("extract_species_from_usms returns one row per usm", {
  stub(
    extract_species_from_usms,
    "parallelizable_loop",
    function(n, par, cores, fn) {
      lapply(seq_len(n), fn)
    }
  )
  stub(
    extract_species_from_usms,
    "SticsRFiles::get_plant_txt",
    function(workspace) {
      list(codeplante = "wheat")
    }
  )

  result <- extract_species_from_usms(
    usms      = c("usm1", "usm2", "usm3"),
    workspace = "/ws",
    parallel  = FALSE,
    cores     = NA
  )

  expect_equal(nrow(result), 3)
})

test_that(
  "extract_species_from_usms passes correct workspace path to get_plant_txt",
  {
    mock_get_plant <- mock(list(codeplante = "wheat"))

    stub(
      extract_species_from_usms,
      "parallelizable_loop",
      function(n, par, cores, fn) {
        lapply(seq_len(n), fn)
      }
    )
    stub(
      extract_species_from_usms,
      "SticsRFiles::get_plant_txt",
      mock_get_plant
    )

    extract_species_from_usms(
      usms      = c("usm1"),
      workspace = "/ws",
      parallel  = FALSE,
      cores     = NA
    )

    args <- mock_args(mock_get_plant)[[1]]
    expect_equal(args$workspace, file.path("/ws", "usm1"))
  }
)

test_that("extract_species_from_usms maps species correctly to usms", {
  stub(
    extract_species_from_usms,
    "parallelizable_loop",
    function(n, par, cores, fn) {
      lapply(seq_len(n), fn)
    }
  )
  stub(
    extract_species_from_usms,
    "SticsRFiles::get_plant_txt",
    function(workspace) {
      if (grepl("usm1", workspace)) {
        list(codeplante = "wheat")
      } else {
        list(codeplante = "maize")
      }
    }
  )

  result <- extract_species_from_usms(
    usms      = c("usm1", "usm2"),
    workspace = "/ws",
    parallel  = FALSE,
    cores     = NA
  )

  expect_equal(result$species[result$usm == "usm1"], "wheat")
  expect_equal(result$species[result$usm == "usm2"], "maize")
})

# ===========================================================================
# Tests: get_rotation_list
# ===========================================================================

make_metadata_file <- function(content) {
  tmp <- tempfile(fileext = ".csv")
  writeLines(content, tmp)
  tmp
}

test_that("get_rotation_list returns an empty list when no rotations", {
  f <- make_metadata_file(c(
    "usm;rotation;rotation_order",
    "usm1;0;0",
    "usm2;0;0"
  ))
  stub(get_rotation_list, "is_debug", function() FALSE)

  result <- get_rotation_list(f)
  expect_equal(length(result), 0)
})

test_that("get_rotation_list returns one vector per rotation group", {
  f <- make_metadata_file(c(
    "usm;rotation;rotation_order",
    "usm1;1;1",
    "usm2;1;2",
    "usm3;2;1",
    "usm4;2;2"
  ))
  stub(get_rotation_list, "is_debug", function() FALSE)

  result <- get_rotation_list(f)
  expect_equal(length(result), 2)
})

test_that("get_rotation_list orders usms within a rotation by rotation_order", {
  f <- make_metadata_file(c(
    "usm;rotation;rotation_order",
    "usm2;1;2",
    "usm1;1;1"
  ))
  stub(get_rotation_list, "is_debug", function() FALSE)

  result <- get_rotation_list(f)
  expect_equal(result[[1]], c("usm1", "usm2"))
})

test_that("get_rotation_list ignores usms with rotation = 0", {
  f <- make_metadata_file(c(
    "usm;rotation;rotation_order",
    "usm1;1;1",
    "usm2;0;0",
    "usm3;1;2"
  ))
  stub(get_rotation_list, "is_debug", function() FALSE)

  result <- get_rotation_list(f)
  expect_equal(length(result), 1)
  expect_false("usm2" %in% result[[1]])
})

test_that("get_rotation_list groups usms correctly across multiple rotations", {
  f <- make_metadata_file(c(
    "usm;rotation;rotation_order",
    "usm1;1;1",
    "usm2;1;2",
    "usm3;2;1"
  ))
  stub(get_rotation_list, "is_debug", function() FALSE)

  result <- get_rotation_list(f)
  expect_equal(result[[1]], c("usm1", "usm2"))
  expect_equal(result[[2]], c("usm3"))
})

test_that("get_rotation_list throws an error when file does not exist", {
  expect_error(
    get_rotation_list("/nonexistent/path/metadata.csv"),
    regexp = "not found"
  )
})

test_that(
  "get_rotation_list throws an error when required columns are missing",
  {
    f <- make_metadata_file(c(
      "usm;campaign",   # manque rotation et rotation_order
      "usm1;2024"
    ))
    stub(get_rotation_list, "is_debug", function() FALSE)

    expect_error(get_rotation_list(f), regexp = "Missing columns")
  }
)

test_that(
  "get_rotation_list throws an error when only some columns are missing",
  {
    f <- make_metadata_file(c(
      "usm;rotation",   # manque rotation_order
      "usm1;1"
    ))
    stub(get_rotation_list, "is_debug", function() FALSE)

    expect_error(get_rotation_list(f), regexp = "rotation_order")
  }
)

test_that("get_rotation_list throws an error when file is empty", {
  f <- make_metadata_file(character(0))
  stub(get_rotation_list, "is_debug", function() FALSE)

  expect_error(get_rotation_list(f))
})

test_that("get_rotation_list returns empty list when file has only a header", {
  f <- make_metadata_file("usm;rotation;rotation_order")
  stub(get_rotation_list, "is_debug", function() FALSE)

  result <- get_rotation_list(f)
  expect_equal(length(result), 0)
})

test_that(
  "get_rotation_list throws an error when rotation column is not numeric",
  {
    f <- make_metadata_file(c(
      "usm;rotation;rotation_order",
      "usm1;abc;1"
    ))
    stub(get_rotation_list, "is_debug", function() FALSE)

    expect_error(get_rotation_list(f))
  }
)

test_that(
  "get_rotation_list excludes USMs with NA rotation",
  {
    f <- make_metadata_file(c(
      "usm;rotation;rotation_order",
      "usm1;1;1",
      "usm2;NA;1"
    ))
    stub(get_rotation_list, "is_debug", function() FALSE)

    result <- get_rotation_list(f)
    usms <- unlist(result)
    expect_true("usm1" %in% usms)
    expect_false("usm2" %in% usms)
  }
)