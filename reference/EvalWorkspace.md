# EvalWorkspace class

Internal class. Manages the evaluation workspace (a Parquet-backed
staging area for simulation/observation/reference data), used internally
by
[`evaluate`](https://sticsrpacks.github.io/SticsREval/reference/evaluate.md).

## Methods

### Public methods

- [`EvalWorkspace$new()`](#method-EvalWorkspace-initialize)

- [`EvalWorkspace$save_sim()`](#method-EvalWorkspace-save_sim)

- [`EvalWorkspace$save_obs()`](#method-EvalWorkspace-save_obs)

- [`EvalWorkspace$save_ref_sim()`](#method-EvalWorkspace-save_ref_sim)

- [`EvalWorkspace$get_species()`](#method-EvalWorkspace-get_species)

- [`EvalWorkspace$get_species_situations()`](#method-EvalWorkspace-get_species_situations)

- [`EvalWorkspace$save_species_usm()`](#method-EvalWorkspace-save_species_usm)

- [`EvalWorkspace$get_sim()`](#method-EvalWorkspace-get_sim)

- [`EvalWorkspace$get_obs()`](#method-EvalWorkspace-get_obs)

- [`EvalWorkspace$get_ref_sim()`](#method-EvalWorkspace-get_ref_sim)

- [`EvalWorkspace$remove_init_obs()`](#method-EvalWorkspace-remove_init_obs)

- [`EvalWorkspace$remove_all_sim()`](#method-EvalWorkspace-remove_all_sim)

- [`EvalWorkspace$remove_all_obs()`](#method-EvalWorkspace-remove_all_obs)

- [`EvalWorkspace$clone()`](#method-EvalWorkspace-clone)

------------------------------------------------------------------------

### `EvalWorkspace$new()`

Create an evaluation workspace manager

#### Usage

    EvalWorkspace$new(data_dir)

#### Arguments

- `data_dir`:

  Path to the evaluation workspace

- `version`:

  Optional, the version of STICS to use for the data extraction. If not
  defined the last evaluated version will be used.

------------------------------------------------------------------------

### `EvalWorkspace$save_sim()`

Save simulations

#### Usage

    EvalWorkspace$save_sim(sim, usms_species)

#### Arguments

- `sim`:

  the list of simulations

- `usms_species`:

  a dataframe which associates a USM to its species

------------------------------------------------------------------------

### `EvalWorkspace$save_obs()`

Save observations

#### Usage

    EvalWorkspace$save_obs(obs, usms_species)

#### Arguments

- `obs`:

  the list of observations

- `usms_species`:

  a dataframe which associates a USM to its species

------------------------------------------------------------------------

### `EvalWorkspace$save_ref_sim()`

Save reference simulations

#### Usage

    EvalWorkspace$save_ref_sim(ref_sim, usms_species)

#### Arguments

- `ref_sim`:

  the list of reference simulations

- `usms_species`:

  a dataframe which associates a USM to its species

------------------------------------------------------------------------

### `EvalWorkspace$get_species()`

Returns the list of species

#### Usage

    EvalWorkspace$get_species()

#### Returns

a list of species as character list

------------------------------------------------------------------------

### `EvalWorkspace$get_species_situations()`

Returns the list of USMs of a species

#### Usage

    EvalWorkspace$get_species_situations(species, usms = NULL)

#### Arguments

- `species`:

  the species to search for

- `usms`:

  Optional, filter the USMs returned by the function

#### Returns

a list of USMs as a character list

------------------------------------------------------------------------

### `EvalWorkspace$save_species_usm()`

Save the association between species and USMs

#### Usage

    EvalWorkspace$save_species_usm(species_usms)

#### Arguments

- `species_usms`:

  a dataframe with two columns: situation and species the situation
  column should contain the USM and the species column should contain
  the associated species

------------------------------------------------------------------------

### `EvalWorkspace$get_sim()`

Return the simulation

#### Usage

    EvalWorkspace$get_sim(
      species = NULL,
      usms = NULL,
      var2exclude = NULL,
      collect = TRUE
    )

#### Arguments

- `species`:

  Optional, if defined filter the simulations with this species

- `usms`:

  Optional, if defined filter the simulations with these USMs

- `var2exclude`:

  Optional, if defined remove the variables from the returned
  simulations

- `collect`:

  Optional, if `TRUE` a dataframe will be returned, otherwise the lazy
  arrow data object will be returned

#### Returns

a dataframe if collect is `TRUE`, a lazy arrow data object

------------------------------------------------------------------------

### `EvalWorkspace$get_obs()`

Return the observation

#### Usage

    EvalWorkspace$get_obs(
      species = NULL,
      usms = NULL,
      var2exclude = NULL,
      collect = TRUE
    )

#### Arguments

- `species`:

  Optional, if defined filter the observations with this species

- `usms`:

  Optional, if defined filter the observations with these USMs

- `var2exclude`:

  Optional, if defined remove the variables from the returned
  observations

- `collect`:

  Optional, if `TRUE` a dataframe will be returned, otherwise the lazy
  arrow data object will be returned

#### Returns

a dataframe if collect is `TRUE`, a lazy arrow data object

------------------------------------------------------------------------

### `EvalWorkspace$get_ref_sim()`

Return the reference simulation

#### Usage

    EvalWorkspace$get_ref_sim(
      species = NULL,
      usms = NULL,
      var2exclude = NULL,
      collect = TRUE
    )

#### Arguments

- `species`:

  Optional, if defined filter the reference simulations with this
  species

- `usms`:

  Optional, if defined filter the reference simulations with these USMs

- `var2exclude`:

  Optional, if defined remove the variables from the returned reference
  simulations

- `collect`:

  Optional, if `TRUE` a dataframe will be returned, otherwise the lazy
  arrow data object will be returned

------------------------------------------------------------------------

### `EvalWorkspace$remove_init_obs()`

Remove the initial observations (HR_1 to HR_5, AZnit_1 to AZnit_5,
resmes and azomes) from the observations dataset This function is used
to remove the initial observations from the dataset, which are not
relevant for the evaluation and can bias the results. The initial
observations are defined as the observations of the first date of each
situation (USM) for the variables HR_1 to HR_5 and AZnit_1 to AZnit_5,
resmes and azomes. This function will replace the initial observations
by NA in the dataset.

#### Usage

    EvalWorkspace$remove_init_obs()

------------------------------------------------------------------------

### `EvalWorkspace$remove_all_sim()`

Remove all the simulations from the evaluation workspace

#### Usage

    EvalWorkspace$remove_all_sim()

------------------------------------------------------------------------

### `EvalWorkspace$remove_all_obs()`

Remove all the observations from the evaluation workspace

#### Usage

    EvalWorkspace$remove_all_obs()

------------------------------------------------------------------------

### `EvalWorkspace$clone()`

The objects of this class are cloneable with this method.

#### Usage

    EvalWorkspace$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
