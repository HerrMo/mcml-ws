# Workshop Session: Dependency Management and Project Tooling in Python

This session uses a small, deliberately naive analysis of German wind power production (`src/windpower`).
The analysis is not the point; the point is making the project **installable, re-runnable and checkable by someone else**.

## Prerequisites

- `git`, a GitHub account, a text editor/IDE.
- [yolobox](https://github.com/finbarr/yolobox) or any other agent sandboxing tool you like if you want to use a coding agent. It is mandatory for this exercise to use your knowledge about agent sandboxing from the previous sessions.
- [uv](https://docs.astral.sh/uv/getting-started/installation/) ≥ 0.12. uv installs Python itself; no system Python is needed.
- Fork this repository on GitHub and clone your fork.

Commands below are run from the repository root.
Each exercise lists **tasks**, **questions** to discuss, and a **check** that tells you whether you are done.
Solutions are in the `python` branch; exercise-specific solution code in `src/` is enclosed in `# SOLUTION` / `# SOLUTION END` markers.

---

## Exercise 0: Orientation

Tasks:

1. Look at the layout:

   ```text
   config.yaml            parameters of the analysis
   pyproject.toml         metadata, dependencies, tool configuration
   uv.lock                fully resolved dependency versions (generated)
   .python-version        interpreter version used for development
   src/windpower/         the package
   tests/                 pytest test suite
   data/raw/              input data, never modified
   data/intermediate/     cache, can always be deleted
   plots/                 output, can always be regenerated
   docs/, mkdocs.yml      documentation
   .github/               issue templates, CI workflows, dependabot
   ```

2. Read `README.md` and `src/windpower/pipeline.py` to see what the analysis does.

Questions:

- Which directories are tracked by git and which are ignored (`.gitignore`)? Why is `data/intermediate/` ignored but `data/raw/` tracked?
- Why is the code in `src/windpower/` instead of top-level scripts?

---

## Exercise 1: Reproducing an old environment

The original author shipped `workshop/requirements-2023.txt`, the output of `pip freeze` on their machine in 2023.
Your task is to get the project running with it again.

Tasks:

1. Try to install it into a fresh environment with the current default Python (as given inside `.python-version`)

   ```bash
   uv venv --python 3.13 .venv-2023
   uv pip install --python .venv-2023 -r workshop/requirements-2023.txt
   ```

   Read the resolver error carefully.
2. Find an interpreter version for which the file is installable. Install it, then install the project itself without letting the installer touch the pinned versions:

   ```bash
   uv venv --python 3.11 .venv-2023
   uv pip install --python .venv-2023 -r workshop/requirements-2023.txt
   uv pip install --python .venv-2023 --no-deps -e .
   .venv-2023/bin/python -m pytest       # Windows: .venv-2023\Scripts\python
   ```

3. Run the analysis in the 2023 virtual environment and in the current one (`uv run windpower`), writing to different output directories (copy `config.yaml` and change `paths.plots` and `paths.intermediate`). Compare the two `metrics.csv` files.
4. Install an additional package into the *project* environment the pip way, and see what uv records:

   ```bash
   uv sync
   uv pip install seaborn                     # nice plotting library
   git status                                 # pyproject.toml, uv.lock?
   uv run python -c "import seaborn"          # works
   uv sync
   uv run python -c "import seaborn"          # ?
   ```

Questions:

- The resolver reports `contourpy==1.1.1 depends on numpy>=1.26.0rc1,<2.0` on 3.13, but not on 3.11. How can a single release of a package have different dependencies? (Hint: wheels per interpreter, environment markers.)
- Which lines of the file `workshop/requirements-2023.txt` are *direct* dependencies of `windpower`, which are packages only installed because a direct dependency requires them? Can you tell from the file? Can you tell when using `uv`?
- In the `rmse` column, the least-squares fit differs in the 15th significant digit between the two environments; the other two models agree exactly. Where can such differences come from? When do they matter?
- What information would you need, beyond the file `workshop/requirements-2023.txt`, to rebuild this environment exactly? Does `uv` resolve all of these concerns?
- After `uv pip install seaborn`, neither `pyproject.toml` nor `uv.lock` mention seaborn, but `uv run` still finds it; the next `uv sync` removes it again. Why is installing into a project environment with `uv pip install` bad practice? What happens when a collaborator clones the repository and runs code that imports seaborn? Which command should you use instead? When is `uv pip` appropriate (think of `.venv-2023`)?

Check: the test suite passes in `.venv-2023`, and you can explain why the file fails on 3.13.

---

## Exercise 2: Dependency management with uv

Goal: replace the pip freeze list `workshop/requirements-2023.txt` by a declared set of direct dependencies (`pyproject.toml`) and a universal lockfile (`uv.lock`).

Start from a clean slate on a separate branch (the solution stays available on the `python` branch):

```bash
git switch -c uv-from-scratch
git rm -q pyproject.toml uv.lock .python-version
rm -rf .venv
uv init --name windpower
```

`uv init` creates a `pyproject.toml` with a `src/` layout and a build system; existing files in `src/` and `README.md` are left untouched.
Fix the generated entry point in `[project.scripts]` to `windpower = "windpower.cli:main"`.
At the end of the exercise, compare with the solution (`git diff python -- pyproject.toml`) and copy the `[tool.ruff*]` and `[tool.pytest.ini_options]` sections, which configure the tools used in exercises 4–6.

Tasks:

1. **Pin the development interpreter**: `uv python pin 3.13` writes `.python-version`. Set `requires-python = ">=3.11"` in `pyproject.toml`.
   What is the difference between the two?
2. **Declare direct dependencies only.** Find all third-party imports:

   ```bash
   grep -rhoE "^(import|from) [a-z_]+" src | sort -u
   ```

   Add each with `uv add <name>` (import name ≠ distribution name: `yaml` → `pyyaml`, `sklearn` → `scikit-learn`). The standard library (`json`, `zipfile`, ...) needs no declaration.
   `joblib` is installed anyway as a dependency of scikit-learn. Should you still declare it?
3. **Dependency groups**: tools needed for development but not for using the package go into groups:

   ```bash
   uv add --dev pytest pytest-cov ruff pre-commit
   uv add --group docs mkdocs-material "mkdocstrings[python]"
   ```

4. **Inspect the lockfile**:
   - `uv tree`, `uv tree --outdated`, `uv tree --invert --package numpy`
   - Search `uv.lock` for `name = "numpy"`. Why are there two entries? Look at `resolution-markers`.
   - `uv lock --check` exits non-zero if `uv.lock` does not match `pyproject.toml`.
5. **Use the environment**: `uv sync --locked`, `uv run pytest`, `uv run windpower`. Delete `.venv/` and repeat. How long does it take?
6. **Upper bounds**: `uv run --group docs mkdocs build` prints a warning about MkDocs 2.0, which removes the plugin system that mkdocstrings depends on. Constrain it with `uv add --group docs "mkdocs>=1.6,<2"`.
   When is an upper bound justified, and when does it cause harm? (Consider an application vs. a library that others install alongside their own dependencies.)
7. **Lower bounds**: `uv add pandas` records the newest version as lower bound (`pandas>=3.0.6`), so the package cannot be installed next to anything that needs pandas 2.
   - Simulate resolving at an earlier date: `uv lock --exclude-newer 2025-06-01 --dry-run`. Why does it fail?
   - Exercise 1 showed that the code works with the 2023 versions. Relax the bounds accordingly (`"pandas>=2.0"`, `"numpy>=1.24"`, ...) and test them: every direct dependency at its lowest allowed version, on the lowest supported Python:

     ```bash
     uv run --python 3.11 --resolution lowest-direct --isolated pytest
     ```

     Why does the same command fail with `--python 3.13`? The CI job `test-lowest` runs this check.
   - Updating: `uv lock --upgrade-package pandas` updates a single package in `uv.lock`, `uv lock --upgrade` all of them.
8. **Interoperability**: `uv export --no-dev --format requirements.txt > requirements.txt` gives a pinned file for people without uv (`pip install -r requirements.txt`). Compare with `workshop/requirements-2023.txt`: what does the exported file contain that the freeze did not (hashes, markers)?

Questions:

- Which files do you commit: `pyproject.toml`, `uv.lock`, `.python-version`, `.venv/`?
- A collaborator runs `uv add seaborn` on their branch and you run `uv add statsmodels` on yours. What happens when you merge? How do you resolve a conflict in `uv.lock`?

Check: `rm -rf .venv && uv sync --locked && uv run pytest` succeeds, and `uv lock --check` passes.

---

## Exercise 3: Randomness: there is no global seed

In Python there is no single switch that makes a program's random numbers reproducible (in R, `set.seed()` is enough to seed everything).
Every library can bring its own random number generator (RNG) with its own state and its own seeding mechanism:

| Library                   | Mechanism                                                                                               |
| ------------------------- | ------------------------------------------------------------------------------------------------------- |
| `random` (standard lib)   | hidden global instance, seeded with `random.seed()`                                                     |
| NumPy, legacy interface   | hidden global `RandomState`, seeded with `np.random.seed()`, used by `np.random.normal()`, ...          |
| NumPy, current interface  | explicit `Generator` objects: `rng = np.random.default_rng(seed)`, then `rng.normal()`, ...             |
| scikit-learn              | `random_state=` argument of each estimator and splitter (int or `RandomState`); `None` falls back to NumPy's legacy global state |
| pandas                    | `random_state=` argument, e.g. `df.sample(random_state=...)` (int or `Generator`)                       |
| PyTorch (not used here)   | `torch.manual_seed()`, `torch.Generator`, separate state per CUDA device                                |

NumPy recommends the `Generator` interface for new code: a generator is an ordinary object that is created from a seed and passed to where it is needed, instead of state hidden in a module.

Tasks:

1. **Separate global states.** Try to guess the output, then run it with `uv run python`:

   ```python
   import random
   import numpy as np
   from sklearn.model_selection import KFold


   def folds():
       return [test.tolist() for _, test in KFold(3, shuffle=True).split(range(6))]


   random.seed(1)
   print(np.random.random(), np.random.default_rng().random(), folds())
   random.seed(1)
   print(np.random.random(), np.random.default_rng().random(), folds())

   np.random.seed(1)
   print(np.random.default_rng().random(), folds())
   np.random.seed(1)
   print(np.random.default_rng().random(), folds())
   ```

   Which calls are affected by `random.seed()`, which by `np.random.seed()`, and which by neither?
2. **Generators.** Replace the global calls by explicit generators: `rng = np.random.default_rng(1)`, then `rng.random()`. Create two generators from the same seed and check that they produce the same stream. Does drawing from one advance the other?
3. **Sources of randomness in this project.** Find them with `grep -rnE "seed|random_state|default_rng" src tests`. For each one, which mechanism is used? Is there any randomness that is *not* controlled by `model.seed` in `config.yaml`?
4. **Solution: one seed, many streams.** `config.yaml` records a single seed, `model.seed`. Every random component of the pipeline gets its seed from it:
   `pipeline.run()` calls `derive_seeds(cfg.model.seed, operators)` in `evaluate.py` once and passes the resulting seeds explicitly to the cross-validation folds of the penalized model, the random forest, the bootstrap interval of the RMSE, and the penalized model of each operator.
   `derive_seeds()` spawns statistically independent child streams with `np.random.SeedSequence(seed).spawn()`; no function seeds or draws from a global state.
   - Read `derive_seeds()` and `pipeline.run()`. Change `model.seed` and rerun `uv run windpower`. Which outputs change, which do not?
   - Try `KFold(3, shuffle=True, random_state=np.random.default_rng(1))`. Why does `derive_seeds()` return integers instead of generators?
   - Why not simply use `seed`, `seed + 1`, `seed + 2`, ...?
   - `derive_seeds()` returns a `Seeds` object with named fields instead of a list that is unpacked by position. What can go wrong with positional unpacking when a random component is added?

Questions:

- Do the random numbers of a `Generator` with a fixed seed stay the same when you upgrade NumPy? Read the [compatibility policy](https://numpy.org/doc/stable/reference/random/compatibility.html). What does this imply for pinning dependency versions (exercise 2)?
- A seed makes a computation reproducible, but results can still depend on it. How would you check whether a conclusion of the analysis depends on `model.seed`?

Check: you can name the seed that controls each random component of the pipeline, and `grep -rnE "^[^#]*(np\.random\.(seed|rand|normal|choice)|random\.seed)" src` finds nothing outside of comments.

---

## Exercise 4: Linting and formatting with ruff and pre-commit

Tasks:

1. Run `uv run ruff check .` and `uv run ruff format --check .`. The rule selection is in `[tool.ruff.lint]` in `pyproject.toml`; look up what `B`, `UP`, `NPY`, `PD` and `S` check (`uv run ruff rule B006`, `uv run ruff rule NPY002`).
2. Introduce violations and let ruff fix them: an unused import, a mutable default argument (`def f(x=[])`), unsorted imports, a line of 150 characters. Which are fixed automatically (`ruff check --fix`), which not?
3. Install the git hooks: `uv run pre-commit install`. Read `.pre-commit-config.yaml`, then commit a file with trailing whitespace. What happens?
4. Try to commit a 1 MB file outside of `data/raw/`. Why is `data/raw/` excluded from `check-added-large-files`? Would you rather use Git LFS or an external data repository here?
5. Add a dependency with `uv add` but do not commit `uv.lock`; edit `pyproject.toml` by hand and commit. What does the `uv-lock` hook do?
6. `uv run pre-commit autoupdate` updates the hook versions. The ruff version in `.pre-commit-config.yaml` and the ruff version in the `dev` group can drift apart. What is the consequence? How can you keep them in sync?

Check: `uv run pre-commit run --all-files` passes.
