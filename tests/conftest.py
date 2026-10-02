from pathlib import Path

import pytest

from windpower.config import Config, load_config

ROOT = Path(__file__).parents[1]


@pytest.fixture(scope="session")
def cfg() -> Config:
    """Project configuration with paths resolved relative to the repository root."""
    cfg = load_config(ROOT / "config.yaml")
    paths = {k: ROOT / v for k, v in cfg.paths.model_dump().items()}
    return cfg.model_copy(update={"paths": cfg.paths.model_copy(update=paths)})
