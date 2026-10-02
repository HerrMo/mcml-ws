import pydantic
import pytest
import yaml

from windpower.config import load_config


def write(tmp_path, data):
    path = tmp_path / "config.yaml"
    path.write_text(yaml.safe_dump(data))
    return path


@pytest.fixture
def raw(cfg):
    return cfg.model_dump(mode="json")


def test_load_project_config(cfg):
    assert cfg.grid.n_cells == 16
    assert cfg.dates.start_train < cfg.dates.end_train
    assert cfg.paths.power.exists()


def test_roundtrip(tmp_path, raw, cfg):
    assert load_config(write(tmp_path, raw)) == cfg


def test_train_range_outside_data(tmp_path, raw):
    raw["dates"]["end_train"] = "2024-01-01"
    with pytest.raises(pydantic.ValidationError, match="outside of available data range"):
        load_config(write(tmp_path, raw))


def test_unsorted_bins(tmp_path, raw):
    raw["grid"]["bins_lat"] = [47, 51, 49]
    with pytest.raises(pydantic.ValidationError, match="strictly increasing"):
        load_config(write(tmp_path, raw))


def test_unknown_key(tmp_path, raw):
    raw["model"]["n_trees"] = 10
    with pytest.raises(pydantic.ValidationError, match="Extra inputs"):
        load_config(write(tmp_path, raw))
