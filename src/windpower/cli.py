"""Command line interface: ``windpower --config config.yaml``."""

from __future__ import annotations

import argparse
import logging

from windpower.config import DEFAULT_CONFIG, load_config
from windpower.pipeline import run


def main(argv: list[str] | None = None) -> None:
    parser = argparse.ArgumentParser(description="Run the wind power analysis.")
    parser.add_argument("-c", "--config", default=DEFAULT_CONFIG, help="YAML configuration file")
    args = parser.parse_args(argv)
    logging.basicConfig(level=logging.INFO, format="%(asctime)s %(name)s: %(message)s")
    metrics = run(load_config(args.config))
    print(metrics.to_string(index=False))


if __name__ == "__main__":
    main()
