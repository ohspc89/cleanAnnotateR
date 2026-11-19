import os
from pathlib import Path
import subprocess

from prefect import flow, task
from prefect.schedules import Cron

PROJECT_DIR = Path(__file__).resolve().parents[1]
CODE_DIR = PROJECT_DIR / "code"

R_PROJECT_DIR = Path(os.environ["QC_R_PROJECT_DIR"]).expanduser()


@task
def run_r_script(script: str, *args: str) -> None:
    subprocess.run(
        ["Rscript", str(CODE_DIR / script), *args],
        cwd=R_PROJECT_DIR,
        check=True,
    )


@flow(name="cleanAnnotateR QC", log_prints=True)
def qc_pipeline() -> None:
    run_r_script("fetch_ids.R")
    run_r_script("perform_qc.R")


if __name__ == "__main__":
    qc_pipeline.serve(
        name="local-qc",
        schedules=[
            Cron(
                "0 14 * * 1-5",
                timezone="America/Los_Angeles",
            )
        ],
    )
